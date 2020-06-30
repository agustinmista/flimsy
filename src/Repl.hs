{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Repl where

import Prelude hiding (readFile)

import System.Exit
import System.Process (rawSystem)
import System.Console.Repline

import Data.IORef

import Data.List
import Data.Text.Lazy (pack)
import Data.Text.Lazy.IO (readFile)

import Data.Foldable
import Control.Monad.State.Strict

import qualified Env as Env

import Syntax
import Parser
import Pretty
import Infer
import Eval
import Prim

----------------------------------------
-- | Repl internal state
----------------------------------------

-- | The internal state of the interpreter
data ReplState =
  ReplState
  { typeEnv     :: TypeEnv  -- from Infer
  , valueEnv    :: ValueEnv -- from Eval
  , loadedFiles :: [FilePath]
  }

initState :: ReplState
initState = ReplState
 { typeEnv = foldl Env.extend Env.empty (fmap fst <$> Env.toList primEnv)
 , valueEnv  = Env.empty
 , loadedFiles = []
 }

getTypeEnv :: MonadState ReplState m => m TypeEnv
getTypeEnv = gets typeEnv

getValueEnv :: MonadState ReplState m => m ValueEnv
getValueEnv = gets valueEnv

insertTypeEnv :: MonadState ReplState m => Var -> Scheme -> m ()
insertTypeEnv var tsc = modify' $ \st ->
  st { typeEnv = typeEnv st `Env.extend` (var, tsc) }

insertValueEnv :: MonadState ReplState m => Var -> IORef Thunk -> m ()
insertValueEnv var val = modify' $ \st ->
  st { valueEnv = valueEnv st `Env.extend` (var, val) }

-- | The Repl type
type Repl a = HaskelineT (StateT ReplState IO) a

-- | Abort the execution when an error is found
hoistError :: Pretty e => Either e a -> Repl a
hoistError (Right val) = return val
hoistError (Left err) = do
  liftIO $ putStrLn (pretty err)
  abort

----------------------------------------
-- | Processing inputs
----------------------------------------

processExpr :: Expr -> Repl ()
processExpr expr = do

  tenv <- getTypeEnv
  tsc  <- hoistError (typeOfExpr tenv expr)
  venv <- getValueEnv
  res  <- liftIO $ evaluate venv primEnv expr
  val  <- hoistError res

  liftIO $ putStrLn (pretty expr)
  liftIO $ putStrLn (": " <> pretty tsc)
  liftIO $ putStrLn ("= " <> pretty val)

processDecl :: Bool -> Decl -> Repl ()
processDecl interactive decl =
  case decl of
    BindD bind -> do
      let (var, expr) = getBind bind

      -- type check the body
      tenv <- getTypeEnv
      tsc <- hoistError (typeOfExpr tenv expr)
      insertTypeEnv var tsc

      -- create/update the evaluation thunk
      venv <- getValueEnv
      let th = mkThunk (evalExpr venv expr)
      case Env.lookup var venv of
        Nothing -> do
          ref <- liftIO $ newIORef th
          insertValueEnv var ref
        Just ref -> do
          when interactive $ do
            liftIO $ putStrLn ("overwriting " <> pretty var)
          updateThunk ref th

      -- report the type of the bind
      when interactive $ do
        liftIO $ putStrLn (pretty var <> " : " <> pretty tsc)

    -- not yet implemented
    _ -> do
      liftIO $ putStrLn ("ignoring: " <> pretty decl)

processFile :: FilePath -> Repl ()
processFile file = do
  contents <- liftIO $ readFile file
  decls <- hoistError (parseSourceFile file contents)
  mapM_ (processDecl False) decls

----------------------------------------
-- | Interactive evaluation
----------------------------------------

evalCmd :: String -> Repl ()
evalCmd source = do
  let input = pack source
  res <- hoistError (parseStdin input)
  case res of
    Left expr -> do
      processExpr expr
    Right decl ->
      processDecl True decl

----------------------------------------
-- | Commands
----------------------------------------

replCommands :: [(String, [String] -> Repl ())]
replCommands =
  [ ("load"   , loadCmd)
  , ("browse" , browseCmd)
  , ("type"   , typeCmd)
  , ("quit"   , quitCmd)
  , ("shell"  , shellCmd)
  , ("!"      , shellCmd)
  ]

-- :load
loadCmd :: [String] -> Repl ()
loadCmd args = dontCrash $ do
  let file = unwords args
  processFile file

-- :browse
browseCmd :: [String] -> Repl ()
browseCmd _ = do
  tenv <- getTypeEnv
  forM_ (Env.toList tenv) $ \(var, ty) -> do
    liftIO $ putStrLn (pretty var <> " : " <> pretty ty)

-- :type
typeCmd :: [String] -> Repl ()
typeCmd args = dontCrash $ do
  let input = pack (unwords args)
  expr <- hoistError (parseExpr input)
  tenv <- getTypeEnv
  tsc  <- hoistError (typeOfExpr tenv expr)
  liftIO $ putStrLn (pretty expr)
  liftIO $ putStrLn (": " <> pretty (tsc))

-- :quit
quitCmd :: [String] -> Repl ()
quitCmd _ = liftIO exitSuccess

-- :!
shellCmd :: [String] -> Repl ()
shellCmd input = dontCrash $ do
  case input of
    (cmd:args) -> do
      liftIO $ void (rawSystem cmd args)
    _ -> do
      liftIO $ putStrLn "empty shell command"

----------------------------------------
-- | Tab completion
----------------------------------------

completer :: WordCompleter (StateT ReplState IO)
completer n = do
  let cmds = [ ':' : cmd | cmd <- fst <$> replCommands ]
  venv <- getValueEnv
  let values = showVar <$> Env.keys venv
  return (filter (isPrefixOf n) (cmds <> values))

----------------------------------------
-- | Shell
----------------------------------------

shell :: Repl a -> IO ()
shell pre =
  flip evalStateT initState $
    evalRepl
      (pure "lang> ")
      evalCmd
      replCommands
      (Just ':')
      (Word completer `Combine` File)
      pre

----------------------------------------
-- | Top level
----------------------------------------

launchRepl :: Maybe FilePath -> IO ()
launchRepl Nothing  = shell (return ())
launchRepl (Just f) = shell (loadCmd [f])

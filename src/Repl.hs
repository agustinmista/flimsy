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

import Data.Set (Set)
import qualified Data.Set as Set
import Env (Env)
import qualified Env as Env
import qualified Prim as Prim

import Syntax
import Parser
import Escape
import Infer
import Eval
import Pretty

interactive :: FilePath
interactive = "<interactive>"

prelude :: FilePath
prelude = "lib/prelude.jm"

----------------------------------------
-- | Repl internal state
----------------------------------------

-- | Binds environment

data ReplBind =
  ReplBind
  { thunk  :: IORef Thunk
  , scheme :: Scheme
  , origin :: FilePath
  }

type ReplEnv = Env ReplBind
type SigEnv = Env Type

-- | Internal state

data ReplState =
  ReplState
  { binds  :: ReplEnv
  , prims  :: PrimEnv
  , loaded :: Set FilePath
  }

initState :: ReplState
initState = ReplState
  { binds  = Env.empty
  , prims  = Prim.environment
  , loaded = Set.fromList [prelude, interactive]
  }

getReplState :: MonadState ReplState m => m ReplState
getReplState = get

getReplBinds :: MonadState ReplState m => m [(Var, ReplBind)]
getReplBinds = Env.toList <$> gets binds

getTcEnv :: MonadState ReplState m => m TcEnv
getTcEnv = do
  bindTys <- fmap scheme <$> gets binds
  primTys <- fmap primTy <$> gets prims
  return (Env.merge bindTys primTys)

getEvalEnv :: MonadState ReplState m => m EvalEnv
getEvalEnv = fmap thunk <$> gets binds

----------------------------------------
-- | The Read-Eval-Print-Loop monad
----------------------------------------

-- | The Repl type
type Repl a = HaskelineT (StateT ReplState IO) a

storeBind :: Var -> Thunk -> Scheme -> FilePath -> Repl ()
storeBind var th tsc file = do
  st <- getReplState
  case Env.lookup var (binds st) of
    Nothing -> do
      ref <- liftIO $ newIORef th
      let st' = st { binds = binds st `Env.extend` (var, ReplBind ref tsc file) }
      put st'
    Just bind -> do
      liftIO $ putStrLn ("overwriting " <> pretty var)
      let st' = st { binds = binds st `Env.extend` (var, ReplBind (thunk bind) tsc file) }
      updateThunk (thunk bind) th
      put st'

-- | Abort the execution when an error is found
hoistError :: Pretty e => Either e a -> Repl a
hoistError (Right val) = return val
hoistError (Left err) = do
  liftIO $ putStrLn (pretty err)
  abort

----------------------------------------
-- | Processing inputs
----------------------------------------

processFile :: FilePath -> Repl ()
processFile file = do
  contents <- liftIO $ readFile file
  decls <- hoistError (parseSourceFile file contents)

  tenv <- getTcEnv
  sortedDecls <- hoistError (tryTopSort tenv decls)

  mapM_ (processDecl (Just file)) sortedDecls

  st <- getReplState
  let st' = st { loaded = Set.insert file (loaded st) }
  put st'

  liftIO $ putStrLn ("Ok, files loaded:")
  let files = Set.filter (\f -> f /= interactive) (loaded st')
  forM_ files $ \f -> do
    liftIO $ putStrLn f

processDecl :: Maybe String -> Decl -> Repl ()
processDecl mbf (BindD bind) = processBind mbf bind

processBind :: Maybe String -> Bind -> Repl ()
processBind mbf bind = do
  let (var, expr) = getBind bind
  -- type check the body
  tenv <- getTcEnv
  tsc <- hoistError (typeOfExpr tenv expr)
  -- create/update the evaluation thunk
  venv <- getEvalEnv
  let file = maybe interactive id mbf
  let th = mkThunk (evalExpr venv expr)
  storeBind var th tsc file

processExpr :: Expr -> Repl ()
processExpr expr = do
  -- type check the expression
  tenv <- getTcEnv
  void $ hoistError (typeOfExpr tenv expr)
  -- evaluate it
  venv <- getEvalEnv
  res  <- liftIO $ evaluate venv Prim.environment expr
  val  <- hoistError res
  -- print it
  liftIO $ putStrLn (pretty val)

----------------------------------------
-- | Interactive evaluation
----------------------------------------

evalCmd :: String -> Repl ()
evalCmd source = do
  let input = pack source
  res <- hoistError (parseStdin interactive input)
  case res of
    Left expr -> do
      processExpr expr
    Right decl ->
      processDecl Nothing decl

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
  , ("info"   , infoCmd)
  ]

-- :load
loadCmd :: [String] -> Repl ()
loadCmd input = dontCrash $ do
  case input of
    [] -> unloadAll
    [file] -> processFile file
    _ -> liftIO $ putStrLn "load modules one by one!"


unloadAll :: Repl ()
unloadAll = do
  let keep = [prelude, Prim.boot]
  modify' $ \st ->
    st { binds = Env.filter (\b -> origin b `elem` keep) (binds st)
       , loaded = Set.filter (\f -> f `elem` keep) (loaded st)
       }
  liftIO $ putStrLn ("Ok, no modules loaded.")

-- :browse
browseCmd :: [String] -> Repl ()
browseCmd input = do
  env <- getReplBinds
  let filtered =
        case input of
          [] -> env
          xs -> filter (\(_,b) -> (origin b) `elem` xs) env
  forM_ filtered $ \(var, bind) -> do
    liftIO $ putStrLn (pretty var <> " : " <> pretty (scheme bind))

-- :type
typeCmd :: [String] -> Repl ()
typeCmd input = dontCrash $ do
  let str = pack (unwords input)
  expr <- hoistError (parseExpr interactive str)
  tenv <- getTcEnv
  tsc  <- hoistError (typeOfExpr tenv expr)
  liftIO $ putStrLn (pretty expr <> " : " <> pretty (tsc))

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

-- :info
infoCmd :: [String] -> Repl ()
infoCmd args = do
  env <- getReplState
  forM_ args $ \arg -> do
    case Env.lookup (mkVar arg) (binds env) of
      Nothing -> do
        liftIO $ putStrLn ("not in scope: " <> arg)
      Just (ReplBind _ ty file) -> do
        liftIO $ putStrLn (arg <> " : " <> pretty ty <> "  -- defined at " <> file)

----------------------------------------
-- | Tab completion
----------------------------------------

completer :: WordCompleter (StateT ReplState IO)
completer n = do
  let cmds = [ ':' : cmd | cmd <- fst <$> replCommands ]
  idents <- getReplBinds
  let values = showVar . fst <$> idents
  return (filter (isPrefixOf n) (cmds <> values))

----------------------------------------
-- | Top level
----------------------------------------

launchRepl :: Maybe FilePath -> IO ()
launchRepl Nothing  = shell (printBanner >> processFile prelude >> return ())
launchRepl (Just f) = shell (printBanner >> processFile prelude >> loadCmd [f])

shell :: Repl a -> IO ()
shell pre =
  flip evalStateT initState $
    evalRepl
      (pure "jemaine> ")
      evalCmd
      replCommands
      (Just ':')
      (Word completer `Combine` File)
      pre

printBanner :: Repl ()
printBanner = liftIO $ putStrLn $ unlines
  [ "Welcome!"
  , "jemaine 0.1.0.0"
  ]

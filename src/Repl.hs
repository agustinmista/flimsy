{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Repl where

import Prelude hiding (readFile)

import Control.Exception (SomeException(..))
import Control.Monad.Catch (catch)

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
prelude = "lib/prelude.fl"

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

type BindEnv = Env ReplBind

-- | Internal state

data ReplState =
  ReplState
  { binds   :: BindEnv
  , prims   :: PrimEnv
  , loaded  :: Set FilePath
  , current :: Maybe FilePath
  , editor  :: FilePath
  }

initState :: ReplState
initState = ReplState
  { binds   = Env.empty
  , prims   = Prim.environment
  , loaded  = Set.empty
  , current = Nothing
  , editor  = "vim"
  }

getReplState :: MonadState ReplState m => m ReplState
getReplState = get

getReplBinds :: MonadState ReplState m => m BindEnv
getReplBinds = gets binds

getLoadedFiles :: MonadState ReplState m => m (Set FilePath)
getLoadedFiles = gets loaded

setLoadedFiles :: MonadState ReplState m => Set FilePath -> m ()
setLoadedFiles files = modify' $ \st -> st { loaded = files }

getCurrentFile :: MonadState ReplState m => m (Maybe FilePath)
getCurrentFile = gets current

setCurrentFile :: MonadState ReplState m => FilePath -> m ()
setCurrentFile f = modify' $ \st -> st { current = Just f }

getEditor :: MonadState ReplState m => m FilePath
getEditor = gets editor

setEditor :: MonadState ReplState m => FilePath -> m ()
setEditor e = modify' $ \st -> st { editor = e }

-- | Project the internal state into the type checking environment
getTcEnv :: MonadState ReplState m => m TcEnv
getTcEnv = do
  bindTys <- fmap scheme <$> gets binds
  primTys <- fmap primTy <$> gets prims
  return (Env.merge bindTys primTys)

-- | Project the internal state into the evaluation environment
getEvalEnv :: MonadState ReplState m => m EvalEnv
getEvalEnv = fmap thunk <$> gets binds

-- | Remove all the binds created inside the REPL
unloadInteractive :: MonadState ReplState m => m ()
unloadInteractive = modify' $ \st ->
  st { binds = Env.filter ((interactive /=) . origin) (binds st) }

-- | Remove all the binds created inside the REPL
unloadBindsOfFile :: MonadState ReplState m => FilePath -> m ()
unloadBindsOfFile file = modify' $ \st ->
  st { binds = Env.filter ((file /=) . origin) (binds st) }


----------------------------------------
-- | The Read-Eval-Print-Loop monad
----------------------------------------

-- | The Repl type
type Repl a = HaskelineT (StateT ReplState IO) a

-- | Store a bind in the internal state
storeBind :: Var -> Thunk -> Scheme -> FilePath -> Repl ()
storeBind var th tsc file = do
  st <- getReplState
  case Env.lookup var (binds st) of
    Nothing -> do
      ref <- liftIO $ newIORef th
      let st' = st { binds = binds st `Env.extend` (var, ReplBind ref tsc file) }
      put st'
    Just bind -> do
      say $ "Shadowing " <> pretty var
      let st' = st { binds = binds st `Env.extend` (var, ReplBind (thunk bind) tsc file) }
      updateThunk (thunk bind) th
      put st'

-- | Abort the execution when an error is found
hoistError :: Pretty e => Either e a -> Repl a
hoistError (Right val) = return val
hoistError (Left err) = do
  say $ pretty err
  abort

continueAfterError :: Repl () -> Repl ()
continueAfterError m = catch m (\SomeException {} -> return ())

say :: String -> Repl ()
say = liftIO . putStrLn

----------------------------------------
-- | Processing inputs
----------------------------------------

processFile :: Bool -> FilePath -> Repl ()
processFile update file = do
  say $ "Loading " <> file
  -- unload any previous bind from this file, as well as any interactive bind
  -- that could get shadowed after loading the file
  unloadInteractive
  unloadBindsOfFile file
  -- parse the input file
  contents <- liftIO $ readFile file
  decls <- hoistError (parseSourceFile file contents)
  -- compute a topological sort of the declarations in the target file
  tenv <- getTcEnv
  sortedDecls <- hoistError (tryTopSort tenv decls)
  -- process each declarations in the safe order computed above
  mapM_ (processDecl (Just file)) sortedDecls
  -- add the file to the list of loaded ones
  st <- getReplState
  let st' = st { loaded = Set.insert file (loaded st) }
  put st'
  -- set the input as current file and report the loaded files
  when update (setCurrentFile file)
  say $ "Ok, files loaded:"
  forM_ (loaded st') say

processDecl :: Maybe String -> Decl -> Repl ()
processDecl mbf (BindD bind) = processBind mbf bind

processBind :: Maybe String -> Bind -> Repl ()
processBind mbf bind = do
  let (var, expr, _) = getBind bind
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
  ty <- hoistError (typeOfExpr tenv expr)
  -- if the expression is pure, we can evaluate it right away, but if its
  -- effectful we need to wrap it using a do expression
  let expr' = case (expr, ty) of
        (DoE _, Forall _ (IOT _)) -> expr
        (_,     Forall _ (IOT _)) -> DoE [ExprStmt expr]
        _                         -> expr
  -- evaluate it
  venv <- getEvalEnv
  res  <- liftIO $ evaluate venv Prim.environment expr'
  val  <- hoistError res
  -- print it
  say $ pretty val

----------------------------------------
-- | Interactive evaluation
----------------------------------------

evalCmd :: String -> Repl ()
evalCmd source = do
  let input = pack source
  res <- hoistError (parseStdin interactive input)
  case res of
    Left  expr -> processExpr expr
    Right decl -> processDecl Nothing decl

----------------------------------------
-- | Commands
----------------------------------------

replCommands :: [(String, [String] -> Repl ())]
replCommands =
  [ ("load"   , loadCmd)
  , ("reload" , reloadCmd)
  , ("browse" , browseCmd)
  , ("type"   , typeCmd)
  , ("quit"   , quitCmd)
  , ("shell"  , shellCmd)
  , ("!"      , shellCmd)
  , ("info"   , infoCmd)
  , ("edit"   , editCmd)
  , ("echo"   , echoCmd)
  ]

-- :load
loadCmd :: [String] -> Repl ()
loadCmd input = continueAfterError $ do
  case input of
    []  -> unloadAll
    [f] -> processFile True f
    _   -> say $ "load modules one by one!"

-- unload all the files except for the prelude
unloadAll :: Repl ()
unloadAll = do
  let keep = [prelude]
  modify' $ \st ->
    st { binds = Env.filter (\b -> origin b `elem` keep) (binds st)
       , loaded = Set.filter (\f -> f `elem` keep) (loaded st)
       }
  say $ "Ok, no modules loaded."

-- :load
reloadCmd :: [String] -> Repl ()
reloadCmd input = continueAfterError $ do
  curr <- getCurrentFile
  case (input, curr) of
    ([], Just file) -> do
      processFile True file
    ([], Nothing) -> do
      say $ "Nothing to reload"
    _   -> say $ "This command accepts no arguments"


-- :browse
browseCmd :: [String] -> Repl ()
browseCmd input = do
  env <- getReplBinds
  ld <- getLoadedFiles
  case input of
    [] -> forM_ (Env.toList env) $ \(var, bind) -> do
      say $ pretty var <> " : " <> pretty (scheme bind)
    files -> forM_ files $ \file -> do
      if Set.member file ld
        then do
          say $ "Loaded from " <> file <> ":"
          let bs = Env.toList (Env.filter ((file ==) . origin) env)
          forM_ bs $ \(var, bind) -> do
            say $ pretty var <> " : " <> pretty (scheme bind)
        else do
          say $ "File " <> file <> " is not loaded!"

-- :type
typeCmd :: [String] -> Repl ()
typeCmd input = continueAfterError $ do
  let str = pack (unwords input)
  expr <- hoistError (parseExpr interactive str)
  tenv <- getTcEnv
  tsc  <- hoistError (typeOfExpr tenv expr)
  say $ pretty expr <> " : " <> pretty (tsc)

-- :quit
quitCmd :: [String] -> Repl ()
quitCmd _ = liftIO exitSuccess

-- :!
shellCmd :: [String] -> Repl ()
shellCmd input = continueAfterError $ do
  case input of
    (cmd:args) -> do
      liftIO $ void (rawSystem cmd args)
    _ -> do
      say $ "empty shell command"

-- :info
infoCmd :: [String] -> Repl ()
infoCmd [] = printInternalState
infoCmd xs = printInfo xs

printInfo :: [String] -> Repl ()
printInfo args = do
  env <- getReplState
  forM_ args $ \arg -> do
    case Env.lookup (mkVar (pack arg)) (binds env) of
      Nothing -> do
        say $ "not in scope: " <> arg
      Just (ReplBind _ ty file) -> do
        say $ arg <> " : " <> pretty ty <> "  -- defined at " <> file

printInternalState :: Repl ()
printInternalState = do
  st <- getReplState
  say $ "Loaded primitives:"
  forM_ (Env.toList (prims st)) $ \(v, Prim ty _) -> do
    say $ pretty v <> " : " <> pretty ty
  say $ "============="
  say $ "Loaded variables:"
  forM_ (Env.toList (binds st)) $ \(v, ReplBind _ ty file) -> do
    say $ pretty v <> " : " <> pretty ty <> "  -- defined at " <> file
  say $ "============="
  say $ "Loaded files:"
  forM_ (Set.toList (loaded st)) $ \f -> do
    say f
  say $ "============="
  case current st of
    Nothing -> say $ "No file is currently in focus"
    Just f  -> say $ "Current file in focus: " <> f

-- :edit
editCmd :: [String] -> Repl ()
editCmd input = continueAfterError $ do
  curr <- getCurrentFile
  e <- getEditor
  case (input, curr) of
    ([],  Nothing) -> say "Nothing to edit"
    ([],  Just f)  -> launchEditor e f
    ([f], _)       -> launchEditor e f
    _              -> say "Edit files one by one!"

launchEditor :: FilePath -> FilePath -> Repl ()
launchEditor e f = do
  -- launch the external editor
  liftIO $ void (rawSystem e [f])
  ld <- getLoadedFiles
  -- if the file was previously loaded, then reload it
  when (f `Set.member` ld) $
    processFile True f

-- :echo
echoCmd :: [String] -> Repl ()
echoCmd args = continueAfterError $ do
  let input = pack (unwords args)
  res <- hoistError (parseStdin interactive input)
  case res of
    Left  expr -> say $ pretty expr
    Right decl -> say $ pretty decl

----------------------------------------
-- | Tab completion
----------------------------------------

completer :: WordCompleter (StateT ReplState IO)
completer n = do
  let cmds = [ ':' : cmd | cmd <- fst <$> replCommands ]
  vars <- Env.keys <$> getReplBinds
  return (filter (isPrefixOf n) (cmds <> (pretty <$> vars)))

----------------------------------------
-- | Top level
----------------------------------------

launchRepl :: Maybe FilePath -> IO ()
launchRepl input = shell $ do
  printBanner
  processFile False prelude
  case input of
    Nothing   -> return ()
    Just file -> processFile True file

shell :: Repl a -> IO ()
shell pre =
  flip evalStateT initState $
    evalRepl
      (pure "floppy> ")
      evalCmd
      replCommands
      (Just ':')
      (Word completer `Combine` File)
      pre

printBanner :: Repl ()
printBanner = say $
  intercalate "\n"
  [ "  ___ _                  "
  , " |  _| |___ ___ ___ _ _  "
  , " |  _| | . | . | . | | | "
  , " |_| |_|___|  _|  _|_  | "
  , "           |_| |_| |___| "
  , "                         "
  , "Welcome! floppy REPL version 0.1.0.0"
  ]

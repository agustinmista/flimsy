{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Repl where

import Prelude hiding (mod)

import System.Exit
import System.FilePath
import System.Directory
import System.Console.Repline hiding (prefix)

import Control.Exception (SomeException(..))
import Control.Monad.State.Strict
import Control.Monad.Catch (catch)

import Data.List
import qualified Data.Map.Strict as Map

import Var
import Parser
import Syntax
import Type
import Infer
import TypeCheck
import Eval
import Pretty
import Error
import Util

import ReplState
import qualified Env as Env

----------------------------------------
-- | The Read-Eval-Print-Loop monad
----------------------------------------

-- | The Repl type
type Repl = HaskelineT (StateT ReplState IO)

-- | Abort the execution when an error is found
hoist :: Either FlimsyError a -> Repl a
hoist (Right val) = return val
hoist (Left err) = say (pretty err) >> abort

-- | Like `dontCrash`, but it doesn't print "Interrupt"
continueAfterError :: Repl () -> Repl ()
continueAfterError m = catch m (\SomeException {} -> return ())

----------------------------------------
-- | Module processing
----------------------------------------

readSourceFile :: FilePath -> IO (Either FlimsyError String)
readSourceFile path = do
  exists <- doesFileExist path
  if exists
    then Right <$> readFile path
    else return (Left (FileDoesNotExist path))

processModule :: Bool -> ModuleName -> FilePath -> Repl ()
processModule update modname path = do
  say $ "Processing " <> modname <> " (" <> path <> ")"
  unloadInteractive
  unloadBindsOfModule modname
  -- parse the input file
  contents <- hoist =<< liftIO (readSourceFile path)
  psMod <- hoist (parseModule path contents)
  -- type check the module and return a list of SCCs of binds to register
  tcEnv <- getBaseTcEnv
  (tcMod, tcSCCs) <- hoist (typeCheckModule tcEnv psMod)
  -- register the binds in the module in the interactive environment
  registerSCCs modname tcSCCs
  -- add the file to the list of loaded ones
  loaded <- registerLoadedModule tcMod
  -- set the input as current file and report the loaded files
  when update (setCurrentModule tcMod)
  say $ "Ok, files loaded: " <> intercalate ", " loaded


----------------------------------------
-- | Interactive evaluation
----------------------------------------

evalInteractiveDecl :: PsDecl -> Repl ()
evalInteractiveDecl (BindD bind) = evalInteractiveBind bind

evalInteractiveBind :: PsBind -> Repl ()
evalInteractiveBind bind = do
  let (_,var, expr) = splitBind bind
  -- type check the body
  tenv <- getInteractiveTcEnv
  (tsc, expr') <- hoist (typeCheckExpr tenv expr)
  -- create/update the evaluation thunk
  venv <- getInteractiveEvalEnv
  let th = mkThunk (evalExpr venv expr')
  registerBind var th tsc interactive

evalInteractiveExpr :: PsExpr -> Repl ()
evalInteractiveExpr expr = do
  -- type check the expression
  tenv <- getInteractiveTcEnv
  (ty, expr') <- hoist (typeCheckExpr tenv expr)
  -- if the expression is IO, we can evaluate it right away, but if its
  -- effectful we want to wrap it using a do expression + print
  let wrap_io_unit e = DoE [ExprStmt e]
  let wrap_io t e    = DoE [BindStmt (mkVar "_x", t) e,
                            ExprStmt (VarE (mkVar "print", t :->: ioT unitT)
                                      `AppE` VarE (mkVar "_x", t))]
  let wrap_pure t e  = DoE [ExprStmt (VarE (mkVar "print", t :->: ioT unitT)
                                      `AppE` e)]
  let wrapped =
        case ty of
          Forall _ (IOT (TupT [])) -> wrap_io_unit expr'
          Forall _ (IOT t)         -> wrap_io    t expr'
          Forall _ t               -> wrap_pure  t expr'
  -- finally, we can evaluate it
  penv <- getPrimEnv
  venv <- getInteractiveEvalEnv
  res  <- liftIO $ evaluate venv penv wrapped
  void (hoist res)

----------------------------------------
-- | Interactive evaluation
----------------------------------------

evalCmd :: String -> Repl ()
evalCmd input = do
  hoist (parseStdin interactive input) >>= \case
    Left  expr -> evalInteractiveExpr expr
    Right decl -> evalInteractiveDecl decl

----------------------------------------
-- | Commands
----------------------------------------

-- :load
loadCmd :: [String] -> Repl ()
loadCmd input = continueAfterError $ do
  case input of
    []     -> unloadAllModules >> say "Ok, no modules loaded."
    [path] -> processModule True (takeBaseName path) path
    _      -> say "Load modules one by one!"

-- :reload
reloadCmd :: [String] -> Repl ()
reloadCmd input = continueAfterError $ do
  curr <- getCurrentModule
  case (input, curr) of
    ([], Just mod) -> processModule True (module_name mod) (module_path mod)
    ([], Nothing)  -> say $ "Nothing to reload"
    _              -> say $ "This command accepts no arguments"

-- :browse
browseCmd :: [String] -> Repl ()
browseCmd input = do
  env <- getReplBinds
  ld <- getLoadedModules
  case input of
    [] -> forM_ (Env.toList env) $ \(var, bind) -> do
      say $ pretty var <> " : " <> pretty (bind_scheme bind)
    mods -> forM_ mods $ \mod -> do
      if Map.member mod ld
        then do
          say $ "Loaded from " <> mod <> ":"
          let bs = Env.toList (Env.filter ((mod ==) . bind_origin) env)
          forM_ bs $ \(var, bind) -> do
            say $ pretty var <> " : " <> pretty (bind_scheme bind)
        else do
          say $ "Module " <> mod <> " is not loaded!"

-- :type
typeCmd :: [String] -> Repl ()
typeCmd input = continueAfterError $ do
  let str = unwords input
  expr <- hoist (parseExpr interactive str)
  tenv <- getInteractiveTcEnv
  (tsc, expr')  <- hoist (typeCheckExpr tenv expr)
  say $ pretty expr' <> " : " <> pretty tsc

-- :quit
quitCmd :: [String] -> Repl ()
quitCmd _ = liftIO exitSuccess

-- :!
shellCmd :: [String] -> Repl ()
shellCmd input = continueAfterError $ do
  case input of
    (cmd:args) -> do
      liftIO $ runShellCommand cmd args
    _ -> do
      say $ "empty shell command"

-- :info
infoCmd :: [String] -> Repl ()
infoCmd [] = printInternalState
infoCmd xs = printInfo xs

printInfo :: [String] -> Repl ()
printInfo args = do
  st <- getReplState
  forM_ args $ \arg -> do
    case Env.lookup (mkVar arg) (repl_binds st) of
      Nothing -> do
        say $ "Not in scope: " <> arg
      Just (ReplBind _ ty file) -> do
        say $ arg <> " : " <> pretty ty <> "  -- defined at " <> file

printInternalState :: Repl ()
printInternalState = do
  st <- getReplState
  say $ "Loaded primitives:"
  forM_ (Env.toList (repl_prims st)) $ \(v, Prim ty _) -> do
    say $ pretty v <> " : " <> pretty ty
  say $ "============="
  say $ "Loaded variables:"
  forM_ (Env.toList (repl_binds st)) $ \(v, ReplBind _ ty file) -> do
    say $ pretty v <> " : " <> pretty ty <> "  -- defined at " <> file
  say $ "============="
  say $ "Loaded modules:"
  forM_ (repl_loaded st) $ \mod -> do
    say (module_name mod <> " (" <> module_path mod <> ")")
  say $ "============="
  case repl_current st of
    Nothing  -> say $ "No file is currently in focus"
    Just mod -> say $ "Current module in focus: " <> module_name mod

-- :edit
editCmd :: [String] -> Repl ()
editCmd input = continueAfterError $ do
  curr <- getCurrentModule
  e <- getEditor
  case (input, curr) of
    ([],  Nothing)  -> say "Nothing to edit"
    ([],  Just mod) -> launchEditor e (module_path mod)
    ([f], _)        -> launchEditor e f
    _               -> say "Edit files one by one!"

launchEditor :: FilePath -> FilePath -> Repl ()
launchEditor editor path = do
  -- launch the external editor
  liftIO $ runShellCommand editor [path]
  loaded <- getLoadedModules
  -- if the file was previously loaded, then reload it
  let modname = takeBaseName path
  when (modname `Map.member` loaded) $ do
    processModule True modname path

-- :echo
echoCmd :: [String] -> Repl ()
echoCmd input = continueAfterError $ do
  hoist (parseStdin interactive (unwords input)) >>= \case
    Left expr -> do
      tenv <- getInteractiveTcEnv
      (_, expr') <- hoist (typeCheckExpr tenv expr)
      say $ pretty expr'
    Right (BindD bind) -> do
      let (isVal, var, expr) = splitBind bind
      -- type check the body
      tenv <- getInteractiveTcEnv
      (tsc, expr') <- hoist (typeCheckExpr tenv expr)
      let ty = instantiate' tsc
      say $ pretty (mergeBind isVal (var,  ty) expr')
      say $ ": " <> pretty ty

----------------------------------------
-- | Tab completion
----------------------------------------

completer :: WordCompleter (StateT ReplState IO)
completer n = do
  let cmds = [ ':' : cmd | cmd <- fst <$> replCommands ]
  binds <- Env.keys <$> getReplBinds
  return (filter (isPrefixOf n) (cmds <> (pretty <$> binds)))

----------------------------------------
-- | Top level
----------------------------------------

launchRepl :: Maybe FilePath -> IO ()
launchRepl input =
  flip evalStateT initState $ evalRepl
    (pure "flimsy> ")
    evalCmd
    replCommands
    (Just ':')
    (Word completer `Combine` File)
    (initRepl input)

loadPrelude :: Repl ()
loadPrelude = processModule False preludeName preludePath

initRepl :: Maybe FilePath -> Repl ()
initRepl input = do
  printBanner
  loadPrelude
  case input of
    Nothing   -> return ()
    Just file -> processModule True (takeBaseName file) file

replCommands :: [(String, [String] -> Repl ())]
replCommands =
  [ ("load"   , loadCmd)
  , ("reload" , reloadCmd)
  , ("browse" , browseCmd)
  , ("type"   , typeCmd)
  , ("shell"  , shellCmd)
  , ("!"      , shellCmd)
  , ("info"   , infoCmd)
  , ("edit"   , editCmd)
  , ("echo"   , echoCmd)
  , ("quit"   , quitCmd)
  ]

printBanner :: Repl ()
printBanner = say $
  intercalate "\n"
  [ ""
  , "  .o88o. oooo   o8o                                        "
  , "  888 `\" `888   `\"'                                        "
  , " o888oo   888  oooo  ooo. .oo.  .oo.    .oooo.o oooo    ooo"
  , "  888     888  `888  `888P\"Y88bP\"Y88b  d88(  \"8  `88.  .8' "
  , "  888     888   888   888   888   888  `\"Y88b.    `88..8'  "
  , "  888     888   888   888   888   888  o.  )88b    `888'   "
  , " o888o   o888o o888o o888o o888o o888o 8\"\"888P'     .8'    "
  , "                                                .o..P'     "
  , "                                                `Y8P'      "
  , "The brittle flimsy REPL, version 0.1.0.0"
  ]

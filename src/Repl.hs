{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Repl where

import Control.Exception (SomeException(..))
import Control.Monad.State.Strict
import Control.Monad.Catch (catch)

import System.Exit
import System.Console.Repline

import Data.List
import qualified Data.Set as Set

import qualified Data.Text.Lazy as Text

import Var
import Parser
import Syntax
import Type
import Escape
import Infer
import Eval
import Pretty
import Util

import ReplState
import qualified Env as Env

----------------------------------------
-- | The Read-Eval-Print-Loop monad
----------------------------------------

-- | The Repl type
type Repl a = HaskelineT (StateT ReplState IO) a


-- | Abort the execution when an error is found
hoistError :: Pretty e => Either e a -> Repl a
hoistError (Right val) = return val
hoistError (Left err) = do
  say $ pretty err
  abort

continueAfterError :: Repl () -> Repl ()
continueAfterError m = catch m (\SomeException {} -> return ())

----------------------------------------
-- | Processing inputs
----------------------------------------

processFile :: Bool -> File -> Repl ()
processFile update file = do
  say $ "Loading " <> file
  -- unload any previous bind from this file, as well as any interactive bind
  -- that could get shadowed after loading the file
  unloadInteractive
  unloadBindsOfFile file
  -- parse the input file
  contents <- liftIO $ readSourceFile file
  decls <- hoistError (parseSourceFile file contents)
  -- compute a topological sort of the declarations in the target file
  tenv <- getTcEnv
  let sccs = calculateSSCs tenv decls
  forM_ sccs (say . Text.pack . show)
  sortedDecls <- hoistError (tryTopSort tenv decls)
  -- process each declarations in the safe order computed above
  mapM_ (processDecl (Just file)) sortedDecls
  -- add the file to the list of loaded ones
  loaded <- registerLoadedFile file
  -- set the input as current file and report the loaded files
  when update (setCurrentFile file)
  say $ "Ok, files loaded:"
  forM_ loaded say

processDecl :: Maybe File -> PsDecl -> Repl ()
processDecl mbf (BindD bind) = processBind mbf bind

processBind :: Maybe File -> PsBind -> Repl ()
processBind mbf bind = do
  let (_,var, expr) = splitBind bind
  -- type check the body
  tenv <- getTcEnv
  (tsc, expr') <- hoistError (typeCheck tenv expr)
  -- create/update the evaluation thunk
  venv <- getEvalEnv
  let file = maybe interactive id mbf
  let th = mkThunk (evalExpr venv expr')
  registerBind var th tsc file

processExpr :: PsExpr -> Repl ()
processExpr expr = do
  -- type check the expression
  tenv <- getTcEnv
  (ty, expr') <- hoistError (typeCheck tenv expr)
  -- if the expression is IO, we can evaluate it right away, but if its
  -- effectful we want to wrap it using a do expression + print
  let wrap_pure t e  = DoE [ExprStmt (VarE (mkVar "print", t :->: ioT unitT) `AppE` e)]
  let wrap_io_unit e = DoE [ExprStmt e]
  let wrap_io t e    = DoE [BindStmt (mkVar "_x", t) e,
                            ExprStmt (VarE (mkVar "print", t :->: ioT unitT) `AppE` VarE (mkVar "_x", t))]
  let wrapped =
        case ty of
          Forall _ (IOT (TupT [])) -> wrap_io_unit expr'
          Forall _ (IOT t)         -> wrap_io t expr'
          Forall _ t               -> wrap_pure t expr'
  -- finally, we can evaluate it
  venv <- getEvalEnv
  penv <- getPrimEnv
  res  <- liftIO $ evaluate venv penv wrapped
  void (hoistError res)

----------------------------------------
-- | Interactive evaluation
----------------------------------------

evalCmd :: Text -> Repl ()
evalCmd input = do
  hoistError (parseStdin interactive input) >>= \case
    Left  expr -> processExpr expr
    Right decl -> processDecl Nothing decl

----------------------------------------
-- | Commands
----------------------------------------

-- :load
loadCmd :: [Text] -> Repl ()
loadCmd input = continueAfterError $ do
  case input of
    []  -> unloadAllFiles >> say "Ok, no modules loaded."
    [f] -> processFile True f
    _   -> say "Load modules one by one!"

-- :reload
reloadCmd :: [Text] -> Repl ()
reloadCmd input = continueAfterError $ do
  curr <- getCurrentFile
  case (input, curr) of
    ([], Just file) -> processFile True file
    ([], Nothing)   -> say $ "Nothing to reload"
    _               -> say $ "This command accepts no arguments"


-- :browse
browseCmd :: [Text] -> Repl ()
browseCmd input = do
  env <- getReplBinds
  ld <- getLoadedFiles
  case input of
    [] -> forM_ (Env.toList env) $ \(var, bind) -> do
      say $ pretty var <> " : " <> pretty (bind_scheme bind)
    files -> forM_ files $ \file -> do
      if Set.member file ld
        then do
          say $ "Loaded from " <> file <> ":"
          let bs = Env.toList (Env.filter ((file ==) . bind_origin) env)
          forM_ bs $ \(var, bind) -> do
            say $ pretty var <> " : " <> pretty (bind_scheme bind)
        else do
          say $ "File " <> file <> " is not loaded!"

-- :type
typeCmd :: [Text] -> Repl ()
typeCmd input = continueAfterError $ do
  let str = Text.unwords input
  expr <- hoistError (parseExpr interactive str)
  tenv <- getTcEnv
  (tsc, expr')  <- hoistError (typeCheck tenv expr)
  say $ pretty expr' <> " : " <> pretty tsc

-- :quit
quitCmd :: [Text] -> Repl ()
quitCmd _ = liftIO exitSuccess

-- :!
shellCmd :: [Text] -> Repl ()
shellCmd input = continueAfterError $ do
  case input of
    (cmd:args) -> do
      liftIO $ runShellCommand cmd args
    _ -> do
      say $ "empty shell command"

-- :info
infoCmd :: [Text] -> Repl ()
infoCmd [] = printInternalState
infoCmd xs = printInfo xs

printInfo :: [Text] -> Repl ()
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
  say $ "Loaded files:"
  forM_ (Set.toList (repl_loaded st)) $ \f -> do
    say f
  say $ "============="
  case repl_current st of
    Nothing -> say $ "No file is currently in focus"
    Just f  -> say $ "Current file in focus: " <> f

-- :edit
editCmd :: [Text] -> Repl ()
editCmd input = continueAfterError $ do
  curr <- getCurrentFile
  e <- getEditor
  case (input, curr) of
    ([],  Nothing) -> say "Nothing to edit"
    ([],  Just f)  -> launchEditor e f
    ([f], _)       -> launchEditor e f
    _              -> say "Edit files one by one!"

launchEditor :: File -> File -> Repl ()
launchEditor e f = do
  -- launch the external editor
  liftIO $ runShellCommand e [f]
  ld <- getLoadedFiles
  -- if the file was previously loaded, then reload it
  when (f `Set.member` ld) $ do
    processFile True f

-- :echo
echoCmd :: [Text] -> Repl ()
echoCmd input = continueAfterError $ do
  hoistError (parseStdin interactive (Text.unwords input)) >>= \case
    Left expr -> do
      tenv <- getTcEnv
      (_, expr') <- hoistError (typeCheck tenv expr)
      say $ pretty expr'
    Right (BindD bind) -> do
      let (isVal, var, expr) = splitBind bind
      -- type check the body
      tenv <- getTcEnv
      (tsc, expr') <- hoistError (typeCheck tenv expr)
      ty <- hoistError (instantiate' tsc)
      say $ pretty (mergeBind isVal (var,  ty) expr')

----------------------------------------
-- | Tab completion
----------------------------------------

completer :: WordCompleter (StateT ReplState IO)
completer n = do
  let cmds = [ ':' : cmd | cmd <- fst <$> replCommands ]
  binds <- Env.keys <$> getReplBinds
  return (filter (isPrefixOf n) (cmds <> (Text.unpack . pretty <$> binds)))

----------------------------------------
-- | Top level
----------------------------------------

launchRepl :: Maybe FilePath -> IO ()
launchRepl input =
  flip evalStateT initState $ evalRepl
    (pure "flimsy> ")
    (evalCmd . Text.pack)
    replCommands
    (Just ':')
    (Word completer `Combine` File)
    (initRepl (Text.pack <$> input))

initRepl :: Maybe File -> Repl ()
initRepl input = do
  printBanner
  processFile False prelude
  case input of
    Nothing   -> return ()
    Just file -> processFile True file

replCommands :: [(String, [String] -> Repl ())]
replCommands =
  fmap (\(s,f) -> (s, f . fmap Text.pack))
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
  Text.intercalate "\n"
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

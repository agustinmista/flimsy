{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ReplState where

import System.FilePath
import Control.Monad.State.Strict

import Data.List
import Data.Graph (SCC(..))
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Var
import Syntax
import Env
import Type
import Infer
import Eval
import Pretty
import Prim
import Util

----------------------------------------
-- | Some base files
----------------------------------------

interactive :: FilePath
interactive = "<interactive>"

preludePath :: FilePath
preludePath = "lib/Prelude.fl"

preludeName :: ModuleName
preludeName = takeBaseName preludePath


----------------------------------------
-- | Repl internal state
----------------------------------------

-- | Binds environment

data ReplBind = ReplBind
  { bind_thunk  :: IORef Thunk
  , bind_scheme :: Scheme
  , bind_origin :: ModuleName
  }

type BindEnv = Env ReplBind

-- | Internal state

type ModuleMap = Map ModuleName TcModule

data ReplState = ReplState
  { repl_binds   :: BindEnv
  , repl_prims   :: PrimEnv
  , repl_loaded  :: ModuleMap
  , repl_current :: Maybe TcModule
  , repl_editor  :: FilePath
  }

initState :: ReplState
initState = ReplState
  { repl_binds   = Env.empty
  , repl_prims   = Prim.primitives
  , repl_loaded  = Map.empty
  , repl_current = Nothing
  , repl_editor  = "vim"
  }

----------------------------------------
-- | ReplState getters
----------------------------------------

getReplState :: MonadState ReplState m => m ReplState
getReplState = get

getReplBinds :: MonadState ReplState m => m BindEnv
getReplBinds = gets repl_binds

getPrimEnv :: MonadState ReplState m => m PrimEnv
getPrimEnv = gets repl_prims

getLoadedModules :: MonadState ReplState m => m (ModuleMap)
getLoadedModules = gets repl_loaded

getCurrentModule :: MonadState ReplState m => m (Maybe TcModule)
getCurrentModule = gets repl_current

getEditor :: MonadState ReplState m => m FilePath
getEditor = gets repl_editor

-- | Project the internal state into the interactive type checking environment
getInteractiveTcEnv :: MonadState ReplState m => m TcEnv
getInteractiveTcEnv = do
  primTys <- fmap prim_scheme <$> getPrimEnv
  bindTys <- fmap bind_scheme <$> getReplBinds
  return (Env.merge primTys bindTys)

-- | Project the internal state into the base type checking environment, which
-- only includes primitives and the prelude (this is used while type checking
-- standalone modules)
getBaseTcEnv :: MonadState ReplState m => m TcEnv
getBaseTcEnv = do
  primTys <- fmap prim_scheme <$> getPrimEnv
  allBinds <- getReplBinds
  let preludeBinds = Env.filter (\b -> bind_origin b == preludeName) allBinds
  let preludeTys = fmap bind_scheme preludeBinds
  return (Env.merge primTys preludeTys)


-- | Project the internal state into the interactive evaluation environment
getInteractiveEvalEnv :: MonadState ReplState m => m EvalEnv
getInteractiveEvalEnv = fmap bind_thunk <$> gets repl_binds

-- | Project the internal state into the base evaluation environment, only
-- includes primitives and the prelude (this is used while type checking
-- standalone modules)
getBaseEvalEnv :: MonadState ReplState m => m EvalEnv
getBaseEvalEnv = do
  allBinds <- getReplBinds
  let preludeBinds = Env.filter (\b -> bind_origin b == preludeName) allBinds
  let preludeThunks = fmap bind_thunk preludeBinds
  return preludeThunks

getEvalEnvOfModule :: MonadState ReplState m => ModuleName -> m EvalEnv
getEvalEnvOfModule modname = do
  allBinds <- getReplBinds
  let modAndPreludeBinds = Env.filter (\b -> bind_origin b `elem` [modname, preludeName]) allBinds
  let modAndPreludeThunks = fmap bind_thunk modAndPreludeBinds
  return modAndPreludeThunks

----------------------------------------
-- | ReplState setters
----------------------------------------

setLoadedModules :: MonadState ReplState m => ModuleMap -> m ()
setLoadedModules mods = modify' $ \st -> st { repl_loaded = mods }

setCurrentModule :: MonadState ReplState m => TcModule -> m ()
setCurrentModule m = modify' $ \st -> st { repl_current = Just m }

setEditor :: MonadState ReplState m => FilePath -> m ()
setEditor e = modify' $ \st -> st { repl_editor = e }

----------------------------------------
-- | Transformations over the Repl state
----------------------------------------

-- | Store/update the binds of a strongly connected component of a module
-- The tricky part is how to deal with cyclic (mutually recursive) definitions
registerSCC :: (MonadState ReplState m, MonadIO m) => ModuleName -> SCC (Decl (Var, Type), [Var], Scheme) -> m ()
registerSCC modname scc = do
  venv <- getEvalEnvOfModule modname
  case scc of
    -- This case is easy, we can just create a thunk for the bind and register it
    AcyclicSCC (BindD bind, escaped, tsc) -> do
      let (_, (var, _), expr) = splitBind bind
      let filteredEnv = Env.restrict escaped venv
      let th = mkThunk (evalExpr filteredEnv expr)
      registerBind var th tsc modname
    -- This case is more tricky because binds are mutually recursive, and hence
    -- the evaluation cannot be extended in a given order. To solve this, we
    -- create a single environment shared among all mutually recursive binds,
    -- and extend it after processing each bind. After this, the resulting
    -- environment quantifies over all the mutually recursive binds. For this to
    -- work, we also need a slightly modified version of evalExpr that uses a
    -- reference instead of an environmet.
    CyclicSCC decls -> do
      let splitComp (escs, varsExprsTscs) (BindD bind, esc, tsc) =
            let (_, (var, _), expr) = splitBind bind
            in (esc <> escs, (var, expr, tsc) : varsExprsTscs)
      let (escapeds, varsExprsTscs) = foldl' splitComp ([],[]) decls
      let filteredEnv = Env.restrict (nub escapeds) venv
      envRef <- liftIO $ newIORef filteredEnv
      forM_ varsExprsTscs $ \(var, expr, tsc) -> do
        let th = mkThunk (evalExpr' envRef expr)
        thRef <- liftIO $ newIORef th
        liftIO $ modifyIORef' envRef $ \env ->
          env `Env.extend` (var, thRef)
        registerBind var th tsc modname

-- | Store/update a bind in the internal evaluation environment
registerBind :: (MonadIO m, MonadState ReplState m) => Var -> Thunk -> Scheme -> ModuleName -> m ()
registerBind var th tsc m = do
  st <- getReplState
  case Env.lookup var (repl_binds st) of
    Nothing -> do
      ref <- liftIO $ newIORef th
      let st' = st { repl_binds = repl_binds st `Env.extend` (var, ReplBind ref tsc m) }
      put st'
    Just bind -> do
      say $ "Shadowing " <> pretty var
      let ref = bind_thunk bind
      let st' = st { repl_binds = repl_binds st `Env.extend` (var, ReplBind ref tsc m) }
      liftIO $ writeIORef ref th
      put st'


-- | Remove all the binds created inside the REPL
unloadInteractive :: MonadState ReplState m => m ()
unloadInteractive = modify' $ \st ->
  st { repl_binds = Env.filter ((interactive /=) . bind_origin) (repl_binds st) }

-- | Remove all the binds created inside the REPL
unloadBindsOfModule :: MonadState ReplState m => ModuleName -> m ()
unloadBindsOfModule m = modify' $ \st ->
  st { repl_binds = Env.filter ((m /=) . bind_origin) (repl_binds st) }

-- | Unload all the modules except for the prelude
unloadAllModules :: MonadState ReplState m => m ()
unloadAllModules = do
  let keep = [preludeName]
  modify' $ \st ->
    st { repl_binds = Env.filter (\b -> bind_origin b `elem` keep) (repl_binds st)
       , repl_loaded = Map.filter (\f -> module_name f `elem` keep) (repl_loaded st)
       }

----------------------------------------
-- | Operations over loaded modules
----------------------------------------

registerLoadedModule :: MonadState ReplState m => TcModule -> m [ModuleName]
registerLoadedModule m = state $ \st ->
  let loaded = Map.insert (module_name m) m (repl_loaded st)
  in (Map.keys loaded, st { repl_loaded = loaded })

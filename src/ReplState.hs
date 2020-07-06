{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module ReplState where

import System.FilePath
import Control.Monad.State.Strict

import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map

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

type ModuleMap = Map ModuleName PsModule

data ReplState = ReplState
  { repl_binds   :: BindEnv
  , repl_prims   :: PrimEnv
  , repl_loaded  :: ModuleMap
  , repl_current :: Maybe PsModule
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

getCurrentModule :: MonadState ReplState m => m (Maybe PsModule)
getCurrentModule = gets repl_current

getEditor :: MonadState ReplState m => m FilePath
getEditor = gets repl_editor

-- | Project the internal state into the type checking environment
getTcEnv :: MonadState ReplState m => m TcEnv
getTcEnv = do
  bindTys <- fmap bind_scheme <$> getReplBinds
  primTys <- fmap prim_scheme <$> getPrimEnv
  return (Env.merge bindTys primTys)

-- | Project the internal state into the evaluation environment
getEvalEnv :: MonadState ReplState m => m EvalEnv
getEvalEnv = fmap bind_thunk <$> gets repl_binds

----------------------------------------
-- | ReplState setters
----------------------------------------

setLoadedModules :: MonadState ReplState m => ModuleMap -> m ()
setLoadedModules mods = modify' $ \st -> st { repl_loaded = mods }

setCurrentModule :: MonadState ReplState m => PsModule -> m ()
setCurrentModule m = modify' $ \st -> st { repl_current = Just m }

setEditor :: MonadState ReplState m => FilePath -> m ()
setEditor e = modify' $ \st -> st { repl_editor = e }

----------------------------------------
-- | Transformations over the Repl state
----------------------------------------

-- | Store/update a bind in the internal state
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
      let st' = st { repl_binds = repl_binds st `Env.extend` (var, ReplBind (bind_thunk bind) tsc m) }
      liftIO $ writeIORef (bind_thunk bind) th
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

registerLoadedModule :: MonadState ReplState m => PsModule -> m [ModuleName]
registerLoadedModule m = state $ \st ->
  let loaded = Map.insert (module_name m) m (repl_loaded st)
  in (Map.keys loaded, st { repl_loaded = loaded })

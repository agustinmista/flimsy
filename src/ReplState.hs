{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module ReplState where

import Data.IORef
import Control.Monad.State.Strict

import Data.Set (Set)
import qualified Data.Set as Set

import Var
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

interactive :: File
interactive = "<interactive>"

prelude :: File
prelude = "lib/prelude.fl"

----------------------------------------
-- | Repl internal state
----------------------------------------

-- | Binds environment

data ReplBind = ReplBind
  { bind_thunk  :: IORef Thunk
  , bind_scheme :: Scheme
  , bind_origin :: File
  }

type BindEnv = Env ReplBind

-- | Internal state

data ReplState = ReplState
  { repl_binds   :: BindEnv
  , repl_prims   :: PrimEnv
  , repl_loaded  :: Set File
  , repl_current :: Maybe Text
  , repl_editor  :: File
  }

initState :: ReplState
initState = ReplState
  { repl_binds   = Env.empty
  , repl_prims   = Prim.primitives
  , repl_loaded  = Set.empty
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

getLoadedFiles :: MonadState ReplState m => m (Set File)
getLoadedFiles = gets repl_loaded

getCurrentFile :: MonadState ReplState m => m (Maybe File)
getCurrentFile = gets repl_current

getEditor :: MonadState ReplState m => m File
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

setLoadedFiles :: MonadState ReplState m => Set File -> m ()
setLoadedFiles files = modify' $ \st -> st { repl_loaded = files }

setCurrentFile :: MonadState ReplState m => File -> m ()
setCurrentFile f = modify' $ \st -> st { repl_current = Just f }

setEditor :: MonadState ReplState m => File -> m ()
setEditor e = modify' $ \st -> st { repl_editor = e }

----------------------------------------
-- | Transformations over the Repl state
----------------------------------------

-- | Store/update a bind in the internal state
registerBind :: (MonadIO m, MonadState ReplState m) => Var -> Thunk -> Scheme -> File -> m ()
registerBind var th tsc file = do
  st <- getReplState
  case Env.lookup var (repl_binds st) of
    Nothing -> do
      ref <- liftIO $ newIORef th
      let st' = st { repl_binds = repl_binds st `Env.extend` (var, ReplBind ref tsc file) }
      put st'
    Just bind -> do
      say $ "Shadowing " <> pretty var
      let st' = st { repl_binds = repl_binds st `Env.extend` (var, ReplBind (bind_thunk bind) tsc file) }
      liftIO $ writeIORef (bind_thunk bind) th
      put st'

-- | Remove all the binds created inside the REPL
unloadInteractive :: MonadState ReplState m => m ()
unloadInteractive = modify' $ \st ->
  st { repl_binds = Env.filter ((interactive /=) . bind_origin) (repl_binds st) }

-- | Remove all the binds created inside the REPL
unloadBindsOfFile :: MonadState ReplState m => File -> m ()
unloadBindsOfFile file = modify' $ \st ->
  st { repl_binds = Env.filter ((file /=) . bind_origin) (repl_binds st) }

-- | Unload all the files except for the prelude
unloadAllFiles :: MonadState ReplState m => m ()
unloadAllFiles = do
  let keep = [prelude]
  modify' $ \st ->
    st { repl_binds = Env.filter (\b -> bind_origin b `elem` keep) (repl_binds st)
       , repl_loaded = Set.filter (\f -> f `elem` keep) (repl_loaded st)
       }


----------------------------------------
-- | Operations over loaded files
----------------------------------------

registerLoadedFile :: MonadState ReplState m => File -> m (Set File)
registerLoadedFile file = state $ \st ->
  let loaded = Set.insert file (repl_loaded st)
  in (loaded, st { repl_loaded = loaded })

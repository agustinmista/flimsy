{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Eval where

import Control.Monad.Except
import Control.Monad.Reader

import Data.IORef

import Env (Env)
import qualified Env as Env

import Syntax
import Error

----------------------------------------
-- | Values
----------------------------------------

data Value =
    LitV Literal
  | ConV Var
  | TupV [Value]
  | SumV (Either Value Value)
  | ClosureV (Thunk -> Eval Value)
  deriving Show

instance Show (Thunk -> Eval Value) where
  show _ = "<<thunk>>"

instance Eq Value where
  LitV v1  == LitV v2  = v1 == v2
  ConV c1  == ConV c2  = c1 == c2
  TupV vs1 == TupV vs2 = and (zipWith (==) vs1 vs2)
  SumV v1  == SumV v2  = v1 == v2
  _        == _        = False

----------------------------------------
-- | Thunks
----------------------------------------

newtype Thunk = Thunk (() -> Eval Value)

mkThunk :: Eval Value -> Thunk
mkThunk e = Thunk (\() -> e)

pureThunk :: Value -> Thunk
pureThunk v = mkThunk (return v)

createThunkRunner :: ValueEnv -> Var -> Expr -> (Thunk -> Eval Value)
createThunkRunner env var body = \th -> do
  ref <- liftIO $ newIORef th
  evalExpr (env `Env.extend` (var, ref)) body

lookupThunkOf :: Var -> ValueEnv -> Eval (IORef Thunk)
lookupThunkOf var venv = do
  case Env.lookup var venv of
    Just ref -> return ref
    Nothing -> throwError (InternalEvaluationError ("missing thunk for: " <> showVar var))

forceThunk :: IORef Thunk -> Eval Value
forceThunk ref = do
  Thunk th <- liftIO $ readIORef ref
  v <- th ()
  updateThunk ref (pureThunk v)
  return v

updateThunk :: MonadIO m => IORef Thunk -> Thunk -> m ()
updateThunk ref th = do
  liftIO $ writeIORef ref th


----------------------------------------
-- | Primitive operations
----------------------------------------

newtype PrimOp = PrimOp (Value -> Eval Value)

----------------------------------------
-- | Evaluation Monad
----------------------------------------

type Eval = ReaderT PrimEnv (ExceptT EvalError IO)

type ValueEnv = Env (IORef Thunk)
type PrimEnv = Env (Scheme, PrimOp)

-- | Run the evaluation monad
runEval :: PrimEnv -> Eval a -> IO (Either EvalError a)
runEval penv m = runExceptT (flip runReaderT penv m)

----------------------------------------
-- | Evaluating expressions
----------------------------------------

evaluate :: ValueEnv -> PrimEnv -> Expr -> IO (Either EvalError Value)
evaluate venv penv expr = runEval penv (evalExpr venv expr)

evalExpr :: ValueEnv -> Expr -> Eval Value
evalExpr env expr = do
  case expr of
    -- VarE
    VarE var -> do
      th <- lookupThunkOf var env
      val <- forceThunk th
      return val
    -- AppE
    AppE fun arg -> do
      -- special case: primitive operations
      penv <- ask
      case fun of
        VarE var | Just (_, PrimOp runner) <- Env.lookup var penv -> do
          argv <- evalExpr env arg
          runner argv
        _ -> do
          ClosureV evalThunk <- evalExpr env fun
          evalThunk (mkThunk (evalExpr env arg))
    -- LamE
    LamE var body -> do
      return (ClosureV (createThunkRunner env var body))
    -- LetE
    LetE bind body -> do
      let (var, expr') = getBind bind
      evalExpr env (AppE (LamE var body) expr')
    -- LitE
    LitE l -> do
      return (LitV l)
    -- InfixE
    InfixE op e1 e2 -> do
      evalExpr env (AppE (AppE (VarE op) e1) e2)
    -- IfE
    IfE cond tr fl -> do
      LitV (BoolL b) <- evalExpr env cond
      if b
        then evalExpr env tr
        else evalExpr env fl
    -- CaseE
    CaseE e alts -> do
      ve <- evalExpr env e
      case matchAlts ve alts of
        Nothing -> throwError (NonExhaustiveCase expr)
        Just (alt, sub) -> do
          patVarRefs <- forM sub $ \(var, val) -> do
            ref <- liftIO $ newIORef (pureThunk val)
            return (var, ref)
          let env' = foldl Env.extend env patVarRefs
          evalExpr env' (getAltBody alt)
    -- FixE
    FixE e -> do
      evalExpr env (AppE e (FixE e))
    -- TupE
    TupE es -> do
      vs <- mapM (evalExpr env) es
      return (TupV vs)
    -- SumE
    SumE (Left e) -> do
      v <- evalExpr env e
      return (SumV (Left v))
    SumE (Right e) -> do
      v <- evalExpr env e
      return (SumV (Right v))
    -- AsE
    AsE e _ -> do
      evalExpr env e


----------------------------------------
-- | Evaluating case expressions
----------------------------------------

matchAlts :: Value -> [Alt] -> Maybe (Alt, [(Var, Value)])
matchAlts _ [] = Nothing
matchAlts v (alt:alts)
  | Just sub <- matches (getAltPat alt) v =
      Just (alt, sub)
  | otherwise =
      matchAlts v alts

matches :: Pat -> Value -> Maybe [(Var, Value)]
matches (LitP lp) (LitV lv) | lp == lv =
  Just []
matches (VarP var) val =
  Just [(var, val)]
matches WildP _ =
  Just []
matches (TupP ps) (TupV vs) =
  concat <$> sequence (zipWith matches ps vs)
matches (SumP (Left p)) (SumV (Left v)) =
  matches p v
matches (SumP (Right p)) (SumV (Right v)) =
  matches p v
matches _ _ =
  Nothing

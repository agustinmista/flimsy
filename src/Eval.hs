{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Eval where

import Control.Monad.Except
import Control.Monad.Reader

import Data.IORef

import Env (Env)
import qualified Env as Env

import Var
import Syntax
import Type
import Error

----------------------------------------
-- | Values
----------------------------------------

data Value =
    LitV Literal
  | ConV Var
  | TupV [Value]
  | SumV (Either Value Value)
  | NilV
  | ConsV Value Value
  | IOV (IO Value)
  | ClosureV (Thunk -> Eval Value)
  | DeferredV Thunk
  deriving Show


instance Show Thunk where
  show _ = "<<thunk>>"

instance Show (Thunk -> Eval Value) where
  show _ = "<<closure>>"

instance Show (IO Value) where
  show _ = "<<io value>>"

instance Eq Value where
  LitV v1       == LitV v2        = v1 == v2
  ConV c1       == ConV c2        = c1 == c2
  TupV vs1      == TupV vs2       = and (zipWith (==) vs1 vs2)
  SumV v1       == SumV v2        = v1 == v2
  NilV          == NilV           = True
  ConsV hd1 tl1 == ConsV hd2 tl2  = hd1 == hd2 && tl1 == tl2
  _             == _              = False

deferredValue :: Eval Value -> Value
deferredValue = DeferredV . mkThunk

forceValue :: Value -> Eval Value
forceValue (DeferredV (Thunk th)) = th () >>= forceValue
forceValue (TupV vs) = TupV <$> sequence (forceValue <$> vs)
forceValue (SumV (Left v)) = SumV . Left <$> forceValue v
forceValue (SumV (Right v)) = SumV . Right <$> forceValue v
forceValue (ConsV hd tl) = ConsV <$> forceValue hd <*> forceValue tl
forceValue v = return v
-- what about closures and IO?

----------------------------------------
-- | Thunks
----------------------------------------

newtype Thunk = Thunk (() -> Eval Value)

mkThunk :: Eval Value -> Thunk
mkThunk e = Thunk (\() -> e)

pureThunk :: Value -> Thunk
pureThunk val = mkThunk (return val)

lookupThunkOf :: Var -> EvalEnv -> Eval (IORef Thunk)
lookupThunkOf var venv = do
  case Env.lookup var venv of
    Just ref -> return ref
    Nothing -> throwError (InternalEvalError ("missing thunk for: " <> showVar var))

updateThunk :: MonadIO m => IORef Thunk -> Thunk -> m ()
updateThunk ref th = do
  liftIO $ writeIORef ref th

-- force a pure thunk, updating it with the calculated value
forceThunk :: IORef Thunk -> Eval Value
forceThunk ref = do
  Thunk th <- liftIO $ readIORef ref
  v <- th ()
  updateThunk ref (pureThunk v)
  return v

-- force a IO thunk, without updating its value!
forceThunkIO :: IORef Thunk -> Eval Value
forceThunkIO ref = do
  Thunk th <- liftIO $ readIORef ref
  th ()

----------------------------------------
-- | Primitive operations
----------------------------------------

newtype PrimRunner = PrimRunner (Value -> Eval Value)

data Prim = Prim
  { primTy :: Scheme
  , primRunner :: PrimRunner
  }

type PrimEnv = Env Prim

----------------------------------------
-- | Evaluation Monad
----------------------------------------

type Eval = ReaderT PrimEnv (ExceptT EvalError IO)

type EvalEnv = Env (IORef Thunk)

-- | Run the evaluation monad
runEval :: PrimEnv -> Eval a -> IO (Either EvalError a)
runEval penv m = runExceptT (flip runReaderT penv m)

----------------------------------------
-- | Evaluating expressions
----------------------------------------

evaluate :: EvalEnv -> PrimEnv -> TcExpr -> IO (Either EvalError Value)
evaluate venv penv expr = runEval penv (evalExpr venv expr)

evaluate':: EvalEnv -> PrimEnv -> TcExpr -> IO (Either EvalError Value)
evaluate' venv penv expr = runEval penv (evalExpr venv expr >>= forceValue)


evalExpr :: EvalEnv -> TcExpr -> Eval Value
evalExpr env expr = do
  -- liftIO $ putStrLn $ "evaluating: " <> show expr
  case expr of
    -- VarE
    VarE (var, ty) -> do
      th <- lookupThunkOf var env
      if isIOType ty
        then forceThunkIO th
        else forceThunk   th
    -- AppE
    AppE fun arg -> do
      -- special case: primitive operations
      penv <- ask
      case fun of
        VarE (var, _) | Just prim <- Env.lookup var penv -> do
          argv <- evalExpr env arg
          let PrimRunner run = primRunner prim
          run argv
        _ -> do
          ClosureV evalThunk <- evalExpr env fun
          evalThunk (mkThunk (evalExpr env arg))
    -- LamE
    LamE (var, _) body -> do
      return (deferredClosure env var body)
    -- LetE
    LetE bind body -> do
      let (_, var, expr') = splitBind bind
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
      matched <- matchAlts alts ve
      case matched of
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
      let vs = deferredExpr env <$> es
      return (TupV vs)
    -- SumE
    SumE (Left e) -> do
      let v = deferredExpr env e
      return (SumV (Left v))
    SumE (Right e) -> do
      let v = deferredExpr env e
      return (SumV (Right v))
    -- ListE
    ListE [] -> do
      return NilV
    ListE (e:es) -> do
      let hd = deferredExpr env e
      let tl = deferredExpr env (ListE es)
      return (ConsV hd tl)
    DoE stmts -> do
      evalDo env stmts

deferredExpr :: EvalEnv -> TcExpr -> Value
deferredExpr env = deferredValue . evalExpr env

deferredClosure :: EvalEnv -> Var -> TcExpr -> Value
deferredClosure env var body = ClosureV $ \th -> do
  ref <- liftIO $ newIORef th
  evalExpr (env `Env.extend` (var, ref)) body

----------------------------------------
-- | Evaluating case expressions
----------------------------------------

-- debugMatch :: TcPat -> Value -> Eval ()
-- debugMatch p v = do
--   liftIO $ putStrLn $ "=============="
--   liftIO $ putStrLn $ "matching: "
--   liftIO $ putStrLn $ show p
--   liftIO $ putStrLn $ "with"
--   liftIO $ putStrLn $ show v
--   liftIO $ putStrLn $ "--------------"

matchAlts :: [TcAlt] -> Value -> Eval (Maybe (TcAlt, [(Var, Value)]))
matchAlts [] _ = return Nothing
matchAlts (alt:alts) v = do
  (matched, v') <- match (getAltPat alt) v
  case matched of
    Just sub -> do
      return (Just (alt, sub))
    Nothing  -> do
      matchAlts alts v'

match :: TcPat -> Value -> Eval (Maybe [(Var, Value)], Value)
match WildP v = do
  return (Just [], v)
match (VarP (var, _)) val = do
  return (Just [(var, val)], val)
match p (DeferredV (Thunk th)) = do
  v <- th ()
  match p v
match (LitP lp) (LitV lv) | lp == lv = do
  return (Just [], LitV lv)
match (TupP ps) (TupV vs) = do
  matchTuple ps vs
match (SumP (Left p)) (SumV (Left v)) = do
  match p v
match (SumP (Right p)) (SumV (Right v)) = do
  match p v
match (ListP p) v = do
  matchList p v
match _ v = do
  return (Nothing, v)

matchList :: TcListP -> Value -> Eval (Maybe [(Var, Value)], Value)
matchList NilP NilV = do
  return (Just [], NilV)
matchList (ConsP [] (Just p)) v = do
  match p v
matchList (ConsP (hp:hps) tlp) (ConsV hv tlv) = do
  (sub, hv') <- match hp hv
  (subs, tlv') <- matchList (ConsP hps tlp) tlv
  return (concat <$> sequence [sub, subs], ConsV hv' tlv')
matchList _ v = do
  return (Nothing, v)

matchTuple :: [TcPat] -> [Value] -> Eval (Maybe [(Var, Value)], Value)
matchTuple [] [] = do
  return (Just [], TupV [])
matchTuple (p:ps) (v:vs) = do
  (sub, v') <- match p v
  (subs, TupV vs') <- matchTuple ps vs
  return (concat <$> sequence [sub, subs], TupV (v':vs'))
matchTuple _ _ = do
  throwError (InternalEvalError "matchTuple: tuple size mismatch")

----------------------------------------
-- | Evaluating do expressions
----------------------------------------

evalDo :: EvalEnv -> [TcDoStmt] -> Eval Value
evalDo env [ExprStmt body] = do
  IOV io <- evalExpr env body
  liftIO io
evalDo env (BindStmt (var, _) body : xs) = do
  IOV io <- evalExpr env body
  val <- liftIO io
  ref <- liftIO $ newIORef (pureThunk val)
  let env' = env `Env.extend` (var, ref)
  evalDo env' xs
evalDo env (ExprStmt body : xs) = do
  IOV io <- evalExpr env body
  void (liftIO io)
  evalDo env xs
evalDo _ _ = do
  throwError (InternalEvalError "evalDo: unexpected input")

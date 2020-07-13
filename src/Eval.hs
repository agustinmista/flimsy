{-# LANGUAGE LambdaCase #-}
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
  | ClosureV Closure
  | ThunkV Thunk
  deriving Show


instance Show Thunk where
  show _ = "<<thunk>>"

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

----------------------------------------
-- | Closures
----------------------------------------

newtype Closure = Closure (Thunk -> Eval Value)

instance Show Closure where
  show _ = "<<closure>>"

newClosure :: EvalEnv -> Var -> TcExpr -> Closure
newClosure env var body = Closure $ \th -> do
  ref <- liftIO $ newIORef th
  evalExpr (env `Env.extend` (var, ref)) body

runClosure :: Closure -> Thunk -> Eval Value
runClosure (Closure clo) th = clo th

----------------------------------------
-- | Thunks
----------------------------------------

newtype Thunk = Thunk (() -> Eval Value)

mkThunk :: Eval Value -> Thunk
mkThunk e = Thunk (\() -> e)

runThunk :: Thunk -> Eval Value
runThunk (Thunk th) = th ()

pureThunk :: Value -> Thunk
pureThunk val = mkThunk (return val)

whnf :: Value -> Eval Value
whnf (ThunkV th) = runThunk th >>= whnf
whnf v           = return v

thunk :: EvalEnv -> TcExpr -> Value
thunk env expr = ThunkV (mkThunk (evalExpr env expr))

-- force a pure thunk ref, updating it with the calculated value
forceThunkRef :: IORef Thunk -> Eval Value
forceThunkRef ref = do
  th <- liftIO $ readIORef ref
  v <- runThunk th
  liftIO $ writeIORef ref (pureThunk v)
  return v

-- force a IO thunk ref, without updating its value!
forceThunkRefIO :: IORef Thunk -> Eval Value
forceThunkRefIO ref = do
  th <- liftIO $ readIORef ref
  runThunk th

lookupThunkOf :: Var -> EvalEnv -> Eval (IORef Thunk)
lookupThunkOf var venv = do
  case Env.lookup var venv of
    Just ref -> return ref
    Nothing -> throwError (InternalEvalError ("missing thunk for: " <> var_name var))

----------------------------------------
-- | Primitive operations
----------------------------------------

newtype PrimRunner = PrimRunner (Value -> Eval Value)

data Prim = Prim
  { prim_scheme :: Scheme
  , prim_runner :: PrimRunner
  }

type PrimEnv = Env Prim

runPrim :: Prim -> Value -> Eval Value
runPrim prim val = runner val
  where (PrimRunner runner) = prim_runner prim

lookupPrim :: TcExpr -> Eval (Maybe Prim)
lookupPrim (VarE (v, _)) = do
  penv <- ask
  return (Env.lookup v penv)
lookupPrim _ = return Nothing

----------------------------------------
-- | Evaluation Monad
----------------------------------------

type Eval = ReaderT PrimEnv (ExceptT FlimsyError IO)

type EvalEnv = Env (IORef Thunk)

-- | Run the evaluation monad
runEval :: PrimEnv -> Eval a -> IO (Either FlimsyError a)
runEval penv m = runExceptT (flip runReaderT penv m)

----------------------------------------
-- | Evaluating expressions
----------------------------------------

evaluate :: EvalEnv -> PrimEnv -> TcExpr -> IO (Either FlimsyError Value)
evaluate venv penv expr = runEval penv (evalExpr venv expr)

evalExpr' :: IORef EvalEnv -> TcExpr -> Eval Value
evalExpr' ref expr = do
  env <- liftIO $ readIORef ref
  evalExpr env expr

evalExpr :: EvalEnv -> TcExpr -> Eval Value
evalExpr env expr = do
  case expr of
    -- VarE
    VarE (var, ty) -> do
      th <- lookupThunkOf var env
      if isIOType ty
        then forceThunkRefIO th
        else forceThunkRef   th
    -- AppE
    AppE fun arg -> do
      -- special case: primitive operations
      lookupPrim fun >>= \case
        Just prim -> do
          argv <- evalExpr env arg
          runPrim prim argv
        Nothing -> do
          ClosureV clo <- evalExpr env fun
          runClosure clo (mkThunk (evalExpr env arg))
    -- LamE
    LamE (var, _) body -> do
      return (ClosureV (newClosure env var body))
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
      LitV (BoolL b) <- whnf =<< evalExpr env cond
      if b
        then evalExpr env tr
        else evalExpr env fl
    -- CaseE
    CaseE e alts -> do
      ve <- evalExpr env e
      matchAlts alts ve >>= \case
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
      let vs = thunk env <$> es
      return (TupV vs)
    -- SumE
    SumE (Left e) -> do
      let v = thunk env e
      return (SumV (Left v))
    SumE (Right e) -> do
      let v = thunk env e
      return (SumV (Right v))
    -- ListE
    ListE [] -> do
      return NilV
    ListE (e:es) -> do
      let hd = thunk env e
      let tl = thunk env (ListE es)
      return (ConsV hd tl)
    DoE stmts -> do
      evalDo env stmts

----------------------------------------
-- | Evaluating case expressions
----------------------------------------

debugMatch :: TcPat -> Value -> Eval ()
debugMatch p v = liftIO $ do
  putStrLn $ "=============="
  putStrLn $ "matching: "
  putStrLn $ show p
  putStrLn $ "with"
  putStrLn $ show v
  putStrLn $ "=============="

-- | Match a value against a list of case alternatives and return a substitution
-- of variables for values when one of them matches. This function forces
-- evaluation only when necessary.
matchAlts :: [TcAlt] -> Value -> Eval (Maybe (TcAlt, [(Var, Value)]))
matchAlts [] _ = return Nothing
matchAlts (alt:alts) v = do
  match (getAltPat alt) v >>= \case
    (Just sub, _)  -> return (Just (alt, sub))
    (Nothing,  v') -> matchAlts alts v'

-- Match a value against a single pattern and possibly return a substitution of
-- pattern variables. Order of clauses <<very>> important here.
match :: TcPat -> Value -> Eval (Maybe [(Var, Value)], Value)
match pat val = do
  -- debugMatch pat val
  case (pat, val) of
    -- Variable and wilcard patterns match any value (even deferred ones).
    (WildP, _)         -> return (Just [], val)
    (VarP (var, _), _) -> return (Just [(var, val)], val)
    -- From this point onwards, if the value is deferred then we need to force
    -- its evaluation and continue matching the result.
    (p, ThunkV th) -> runThunk th >>= match p
    -- The rest of the patterns are only satisfied by their corresponding values.
    (LitP p, LitV v) | p == v -> return (Just [], val)
    (SumP p, SumV v)          -> matchSum p v
    (TupP ps, TupV vs)        -> matchTuple ps vs
    (ListP p, v)              -> matchList p v
    _                         -> return (Nothing, val)

-- Match sum values
matchSum :: Either TcPat TcPat -> Either Value Value -> Eval (Maybe [(Var, Value)], Value)
matchSum (Left p) (Left v) = do
  (sub, v') <- match p v
  return (sub, SumV (Left v'))
matchSum (Right p) (Right v) = do
  (sub, v') <- match p v
  return (sub, SumV (Right v'))
matchSum _ v =
  return (Nothing, SumV v)

-- Match tuple values from left to right
matchTuple :: [TcPat] -> [Value] -> Eval (Maybe [(Var, Value)], Value)
matchTuple [] [] = do
  return (Just [], TupV [])
matchTuple (p:ps) (v:vs) = do
  match p v >>= \case
    (Nothing,  v') -> return (Nothing, TupV (v':vs))
    (Just sub, v') -> do
      matchTuple ps vs >>= \case
        (Nothing,   TupV vs') -> return (Nothing,            TupV (v':vs'))
        (Just subs, TupV vs') -> return (Just (sub <> subs), TupV (v':vs'))
        _ -> throwError (InternalEvalError "matchTuple: unexpected non-tuple value")
matchTuple _ _ = do
  throwError (InternalEvalError "matchTuple: tuple size mismatch")

-- Match list values from left to right
matchList :: TcListP -> Value -> Eval (Maybe [(Var, Value)], Value)
matchList NilP NilV = do
  return (Just [], NilV)
matchList (ConsP [] (Just p)) v = do
  match p v
matchList (ConsP (hp:hps) tlp) (ConsV hv tlv) = do
  match hp hv >>= \case
    (Nothing,   hv') -> return (Nothing, hv')
    (Just sub,  hv') -> do
      matchList (ConsP hps tlp) tlv >>= \case
        (Nothing,   tlv') -> return (Nothing, tlv')
        (Just subs, tlv') -> return (Just (sub <> subs), ConsV hv' tlv')
matchList _ v = do
  return (Nothing, v)

----------------------------------------
-- | Evaluating do expressions
----------------------------------------

-- Do expressions are evaluated strictly, because we want their effects to
-- happen in the appropriate order.
evalDo :: EvalEnv -> [TcDoStmt] -> Eval Value
evalDo env stmts = do
  case stmts of
    [ExprStmt body] -> do
      IOV io <- whnf =<< evalExpr env body
      v <- liftIO io
      return (IOV (return v))
    (BindStmt (var, _) body : xs) -> do
      IOV io <- whnf =<< evalExpr env body
      val <- liftIO io
      ref <- liftIO $ newIORef (pureThunk val)
      let env' = env `Env.extend` (var, ref)
      evalDo env' xs
    (ExprStmt body : xs) -> do
      IOV io <- whnf =<< evalExpr env body
      void (liftIO io)
      evalDo env xs
    _ -> throwError (InternalEvalError "evalDo: unexpected input")

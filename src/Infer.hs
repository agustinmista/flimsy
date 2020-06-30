{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Infer where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import Data.List (nub)

import Env (Env)
import qualified Env as Env

import Syntax
import Subst
import Solve
import Error

----------------------------------------
-- | Inference monad
----------------------------------------

type Infer = ReaderT TypeEnv (StateT InferState (Except TypeError))

type TypeEnv = Env Scheme

-- | Run the inference monad
runInfer :: TypeEnv -> Infer a -> Either TypeError a
runInfer env m = runExcept (flip evalStateT initInfer (runReaderT m env))

-- | Inference state
data InferState = InferState { count :: Int }

initInfer :: InferState
initInfer = InferState { count = 0 }

-- | Lookup type in the environment
lookupTypeOf :: Expr -> Var -> Infer Type
lookupTypeOf expr var = do
  env <- ask
  case Env.lookup var env of
    Nothing  -> throwError (UnboundVariable var :@ expr)
    Just tsc -> instantiate tsc

-- | Create a fresh type variable
freshTVar :: Infer Type
freshTVar = do
  s <- get
  put s { count = count s + 1 }
  return (VarT (letters !! count s))

-- | Instantiate a type scheme with fresh type variables
instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const freshTVar) as
  let s = fromList (zip as as')
  return (apply s t)

-- | Extend type environment
inExtEnv :: (Var, Scheme) -> Infer a -> Infer a
inExtEnv (var, tsc) m = do
  let scope e = Env.remove e var `Env.extend` (var, tsc)
  local scope m

----------------------------------------
-- | Inferring type of expressions
----------------------------------------

typeOfExpr :: TypeEnv -> Expr -> Either TypeError Scheme
typeOfExpr env expr = do
  (ty, cs) <- runInfer env (inferExpr expr)
  case runSolve cs of
    Left err -> throwError (err :@ expr)
    Right subst -> return (closeOver (apply subst ty))

constraintsOfExpr :: TypeEnv -> Expr -> Either TypeError ([Constraint], Subst, Type, Scheme)
constraintsOfExpr env expr = do
  (ty, cs) <- runInfer env (inferExpr expr)
  case runSolve cs of
    Left err -> throwError (err :@ expr)
    Right subst -> return (cs, subst, ty, closeOver (apply subst ty))

inferExpr :: Expr -> Infer (Type, [Constraint])
inferExpr expr = do
  case expr of
    -- VarE
    VarE x -> do
      t <- lookupTypeOf expr x
      return (t, [])
    -- AppE
    AppE e1 e2 -> do
      (t1, c1) <- inferExpr e1
      (t2, c2) <- inferExpr e2
      tv <- freshTVar
      return (tv, c1 <> c2 <> [(t1, t2 :->: tv)])
    -- LamE
    LamE x e -> do
      tv <- freshTVar
      (t, c) <- inExtEnv (x, Forall [] tv) (inferExpr e)
      return (tv :->: t, c)
    -- LetE
    LetE b e2 -> do
      let (var, e1) = getBind b
      env <- ask
      (t1, c1) <- inferExpr e1
      case runSolve c1 of
        Left err -> throwError (err :@ expr)
        Right sub -> do
          let tsc = generalize (apply sub env) (apply sub t1)
          (t2, c2) <- inExtEnv (var, tsc) (local (apply sub) (inferExpr e2))
          return (t2, c1 <> c2)
    -- LitE
    LitE (IntL _) -> return (intT, [])
    LitE (DoubleL _) -> return (doubleT, [])
    LitE (StringL _) -> return (stringT, [])
    LitE (BoolL _) -> return (boolT, [])
    LitE (CharL _) -> return (charT, [])
    -- InfixE
    InfixE op e1 e2 -> do
      opt <- lookupTypeOf expr op
      (t1, c1) <- inferExpr e1
      (t2, c2) <- inferExpr e2
      tv <- freshTVar
      let u1 = t1 :->: (t2 :->: tv)
      return (tv, c1 <> c2 <> [(opt, u1)])
    -- IfE
    IfE cond tr fl -> do
      (t1, c1) <- inferExpr cond
      (t2, c2) <- inferExpr tr
      (t3, c3) <- inferExpr fl
      return (t2, c1 <> c2 <> c3 <> [(t1, boolT), (t2, t3)])
    -- CaseE
    CaseE e alts -> do
      (te, ce) <- inferExpr e
      (tas, cs) <- unzip <$> mapM (inferCaseAlt expr te) alts
      -- let csas = [(te, ta) | ta <- tas]
      case tas of
        [] -> error "inferExpr: unexpected empty case"
        (t:ts) -> do
          let csb = [(t,t') | t' <- ts]
          return (t, ce <> concat cs <> csb)
    -- FixE
    FixE e -> do
      (t1, c1) <- inferExpr e
      tv <- freshTVar
      return (tv, c1 <> [(tv :->: tv, t1)])
    -- TupE
    TupE es -> do
      (ts, cs) <- unzip <$> mapM inferExpr es
      return (TupT ts, concat cs)
    -- SumE
    SumE (Left e) -> do
      tv <- freshTVar
      (t1, c1) <- inferExpr e
      return (t1 :+: tv, c1)
    SumE (Right e) -> do
      tv <- freshTVar
      (t1, c1) <- inferExpr e
      return (tv :+: t1, c1)
    -- AsE
    AsE e t -> do
      (t1, c1) <- inferExpr e
      return (t, c1 <> [(t, t1)])

----------------------------------------
-- | Inferring case alternatives
----------------------------------------

inferCaseAlt :: Expr -> Type -> Alt -> Infer (Type, [Constraint])
inferCaseAlt caseE _ (Alt pat _) | nonLinear pat = do
  throwError (NonLinearPattern pat :@ caseE)
inferCaseAlt caseE te (Alt pat body) = do
  -- first check that the type of the pattern unifies
  -- with the type of the scrutinee and obtain a substitution
  -- for its pattern variables
  (tp, tscs) <- inferPat pat
  case runSolve [(te, tp)] of
    Left err -> throwError (err :@ caseE)
    Right sub -> do
      -- then, substitute the pattern variables in the body of the pattern
      -- alternative and infer its type
      let patVarTys = fmap (apply sub) <$> tscs
      let withPatVarsInScope m = foldr inExtEnv m patVarTys
      (tb, cb) <- withPatVarsInScope (local (apply sub) (inferExpr body))
      return (tb, cb <> [(te, tp)])

----------------------------------------
-- | Inferring type of patterns
----------------------------------------

typeOfPat :: Pat -> Either TypeError (Scheme, [(Var, Scheme)])
typeOfPat pat = do
  (ty, ins) <- runInfer Env.empty (inferPat pat)
  return (closeOver ty, ins)

inferPat :: Pat -> Infer (Type, [(Var, Scheme)])
inferPat pat =
  local (const Env.empty) $ do
    case pat of
      -- LitP
      LitP (IntL _) -> return (intT, [])
      LitP (DoubleL _) -> return (doubleT, [])
      LitP (StringL _) -> return (stringT, [])
      LitP (BoolL _) -> return (boolT, [])
      LitP (CharL _) -> return (charT, [])
      -- VarP
      VarP v -> do
        tv <- freshTVar
        return (tv, [(v, Forall [] tv)])
      -- WildP
      WildP -> do
        tv <- freshTVar
        return (tv, [])
      -- TupP
      TupP ps -> do
        (ts, ins) <- unzip <$> mapM inferPat ps
        return (TupT ts, concat ins)
      -- SumP
      SumP (Left p) -> do
        tv <- freshTVar
        (t1, ins) <- inferPat p
        return (t1 :+: tv, ins)
      SumP (Right p) -> do
        tv <- freshTVar
        (t1, ins) <- inferPat p
        return (tv :+: t1, ins)

patToExpr :: Pat -> Expr
patToExpr (LitP l) = LitE l
patToExpr (VarP v) = VarE v
patToExpr WildP = VarE (mkVar "_")
patToExpr (TupP ps) = TupE (patToExpr <$> ps)
patToExpr (SumP (Left p)) = SumE (Left (patToExpr p))
patToExpr (SumP (Right p)) = SumE (Right (patToExpr p))

----------------------------------------
-- | Helpers
----------------------------------------

-- | Canonicalize and return the polymorphic toplevel type
closeOver :: Type -> Scheme
closeOver = normalize . generalize Env.empty


-- | Type variables supply
letters :: [TVar]
letters = mkTVar <$> names
  where
    names =
      [ [v]
      | v <- ['a' .. 'z']] <>
      [ c : show n
      | c <- ['a' .. 'z']
      , n <- [0 :: Int ..]
      ]

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall as t
  where
    as = toList (ftv t `difference` ftv env)

normalize :: Scheme -> Scheme
normalize (Forall _ body) =
  Forall (snd <$> ord) (normtype body)
  where
    ord = zip (nub (fv body)) letters

    fv (VarT a)   = [a]
    fv (ConT _)   = []
    fv (a :->: b) = fv a <> fv b
    fv (a :+: b)  = fv a <> fv b
    fv (TupT ts)  = concatMap fv ts

    normtype (VarT a) =
      case lookup a ord of
        Just x  -> VarT x
        Nothing -> error "normalize: type variable not in signature"
    normtype (ConT a) = ConT a
    normtype (a :->: b) = normtype a :->: normtype b
    normtype (a :+: b) = normtype a :+: normtype b
    normtype (TupT ts) = TupT (normtype <$> ts)
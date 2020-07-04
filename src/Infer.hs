{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Infer where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import Data.List

import Env (Env)
import qualified Env as Env

import Var
import Syntax
import Type
import Subst
import Solve
import Error

import Debug.Trace

debug :: Monad m => String -> m ()
debug msg = trace msg (return ())

----------------------------------------
-- | Inference monad
----------------------------------------

type Infer = ReaderT TcEnv (StateT InferState (Except TypeError))

type TcEnv = Env Scheme

-- | Run the inference monad
runInfer :: TcEnv -> Infer a -> Either TypeError a
runInfer env m = runExcept (flip evalStateT initInfer (runReaderT m env))

-- | Inference state
data InferState = InferState { count :: Int }

initInfer :: InferState
initInfer = InferState { count = 0 }

-- | Lookup type in the environment
lookupTypeOf :: PsExpr -> Var -> Infer Type
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

instantiate' :: Scheme -> Either TypeError Type
instantiate' tsc = runInfer Env.empty (instantiate tsc)

-- | Extend type environment
inExtEnv :: (Var, Scheme) -> Infer a -> Infer a
inExtEnv (var, tsc) m = do
  let scope e = Env.remove e var `Env.extend` (var, tsc)
  local scope m

----------------------------------------
-- | Inferring type of expressions
----------------------------------------

typeCheck :: TcEnv -> PsExpr -> Either TypeError (Scheme, TcExpr)
typeCheck env expr = do
  (ty, cs, expr') <- runInfer env (inferExpr expr)
  case runSolve cs of
    Left err -> throwError (err :@ expr)
    Right subst -> return (closeOver (apply subst ty), fmap (apply subst) <$> expr')

constraintsOfExpr :: TcEnv -> PsExpr -> Either TypeError ([Constraint], Subst, Type, Scheme)
constraintsOfExpr env expr = do
  (ty, cs, _) <- runInfer env (inferExpr expr)
  case runSolve cs of
    Left err -> throwError (err :@ expr)
    Right subst -> return (cs, subst, ty, closeOver (apply subst ty))

inferExpr :: PsExpr -> Infer (Type, [Constraint], TcExpr)
inferExpr expr = do
  case expr of
    -- VarE
    VarE var -> do
      t <- lookupTypeOf expr var
      return (t, [], VarE (var, t))
    -- AppE
    AppE e1 e2 -> do
      (t1, c1, e1') <- inferExpr e1
      (t2, c2, e2') <- inferExpr e2
      tv <- freshTVar
      return (tv, c1 <> c2 <> [(t1, t2 :->: tv)], AppE e1' e2')
    -- LamE
    LamE var body -> do
      tv <- freshTVar
      (t, c, body') <- inExtEnv (var, Forall [] tv) (inferExpr body)
      return (tv :->: t, c, LamE (var, tv) body')
    -- LetE
    LetE b e2 -> do
      let (isVal, var, e1) = splitBind b
      env <- ask
      (t1, c1, e1') <- inferExpr e1
      case runSolve c1 of
        Left err -> throwError (err :@ expr)
        Right sub -> do
          let tsc = generalize (apply sub env) (apply sub t1)
          (t2, c2, e2') <- inExtEnv (var, tsc) (local (apply sub) (inferExpr e2))
          let b' = mergeBind isVal (var, t1) e1'
          return (t2, c1 <> c2, LetE b' e2')
    -- LitE
    LitE (IntL n) ->
      return (intT, [], LitE (IntL n))
    LitE (DoubleL n) ->
      return (doubleT, [], LitE (DoubleL n))
    LitE (StringL s) ->
      return (stringT, [], LitE (StringL s))
    LitE (BoolL b) ->
      return (boolT, [], LitE (BoolL b))
    LitE (CharL c) ->
      return (charT, [] , LitE (CharL c))
    -- InfixE
    InfixE op e1 e2 -> do
      opt <- lookupTypeOf expr op
      (t1, c1, e1') <- inferExpr e1
      (t2, c2, e2') <- inferExpr e2
      tv <- freshTVar
      let u1 = t1 :->: (t2 :->: tv)
      return (tv, c1 <> c2 <> [(u1, opt)], InfixE (op, opt) e1' e2')
    -- IfE
    IfE cond tr fl -> do
      (t1, c1, cond') <- inferExpr cond
      (t2, c2, tr') <- inferExpr tr
      (t3, c3, fl') <- inferExpr fl
      return (t2, c1 <> c2 <> c3 <> [(boolT, t1), (t2, t3)], IfE cond' tr' fl')
    -- CaseE
    CaseE e alts -> do
      (te, ce, e') <- inferExpr e
      (tas, cs, alts') <- unzip3 <$> mapM (inferCaseAlt expr te) alts
      -- let csas = [(te, ta) | ta <- tas]
      case tas of
        [] -> throwError (InternalTcError "unexpected empty case on CaseE")
        (t:ts) -> do
          let csb = [(t,t') | t' <- ts]
          return (t, ce <> concat cs <> csb, CaseE e' alts')
    -- FixE
    FixE e -> do
      (t1, c1, e') <- inferExpr e
      tv <- freshTVar
      return (tv, c1 <> [(tv :->: tv, t1)], FixE e')
    -- TupE
    TupE es -> do
      (ts, cs, es') <- unzip3 <$> mapM inferExpr es
      return (TupT ts, concat cs, TupE es')
    -- SumE
    SumE (Left e) -> do
      tv <- freshTVar
      (t1, c1, e') <- inferExpr e
      return (t1 :+: tv, c1, SumE (Left e'))
    SumE (Right e) -> do
      tv <- freshTVar
      (t1, c1, e') <- inferExpr e
      return (tv :+: t1, c1, SumE (Right e'))
    -- ListE
    ListE [] -> do
      tv <- freshTVar
      return (ListT tv, [], ListE [])
    ListE (e:es) -> do
      (t, c, e') <- inferExpr e
      (ts, cs, es') <- unzip3 <$> mapM inferExpr es
      let lcs = [ (t,t') | t' <- ts ]
      return (ListT t, c <> concat cs <> lcs, ListE (e':es'))
    -- DoE
    DoE stmts -> do
      (t, cs, stmts') <- inferDo stmts
      return (t, cs, DoE stmts')

----------------------------------------
-- | Inferring case alternatives
----------------------------------------

inferCaseAlt :: PsExpr -> Type -> PsAlt -> Infer (Type, [Constraint], TcAlt)
inferCaseAlt caseE _ (Alt pat _) | nonLinear pat = do
  throwError (NonLinearPattern pat :@ caseE)
inferCaseAlt caseE te (Alt pat body) = do
  -- first check that the type of the pattern unifies
  -- with the type of the scrutinee and obtain a substitution
  -- for its pattern variables
  (tp, csp, tscs, pat') <- inferPat pat
  case runSolve [(te, tp)] of
    Left err -> throwError (err :@ caseE)
    Right sub -> do
      -- then, substitute the pattern variables in the body of the pattern
      -- alternative and infer its type
      let patVarTys = fmap (apply sub) <$> tscs
      let withPatVarsInScope m = foldr inExtEnv m patVarTys
      (tb, csb, body') <- withPatVarsInScope (local (apply sub) (inferExpr body))
      return (tb, csp <> csb <> [(te, tp)], Alt pat' body')

----------------------------------------
-- | Inferring type of patterns
----------------------------------------

-- typeOfPat :: Pat -> Either TypeError (Scheme, [Constraint], [(Var, Scheme)])
-- typeOfPat pat = do
--   (ty, cs, ins, _) <- runInfer Env.empty (inferPat pat)
--   return (closeOver ty, cs, ins)

inferPat :: PsPat -> Infer (Type, [Constraint], [(Var, Scheme)], TcPat)
inferPat pat =
  local (const Env.empty) $ do
    case pat of
      -- LitP
      LitP (IntL n) ->
        return (intT, [], [], LitP (IntL n))
      LitP (DoubleL n) ->
        return (doubleT, [], [], LitP (DoubleL n))
      LitP (StringL s) ->
        return (stringT, [], [], LitP (StringL s))
      LitP (BoolL b) ->
        return (boolT, [], [], LitP (BoolL b))
      LitP (CharL c) ->
        return (charT, [], [], LitP (CharL c))
      -- VarP
      VarP var -> do
        tv <- freshTVar
        return (tv, [], [(var, Forall [] tv)], VarP (var, tv))
      -- WildP
      WildP -> do
        tv <- freshTVar
        return (tv, [], [], WildP)
      -- TupP
      TupP ps -> do
        (ts, css, inss, ps') <- unzip4 <$> mapM inferPat ps
        return (TupT ts, concat css, concat inss, TupP ps')
      -- SumP
      SumP (Left p) -> do
        tv <- freshTVar
        (t1, cs, ins, p') <- inferPat p
        return (t1 :+: tv, cs, ins, SumP (Left p'))
      SumP (Right p) -> do
        tv <- freshTVar
        (t1, cs, ins, p') <- inferPat p
        return (tv :+: t1, cs, ins, SumP (Right p'))
      -- ListP
      ListP NilP -> do
        tv <- freshTVar
        return (ListT tv, [], [], ListP NilP)
      ListP (ConsP [] _) -> do
        throwError (InternalTcError "unexpected empty list pattern")
      ListP (ConsP (hd:hds) Nothing) -> do
        (hdt,  hdcs, hdins, hd') <- inferPat hd
        (hdts, hdcss, hdinss, hds') <- unzip4 <$> mapM inferPat hds
        let cs = [ (hdt, t') | t' <- hdts ]
        return (ListT hdt, hdcs <> concat hdcss <> cs, hdins <> concat hdinss,
                ListP (ConsP (hd':hds') Nothing))
      ListP (ConsP (hd:hds) (Just tl)) -> do
        (hdt,  hdcs, hdins, hd') <- inferPat hd
        (hdts, hdcss, hdinss, hds') <- unzip4 <$> mapM inferPat hds
        (tlt, tlcs, tlins, tl') <- inferPat tl
        let cs = [ (hdt, t') | t' <- hdts ] <> [(ListT hdt, tlt)]
        return (ListT hdt, hdcs <> concat hdcss <> tlcs <> cs,
                 hdins <> concat hdinss <> tlins,
                 ListP (ConsP (hd':hds') (Just tl')))

-- patToExpr :: Pat -> Expr
-- patToExpr (LitP l) = LitE l
-- patToExpr (VarP v) = VarE False v
-- patToExpr WildP = VarE False (mkVar "_")
-- patToExpr (TupP ps) = TupE (patToExpr <$> ps)
-- patToExpr (SumP (Left p)) = SumE (Left (patToExpr p))
-- patToExpr (SumP (Right p)) = SumE (Right (patToExpr p))
-- patToExpr (ListP NilP) = ListE []
-- patToExpr (ListP (ConsP hds Nothing)) = ListE (patToExpr <$> hds)
-- patToExpr (ListP (ConsP hds (Just _))) = ListE ((patToExpr <$> hds)
--                                                 <> [VarE False (mkVar "...")])

----------------------------------------
-- | Inferring do statements
----------------------------------------

inferDo :: [PsDoStmt] -> Infer (Type, [Constraint], [TcDoStmt])
inferDo [ExprStmt e] = do
  tv <- freshTVar
  (t, cs, e') <- inferExpr e
  let cio = [(IOT tv, t)]
  return (t, cs <> cio, [ExprStmt e'])
inferDo (BindStmt v e : xs) = do
  tv <- freshTVar
  (te, cse, e') <- inferExpr e
  (txs, csxs, xs') <- inExtEnv (v, Forall [] tv) (inferDo xs)
  let cio = [(IOT tv, te)]
  return (txs, cse <> csxs <> cio, (BindStmt (v, tv) e' : xs'))
inferDo (ExprStmt e : xs) = do
  tv <- freshTVar
  (te, cse, e') <- inferExpr e
  (txs, csxs, xs') <- inferDo xs
  let cio = [(IOT tv, te)]
  return (txs, cse <> csxs <> cio, (ExprStmt e' : xs'))
inferDo _ = do
  throwError (InternalTcError "inferDo: unexpected input")

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

generalize :: TcEnv -> Type -> Scheme
generalize env t = Forall as t
  where
    as = toList (ftv t `difference` ftv env)

normalize :: Scheme -> Scheme
normalize (Forall _ body) =
  Forall (snd <$> ord) (normtype body)
  where
    ord = zip (toList (ftv body)) letters

    -- fv (VarT a)   = [a]
    -- fv (ConT _)   = []
    -- fv (a :->: b) = fv a <> fv b
    -- fv (a :+: b)  = fv a <> fv b
    -- fv (TupT ts)  = concatMap fv ts
    -- fv (ListT t)  = fv t

    normtype (VarT a) =
      case lookup a ord of
        Just x  -> VarT x
        Nothing -> error "normalize: type variable not in signature"
    normtype (ConT a) = ConT a
    normtype (a :->: b) = normtype a :->: normtype b
    normtype (a :+: b) = normtype a :+: normtype b
    normtype (TupT ts) = TupT (normtype <$> ts)
    normtype (ListT t) = ListT (normtype t)
    normtype (IOT t)   = IOT (normtype t)

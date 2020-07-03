module Solve where

import Control.Monad.Except

import Type
import Subst
import Error

----------------------------------------
-- | Constraint solver
----------------------------------------

-- | Constraint solver monad
type Solve = Except TypeError

-- | Type constraints
type Constraint = (Type, Type)

-- | Run the constraint solver
runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runExcept (solver (mempty, cs))

-- Unification solver
solver :: (Subst, [Constraint]) -> Solve Subst
solver (sub, cs) =
  case cs of
    [] -> return sub
    ((t1, t2): cs0) -> do
      su1  <- unify t1 t2
      solver (su1 `compose` sub, apply su1 cs0)


unify :: Type -> Type -> Solve Subst
unify t1 t2 | t1 == t2 = return mempty
unify (VarT v) t = v `bindVarType` t
unify t (VarT v) = v `bindVarType` t
unify (t1 :->: t2) (t3 :->: t4) = unifyMany [t1, t2] [t3, t4]
unify (t1 :+: t2) (t3 :+: t4) = unifyMany [t1, t2] [t3, t4]
unify (TupT ts1) (TupT ts2) = unifyMany ts1 ts2
unify (ListT t1) (ListT t2) = unify t1 t2
unify (IOT t1) (IOT t2) = unify t1 t2
unify t1 t2 = throwError (UnificationFail t1 t2)

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return mempty
unifyMany (t1 : ts1) (t2 : ts2) = do
  su1 <- unify t1 t2
  su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
  return (su2 `compose` su1)
unifyMany t1 t2 = throwError (UnificationMismatch t1 t2)

bindVarType :: TVar -> Type -> Solve Subst
bindVarType a t
  | t == VarT a = return mempty
  | occursCheck a t = throwError (InfiniteType a t)
  | otherwise = return (singleton a t)

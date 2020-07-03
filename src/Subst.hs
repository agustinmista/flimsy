{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Subst
  ( Subst
  , singleton
  , fromList
  , compose
  , occursCheck
  , Substitutable(..)
  , Set.difference
  , Set.toList
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Type
import Env (Env, elems)

----------------------------------------
-- | Type substitutions
----------------------------------------

newtype Subst = Subst (Map TVar Type)
  deriving (Eq, Ord, Show)

instance Semigroup Subst where
  Subst x <> Subst y = Subst (x <> y)

instance Monoid Subst where
  mempty = Subst mempty


singleton :: TVar -> Type -> Subst
singleton tv t = Subst (Map.singleton tv t)

fromList :: [(TVar, Type)] -> Subst
fromList xs = Subst (Map.fromList xs)

-- | Compose substitutions
compose :: Subst -> Subst -> Subst
compose (Subst s1) (Subst s2) =
  Subst (Map.map (apply (Subst s1)) s2 `Map.union` s1)

-- | Substitutable types
class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set TVar

occursCheck ::  Substitutable a => TVar -> a -> Bool
occursCheck tv a = tv `Set.member` ftv a

instance Substitutable Type where
  apply (Subst s) (VarT a)     = Map.findWithDefault (VarT a) a s
  apply _         (ConT a)     = ConT a
  apply s         (t1 :->: t2) = apply s t1 :->: apply s t2
  apply s         (t1 :+: t2)  = apply s t1 :+: apply s t2
  apply s         (TupT ts)    = TupT (apply s <$> ts)
  apply s         (ListT t)    = ListT (apply s t)
  apply s         (IOT t)      = IOT (apply s t)

  ftv (VarT a)     = Set.singleton a
  ftv (ConT _)     = Set.empty
  ftv (t1 :->: t2) = ftv t1 `Set.union` ftv t2
  ftv (t1 :+: t2)  = ftv t1 `Set.union` ftv t2
  ftv (TupT ts)    = Set.unions (ftv <$> ts)
  ftv (ListT t)    = ftv t
  ftv (IOT t)      = ftv t

instance Substitutable Scheme where
  apply (Subst s) (Forall as t) =
    Forall as (apply s' t)
      where s' = Subst (foldr Map.delete s as)
  ftv (Forall as t) =
    ftv t `Set.difference` Set.fromList as

instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
   apply s (t1, t2) = (apply s t1, apply s t2)
   ftv (t1, t2) = ftv t1 `Set.union` ftv t2

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable a => Substitutable (Env a) where
  apply s env = apply s <$> env
  ftv env = ftv (Env.elems env)

module TypeEnv
  ( Env
  , empty
  , extend
  , remove
  , lookup
  , merge
  , keys
  , fromList
  , toList
  ) where

import Prelude hiding (lookup)

import Data.Map (Map)
import qualified Data.Map as Map

import Syntax

-------------------------------------------------------------------------------
-- Typing Environment
-------------------------------------------------------------------------------

data Env a = Env (Map Var a)
  deriving (Eq, Show)

instance Functor Env where
  fmap f (Env a) = Env (fmap f a)

instance Semigroup (Env a) where
  (<>) = merge

instance Monoid (Env a) where
  mempty = empty

empty :: Env a
empty = Env mempty

extend :: Env a -> (Var, a) -> Env a
extend (Env env) (var, a) = Env (Map.insert var a env)

remove :: Env a -> Var -> Env a
remove (Env env) var = Env (Map.delete var env)

lookup :: Var -> Env a -> Maybe a
lookup key (Env env) = Map.lookup key env

merge :: Env a -> Env a -> Env a
merge (Env a) (Env b) = Env (Map.union a b)

keys :: Env a -> [Var]
keys (Env env) = Map.keys env

fromList :: [(Var, a)] -> Env a
fromList xs = Env (Map.fromList xs)

toList :: Env a -> [(Var, a)]
toList (Env env) = Map.toList env

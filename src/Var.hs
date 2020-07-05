module Var where

import Data.Text.Lazy (Text)

----------------------------------------
-- | Variables
----------------------------------------

newtype Var = Var Text
  deriving (Show, Eq, Ord)

mkVar :: Text -> Var
mkVar = Var

varName :: Var -> Text
varName (Var t) = t

----------------------------------------
-- | Variables
----------------------------------------

newtype TVar = TVar Text
  deriving (Show, Eq, Ord)

mkTVar :: Text -> TVar
mkTVar = TVar

tVarName :: TVar -> Text
tVarName (TVar v) = v

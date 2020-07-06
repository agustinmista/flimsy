module Var where

----------------------------------------
-- | Variables
----------------------------------------

data Var = Var
  { var_qual :: Maybe String
  , var_name :: String
  } deriving (Show, Eq, Ord)

mkVar :: String -> Var
mkVar = Var Nothing

mkQualVar :: String -> String -> Var
mkQualVar m v = Var (Just m) v

----------------------------------------
-- | Type Variables
----------------------------------------

newtype TVar = TVar
  { tvar_name :: String
  } deriving (Show, Eq, Ord)

mkTVar :: String -> TVar
mkTVar = TVar

tVarName :: TVar -> String
tVarName (TVar v) = v

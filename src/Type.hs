{-# LANGUAGE OverloadedStrings #-}
module Type where

import Data.Text.Lazy (Text, pack, unpack)

import Var

----------------------------------------
-- | Types
----------------------------------------

data Type =
    VarT TVar
  | ConT Var
  | Type :->: Type
  | Type :+: Type
  | TupT [Type]
  | ListT Type
  | IOT Type
  deriving (Show, Eq, Ord)

infix  :+:
infixr :->:

-- | Primitive types
unitT :: Type
unitT = TupT []

intT :: Type
intT = ConT (mkVar "Int")

doubleT :: Type
doubleT = ConT (mkVar "Double")

stringT :: Type
stringT = ConT (mkVar "String")

boolT :: Type
boolT = ConT (mkVar "Bool")

charT :: Type
charT = ConT (mkVar "Char")

ioT :: Type -> Type
ioT = IOT

isIOType :: Type -> Bool
isIOType (IOT _) = True
isIOType _       = False

----------------------------------------
-- | Type schemes
----------------------------------------

newtype TVar = TVar { unTVar :: Text }
  deriving (Show, Eq, Ord)

mkTVar :: String -> TVar
mkTVar = TVar . pack

showTVar :: TVar -> String
showTVar = unpack . unTVar

data Scheme = Forall [TVar] Type
  deriving (Show, Eq, Ord)

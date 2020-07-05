{-# LANGUAGE OverloadedStrings #-}
module Type where

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

data Scheme = Forall [TVar] Type
  deriving (Show, Eq, Ord)

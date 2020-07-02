{-# LANGUAGE OverloadedStrings #-}
module Syntax where

import Data.List
import Data.Text.Lazy (Text, pack, unpack)

----------------------------------------
-- | Variables
----------------------------------------

data Var = Var { varIO :: Bool, varName :: Text }
  deriving (Show, Eq, Ord)

mkVar :: Text -> Var
mkVar = Var False

markAsIO :: Var -> Var
markAsIO var = var { varIO = True }

showVar :: Var -> String
showVar = unpack . varName

----------------------------------------
-- | Top level declarations
----------------------------------------

data Decl =
  BindD Bind
  deriving (Show, Eq)

----------------------------------------
-- | Binds
----------------------------------------

data Bind =
    ValB Var Expr
  | FunB Var [Var] Expr
  deriving (Show, Eq)

-- | Split a bind
getBind :: Bind -> (Var, Expr, Expr -> Bind)
getBind (ValB name expr)      = (name, expr, ValB name)
getBind (FunB name args expr) = (name, funToFix name args expr, fixToFun name args)
  where
    funToFix nm as body =
      FixE (LamE nm (foldr LamE body as))

    fixToFun nm as (FixE (LamE nm' body)) | nm == nm' =
      FunB nm as (dropLams as body)
    fixToFun _ _ _ = impossible

    dropLams []     e                   = e
    dropLams (x:xs) (LamE v e) | x == v = dropLams xs e
    dropLams _      _                   = impossible

    impossible = error "fixToFun: impossible case"


----------------------------------------
-- | Expressions
----------------------------------------

data Expr =
    VarE Var
  | AppE Expr Expr
  | LamE Var Expr
  | LetE Bind Expr
  | LitE Literal
  | InfixE Var Expr Expr
  | IfE Expr Expr Expr
  | CaseE Expr [Alt]
  | FixE Expr
  | TupE [Expr]
  | SumE (Either Expr Expr)
  | ListE [Expr]
  | DoE [DoStmt]
  deriving (Show, Eq)

----------------------------------------
-- | Literals
----------------------------------------

data Literal =
    IntL Int
  | DoubleL Double
  | StringL Text
  | BoolL Bool
  | CharL Char
  deriving (Show, Eq)

----------------------------------------
-- | Case alternatives
----------------------------------------

data Alt = Alt Pat Expr
  deriving (Show, Eq)

getAltPat :: Alt -> Pat
getAltPat (Alt p _) = p

getAltBody :: Alt -> Expr
getAltBody (Alt _ e) = e

----------------------------------------
-- | Patterns
----------------------------------------

data Pat =
    LitP Literal
  | VarP Var
  | WildP
  | TupP [Pat]
  | SumP (Either Pat Pat)
  | ListP ListP
  deriving (Show, Eq)

data ListP =
    NilP
  | ConsP [Pat] (Maybe Pat)
  deriving (Show, Eq)

patVars :: Pat -> [Var]
patVars (VarP v) = [v]
patVars (TupP ps) = concatMap patVars ps
patVars (SumP (Left p)) = patVars p
patVars (SumP (Right p)) = patVars p
patVars (ListP (ConsP hds Nothing)) = concatMap patVars hds
patVars (ListP (ConsP hds (Just tl))) = concatMap patVars hds <> patVars tl
patVars _ = []

nonLinear :: Pat -> Bool
nonLinear pat =
  length (nub pvs) < length pvs
  where pvs = patVars pat

----------------------------------------
-- | Do statements
----------------------------------------

data DoStmt =
    BindStmt Var Expr
  | ExprStmt Expr
  deriving (Show, Eq)

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

isPrimType :: Type -> Bool
isPrimType c = c `elem` [intT, doubleT, stringT, boolT, charT]

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

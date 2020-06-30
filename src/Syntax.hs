module Syntax where

import Data.List
import Data.Text.Lazy (Text, pack, unpack)

----------------------------------------
-- | Variables
----------------------------------------

newtype Var = Var { unVar :: Text }
  deriving (Show, Eq, Ord)

mkVar :: String -> Var
mkVar = Var .  pack

showVar :: Var -> String
showVar = unpack . unVar

----------------------------------------
-- | Top level declarations
----------------------------------------

data Decl =
    BindD Bind
  | SigD Var Type
  | InfixD Fixity Integer Var Var
  deriving (Show, Eq)

-- | Predicates
isInfixD :: Decl -> Bool
isInfixD InfixD {} = True
isInfixD _         = False

isBindD :: Decl -> Bool
isBindD BindD {} = True
isBindD _        = False

isSigD :: Decl -> Bool
isSigD SigD {} = True
isSigD _       = False

----------------------------------------
-- | Binds
----------------------------------------

data Bind =
    ValB Var Expr
  | FunB Var [Var] Expr
  deriving (Show, Eq)

-- | Split a bind
getBind :: Bind -> (Var, Expr)
getBind (ValB name expr)      = (name, expr)
getBind (FunB name args expr) = (name, abstractAndFix name args expr)

-- | Abstract a function body with its arguments using lambdas
abstractAndFix :: Var -> [Var] -> Expr -> Expr
abstractAndFix name args body = FixE (LamE name (foldr LamE body args))

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
  | AsE Expr Type
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
  deriving (Show, Eq)

patVars :: Pat -> [Var]
patVars (VarP v) = [v]
patVars (TupP ps) = concatMap patVars ps
patVars (SumP (Left p)) = patVars p
patVars (SumP (Right p)) = patVars p
patVars _ = []

nonLinear :: Pat -> Bool
nonLinear pat =
  length (nub pvs) < length pvs
  where pvs = patVars pat

----------------------------------------
-- | Fixities
----------------------------------------

data Fixity = L | R | None
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

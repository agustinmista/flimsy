{-# LANGUAGE OverloadedStrings #-}
module Syntax where

import Data.Function
import Data.List

import Var
import Type

----------------------------------------
-- | Modules
----------------------------------------

type ModuleName = String

data Module a = Module
  { module_name :: ModuleName
  , module_path :: FilePath
  , module_decls :: [Decl a]
  } deriving (Show, Eq)

instance Eq a => Ord (Module a) where
  compare = compare `on` module_name

type PsModule = Module Var
type TcModule = Module (Var, Type)

----------------------------------------
-- | Top level declarations
----------------------------------------

data Decl a =
  BindD (Bind a)
  deriving (Show, Eq)

type PsDecl = Decl Var
type TcDecl = Decl (Var, Type)

----------------------------------------
-- | Binds
----------------------------------------

data Bind a =
    ValB a     (Expr a)
  | FunB a [a] (Expr a)
  deriving (Show, Eq)

instance Functor Bind where
  fmap f (ValB var      body) = ValB (f var)               (fmap f body)
  fmap f (FunB var args body) = FunB (f var) (fmap f args) (fmap f body)

type PsBind = Bind Var
type TcBind = Bind (Var, Type)

-- | Split a bind
splitBind :: Bind a -> (Bool, a, Expr a)
splitBind (ValB name expr)      = (True, name, FixE (LamE name expr))
splitBind (FunB name args expr) = (False, name, FixE (LamE name (foldr LamE expr args)))

mergeBind :: Bool -> a -> Expr a -> Bind a
mergeBind True  name (FixE (LamE _ expr)) = ValB name expr
mergeBind False name (FixE (LamE _ expr)) = FunB name args expr'
  where
    (args, expr') = collectLams expr
    collectLams :: Expr a -> ([a], Expr a)
    collectLams (LamE v e) = let (vs, e') = collectLams e in (v:vs, e')
    collectLams e          = ([], e)
mergeBind _ _ _ = error "mergeBind: impossible case"

bindName :: Bind a -> a
bindName bind = let (_, a, _) = splitBind bind in a

----------------------------------------
-- | Expressions
----------------------------------------

data Expr a =
    VarE a
  | AppE (Expr a) (Expr a)
  | LamE a (Expr a)
  | LetE (Bind a) (Expr a)
  | LitE Literal
  | InfixE a (Expr a) (Expr a)
  | IfE (Expr a) (Expr a) (Expr a)
  | CaseE (Expr a) [Alt a]
  | FixE (Expr a)
  | TupE [Expr a]
  | SumE (Either (Expr a) (Expr a))
  | ListE [Expr a]
  | DoE [DoStmt a]
  deriving (Show, Eq)

instance Functor Expr where
  fmap f (VarE v)          = VarE (f v)
  fmap f (AppE x y)        = AppE (fmap f x) (fmap f y)
  fmap f (LamE v b)        = LamE (f v) (fmap f b)
  fmap f (LetE b e)        = LetE (fmap f b) (fmap f e)
  fmap _ (LitE l)          = LitE l
  fmap f (InfixE op e1 e2) = InfixE (f op) (fmap f e1) (fmap f e2)
  fmap f (IfE cond tr fl)  = IfE (fmap f cond) (fmap f tr) (fmap f fl)
  fmap f (CaseE e alts)    = CaseE (fmap f e) (fmap f <$> alts)
  fmap f (FixE e)          = FixE (fmap f e)
  fmap f (TupE es)         = TupE (fmap f <$> es)
  fmap f (SumE (Left e))   = SumE (Left (fmap f e))
  fmap f (SumE (Right e))  = SumE (Right (fmap f e))
  fmap f (ListE es)        = ListE (fmap f <$> es)
  fmap f (DoE stmts)       = DoE (fmap f <$> stmts)

type PsExpr = Expr Var
type TcExpr = Expr (Var, Type)

----------------------------------------
-- | Literals
----------------------------------------

data Literal =
    IntL Int
  | DoubleL Double
  | StringL String
  | BoolL Bool
  | CharL Char
  deriving (Show, Eq)

----------------------------------------
-- | Case alternatives
----------------------------------------

data Alt a = Alt (Pat a) (Expr a)
  deriving (Show, Eq)

instance Functor Alt where
  fmap f (Alt pat body) = Alt (fmap f pat) (fmap f body)

type PsAlt = Alt Var
type TcAlt = Alt (Var, Type)

getAltPat :: Alt a -> Pat a
getAltPat (Alt p _) = p

getAltBody :: Alt a -> Expr a
getAltBody (Alt _ e) = e

----------------------------------------
-- | Patterns
----------------------------------------

data Pat a =
    LitP Literal
  | VarP a
  | WildP
  | TupP [Pat a]
  | SumP (Either (Pat a) (Pat a))
  | ListP (ListP a)
  deriving (Show, Eq)

instance Functor Pat where
  fmap _ (LitP l)         = LitP l
  fmap f (VarP v)         = VarP (f v)
  fmap _ WildP            = WildP
  fmap f (TupP ps)        = TupP (fmap f <$> ps)
  fmap f (SumP (Left e))  = SumP (Left (fmap f e))
  fmap f (SumP (Right e)) = SumP (Right (fmap f e))
  fmap f (ListP p)        = ListP (fmap f p)

type PsPat = Pat Var
type TcPat = Pat (Var, Type)

data ListP a =
    NilP
  | ConsP [Pat a] (Maybe (Pat a))
  deriving (Show, Eq)

instance Functor ListP where
  fmap _ NilP = NilP
  fmap f (ConsP hdp mbtlp) = ConsP (fmap f <$> hdp) (fmap (fmap f) mbtlp)

type PsListP = ListP Var
type TcListP = ListP (Var, Type)

patVars :: Pat a -> [a]
patVars (VarP v) = [v]
patVars (TupP ps) = concatMap patVars ps
patVars (SumP (Left p)) = patVars p
patVars (SumP (Right p)) = patVars p
patVars (ListP (ConsP hds Nothing)) = concatMap patVars hds
patVars (ListP (ConsP hds (Just tl))) = concatMap patVars hds <> patVars tl
patVars _ = []

nonLinear :: Eq a => Pat a -> Bool
nonLinear pat =
  length (nub pvs) < length pvs
  where pvs = patVars pat

----------------------------------------
-- | Do statements
----------------------------------------

data DoStmt a =
    BindStmt a (Expr a)
  | ExprStmt (Expr a)
  deriving (Show, Eq)


instance Functor DoStmt where
  fmap f (BindStmt v e) = BindStmt (f v) (fmap f e)
  fmap f (ExprStmt e)   = ExprStmt (fmap f e)

type PsDoStmt = DoStmt Var
type TcDoStmt = DoStmt (Var, Type)

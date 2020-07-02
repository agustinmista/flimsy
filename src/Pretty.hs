{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Pretty
  ( Pretty(..)
  , pretty
  ) where

import Prelude hiding ((<>))
import Text.Parsec (ParseError)
import Text.PrettyPrint

import Syntax
import Eval
import Error

----------------------------------------
-- | Pretty printing
----------------------------------------

-- | Main pretty printing function
pretty :: Pretty a => a -> String
pretty = renderStyle (Style PageMode 80 2) . pp

-- | Type class for pretty printable types with an explicit level
class Pretty p where
  ppr :: Int -> p -> Doc

  pp, pp' :: p -> Doc
  pp  = ppr 0
  pp' = ppr 1

----------------------------------------
-- | Variables
----------------------------------------

instance Pretty Var where
  ppr _ v = text (showVar v)

----------------------------------------
-- | Top level declarations
----------------------------------------

instance Pretty Decl where
  ppr _ (BindD b) =
    pp b

----------------------------------------
-- | Binds
----------------------------------------

instance Pretty Bind where

  -- | val binds
  ppr _ (ValB v e) =
    text "val" <+> pp v <+> equals <+> pp e
  -- | fun binds
  ppr _ (FunB f args body) =
    text "fun" <+> pp f <+> hsep (pp <$> args)
    <+> equals <+> pp body

----------------------------------------
-- | Expressions
----------------------------------------

instance Pretty Expr where
  -- | variables
  ppr _ (VarE v) =
    pp v
  -- | function application
  ppr p e@(AppE {}) =
    parensIf (p > 0) $
      pp' (viewFun e)
      <+> sep ((\arg -> (pp' arg)) <$> viewArgs e)
  -- | lambda abstractions
  ppr p e@(LamE {}) =
    parensIf (p > 0) $
      text "fn"
      <+> hsep (pp <$> viewLamVars e)
      <+> text "=>"
      <+> pp (viewLamBody e)
  -- | let expressions
  ppr p e@(LetE _ (LetE {})) =
    parensIf (p > 0) $
      text "let" <+> vcat (pp <$> viewLetBinds e)
      $+$ text "in" <+> pp (viewLetBody e)
      $+$ text "end"
  ppr p (LetE b e) =
    parensIf (p > 0) $
      text "let" <+> ppr p b
      $+$ text "in" <+> pp e
      $+$ text "end"
  -- | literals
  ppr _ (LitE l) =
    pp l
  -- | infix operators
  ppr p (InfixE op e1 e2) =
    parensIf (p > 0) $
      pp' e1 <+> pp op <+> pp' e2
  -- | If then else expressions
  ppr p (IfE b th el) =
    parensIf (p > 0) $
      text "if" <+> pp b
      $+$ text "then" <+> pp th
      $+$ text "else" <+> pp el
  -- | case expressions
  ppr p (CaseE e alts) =
    parensIf (p > 0) $
      text "case" <+> pp e <+> text "of"
      $+$ vcat ((\alt -> text "|" <+> pp alt) <$> alts)
  -- | fixpoints
  ppr p (FixE e) =
    parensIf (p > 0) $
      text "fix" <+> (pp' e)
  -- | tuples
  ppr _ (TupE es) =
    parens $
      cat (punctuate comma (pp <$> es))
  -- | sum injections
  ppr p (SumE (Left l)) =
    parensIf (p > 0) $
      text "left" <+> pp' l
  ppr p (SumE (Right r)) =
    parensIf (p > 0) $
      text "right" <+> pp' r
  ppr _ (ListE []) =
    text "[]"
  ppr _ (ListE xs) =
    lbrack <> cat (punctuate comma (pp <$> xs)) <> rbrack
  ppr p (DoE stmts) =
    parensIf (p > 0) $
      text "do" <+> lbrace
      <+> hsep (punctuate semi (pp <$> stmts))
      <+> rbrace

----------------------------------------
-- | Case alternatives
----------------------------------------

instance Pretty Alt where
  ppr _ (Alt pat e) = pp pat <+> text "=>" <+> pp e

----------------------------------------
-- | Patterns
----------------------------------------

instance Pretty Pat where
  ppr _ (LitP l) =
    pp l
  ppr _ (VarP v) =
    pp v
  ppr _ WildP =
    text "_"
  ppr _ (TupP ps) =
    parens $
      cat (punctuate comma (pp <$> ps))
  ppr _ (SumP (Left l)) =
    text "left" <+> pp l
  ppr _ (SumP (Right r)) =
    text "right" <+> pp r
  ppr _ (ListP p) =
    pp p

instance Pretty ListP where
  ppr _ NilP = text "[]"
  ppr _ (ConsP ps Nothing) =
    brackets $
      cat (punctuate comma (pp <$> ps))
  ppr _ (ConsP hds (Just tl)) =
    brackets $
      cat (punctuate comma (pp <$> hds)) <> text "|" <> pp tl

----------------------------------------
-- | Literals
----------------------------------------

instance Pretty Literal where
  ppr _ (IntL n) = int n
  ppr _ (DoubleL n) = double n
  ppr _ (StringL s) = text (show s)
  ppr _ (BoolL True) = text "true"
  ppr _ (BoolL False) = text "false"
  ppr _ (CharL c) = text (show c)

----------------------------------------
-- | Do statements
----------------------------------------

instance Pretty DoStmt where
  ppr _ (BindStmt v e) =
    pp v <+> text "<-" <+> pp e
  ppr _ (ExprStmt e) =
    pp e

----------------------------------------
-- | Types
----------------------------------------

instance Pretty Type where
  ppr _ (VarT v) =
    pp v
  ppr _ (ConT v) =
    pp v
  ppr p (t1 :->: t2) =
    parensIf (p > 0) $
      ppr (p+1) t1 <+> text "->" <+> ppr p t2
  ppr _ (TupT ts) =
    parens $
      cat (punctuate comma (pp <$> ts))
  ppr _ (t1 :+: t2) =
    pp t1 <+> text "+" <+> pp t2
  ppr _ (ListT t) =
    brackets $
      pp t
  ppr _ (IOT t) =
    text "IO" <+> (pp' t)



----------------------------------------
-- | Type schemes
----------------------------------------

instance Pretty TVar where
  ppr _ v = text (showTVar v)

instance Pretty Scheme where
  ppr _ (Forall [] t) =
    pp t
  ppr _ (Forall tv t) =
    text "∀" <+> hsep (pp <$> tv) <+> char '.' <+> pp t

----------------------------------------
-- | Errors
----------------------------------------

instance Pretty ParseError where
  ppr _ s =
    text "parse error!"
    $+$ text (show s)

instance Pretty EscapeError where
  ppr _ (CyclicDeclarations vs) =
    text "dependency error!"
    $+$ text "declarations form a cycle:"
    $+$ hsep (punctuate (text " ->") (pp <$> (last vs : vs)))

instance Pretty TypeError where
  ppr _ (UnificationFail t1 t2) =
    text "type error!"
    $+$ text "could not match expected type:"
    $+$ text "  " <+> pp t1
    $+$ text "with actual type:"
    $+$ text "  " <+> pp t2
  ppr _ (InfiniteType tv t) =
    text "type error!"
    $+$ text "cannot construct infinite type:"
    $+$ text "  " <+> pp tv <+> text "~" <+> pp' t
  ppr _ (UnboundVariable v) =
    text "type error!"
    $+$ text "variable not found:"
    $+$ text "  " <+> pp v
  ppr _ (UnificationMismatch ts1 ts2) =
    text "type error!" <+> text "could not unify types:"
    $+$ text "  " <+> vcat (pp <$> ts1)
    $+$ text "with:"
    $+$ text "  " <+> vcat (pp <$> ts2)
  ppr _ (NonLinearPattern pat) =
    text "type error!" <+> text "non linear pattern:"
    $+$ text "  " <+> pp pat
  ppr _ (InternalTypeCheckingError msg) =
    text "type error!"
    $+$ text msg
  ppr _ (err :@ expr) =
    pp err
    $+$ text "in the expression:"
    $+$ text "  " <+> pp expr

instance Pretty EvalError where
  ppr _ (InternalEvaluationError msg) =
    text "runtime error!"
    $+$ text msg
  ppr _ (MarshallingError ident) =
    text "runtime error!"
    $+$ text "bad marshalling of primitive:"
    $+$ text ident
  ppr _ (NonExhaustiveCase expr) =
    text "runtime error!"
    $+$ text "non-exhaustive patterns in case"
    $+$ text "in the expression:"
    $+$ text "  " <+> pp expr

----------------------------------------
-- | Values
----------------------------------------

instance Pretty Value where
  ppr _ (LitV l) =
    pp l
  ppr _ (ConV v) =
    pp v
  ppr _ (TupV vs) =
    parens $
      cat (punctuate comma (pp <$> vs))
  ppr p (SumV (Left l)) =
    parensIf (p > 0) $
      text "left" <+> pp' l
  ppr p (SumV (Right r)) =
    parensIf (p > 0) $
      text "right" <+> pp' r
  ppr _ (ListV vs) =
    brackets $
      cat (punctuate comma (pp <$> vs))
  ppr _ (ClosureV {}) =
    text "<<function>>"

----------------------------------------
-- | Auxiliary functions
----------------------------------------

-- | Views for lambda terms
viewLamVars :: Expr -> [Var]
viewLamVars (LamE v e) = v : viewLamVars e
viewLamVars _          = []

viewLamBody :: Expr -> Expr
viewLamBody (LamE _ e) = viewLamBody e
viewLamBody x          = x

-- | Views for function applications
viewApp :: Expr -> (Expr, [Expr])
viewApp (AppE e1 e2) = go e1 [e2]
  where
    go (AppE a b) xs = go a (b : xs)
    go f xs          = (f, xs)
viewApp _ = error "viewApp: not an AppE"

viewArgs :: Expr -> [Expr]
viewArgs = snd . viewApp

viewFun :: Expr -> Expr
viewFun = fst . viewApp

-- | Views for let expressions
viewLet :: Expr -> ([Bind], Expr)
viewLet (LetE b e) = (b : bs, body)
  where (bs, body) = viewLet e
viewLet body = ([], body)

viewLetBinds :: Expr -> [Bind]
viewLetBinds = fst . viewLet

viewLetBody :: Expr -> Expr
viewLetBody = snd . viewLet

-- | Conditional parens combinator
parensIf ::  Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

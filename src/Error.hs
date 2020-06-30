module Error where

import Syntax

----------------------------------------
-- | Type inference errors
----------------------------------------

data TypeError =
    UnificationFail     Type Type
  | InfiniteType        TVar Type
  | UnboundVariable     Var
  | UnificationMismatch [Type] [Type]
  | NonLinearPattern    Pat
  | TypeError :@ Expr
  | InternalTypeCheckingError String
  deriving Show

----------------------------------------
-- | Runtime errors
----------------------------------------

data EvalError =
    InternalEvaluationError String
  | NonExhaustiveCase Expr
  deriving Show

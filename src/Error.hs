module Error where

import Syntax

----------------------------------------
-- | Escape errors
----------------------------------------

data EscapeError =
    CyclicDeclarations [Var]
  deriving Show

----------------------------------------
-- | Type inference errors
----------------------------------------

data TypeError =
    InternalTypeCheckingError String
  | UnificationFail     Type Type
  | InfiniteType        TVar Type
  | UnboundVariable     Var
  | UnificationMismatch [Type] [Type]
  | NonLinearPattern    Pat
  | TypeError :@ Expr
  deriving Show

----------------------------------------
-- | Runtime errors
----------------------------------------

data EvalError =
    InternalEvaluationError String
  | MarshallingError String
  | NonExhaustiveCase Expr
  deriving Show

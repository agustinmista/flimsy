module Error where

import Data.Text.Lazy (Text)

import Var
import Syntax
import Type

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
    InternalTcError     Text
  | UnificationFail     Type Type
  | InfiniteType        TVar Type
  | UnboundVariable     Var
  | UnificationMismatch [Type] [Type]
  | NonLinearPattern    PsPat
  | TypeError :@ PsExpr
  deriving Show

----------------------------------------
-- | Runtime errors
----------------------------------------

data EvalError =
    InternalEvalError Text
  | MarshallingError  Text
  | NonExhaustiveCase TcExpr
  deriving Show

module Error where

import Text.Parsec.Error

import Var
import Syntax
import Type

----------------------------------------
-- | Flimsy Errors
----------------------------------------

data FlimsyError
  -- Parsing errors
  = ParseError ParseError
  -- Escape errors
  | CyclicDeclarations [Var]
  -- Type inference errors
  | InternalTcError String
  | UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable Var
  | UnificationMismatch [Type] [Type]
  | NonLinearPattern PsPat
  | FlimsyError :@ PsExpr
  -- Runtime errors
  | InternalEvalError String
  | MarshallingError String
  | NonExhaustiveCase TcExpr
  -- IO Errors
  | FileDoesNotExist String
  deriving Show

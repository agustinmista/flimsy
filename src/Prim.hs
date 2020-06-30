module Prim where

import qualified Data.Text.Lazy.IO as Text

import Control.Monad.Except

import qualified Env as Env

import Syntax
import Eval

----------------------------------------
-- | Primitive operations
----------------------------------------

primEnv :: PrimEnv
primEnv = Env.fromList
  [ prim "prim_int_add__"
    (forAll [] ([intT, intT] --> intT)) $
    \[LitV (IntL n1), LitV (IntL n2)] ->
      return (LitV (IntL (n1+n2)))
  , prim "prim_int_sub__"
    (forAll [] ([intT, intT] --> intT)) $
    \[LitV (IntL n1), LitV (IntL n2)] ->
      return (LitV (IntL (n1-n2)))
  , prim "prim_int_mul__"
    (forAll [] ([intT, intT] --> intT)) $
    \[LitV (IntL n1), LitV (IntL n2)] ->
      return (LitV (IntL (n1*n2)))
  , prim "prim_int_div__"
    (forAll [] ([intT, intT] --> intT)) $
     \[LitV (IntL n1), LitV (IntL n2)] ->
       return (LitV (IntL (n1 `div` n2)))
  , prim "prim_eq__"
    (forAll ["a"] ([var "a", var "a"] --> boolT)) $
    \[a, b] ->
      return (LitV (BoolL (a == b)))
  , prim "prim_getline__"
    (forAll [] ([stringT] --> stringT)) $
    \[LitV (StringL msg)] -> do
      liftIO $ Text.putStr msg
      str <- liftIO $ Text.getLine
      return (LitV (StringL str))
  , prim "prim_putline__"
    (forAll [] ([stringT] --> unit)) $
    \[LitV (StringL str)] -> do
      liftIO $ Text.putStrLn str
      return (TupV [])
  ]

----------------------------------------
-- | Builders
----------------------------------------

unit = TupT []

var x = VarT (mkTVar x)

prim name ty body = (mkVar name, (ty, impl body))

forAll vs ty = Forall (mkTVar <$> vs) ty

args --> ret = TupT args :->: ret

impl body = PrimOp $ \x ->
  case x of
    TupV vs -> body vs

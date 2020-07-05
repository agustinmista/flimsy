{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module Prim
  ( primitives
  ) where

import qualified Data.Text.Lazy.IO as Text
import Control.Monad.Except

import qualified Env as Env

import Var
import Infer
import Syntax
import Parser
import Eval
import Error
import Util

boot :: File
boot = "<boot>"

----------------------------------------
-- | Primitive operations
----------------------------------------

primitives :: PrimEnv
primitives = Env.fromList
  -- integer primitives
  [ prim "flimsy_prim_int_add" "(Int,Int) -> Int" (flimsy_prim_int_binop (+))
  , prim "flimsy_prim_int_sub" "(Int,Int) -> Int" (flimsy_prim_int_binop (-))
  , prim "flimsy_prim_int_mul" "(Int,Int) -> Int" (flimsy_prim_int_binop (*))
  , prim "flimsy_prim_int_div" "(Int,Int) -> Int" (flimsy_prim_int_binop div)
  -- list primitives
  , prim "flimsy_prim_list_cons" "(a, [a]) -> [a]" flimsy_prim_list_cons
  -- io primitives
  , prim "flimsy_prim_io_getline" "() -> IO String" flimsy_prim_io_getline
  , prim "flimsy_prim_io_putstr"  "String -> IO ()" flimsy_prim_io_putstr
  , prim "flimsy_prim_io_putline" "String -> IO ()" flimsy_prim_io_putline
  , prim "flimsy_prim_io_return"  "a -> IO a"       flimsy_prim_io_return
  -- show/read primitives
  , prim "flimsy_prim_show"       "a -> String"     flimsy_prim_show
  ]

----------------------------------------
-- | Builders
----------------------------------------

prim :: Text -> Text -> (Value -> Eval Value) -> (Var, Prim)
prim name tystr body = (mkVar name, Prim (closeOver ty) (PrimRunner body))
  where Right ty = parseType boot tystr

pattern Int :: Int -> Value
pattern Int n = LitV (IntL n)

-- pattern Bool :: Bool -> Value
-- pattern Bool b = LitV (BoolL b)

pattern String :: Text -> Value
pattern String s = LitV (StringL s)

pattern (:*:) :: Value -> Value -> Value
pattern (:*:) x y = TupV [x, y]

showText :: Show a => a -> Text
showText = pack . show

----------------------------------------
-- | Builders
----------------------------------------

flimsy_prim_int_binop :: (Int -> Int -> Int) -> Value -> Eval Value
flimsy_prim_int_binop f (x :*: y) = do
  Int x' <- whnf x
  Int y' <- whnf y
  return (Int (f x' y'))
flimsy_prim_int_binop _ _ =
  throwError (MarshallingError "flimsy_prim_int_binop")

flimsy_prim_io_getline :: Value -> Eval Value
flimsy_prim_io_getline (TupV []) = return $ IOV $ do
  l <- Text.getLine
  return (LitV (StringL l))
flimsy_prim_io_getline _ =
  throwError (MarshallingError "flimsy_prim_io_getline")


flimsy_prim_io_putstr :: Value -> Eval Value
flimsy_prim_io_putstr (LitV (StringL l)) = return $ IOV $ do
  Text.putStr l
  return (TupV [])
flimsy_prim_io_putstr _ =
  throwError (MarshallingError "flimsy_prim_io_putstr")

flimsy_prim_io_putline :: Value -> Eval Value
flimsy_prim_io_putline (LitV (StringL l)) = return $ IOV $ do
  Text.putStrLn l
  return (TupV [])
flimsy_prim_io_putline _ =
  throwError (MarshallingError "flimsy_prim_io_putline")

flimsy_prim_list_cons :: Value -> Eval Value
flimsy_prim_list_cons (hd :*: tl) =
  return (ConsV hd tl)
flimsy_prim_list_cons _ =
  throwError (MarshallingError "flimsy_prim_list_cons")

flimsy_prim_io_return :: Value -> Eval Value
flimsy_prim_io_return val =
  return $ IOV $ return val

flimsy_prim_show :: Value -> Eval Value
flimsy_prim_show val = do
  case val of
    LitV (IntL n) -> do
      return (String (showText n))
    LitV (DoubleL n) -> do
      return (String (showText n))
    LitV (StringL s) -> do
      return (String (showText s))
    LitV (BoolL True) -> do
      return (String "true")
    LitV (BoolL False) -> do
      return (String "false")
    LitV (CharL c) -> do
      return (String (showText c))
    ConV c -> do
      return (String (showText c))
    TupV [] -> do
      return (String "()")
    TupV (v:vs) -> do
      let showElems x [] = do
            String x' <- flimsy_prim_show x
            return (String x')
          showElems x (y:ys) = do
            String x' <- flimsy_prim_show x
            String ys' <- showElems y ys
            return (String (x' <> "," <> ys'))
      String s <- showElems v vs
      return (String ("(" <> s <> ")"))
    SumV (Left v) -> do
      String v' <- flimsy_prim_show v
      return (String ("left " <> v'))
    SumV (Right v) -> do
      String v' <- flimsy_prim_show v
      return (String ("right " <> v'))
    NilV -> do
      return (String "[]")
    ConsV hd tl -> do
      let showElems x NilV = do
            String x' <- flimsy_prim_show x
            return (String x')
          showElems x (ConsV y ys) = do
            String x' <- flimsy_prim_show x
            String ys' <- showElems y ys
            return (String (x' <> "," <> ys'))
          showElems x (ThunkV th) = do
            runThunk th >>= showElems x
          showElems _ _ = do
            throwError (MarshallingError "unexpected value in flimsy_prim_show")
      String s <- showElems hd tl
      return (String ("[" <> s <> "]"))
    IOV {} -> do
      return (String "<<io>>")
    ClosureV {} -> do
      return (String "<<closure>>")
    ThunkV th -> do
      runThunk th >>= flimsy_prim_show

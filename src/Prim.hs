{-# LANGUAGE PatternSynonyms #-}
module Prim
  ( primitives
  ) where

import Control.Monad.Except

import qualified Env as Env

import Var
import Infer
import Syntax
import Parser
import Eval
import Error

boot :: FilePath
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
  -- double primitives
  , prim "flimsy_prim_double_add" "(Double,Double) -> Double" (flimsy_prim_double_binop (+))
  , prim "flimsy_prim_double_sub" "(Double,Double) -> Double" (flimsy_prim_double_binop (-))
  , prim "flimsy_prim_double_mul" "(Double,Double) -> Double" (flimsy_prim_double_binop (*))
  , prim "flimsy_prim_double_div" "(Double,Double) -> Double" (flimsy_prim_double_binop (/))
  -- list primitives
  , prim "flimsy_prim_list_cons" "(a,[a]) -> [a]" flimsy_prim_list_cons
  -- io primitives
  , prim "flimsy_prim_io_getline" "() -> IO String" flimsy_prim_io_getline
  , prim "flimsy_prim_io_putstr"  "String -> IO ()" flimsy_prim_io_putstr
  , prim "flimsy_prim_io_putline" "String -> IO ()" flimsy_prim_io_putline
  , prim "flimsy_prim_io_return"  "a -> IO a"       flimsy_prim_io_return
  -- show/read primitives
  , prim "flimsy_prim_show"       "a -> String"     flimsy_prim_show
  -- eq primitive
  , prim "flimsy_prim_eq"         "(a,a) -> Bool"   flimsy_prim_eq
  ]

----------------------------------------
-- | Builders
----------------------------------------

prim :: String -> String -> (Value -> Eval Value) -> (Var, Prim)
prim name tystr body = (mkVar name, Prim (closeOver ty) (PrimRunner body))
  where Right ty = parseType boot tystr

pattern Int :: Int -> Value
pattern Int n = LitV (IntL n)

pattern Double :: Double -> Value
pattern Double n = LitV (DoubleL n)

pattern Bool :: Bool -> Value
pattern Bool b = LitV (BoolL b)

pattern String :: String -> Value
pattern String s = LitV (StringL s)

pattern Char :: Char -> Value
pattern Char c = LitV (CharL c)

pattern Unit :: Value
pattern Unit = TupV []

pattern (:*:) :: Value -> Value -> Value
pattern (:*:) x y = TupV [x, y]

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

flimsy_prim_double_binop :: (Double -> Double -> Double) -> Value -> Eval Value
flimsy_prim_double_binop f (x :*: y) = do
  Double x' <- whnf x
  Double y' <- whnf y
  return (Double (f x' y'))
flimsy_prim_double_binop _ _ =
  throwError (MarshallingError "flimsy_prim_double_binop")


flimsy_prim_io_getline :: Value -> Eval Value
flimsy_prim_io_getline Unit = return $ IOV $ do
  l <- getLine
  return (String l)
flimsy_prim_io_getline _ =
  throwError (MarshallingError "flimsy_prim_io_getline")

flimsy_prim_io_putstr :: Value -> Eval Value
flimsy_prim_io_putstr (String l) = return $ IOV $ do
  putStr l
  return Unit
flimsy_prim_io_putstr _ =
  throwError (MarshallingError "flimsy_prim_io_putstr")

flimsy_prim_io_putline :: Value -> Eval Value
flimsy_prim_io_putline (String l) = return $ IOV $ do
  putStrLn l
  return Unit
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
      return (String (show n))
    LitV (DoubleL n) -> do
      return (String (show n))
    LitV (StringL s) -> do
      return (String (show s))
    LitV (BoolL True) -> do
      return (String "true")
    LitV (BoolL False) -> do
      return (String "false")
    LitV (CharL c) -> do
      return (String (show c))
    ConV c -> do
      return (String (show c))
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
      return (String ("left (" <> v' <> ")"))
    SumV (Right v) -> do
      String v' <- flimsy_prim_show v
      return (String ("right (" <> v' <> ")"))
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

flimsy_prim_eq :: Value -> Eval Value
flimsy_prim_eq (x :*: y) = do
  case (x, y) of
    (ThunkV th, _) -> runThunk th >>= \x' -> flimsy_prim_eq (x' :*: y)
    (_, ThunkV th) -> runThunk th >>= \y' -> flimsy_prim_eq (x :*: y')
    (LitV l1, LitV l2) -> return (Bool (l1 == l2))
    (ConV c1, ConV c2) -> return (Bool (c1 == c2))
    (TupV [], TupV []) -> return (Bool True)
    (TupV (v:vs), TupV (v':vs')) -> do
      Bool b <- flimsy_prim_eq (v :*: v')
      if b
        then flimsy_prim_eq (TupV vs :*: TupV vs')
        else return (Bool False)
    (SumV (Left v), SumV (Left v')) -> flimsy_prim_eq (v :*: v')
    (SumV (Right v), SumV (Right v')) -> flimsy_prim_eq (v :*: v')
    (NilV, NilV) -> return (Bool True)
    (ConsV v vs, ConsV v' vs') -> do
      Bool b <- flimsy_prim_eq (v :*: v')
      if b
        then flimsy_prim_eq (vs :*: vs')
        else return (Bool False)
    _ -> return (Bool False)
flimsy_prim_eq _ =
  throwError (MarshallingError "flimsy_prim_eq")

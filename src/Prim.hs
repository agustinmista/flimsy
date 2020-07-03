{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module Prim
  ( environment
  , boot
  ) where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as Text
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

environment :: PrimEnv
environment = Env.fromList
  [ prim "floppy_prim_int_add" "(Int,Int) -> Int" $ \(Int x :*: Int y) -> pure (Int (x+y))
  , prim "floppy_prim_int_sub" "(Int,Int) -> Int" $ \(Int x :*: Int y) -> pure (Int (x-y))
  , prim "floppy_prim_int_mul" "(Int,Int) -> Int" $ \(Int x :*: Int y) -> pure (Int (x*y))
  , prim "floppy_prim_int_div" "(Int,Int) -> Int" $ \(Int x :*: Int y) -> pure (Int (div x y))

  , prim "floppy_prim_eq"      "(a,a) -> Bool"    $ \(x :*: y)         -> pure (Bool (x==y))

  , prim "floppy_prim_io_getline" "() -> IO String" getline
  , prim "floppy_prim_io_putline" "String -> IO ()" putline
  , prim "floppy_prim_io_return"  "a -> IO a"       ret

  , prim "floppy_prim_list_cons" "(a, [a]) -> [a]" cons

  ]

getline :: Value -> Eval Value
getline (TupV []) = return $ IOV $ do
  l <- Text.getLine
  return (LitV (StringL l))
getline _ = throwError (MarshallingError "getline")

putline :: Value -> Eval Value
putline (LitV (StringL l)) = return $ IOV $ do
  Text.putStrLn l
  return (TupV [])
putline _ = throwError (MarshallingError "putline")

cons :: Value -> Eval Value
cons (TupV [a, ListV as]) = pure (ListV (a:as))
cons _ = throwError (MarshallingError "cons")

ret :: Value -> Eval Value
ret val = return $ IOV $ do
  return val



----------------------------------------
-- | Builders
----------------------------------------

prim :: Text -> Text -> (Value -> Eval Value) -> (Var, Prim)
prim name tystr body = (mkVar name, Prim (closeOver ty) (PrimRunner body))
  where Right ty = parseType boot tystr

pattern Int :: Int -> Value
pattern Int n = LitV (IntL n)

pattern Bool :: Bool -> Value
pattern Bool b = LitV (BoolL b)

pattern (:*:) :: Value -> Value -> Value
pattern (:*:) x y = TupV [x, y]

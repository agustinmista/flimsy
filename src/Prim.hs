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
  , prim "floppy_prim_io_return"  "a -> IO a"       return

  , prim "floppy_prim_list_cons" "(a, [a]) -> [a]" cons

  ]

getline :: Value -> Eval Value
getline (TupV []) = String <$> liftIO Text.getLine
getline _ = throwError (MarshallingError "getline")

putline :: Value -> Eval Value
putline (String s) = unit <$> liftIO (Text.putStrLn s)
putline _ = throwError (MarshallingError "putline")

cons :: Value -> Eval Value
cons (a :*: ListV as) = pure (ListV (a:as))
cons _ = throwError (MarshallingError "cons")

----------------------------------------
-- | Builders
----------------------------------------

prim :: Text -> Text -> (Value -> Eval Value) -> (Var, Prim)
prim name tystr body = (Var False name, Prim (closeOver ty) (PrimRunner body))
  where Right ty = parseType boot tystr

unit :: () -> Value
unit _ = TupV []

pattern Int :: Int -> Value
pattern Int n = LitV (IntL n)

pattern Bool :: Bool -> Value
pattern Bool b = LitV (BoolL b)

pattern String :: Text -> Value
pattern String s = LitV (StringL s)

pattern (:*:) :: Value -> Value -> Value
pattern (:*:) x y = TupV [x, y]

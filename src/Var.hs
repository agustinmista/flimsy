module Var where

import Data.Text.Lazy (Text)

----------------------------------------
-- | Variables
----------------------------------------

newtype Var = Var Text
  deriving (Show, Eq, Ord)

mkVar :: Text -> Var
mkVar = Var

showVar :: Var -> Text
showVar (Var t) = t

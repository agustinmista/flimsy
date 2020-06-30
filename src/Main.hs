module Main where

import System.Environment

import Repl

----------------------------------------
-- | Entry point
----------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> launchRepl Nothing
    [fname] -> launchRepl (Just fname)
    _       -> putStrLn "invalid arguments"

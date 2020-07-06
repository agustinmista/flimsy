module Util where

import Control.Monad
import Control.Monad.IO.Class

import System.Process

-- | Call shell commands
runShellCommand :: FilePath -> [String] -> IO ()
runShellCommand cmd args = void (rawSystem cmd args)

-- | Say something
say :: MonadIO m => String -> m ()
say = liftIO . putStrLn

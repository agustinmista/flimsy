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

-- | Tuple projections

fst3 :: (a, b, c) -> a
fst3 (a,_,_) = a

snd3 :: (a, b, c) -> b
snd3 (_,b,_) = b

trd3 :: (a, b, c) -> c
trd3 (_,_,c) = c

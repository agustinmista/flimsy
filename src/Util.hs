module Util
  ( module Util
  , Text, pack, unpack
  ) where

import Control.Monad

import System.Process

import Data.Text.Lazy (Text,pack,unpack)
import qualified Data.Text.Lazy.IO as Text

import Control.Monad.IO.Class

-- | File paths as Text for consistency
type File = Text

toFilePath :: File -> FilePath
toFilePath = unpack

readSourceFile :: File -> IO Text
readSourceFile file = Text.readFile (toFilePath file)

-- | Call shell commands

runShellCommand :: Text -> [Text] -> IO ()
runShellCommand cmd args = void (rawSystem (unpack cmd) (unpack <$> args))

-- | Say something
say :: MonadIO m => Text -> m ()
say = liftIO . Text.putStrLn

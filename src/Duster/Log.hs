module Duster.Log where

import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Data.Text (Text)
import qualified Data.Text as T

newtype Message = Message {unMessage :: Text}

newtype Logger = Logger {unLogger :: MVar ()}

createLogger :: IO Logger
createLogger = Logger <$> newMVar ()

logMessage :: Logger -> Message -> IO ()
logMessage logger message = do
  -- Acquire the logger.
  _ <- takeMVar (unLogger logger)
  -- Print the message.
  putStrLn . T.unpack . unMessage $ message
  -- Release the logger.
  putMVar (unLogger logger) ()

logString :: Logger -> String -> IO ()
logString logger s = logMessage logger (Message . T.pack $ s)

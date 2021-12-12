{-# LANGUAGE OverloadedStrings #-}
module Services.LoggerService.LoggerService (
  Handle (..),
  Priority (..),
  logDebug,
  logInfo,
  logWarning,
  logError
  ) where

import Control.Exception ( throw )
import Data.Yaml ( (.:), withObject, FromJSON(parseJSON), Parser )
import Exceptions.ParseException ( ParseException(ParseException) )
import qualified Data.Text as T

-- Log message priorities
data Priority
  = Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Ord, Show, Read)

-- Way to read priority from configuration
instance FromJSON Priority where
  parseJSON = withObject "Priority" $ \ v -> do
    logLevel <- v .: "log-level"  :: Parser T.Text
    return $ case logLevel of
      "Debug" -> Debug
      "Info" -> Info
      "Warning" -> Warning
      "Error" -> Error
      _ -> throw $ ParseException $ "Found " <> logLevel <> ", but it is undefined"

newtype Handle = Handle
  -- Write log message
  { log :: Priority -> T.Text -> IO () }

-- Write Debug, Info, Warning, Error log message
logDebug, logInfo, logWarning, logError :: Handle -> T.Text -> IO ()

logDebug = (`Services.LoggerService.LoggerService.log` Debug)
logInfo = (`Services.LoggerService.LoggerService.log` Info)
logWarning = (`Services.LoggerService.LoggerService.log` Warning)
logError = (`Services.LoggerService.LoggerService.log` Error)

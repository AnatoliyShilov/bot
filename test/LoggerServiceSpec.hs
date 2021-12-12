{-# LANGUAGE OverloadedStrings #-}
module LoggerServiceSpec ( loggerServiceSpec ) where

import Test.Hspec ( hspec, describe, it )
import Control.Concurrent.Async ( mapConcurrently_ )
import qualified Services.LoggerService.LoggerService as LoggerService
import qualified Services.LoggerService.ConsoleLoggerService as ConsoleLoggerService

-- List of partically applied log functions
logList :: [LoggerService.Handle -> IO ()]
logList = [
  (`LoggerService.logInfo` "Info message 1"),
  (`LoggerService.logDebug` "Debug message 1"),
  (`LoggerService.logDebug` "Debug message 2"),
  (`LoggerService.logError` "Error message 1"),
  (`LoggerService.logWarning` "Warning message 1"),
  (`LoggerService.logInfo` "Info message 2"),
  (`LoggerService.logWarning` "Warning message 2"),
  (`LoggerService.logError` "Error message 2")
  ]

loggerServiceSpec :: IO ()
loggerServiceSpec = hspec $ do
  describe "ConsoleLoggerService" $ do
    it "Create Debug logger handle" $ do
      -- Create debug logger service
      handle <- ConsoleLoggerService.new LoggerService.Debug
      -- Check functions which applied on current log level
      mapConcurrently_ (\ logFunc -> logFunc handle) logList
      
    it "Create Info logger handle" $ do
      -- Create info logger service
      handle <- ConsoleLoggerService.new LoggerService.Info
      -- Check functions which applied on current log level
      mapConcurrently_ (\ logFunc -> logFunc handle) logList

    it "Create Warning logger handle" $ do
      -- Create warning logger service
      handle <- ConsoleLoggerService.new LoggerService.Warning
      -- Check functions which applied on current log level
      mapConcurrently_ (\ logFunc -> logFunc handle) logList

    it "Create Error logger handle" $ do
      -- Create error logger service
      handle <- ConsoleLoggerService.new LoggerService.Error
      -- Check functions which applied on current log level
      mapConcurrently_ (\ logFunc -> logFunc handle) logList

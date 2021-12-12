module ListenerServiceSpec (listenerServiceSpec) where

import Test.Hspec ( it, describe, hspec )
import Control.Concurrent.Async ( async, cancel )
import qualified Services.ConfigService as ConfigService
import qualified Services.LoggerService.LoggerService as LoggerService
import qualified Services.LoggerService.ConsoleLoggerService as ConsoleLoggerService
import qualified Services.HttpServices.VkService.VkService_5_131 as VkServiceImpl
import qualified Services.ListenerService.ListenerService as ListenerService
import qualified Services.ListenerService.VkListenerService as VkListenerService
import qualified Models.Configuration as Config

listenerServiceSpec :: IO ()
listenerServiceSpec = hspec $ do
  describe "ListenerService" $ do
    it "VkListenerService" $ do
      -- Create configuration service
      config <- ConfigService.load
      -- Create logger service
      loggerService <- ConsoleLoggerService.new LoggerService.Debug
      -- Create vk service
      vkService <- VkServiceImpl.new config (head $ Config.vkBotList config) loggerService VkServiceImpl.apiVersion
      listenerService <- VkListenerService.new (head $ Config.vkBotList config) loggerService vkService
      action <- async $ ListenerService.listen listenerService
      getLine
      cancel action

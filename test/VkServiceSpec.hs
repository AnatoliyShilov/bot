module VkServiceSpec ( vkServiceSpec ) where

import Test.Hspec ( hspec, describe, it, shouldBe )
import Network.HTTP.Simple ( getResponseBody )
import Data.Maybe ( fromMaybe, isNothing )
import qualified Services.ConfigService as ConfigService
import qualified Services.LoggerService.LoggerService as LoggerService
import qualified Services.LoggerService.ConsoleLoggerService as ConsoleLoggerService
import qualified Services.HttpServices.VkService.VkService as VkService
import qualified Services.HttpServices.VkService.VkService_5_131 as VkServiceImpl
import qualified Models.Configuration as Config
import qualified Models.Vk.Methods.ResolveScreenName as ResolveScreenName
import qualified Models.Vk.Methods.GetLongPollServer as GetLongPollServer
import qualified Models.Vk.Methods.CheckEvents as CheckEvents
import qualified Models.Vk.Methods.MessageSend as MessageSend
import qualified Data.Text as T

vkServiceSpec :: IO ()
vkServiceSpec = hspec $ do
   describe "VkService" $ do
    it "ResolveScreenName" $ do
      -- Create configuration service
      config <- ConfigService.load
      -- Create logger service
      loggerService <- ConsoleLoggerService.new LoggerService.Debug
      -- Create vk service
      vkService <- VkServiceImpl.new config (head $ Config.vkBotList config) loggerService VkServiceImpl.apiVersion
      -- Send request
      res <- VkService.resolveScreenName vkService
      let resFact = getResponseBody res
      -- Check group id 
      LoggerService.logDebug loggerService . T.pack $ show resFact
      let res' = ResolveScreenName.Response 209318335 $ T.pack "group"
      resFact `shouldBe` res'

    it "GetLongPollServer" $ do
      -- Create configuration service
      config <- ConfigService.load
      -- Create logger service
      loggerService <- ConsoleLoggerService.new LoggerService.Debug
      -- Create vk service
      vkService <- VkServiceImpl.new config (head $ Config.vkBotList config) loggerService VkServiceImpl.apiVersion
      -- Send request
      res <- VkService.getLongPollServer vkService
      let resFact = getResponseBody res
      -- Check server url
      LoggerService.logDebug loggerService . T.pack $ show resFact
      let server = T.pack "https://lp.vk.com/wh209318335"
      let res' = GetLongPollServer.Response T.empty server T.empty
      resFact `shouldBe` res'

    it "CheckEvents" $ do
      -- Create configuration service
      config <- ConfigService.load
      -- Create logger service
      loggerService <- ConsoleLoggerService.new LoggerService.Debug
      -- Create vk service
      vkService <- VkServiceImpl.new config (head $ Config.vkBotList config) loggerService VkServiceImpl.apiVersion
      -- Get long poll server parameters
      longPollRes <- VkService.getLongPollServer vkService
      LoggerService.logDebug loggerService . T.pack $ show longPollRes
      -- Receive updates from long poll server
      let checkEventsReq = CheckEvents.fromLongPollServer (getResponseBody longPollRes) 25
      checkEventsRes <- VkService.checkEvents vkService checkEventsReq
      LoggerService.logDebug loggerService . T.pack $ show checkEventsRes

    it "MessageSend" $ do
      -- Create configuration service
      config <- ConfigService.load
      -- Create logger service
      loggerService <- ConsoleLoggerService.new LoggerService.Debug
      -- Create vk service
      vkService <- VkServiceImpl.new config (head $ Config.vkBotList config) loggerService VkServiceImpl.apiVersion
      -- Create request to send message to concrete user
      req <- MessageSend.newRequest 152125471 >>=
        MessageSend.withText (T.pack "MessageSend spec кириллица")
      res <- VkService.messageSend vkService req
      LoggerService.logDebug loggerService $ T.pack "message was sended"
      LoggerService.logDebug loggerService . T.pack $ show res

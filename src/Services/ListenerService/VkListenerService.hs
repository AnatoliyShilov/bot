{-# LANGUAGE OverloadedStrings #-}
module Services.ListenerService.VkListenerService ( new ) where

import Data.Maybe ( fromMaybe, isJust, mapMaybe )
import Control.Monad ( when, forever, unless, void )
import Network.HTTP.Simple ( getResponseBody )
import qualified Data.Text as T
import qualified Services.LoggerService.LoggerService as LoggerService
import qualified Services.HttpServices.VkService.VkService as VkService
import qualified Services.ListenerService.ListenerService as ListenerService
import qualified Services.PatternAIService as AIService
import qualified Models.Vk.Methods.GetLongPollServer as GetLongPollServer
import qualified Models.Vk.Methods.CheckEvents as CheckEvents
import qualified Models.Vk.Methods.MessageSend as MessageSend
import qualified Models.Vk.Message as Message
import qualified Models.Configuration as Config

new :: Config.VkBotConfiguration -> LoggerService.Handle -> VkService.Handle -> IO ListenerService.Handle
new vkBotConfig loggerService vkService =
  return $ ListenerService.Handle {
    ListenerService.listen = createListen vkBotConfig loggerService vkService
    }

iterateM :: Monad m => (a -> m a) -> a -> m a
iterateM f a = f a >>= iterateM f

createListen :: Config.VkBotConfiguration -> LoggerService.Handle -> VkService.Handle -> IO ()
createListen vkBotConfig loggerService vkService = do
  -- Get long poll server
  longPollRes <- VkService.getLongPollServer vkService
  let checkEventsReq = CheckEvents.fromLongPollServer (getResponseBody longPollRes) 25
  -- forever
  void $ iterateM (\ checkEventsReq -> do
    LoggerService.logDebug loggerService $ T.pack $ show checkEventsReq
    -- Check events
    res <- VkService.checkEvents vkService checkEventsReq
    let checkEventsRes = getResponseBody res
    let objects = map (CheckEvents.object . CheckEvents.updateObject) $ CheckEvents.updates checkEventsRes
    mapM_ (\ object -> do
      when (CheckEvents.isMessage object) $ do
        let message = CheckEvents.fromMessage object
        let fromId = Message.fromId message
        let text = Message.text message
        unless (T.null text) $ do
          -- Match message
          let answers = getAnswers (Config.patterns vkBotConfig) text
          unless (null answers) $ do
            -- Send answer back
            messageSendReq <- MessageSend.newRequest fromId >>=
              MessageSend.withText (head answers)
            void $ VkService.messageSend vkService messageSendReq
      ) objects
    return $ checkEventsReq { CheckEvents.requestTs = CheckEvents.responseTs checkEventsRes }
    ) checkEventsReq

getAnswers :: [Config.Pattern] -> T.Text -> [T.Text]
getAnswers patterns input = 
  mapMaybe (\ pattern -> if isJust $ AIService.match (Config.query pattern) (T.splitOn " " input) then Just $ Config.answer pattern else Nothing) patterns

{-# LANGUAGE OverloadedStrings #-}
module Services.HttpServices.VkService.VkService_5_131 (
  apiVersion,
  new,
  with
  ) where

import Data.Maybe ( fromJust, isNothing )
import Data.Functor ( (<&>) )
import Data.Aeson ( FromJSON )
import Control.Exception ( throw )
import Control.Monad ( when )
import Exceptions.BadConfigException ( BadConfigException(BadConfigException) )
import Queryable ( Queryable (..) )
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Simple as Http
import qualified Services.LoggerService.LoggerService as LoggerService
import qualified Services.HttpServices.VkService.VkService as VkService
import qualified Models.Configuration as Config
import qualified Models.Vk.Methods.ResolveScreenName as ResolveScreenName
import qualified Models.Vk.Methods.GetLongPollServer as GetLongPollServer
import qualified Models.Vk.Methods.CheckEvents as CheckEvents
import qualified Models.Vk.Methods.MessageSend as MessageSend

-- Vk api version which current implementation has work
apiVersion :: VkService.ApiVersion
apiVersion = VkService.ApiVersion 5.131

-- Vk service configuration
data Config = Config {
  -- Vk api url
  apiUrl :: T.Text,
  -- Vk auth token
  authToken :: T.Text,
  -- 
  groupId :: Int,

  groupName :: T.Text
  }

data Handle = Handle {
  -- Vk api version
  version :: VkService.ApiVersion,
  -- Vk service configuration
  config :: Config,
  -- Logger service
  logger :: LoggerService.Handle
  }

-- Reading specific fields from configuration to build inner Config
readConfig :: Config.Configuration -> Config.VkBotConfiguration -> IO Config
readConfig config botConfig = do
  return $ Config {
    apiUrl = Config.vkApiUrl config,
    authToken = Config.authToken botConfig,
    groupId = Config.groupId botConfig,
    groupName = Config.groupName botConfig
    }

-- Creating new vk service handle
new :: Config.Configuration -> Config.VkBotConfiguration -> LoggerService.Handle -> VkService.ApiVersion -> IO VkService.Handle
new config botConfig loggerService apiVersion = do
  config <- readConfig config botConfig
  let handle = Handle {
    version = apiVersion,
    config = config,
    logger = loggerService
    }
  return $ VkService.Handle {
    VkService.resolveScreenName = createResolveScreenName handle,
    VkService.getLongPollServer = createGetLongPollServer handle,
    VkService.checkEvents = createCheckEvents handle,
    VkService.messageSend = createMessageSend handle
    }

-- Perform IO action with new handle
with :: Config.Configuration -> Config.VkBotConfiguration -> LoggerService.Handle -> VkService.ApiVersion -> (VkService.Handle -> IO a) -> IO a
with config botConfig logger apiVersion ioAction = new config botConfig logger apiVersion >>= ioAction

-- Implementation of resolve screen name method
createResolveScreenName :: Handle -> IO (Http.Response ResolveScreenName.Response)
createResolveScreenName handle = do
  let url = apiUrl (config handle) <> "/utils.resolveScreenName"
  let params = ResolveScreenName.Request . groupName $ config handle
  postQuery handle params url

-- Implementation of get long poll server
createGetLongPollServer :: Handle -> IO (Http.Response GetLongPollServer.Response)
createGetLongPollServer handle = do
  let url = apiUrl (config handle) <> "/groups.getLongPollServer"
  let params = GetLongPollServer.Request . groupId $ config handle
  postQuery handle params url

-- Implementation of check events method
createCheckEvents :: Handle -> CheckEvents.Request -> IO (Http.Response CheckEvents.Response)
createCheckEvents handle params = do
  let serverUrl = CheckEvents.server params
  postQuery handle params serverUrl

-- Implementation of send message method
createMessageSend :: Handle -> MessageSend.Request -> IO (Http.Response MessageSend.Response)
createMessageSend handle params = do
  let url = apiUrl (config handle) <> "/messages.send"
  postQuery handle params url

-- Send post with query
postQuery :: (Queryable params, FromJSON response) =>
  Handle -> params -> T.Text -> IO (Http.Response response)
postQuery handle params url = do
  let apiVersion = version handle
  let loggerService = logger handle
  request <- Http.parseRequest ("POST " <> T.unpack url)
    <&> Http.setRequestHeader "Authorization" [TE.encodeUtf8 . authToken $ config handle]
    <&> Http.setRequestQueryString (asQuery params <> asQuery apiVersion)
  LoggerService.logDebug loggerService . T.pack . ("Request " <>) $ show request
  Http.httpJSON request

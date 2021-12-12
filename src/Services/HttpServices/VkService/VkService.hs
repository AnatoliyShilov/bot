{-# LANGUAGE OverloadedStrings #-}
module Services.HttpServices.VkService.VkService (
  Handle (..),
  ApiVersion (..)
  ) where

import Data.Aeson ( FromJSON (parseJSON), (.:), withObject )
import Queryable ( Queryable (..) )
import Network.HTTP.Simple ( Response )
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Models.Vk.Methods.ResolveScreenName as ResolveScreenName
import qualified Models.Vk.Methods.GetLongPollServer as GetLongPollServer
import qualified Models.Vk.Methods.CheckEvents as CheckEvents
import qualified Models.Vk.Methods.MessageSend as MessageSend

-- Vk api version
newtype ApiVersion = ApiVersion { v :: Float }

instance Queryable ApiVersion where
  asQuery (ApiVersion v) = [("v", Just . TE.encodeUtf8 . T.pack $ show v)]

data Handle = Handle {
  -- Get object id by short name
  resolveScreenName :: IO (Response ResolveScreenName.Response),
  -- Get long poll server connection
  getLongPollServer :: IO (Response GetLongPollServer.Response),
  -- Get new events from poll server
  checkEvents :: CheckEvents.Request -> IO (Response CheckEvents.Response),
  -- Send message
  messageSend :: MessageSend.Request -> IO (Response MessageSend.Response)
  }

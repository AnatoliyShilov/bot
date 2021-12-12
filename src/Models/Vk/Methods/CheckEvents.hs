{-# LANGUAGE OverloadedStrings #-}
module Models.Vk.Methods.CheckEvents (
  Request (..),
  newRequest,
  fromLongPollServer,
  fromMessage,
  isMessage,
  ObjectMetadata (..),
  ResponseObject (..),
  UpdateItem (..),
  Response (..)
  ) where

import Data.Aeson ( FromJSON (parseJSON), (.:), withObject )
import Data.Foldable ( asum )
import Control.Exception ( throw )
import Queryable ( Queryable (..) )
import Exceptions.PatternNotFoundException
import qualified Network.HTTP.Simple as Http
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Models.Vk.Message as Message
import qualified Models.Vk.Methods.GetLongPollServer as GetLongPollServer

data Request = Request {
  act :: T.Text,
  server :: T.Text,
  key :: T.Text,
  requestTs :: T.Text,
  wait :: Int
  } deriving (Show)

instance Queryable Request where
  asQuery (Request act _ key ts wait) = [
    ("act", Just $ TE.encodeUtf8 act),
    ("key", Just $ TE.encodeUtf8 key),
    ("ts", Just $ TE.encodeUtf8 ts),
    ("wait", Just . TE.encodeUtf8 . T.pack $ show wait)
    ]

newRequest :: T.Text -> T.Text -> T.Text -> Int -> Request
newRequest = Request "a_check"

fromLongPollServer :: GetLongPollServer.Response -> Int -> Request
fromLongPollServer longPoolServer = Request "a_check"
  (GetLongPollServer.server longPoolServer)
  (GetLongPollServer.key longPoolServer)
  (GetLongPollServer.ts longPoolServer)

data ObjectMetadata
  = Message Message.Message
  | Unrecognized
  deriving (Show)

fromMessage :: ObjectMetadata -> Message.Message
fromMessage (Message message) = message
fromMessage _ = throw $
  PatternNotFoundException "fromMessage cannot be applied to none Message argument"

isMessage :: ObjectMetadata -> Bool
isMessage (Message _) = True
isMessage _ = False

data ResponseObject = ResponseObject {
  object :: ObjectMetadata,
  clientInfo :: Int -- TODO
  } deriving (Show)

instance FromJSON ResponseObject where
  parseJSON = withObject "ResponseObject" $ \ v -> do
    objectMetadata <- asum [
      Message <$> v .: "message",
      return Unrecognized 
      ]
    return $ ResponseObject objectMetadata 0

data UpdateItem = UpdateItem {
  updateType :: T.Text,
  updateObject :: ResponseObject,
  groupId :: Int
  } deriving (Show)

instance FromJSON UpdateItem where
  parseJSON = withObject "UpdateItem" $ \ v -> do UpdateItem
    <$> v .: "type"
    <*> v .: "object"
    <*> v .: "group_id"

data Response = Response {
  responseTs :: T.Text,
  updates :: [UpdateItem]
  } deriving (Show)

instance FromJSON Response where
  parseJSON = withObject "Response" $ \ v -> do Response
    <$> v .: "ts"
    <*> v .: "updates"

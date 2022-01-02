{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Models.Vk.Methods.MessageSend (
  Request (..),
  newRequest,
  withChat,
  withAttachment,
  withText,
  AttachmentMeta (..),
  AttachmentType (..),
  Response (..)
  ) where

import System.Random ( newStdGen, Random(randomR) )
import Data.Maybe ( maybe, mapMaybe )
import Data.Char ( isDigit )
import Data.List ( intercalate, stripPrefix )
import Data.Foldable ( asum )
import Data.Functor ( (<&>) )
import Data.Aeson ( (.:), withObject, FromJSON(parseJSON) )
import Queryable ( Queryable(..), ToQueryItem(..), append )
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

data Request = Request {
  -- userId :: Int,
  randomId :: Int,
  -- Для пользователя:
  -- id пользователя.
  -- Для групповой беседы:
  -- 2000000000 + id беседы.
  -- Для сообщества:
  -- -id сообщества.
  peerId :: Int,
  -- peerIds :: [Int],
  -- domain :: T.Text,
  chatId :: Maybe Int,
  message :: Maybe T.Text,
  -- lat :: Float,
  -- long :: Float,
  attachment :: Maybe [AttachmentMeta]
  -- replyTo :: Int,
  -- forwardMessages :: [Int],
  -- forward
  -- stickerId :: Int
  -- groupId :: Int
  -- keyboard 
  -- template
  -- payload
  -- contentSource
  -- dontParseLink
  -- disableMentions
  -- intent
  -- subscribeId
  }

newRequest :: Int -> IO Request
newRequest peerId = do
  stdGen <- newStdGen
  return Request {
    randomId = fst $ randomR (1, 99) stdGen,
    peerId = peerId,
    chatId = Nothing,
    message = Nothing,
    attachment = Nothing
    }

withChat :: Int -> Request -> IO Request
withChat chatId request = return request { chatId = Just chatId }

withText :: T.Text -> Request -> IO Request
withText message request =
  return $ if T.null message
    then request
    else request { message = Just message }

withAttachment :: Maybe [AttachmentMeta] -> Request -> IO Request
withAttachment attachment requset = return requset { attachment = attachment }

instance ToQueryItem [AttachmentMeta] where
  toQueryItem name attachments =
    (name, Just . TE.encodeUtf8 . T.pack . intercalate "," $ map show attachments)

instance Queryable Request where
  asQuery (Request randomId peerId chatId message attachment) =
    let
      defaultQuery = [
        toQueryItem "random_id" randomId,
        toQueryItem "peer_id" peerId
        ]
      addChat = append ("chat_id", chatId)
      addMessage = append ("message", message)
      addAttachment attachment = append ("attachment", Just attachment) defaultQuery
    in
      addChat . addMessage $ maybe defaultQuery addAttachment attachment

-- <type><owner_id>_<media_id>
data AttachmentMeta = AttachmentMeta {
  -- Тип медиавложения
  attachmentType :: AttachmentType,
  -- Идентификатор владельца медиавложения (если объект находится в сообществе, этот параметр должен быть отрицательным)
  ownerId :: Int,
  -- Идентификатор медиавложения
  mediaId :: Int
  }

instance Show AttachmentMeta where
  show (AttachmentMeta attachmentType ownerId mediaId) = show attachmentType <> "-" <> show ownerId <> "_" <> show mediaId

instance Read AttachmentMeta where
  readsPrec _ str =
    let
      (attachmentType, '-' : ownerMediaRest) = break (== '-') str
      (ownerId, '_' : mediaRest) = span isDigit ownerMediaRest
      (mediaId, rest) = span isDigit mediaRest
    in
      [(AttachmentMeta { attachmentType = read attachmentType, ownerId = read ownerId, mediaId = read mediaId }, rest)]

-- Тип медиавложения
data AttachmentType
  -- photo - фотография
  = Photo
  -- video - видео
  | Video
  -- audio - аудио
  | Audio
  -- doc - документ
  | Doc
  -- wall - запись на стене
  | Wall
  -- market - товар
  | Market
  -- poll - опрос
  | Poll

instance Show AttachmentType where
  show Photo = "photo"
  show Video = "video"
  show Audio = "audio"
  show Doc = "doc"
  show Wall = "wall"
  show Market = "market"
  show Poll = "poll"

instance Read AttachmentType where
  readsPrec _ str =
    mapMaybe (\ prefix -> stripPrefix (fst prefix) str <&> (,) (snd prefix)) [
      ("photo", Photo),
      ("video", Video),
      ("audio", Audio),
      ("doc", Doc),
      ("wall", Wall),
      ("market", Market),
      ("poll", Poll)
      ]

newtype Response = Response {
  ids :: [Int]
  } deriving (Show)

data Values = HasOne Int | HasMany [Int]

valueToResponse :: Values -> Response
valueToResponse (HasOne id) =
  Response { ids = [id] }
valueToResponse (HasMany ids) =
  Response { ids = ids }

instance FromJSON Response where
  parseJSON = withObject "Response" $ \ v ->
    asum [
      HasOne <$> v .: "response",
      HasMany <$> v .: "response"
      ] <&> valueToResponse

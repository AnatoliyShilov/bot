{-# LANGUAGE OverloadedStrings #-}
module Models.Vk.Attachment where

import Control.Exception ( throw )
import Exceptions.ParseException ( ParseException(ParseException) )
import Data.Aeson ( FromJSON (parseJSON), (.:), withObject )
import qualified Data.Text as T
import qualified Models.Vk.Photo as Photo
import qualified Models.Vk.Video as Video
import qualified Models.Vk.Audio as Audio
import qualified Models.Vk.Doc as Doc
import qualified Models.Vk.Link as Link
import qualified Models.Vk.Market as Market
import qualified Models.Vk.MarketAlbum as MarketAlbum
import qualified Models.Vk.Wall as Wall -- TODO
import qualified Models.Vk.WallReply as WallReply -- TODO
import qualified Models.Vk.Sticker as Sticker
import qualified Models.Vk.Gift as Gift

-- | Медиавложение
data Attachment = Attachment {
  -- | Тип вложения
  attachmentType :: T.Text,
  -- |  Содержит объект. Название второго поля совпадает со значением, переданным в type
  object :: AttachmentObject
  } deriving (Show)

instance FromJSON Attachment where
  parseJSON = withObject "Attachment" $ \ v -> do 
    attachmentType <- v .: "type"
    attachmentObject <- case attachmentType of
          "photo" -> Photo <$> v .: attachmentType
          "video" -> Video <$> v .: attachmentType
          "audio" -> Audio <$> v .: attachmentType
          "doc" -> Doc <$> v .: attachmentType
          "link" -> Link <$> v .: attachmentType
          "market" -> Market <$> v .: attachmentType
          "market_album" -> MarketAlbum <$> v .: attachmentType
          "wall" -> Wall <$> v .: attachmentType
          "wall_reply" -> WallReply <$> v .: attachmentType
          "sticker" -> Sticker <$> v .: attachmentType
          "gift" -> Gift <$> v .: attachmentType
          _ -> throw . ParseException $ "Fatal error: attachment type " <> attachmentType <> " unrecognized"
    return $ Attachment attachmentType attachmentObject

-- | Объект, представляющий медиавложение
data AttachmentObject
  -- | Фотография
  = Photo Photo.Photo
  -- | Видеозапись
  | Video Video.Video
  -- | Аудиозапись
  | Audio Audio.Audio
  -- | Документ
  | Doc Doc.Doc
  -- | Ссылка
  | Link Link.Link
  -- | Товар
  | Market Market.Market
  -- | Подборка товаров
  | MarketAlbum MarketAlbum.MarketAlbum
  -- | Запись на стене
  | Wall Wall.Wall -- TODO
  -- | Комментарий на стене
  | WallReply WallReply.WallReply -- TODO
  -- | Стикер
  | Sticker Sticker.Sticker
  -- | Подарок
  | Gift Gift.Gift
  deriving (Show)

{-# LANGUAGE OverloadedStrings #-}
module Models.Vk.Photo where

import Data.Aeson ( FromJSON (parseJSON), (.:), withObject )
import qualified Data.Text as T
import qualified Models.Vk.PhotoCopy as PhotoCopy

-- | Фотография
data Photo = Photo {
  -- | Идентификатор фотографии
  id :: Int,
  -- | Идентификатор альбома, в котором находится фотография
  albumId :: Int,
  -- | Идентификатор владельца фотографии
  ownerId :: Int,
  -- | Идентификатор пользователя, загрузившего фото. Для фотографий, размещенных от имени сообщества, user_id = 100
  userId :: Int,
  -- | Текст описания фотографии
  text :: T.Text,
  -- | Дата добавления в формате Unixtime
  date :: Int,
  -- | Массив с копиями изображения в разных размерах
  sizes :: [PhotoCopy.PhotoCopy],
  -- | Ширина оригинала фотографии в пикселах
  width :: Int,
  -- | Высота оригинала фотографии в пикселах
  height :: Int
  } deriving (Show)

instance FromJSON Photo where
  parseJSON = withObject "Photo" $ \ v -> do Photo
    <$> v .: "id"
    <*> v .: "album_id"
    <*> v .: "owner_id"
    <*> v .: "user_id"
    <*> v .: "text"
    <*> v .: "date"
    <*> v .: "sizes"
    <*> v .: "width"
    <*> v .: "height"

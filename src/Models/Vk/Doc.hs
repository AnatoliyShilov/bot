{-# LANGUAGE OverloadedStrings #-}
module Models.Vk.Doc where

import Data.Aeson ( FromJSON (parseJSON), (.:), withObject )
import qualified Data.Text as T
import qualified Models.Vk.PhotoCopy as PhotoCopy

-- | Документ
data Doc = Doc {
  -- | Идентификатор документа
  id :: Int,
  -- | Идентификатор пользователя, загрузившего документ
  ownerId :: Int,
  -- | Название документа
  title :: T.Text,
  -- | Размер документа в байтах
  size :: Int,
  -- | Расширение документа
  ext :: T.Text,
  -- | Адрес документа, по которому его можно загрузить
  url :: T.Text,
  -- | Дата добавления в формате Unixtime
  date :: Int,
  -- | Тип документа
  -- | 1 — текстовые документы
  -- | 2 — архивы
  -- | 3 — gif
  -- | 4 — изображения
  -- | 5 — аудио
  -- | 6 — видео
  -- | 7 — электронные книги
  -- | 8 — неизвестно
  docType :: Int,
  -- | Информация для предварительного просмотра документа
  preview :: Preview
  } deriving (Show)

instance FromJSON Doc where
  parseJSON = withObject "Doc" $ \ v -> do Doc
    <$> v .: "id"
    <*> v .: "owner_id"
    <*> v .: "title"
    <*> v .: "size"
    <*> v .: "ext"
    <*> v .: "url"
    <*> v .: "date"
    <*> v .: "type"
    <*> v .: "preview"

-- | Информация для предварительного просмотра документа
data Preview = Preview {
  -- | Изображения для предпросмотра. массив копий изображения в разных размерах
  photo :: [PhotoCopy.PhotoCopy],
  -- | Данные о граффити
  graffiti :: Graffiti,
  -- | Данные об аудиосообщении
  audioMessage :: AudioMessage
  } deriving (Show)

instance FromJSON Preview where
  parseJSON = withObject "Preview" $ \ v -> do Preview
    <$> v .: "photo"
    <*> v .: "graffiti"
    <*> v .: "audio_message"

-- | Данные о граффити
data Graffiti = Graffiti {
  -- | URL документа с граффити
  src :: T.Text,
  -- | Ширина изображения в px
  width :: Int,
  -- | Высота изображения в px
  height :: Int
  } deriving (Show)

instance FromJSON Graffiti where
  parseJSON = withObject "Graffiti" $ \ v -> do Graffiti
    <$> v .: "src"
    <*> v .: "width"
    <*> v .: "height"

-- | Данные об аудиосообщении
data AudioMessage = AudioMessage {
  -- | Длительность аудиосообщения в секундах
  duration :: Int,
  -- | Массив значений (integer) для визуального отображения звука
  waveform :: [Int],
  -- | URL .ogg-файла
  linkOgg :: T.Text,
  -- | URL .mp3-файла
  linkMp3 :: T.Text
  } deriving (Show)

instance FromJSON AudioMessage where
  parseJSON = withObject "AudioMessage" $ \ v -> do AudioMessage
    <$> v .: "duration"
    <*> v .: "wave_form"
    <*> v .: "link_ogg"
    <*> v .: "link_mp3"

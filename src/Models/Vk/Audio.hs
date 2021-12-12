{-# LANGUAGE OverloadedStrings #-}
module Models.Vk.Audio where

import Data.Aeson ( FromJSON (parseJSON), (.:), withObject )
import qualified Data.Text as T

-- | Аудиозапись
data Audio = Audio {
  -- | Идентификатор аудиозаписи
  id :: Int,
  -- | Идентификатор владельца аудиозаписи
  ownerId :: Int,
  -- | Исполнитель
  artist :: T.Text,
  -- | Название композиции
  title :: T.Text,
  -- | Длительность аудиозаписи в секундах
  duration :: Int,
  -- | Ссылка на mp3
  url :: T.Text,
  -- | Идентификатор текста аудиозаписи (если доступно)
  lyricsId :: Int,
  -- | Идентификатор альбома, в котором находится аудиозапись (если присвоен)
  albumId :: Int,
  -- | Идентификатор жанра из списка аудио жанров
  -- |  1 — Rock
  -- |  2 — Pop
  -- |  3 — Rap & Hip-Hop
  -- | 4 — Easy Listening
  -- | 5 — House & Dance
  -- | 6 — Instrumental
  -- | 7 — Metal
  -- | 21 — Alternative
  -- | 8 — Dubstep
  -- | 1001 — Jazz & Blues
  -- | 10 — Drum & Bass
  -- | 11 — Trance
  -- | 12 — Chanson
  -- | 13 — Ethnic
  -- | 14 — Acoustic & Vocal
  -- | 15 — Reggae
  -- | 16 — Classical
  -- | 17 — Indie Pop
  -- | 19 — Speech
  -- | 22 — Electropop & Disco
  -- | 18 — Other
  genreId :: Int,
  -- | Дата добавления
  date :: Int,
  -- | 1, если включена опция «Не выводить при поиске». Если опция отключена, поле не возвращается
  noSearch :: Int,
  -- | 1, если аудио в высоком качестве
  isHq :: Int
  } deriving (Show)
  
instance FromJSON Audio where
  parseJSON = withObject "Audio" $ \ v -> do Audio
    <$> v .: "id"
    <*> v .: "owner_id"
    <*> v .: "artist"
    <*> v .: "title"
    <*> v .: "duration"
    <*> v .: "url"
    <*> v .: "lyrics_id"
    <*> v .: "album_id"
    <*> v .: "genre_id"
    <*> v .: "date"
    <*> v .: "no_search"
    <*> v .: "is_hq"

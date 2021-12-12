{-# LANGUAGE OverloadedStrings #-}
module Models.Vk.Geo where

import Data.Aeson ( FromJSON (parseJSON), (.:), withObject )
import qualified Data.Text as T

-- | Информация о местоположении
data Geo = Geo {
  -- | Тип места
  geoType :: T.Text,
  -- | Координаты места, (географическая широта, географическая долгота)
  coordinates :: (Float, Float),
  -- | Описание места
  place :: Place,
  -- | Информация о том, отображается ли карта
  showmap :: Int
  } deriving (Show)

instance FromJSON Geo where
  parseJSON = withObject "Geo" $ \ v -> do Geo
    <$> v .: "type"
    <*> v .: "coordinates"
    <*> v .: "place"
    <*> v .: "showmap"

-- | Описание места
data Place = Place {
  -- | Идентификатор места
  id :: Int,
  -- | Название места
  title :: T.Text,
  -- | Географическая широта
  latitude :: Float,
  -- | Географическая долгота
  longitude :: Float,
  -- | Дата создания
  created :: Int,
  -- | URL изображения-иконки
  icon :: T.Text,
  -- | Название страны
  country :: T.Text,
  -- | Название города
  city :: T.Text
  } deriving (Show)

instance FromJSON Place where
  parseJSON = withObject "Place" $ \ v -> do Place
    <$> v .: "id"
    <*> v .: "title"
    <*> v .: "latitude"
    <*> v .: "longitude"
    <*> v .: "created"
    <*> v .: "icon"
    <*> v .: "country"
    <*> v .: "city"
    
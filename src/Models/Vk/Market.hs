{-# LANGUAGE OverloadedStrings #-}
module Models.Vk.Market where

import Data.Aeson ( FromJSON (parseJSON), (.:), withObject )
import qualified Data.Text as T
import qualified Models.Vk.Price as Price
import qualified Models.Vk.Category as Category

-- | Товар
data Market = Market {
  -- | Идентификатор товара
  id :: Int,
  -- | Идентификатор владельца товара
  ownerId :: Int,
  -- | Название товара
  title :: T.Text,
  -- | Текст описания товара
  description :: T.Text,
  -- | Цена
  price :: Price.Price,
  -- | Габариты товара
  dimensions :: Dimensions,
  -- | Вес в граммах
  weight :: Int,
  -- | Категория товара
  category :: Category.Category,
  -- | URL изображения-обложки товара
  thumbPhoto :: T.Text,
  -- | Дата создания товара в формате Unixtime
  date :: Int,
  -- | Статус доступности товара
  -- | 0 — товар доступен
  -- | 1 — товар удален
  -- | 2 — товар недоступен
  availability :: Int,
  -- | true, если объект добавлен в закладки у текущего пользователя
  isFavorite :: Bool,
  -- | Артикул товара, произвольная строка длиной до 50 символов
  sku :: T.Text
  } deriving (Show)

instance FromJSON Market where
  parseJSON = withObject "Market" $ \ v -> do Market
    <$> v .: "id"
    <*> v .: "owner_id"
    <*> v .: "title"
    <*> v .: "description"
    <*> v .: "price"
    <*> v .: "dimensions"
    <*> v .: "weight"
    <*> v .: "category"
    <*> v .: "thumbPhoto"
    <*> v .: "date"
    <*> v .: "availability"
    <*> v .: "is_favorite"
    <*> v .: "sku"

-- | Габариты товара
data Dimensions = Dimensions {
  -- | ширина в миллиметрах
  width :: Int,
  -- | высота в миллиметрах
  height :: Int,
  -- | длина в миллиметрах
  length :: Int
  } deriving (Show)

instance FromJSON Dimensions where
  parseJSON = withObject "Dimensions" $ \ v -> do Dimensions
    <$> v .: "width"
    <*> v .: "height"
    <*> v .: "length"

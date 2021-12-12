{-# LANGUAGE OverloadedStrings #-}
module Models.Vk.MarketAlbum where

import Data.Aeson ( FromJSON (parseJSON), (.:), withObject )
import qualified Data.Text as T
import qualified Models.Vk.Photo as Photo

-- | Подборка товаров
data MarketAlbum = MarketAlbum {
  -- | Идентификатор подборки
  id :: Int,
  -- | Идентификатор владельца подборки
  ownerId :: Int,
  -- | Название подборки
  title :: T.Text,
  -- | Является ли подборка основной
  isMain :: Bool,
  -- | Является ли подборка скрытой
  isHidden :: Bool,
  -- | Обложка подборки
  photo :: Photo.Photo,
  -- | Число товаров в подборке
  count :: Int
  } deriving (Show)

instance FromJSON MarketAlbum where
  parseJSON = withObject "MarketAlbum" $ \ v -> do MarketAlbum
    <$> v .: "id"
    <*> v .: "owner_id"
    <*> v .: "title"
    <*> v .: "is_main"
    <*> v .: "is_hidden"
    <*> v .: "photo"
    <*> v .: "count"

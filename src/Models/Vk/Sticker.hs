{-# LANGUAGE OverloadedStrings #-}
module Models.Vk.Sticker where

import Data.Aeson ( FromJSON (parseJSON), (.:), withObject )
import qualified Data.Text as T

-- | Стикер
data Sticker = Sticker {
  -- | Идентификатор набора
  productId :: Int,
  -- | Идентификатор стикера
  stickerId :: Int,
  -- | Изображения для стикера (с прозрачным фоном)
  images :: [Image],
  -- | Изображения для стикера (с непрозрачным фоном)
  imagesWithBackground :: [Image],
  -- | URL анимации стикера
  animationUrl :: T.Text,
  -- | Информация о том, доступен ли стикер
  isAllowed :: Bool
  } deriving (Show)

instance FromJSON Sticker where
  parseJSON = withObject "Sticker" $ \ v -> do Sticker
    <$> v .: "product_id"
    <*> v .: "sticker_id"
    <*> v .: "images"
    <*> v .: "images_with_background"
    <*> v .: "animation_url"
    <*> v .: "is_allowed"

-- | Изображение для стикера
data Image = Image {
  -- | URL копии изображения
  url :: T.Text,
  -- | Ширина копии в px
  width :: Int,
  -- | Высота копии в px
  height :: Int
  } deriving (Show)

instance FromJSON Image where
  parseJSON = withObject "Image" $ \ v -> do Image
    <$> v .: "url"
    <*> v .: "width"
    <*> v .: "height"

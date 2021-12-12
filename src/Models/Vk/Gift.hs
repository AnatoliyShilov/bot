{-# LANGUAGE OverloadedStrings #-}
module Models.Vk.Gift where

import Data.Aeson ( FromJSON (parseJSON), (.:), withObject )
import qualified Data.Text as T

-- | Подарок
data Gift = Gift {
  -- | Идентификатор подарка
  id :: Int,
  -- | URL изображения 256x256px
  thumb256 :: T.Text,
  -- | URL изображения 96x96px
  thumb96 :: T.Text,
  -- | URL изображения 48x48px
  thumb48 :: T.Text
  } deriving (Show)

instance FromJSON Gift where
  parseJSON = withObject "Gift" $ \ v -> do Gift
    <$> v .: "id"
    <*> v .: "thumb_256"
    <*> v .: "thumb_96"
    <*> v .: "thumb_48"
  
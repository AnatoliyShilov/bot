{-# LANGUAGE OverloadedStrings #-}
module Models.Vk.PhotoCopy where

import Data.Aeson ( FromJSON (parseJSON), (.:), withObject )
import qualified Data.Text as T

-- | Копия фото определенного размера
data PhotoCopy = PhotoCopy {
  -- | Тип копии
  copyType :: T.Text,
   -- | URL копии
  url :: T.Text,
  -- | Высота в px
  width :: Int,
  -- | Ширина в px
  height :: Int
  } deriving (Show)

instance FromJSON PhotoCopy where
  parseJSON = withObject "PhotoCopy" $ \ v -> do PhotoCopy
    <$> v .: "type"
    <*> v .: "url"
    <*> v .: "width"
    <*> v .: "height"

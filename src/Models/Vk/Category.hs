{-# LANGUAGE OverloadedStrings #-}
module Models.Vk.Category where

import Data.Aeson ( FromJSON (parseJSON), (.:), withObject )
import qualified Data.Text as T
import qualified Models.Vk.Section as Section

-- | Категория товара
data Category = Category {
  -- | Идентификатор категории
  id :: Int,
  -- | Название категории
  name :: T.Text,
  -- | Секция
  section :: Section.Section
  } deriving (Show)

instance FromJSON Category where
  parseJSON = withObject "Category" $ \ v -> do Category
    <$> v .: "id"
    <*> v .: "name"
    <*> v .: "section"

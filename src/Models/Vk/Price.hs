{-# LANGUAGE OverloadedStrings #-}
module Models.Vk.Price where

import Data.Aeson ( FromJSON (parseJSON), (.:), withObject )
import qualified Data.Text as T

-- | Объект, описывающий цену
data Price = Price {
  -- | Целочисленное значение цены, умноженное на 100
  amount :: Int,
  -- | Информация о валюте
  currency :: Currency,
  -- | Строка с локализованной ценой и валютой
  text :: T.Text
  } deriving (Show)

instance FromJSON Price where
  parseJSON = withObject "Price" $ \ v -> do Price
    <$> v .: "amount"
    <*> v .: "currency"
    <*> v .: "text"

-- | Информация о валюте
data Currency = Currency {
  -- | Идентификатор валюты
  id :: Int,
  -- | Буквенное обозначение валюты
  name :: T.Text
  } deriving (Show)

instance FromJSON Currency where
  parseJSON = withObject "Currency" $ \ v -> do Currency
    <$> v .: "id"
    <*> v .: "name"

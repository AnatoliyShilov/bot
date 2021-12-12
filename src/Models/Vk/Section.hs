{-# LANGUAGE OverloadedStrings #-}
module Models.Vk.Section where

import Data.Aeson ( FromJSON (parseJSON), (.:), withObject )
import qualified Data.Text as T

-- | Секция
data Section = Section {
  -- | Идентификатор секции
  id :: Int,
  -- | Название секции
  name :: T.Text
  } deriving (Show)

instance FromJSON Section where
  parseJSON = withObject "Section" $ \ v -> do Section
    <$> v .: "id"
    <*> v .: "name"

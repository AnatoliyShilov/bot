{-# LANGUAGE OverloadedStrings #-}
module Models.Vk.Link where

import Data.Aeson ( FromJSON (parseJSON), (.:), withObject )
import qualified Data.Text as T
import qualified Models.Vk.Photo as Photo
import qualified Models.Vk.Price as Price

-- | Прикрепленная ссылка
data Link = Link {
  -- | URL ссылки
  url :: T.Text,
  -- | Заголовок ссылки
  title :: T.Text,
  -- | Подпись ссылки
  caption :: T.Text,
  -- | Описание ссылки
  description :: T.Text,
  -- | Изображение превью
  photo :: Photo.Photo,
  -- | Информация о продукте
  product :: Price.Price,
  -- | Информация о кнопке для перехода
  button :: Button,
  -- | Идентификатор вики-страницы с контентом для предпросмотра содержимого страницы
  -- | Возвращается в формате "owner_id_page_id"
  previewPage :: T.Text,
  -- | URL страницы с контентом для предпросмотра содержимого страницы
  previewUrl :: T.Text
  } deriving (Show)

instance FromJSON Link where
  parseJSON = withObject "Link" $ \ v -> do Link
    <$> v .: "url"
    <*> v .: "title"
    <*> v .: "caption"
    <*> v .: "description"
    <*> v .: "photo"
    <*> v .: "product"
    <*> v .: "button"
    <*> v .: "preview_page"
    <*> v .: "preview_url"

-- | Объект, описывающий информацию о кнопке
data Button = Button {
  -- | Название кнопки
  buttonTitle :: T.Text,
  -- | Действие для кнопки
  action :: Action
  } deriving (Show)

instance FromJSON Button where
  parseJSON = withObject "Button" $ \ v -> do Button
    <$> v .: "title"
    <*> v .: "action"

-- | Действие для кнопки
data Action = Action {
  -- | Тип действия
  -- | open_url — открыть адрес из поля url
  actionType :: T.Text,
  -- | URL для перехода
  actionUrl :: T.Text
  } deriving (Show)

instance FromJSON Action where
  parseJSON = withObject "Action" $ \ v -> do Action
    <$> v .: "type"
    <*> v .: "url"

{-# LANGUAGE OverloadedStrings #-}
module Models.Vk.Action where

import Data.Aeson ( FromJSON (parseJSON), (.:), withObject )
import qualified Data.Text as T

-- | Информация о сервисном действии с чатом
data Action = Action {
  -- | тип действия
  -- | chat_photo_update — обновлена фотография беседы
  -- | chat_photo_remove — удалена фотография беседы
  -- | chat_create — создана беседа
  -- | chat_title_update — обновлено название беседы
  -- | chat_invite_user — приглашен пользователь
  -- | chat_kick_user — исключен пользователь
  -- | chat_pin_message — закреплено сообщение
  -- | chat_unpin_message — откреплено сообщение
  -- | chat_invite_user_by_link — пользователь присоединился к беседе по ссылке
  actionType :: T.Text,
  -- | Идентификатор пользователя (если > 0) или email (если < 0), которого пригласили или исключили
  memberId :: Int,
  -- | Название беседы
  text :: T.Text,
  -- | email, который пригласили или исключили
  email :: T.Text,
  -- | Изображение-обложка чата. (URL изображения 50x50px, URL изображения 100x100px, URL изображения 200x200px)
  photo :: (T.Text, T.Text, T.Text)
  } deriving (Show)

instance FromJSON Action where
  parseJSON = withObject "Action" $ \ v -> do Action
    <$> v .: "type"
    <*> v .: "member_id"
    <*> v .: "text"
    <*> v .: "email"
    <*> v .: "photo"
    
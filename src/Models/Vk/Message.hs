{-# LANGUAGE OverloadedStrings #-}
module Models.Vk.Message where

import Data.Aeson ( FromJSON (parseJSON), (.:), (.:?), withObject )
import qualified Data.Text as T
import qualified Models.Vk.Geo as Geo
import qualified Models.Vk.Action as Action
import qualified Models.Vk.Attachment as Attachment -- TODO

-- | Личное сообщение
data Message = Message {
  -- | Идентификатор сообщения
  id :: Int,
  -- | Время отправки в Unixtime
  date :: Int,
  -- | Идентификатор назначения
  peerId :: Int,
  -- | Идентификатор отправителя
  fromId :: Int,
  -- | Текст сообщения
  text :: T.Text,
  -- | Идентификатор, используемый при отправке сообщения. Возвращается только для исходящих сообщений
  randomId :: Int,
  -- | Произвольный параметр для работы с источниками переходов
  ref :: Maybe T.Text,
  -- | Произвольный параметр для работы с источниками переходов
  refSource :: Maybe T.Text,
  -- | Медиавложения сообщения
  attachments :: [Attachment.Attachment], -- TODO
  -- | True, если сообщение помечено как важное
  important :: Bool,
  -- | Информация о местоположении
  geo :: Maybe Geo.Geo,
  -- | Сервисное поле для сообщений ботам (полезная нагрузка)
  payload :: Maybe T.Text,
  -- | Объект клавиатуры для ботов
  keyboard :: Maybe Int, -- TODO
  -- | Массив пересланных сообщений
  fwdMessages :: [Message],
  -- | Сообщение, в ответ на которое отправлено текущее
  replyMessage :: Maybe Message,
  -- | Информация о сервисном действии с чатом
  action :: Maybe Action.Action,
  -- | Только для сообщений сообщества. Содержит идентификатор пользователя (администратора сообщества), отправившего это сообщение
  adminAuthorId :: Maybe Int,
  -- | Уникальный автоматически увеличивающийся номер для всех сообщений с этим peer
  conversationMessageId :: Int,
  -- | Это сообщение обрезано для бота
  isCropped :: Maybe Bool,
  -- | Количество участников
  membersCount :: Maybe Int,
  -- | Дата, когда сообщение было обновлено в Unixtime
  updateTime :: Maybe Int,
  -- | Было ли вложенное аудиосообщение уже прослушано вами
  wasListened :: Maybe Bool,
  -- | Дата, когда сообщение было закреплено в Unixtime
  pinnedAt :: Maybe Int,
  -- | Строка для сопоставления пользователя Notify и ВКонтакте
  messageTag :: Maybe T.Text
  } deriving (Show)

instance FromJSON Message where
  parseJSON = withObject "Message" $ \ v -> do Message
    <$> v .: "id"
    <*> v .: "date"
    <*> v .: "peer_id"
    <*> v .: "from_id"
    <*> v .: "text"
    <*> v .: "random_id"
    <*> v .:? "ref"
    <*> v .:? "ref_source"
    <*> v .: "attachments"
    <*> v .: "important"
    <*> v .:? "geo"
    <*> v .:? "payload"
    <*> v .:? "keyboard"
    <*> v .: "fwd_messages"
    <*> v .:? "reply_message"
    <*> v .:? "action"
    <*> v .:? "admin_author_id"
    <*> v .: "conversation_message_id"
    <*> v .:? "is_cropped"
    <*> v .:? "members_count"
    <*> v .:? "update_time"
    <*> v .:? "was_listened"
    <*> v .:? "pinned_at"
    <*> v .:? "message_tag"

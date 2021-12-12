{-# LANGUAGE OverloadedStrings #-}
module Models.Vk.Video where

import Data.Aeson ( FromJSON (parseJSON), (.:), withObject )
import qualified Data.Text as T

-- | Видеозапись
data Video = Video {
  -- | Идентификатор видеозаписи
  id :: Int,
  -- | Идентификатор владельца видеозаписи
  ownerId :: Int,
  -- | Название видеозаписи
  title :: T.Text,
  -- | Текст описания видеозаписи
  description :: T.Text,
  -- | Длительность ролика в секундах
  duration :: Int,
  -- | Изображение обложки
  image :: [Image],
  -- | Изображение первого кадра
  firstFrame :: [FirstFrame],
  -- | Дата создания видеозаписи в формате Unixtime
  date :: Int,
  -- | Дата добавления видеозаписи пользователем или группой в формате Unixtime
  addingDate :: Int,
  -- | Количество просмотров видеозаписи
  views :: Int,
  -- | Если видео внешнее, количество просмотров в ВК
  localViews :: Int,
  -- | Количество комментариев к видеозаписи
  comments :: Int,
  -- | URL страницы с плеером, который можно использовать для воспроизведения ролика в браузере
  player :: T.Text,
  -- | Название платформы (для видеозаписей, добавленных с внешних сайтов)
  platform :: T.Text,
  -- | Может ли пользователь добавить видеозапись к себе
  -- | 0 - Не может добавить
  -- | 1 - Может добавить
  canAdd :: Int,
  -- | Поле возвращается, если видеозапись приватная (например, была загружена в личное сообщение), всегда содержит 1
  isPrivate :: Int,
  -- | Ключ доступа к объекту
  accessKey :: T.Text,
  -- | Поле возвращается в том случае, если видеоролик находится в процессе обработки, всегда содержит 1
  processing :: Int,
  -- | true, если объект добавлен в закладки у текущего пользователя
  isFavorite :: Bool,
  -- | Может ли пользователь комментировать видео
  -- | 0 - Не может комментировать
  -- | 1 - Может комментировать
  canComment :: Int,
  -- | Может ли пользователь редактировать видео
  -- | 0 - Не может редактировать
  -- | 1 - Может редактировать
  canEdit :: Int,
  -- | 'Может ли пользователь добавить видео в список "Мне нравится"
  -- | 0 - Не может добавить
  -- | 1 - Может добавить
  canLike :: Int,
  -- | Может ли пользователь сделать репост видео
  -- | 0 - Не может сделать репост
  -- | 1 - Может сделать репост
  canRepost :: Int,
  -- | Может ли пользователь подписаться на автора видео
  -- | 0 - Не может подписаться
  -- | 1 - Может подписаться
  canSubscribe :: Int,
  -- | Может ли пользователь добавить видео в избранное
  -- | 0 - Не может добавить
  -- | 1 - Может добавить
  canAddToFaves :: Int,
  -- | Может ли пользователь прикрепить кнопку действия к видео
  -- | 0 - Не может прикрепить
  -- | 1 - Может прикрепить
  canAttachLink :: Int,
  -- | Ширина видео
  width :: Int,
  -- | Высота видео
  height :: Int,
  -- | Идентификатор пользователя, загрузившего видео, если оно было загружено в группу одним из участников
  userId :: Int,
  -- | Конвертируется ли видео
  -- | 0 - Не конвертируется
  -- | 1 - Конвертируется
  converting :: Int,
  -- | Добавлено ли видео в альбомы пользователя
  -- | 0 - Не добавлено
  -- | 1 - Добавлено
  added :: Int,
  -- | Подписан ли пользователь на автора видео
  -- | 0 - Не подписан
  -- | 1 - Подписан
  isSubscribed :: Int,
  -- | Поле возвращается в том случае, если видео зациклено, всегда содержит 1
  repeat :: Int,
  -- | Тип видеозаписи
  -- | video
  -- | music_video
  -- | movie
  videoType :: T.Text,
  -- | Баланс донатов в прямой трансляции
  balance :: Int,
  -- | Статус прямой трансляции
  -- | waiting
  -- | started
  -- | finished
  -- | failed
  -- | upcoming
  liveStatus :: T.Text,
  -- | Поле возвращается в том случае, если видеозапись является прямой трансляцией
  -- | Всегда содержит 1
  -- | Если live == 1, то в поле duration содержится значение 0
  live :: Int,
  -- | (для live = 1). Поле свидетельствует о том, что трансляция скоро начнётся
  upcoming :: Int,
  -- | Количество зрителей прямой трансляции
  spectrators :: Int,
  -- | Объект отметки "Мне нравится"
  likes :: Like,
  -- | Объект репоста
  reposts :: Repost
  } deriving (Show)

instance FromJSON Video where
  parseJSON = withObject "Video" $ \ v -> do Video
    <$> v .: "id"
    <*> v .: "owner_id"
    <*> v .: "title"
    <*> v .: "description"
    <*> v .: "duration"
    <*> v .: "image"
    <*> v .: "first_frame"
    <*> v .: "date"
    <*> v .: "adding_date"
    <*> v .: "views"
    <*> v .: "local_views"
    <*> v .: "comments"
    <*> v .: "player"
    <*> v .: "platform"
    <*> v .: "can_add"
    <*> v .: "is_private"
    <*> v .: "access_key"
    <*> v .: "processing"
    <*> v .: "is_favorite"
    <*> v .: "can_comment"
    <*> v .: "can_edit"
    <*> v .: "can_like"
    <*> v .: "can_repost"
    <*> v .: "can_subscribe"
    <*> v .: "can_add_to_faves"
    <*> v .: "can_attach_link"
    <*> v .: "width"
    <*> v .: "height"
    <*> v .: "user_id"
    <*> v .: "converting"
    <*> v .: "added"
    <*> v .: "is_subscribed"
    <*> v .: "repeat"
    <*> v .: "type"
    <*> v .: "balance"
    <*> v .: "live_status"
    <*> v .: "live"
    <*> v .: "upcoming"
    <*> v .: "spectrators"
    <*> v .: "likes"
    <*> v .: "reposts"

-- | Изображение обложки
data Image = Image {
  -- | Высота изображения
  imageHeight :: Int,
  -- | Ссылка на изображение
  imageUrl :: T.Text,
  -- | Ширина изображения
  imageWidth :: Int,
  -- |  Поле возвращается, если изображение с отбивкой, всегда содержит 1
  withPadding :: Int
  } deriving (Show)

instance FromJSON Image where
  parseJSON = withObject "Image" $ \ v -> do Image
    <$> v .: "height"
    <*> v .: "url"
    <*> v .: "width"
    <*> v .: "with_padding"

-- | Изображение первого кадра
data FirstFrame = FirstFrame {
  -- | Высота изображения
  frameHeight :: Int,
  -- | Ссылка на изображение
  frameUrl :: T.Text,
  -- | Ширина изображения
  frameWidth :: Int
  } deriving (Show)

instance FromJSON FirstFrame where
  parseJSON = withObject "FirstFrame" $ \ v -> do FirstFrame
    <$> v .: "height"
    <*> v .: "url"
    <*> v .: "width"

-- | Объект отметки "Мне нравится"
data Like = Like {
  -- | Количество лайков
  likesCount :: Int,
  -- | Добавлено ли видео в список "Мне нравится" текущего пользователя
  -- | 0 - Не добавлено
  -- | 1 - Добавлено
  userLikes :: Int
  } deriving (Show)

instance FromJSON Like where
  parseJSON = withObject "Like" $ \ v -> do Like
    <$> v .: "count"
    <*> v .: "user_likes"

-- | Объект репоста
data Repost = Repost {
  -- | Счетчик общего количества репостов. Содержит сумму репостов на стену и в личные сообщения
  repostsCount :: Int,
  -- | Счетчик репостов на стену
  wallCount :: Int,
  -- | Счетчик репостов в личные сообщения
  mainCount :: Int,
  -- | Информация о том, сделал ли текущий пользователь репост этого видео
  userReposted :: Int
  } deriving (Show)

instance FromJSON Repost where
  parseJSON = withObject "Repost" $ \ v -> do Repost
    <$> v .: "count"
    <*> v .: "wall_count"
    <*> v .: "main_count"
    <*> v .: "user_reposted"


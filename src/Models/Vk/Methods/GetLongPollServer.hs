{-# LANGUAGE OverloadedStrings #-}
module Models.Vk.Methods.GetLongPollServer (
  Request (..),
  Response (..)
  ) where

import Queryable ( Queryable (..) )
import Data.Aeson ( FromJSON (parseJSON), (.:), withObject )
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Simple as Http

newtype Request = Request { groupId :: Int }

instance Queryable Request where
  asQuery (Request groupId) = [("group_id", Just . TE.encodeUtf8 . T.pack $ show groupId)]

data Response = Response {
  key :: T.Text,
  server :: T.Text,
  ts :: T.Text
  } deriving (Show)

instance Eq Response where
  (Response _ server1 _) == (Response _ server2 _) = server1 == server2

instance FromJSON Response where
  parseJSON = withObject "Response" $ \ v -> do
    v' <- v .: "response"
    Response
      <$> v' .: "key"
      <*> v' .: "server"
      <*> v' .: "ts"

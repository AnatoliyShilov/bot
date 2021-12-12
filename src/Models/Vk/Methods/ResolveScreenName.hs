{-# LANGUAGE OverloadedStrings #-}
module Models.Vk.Methods.ResolveScreenName (
  Request (..),
  Response (..)
  ) where

import Queryable ( Queryable(..) )
import Data.Aeson ( FromJSON(parseJSON), (.:), withObject )
import qualified Network.HTTP.Simple as Http
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

newtype Request = Request { screenName :: T.Text }

instance Queryable Request where
  asQuery (Request screenName) = [("screen_name", Just $ TE.encodeUtf8 screenName)]

data Response = Response {
  objectId :: Int,
  objectType :: T.Text
  } deriving (Show, Eq)

instance FromJSON Response where
  parseJSON = withObject "Response" $ \ v -> do
    v' <- v .: "response"
    Response
      <$> v' .: "object_id"
      <*> v' .: "type"

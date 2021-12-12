{-# LANGUAGE FlexibleInstances #-}
module Queryable (
  Queryable (..),
  ToQueryItem (..),
  append,
  appendIsJust
  ) where

import Data.ByteString ( ByteString )
import Data.Maybe ( mapMaybe )
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- Define the function which convert any type to Query (aka [QueryItem (aka (ByteString, Maybe ByteString))])
class Queryable a where
  asQuery :: a -> [(ByteString, Maybe ByteString)]

-- Define the function and default implementation which convert any type to QueryItem (aka (ByteString, Maybe ByteString))
class (Show a) => ToQueryItem a where
  toQueryItem :: ByteString -> a -> (ByteString, Maybe ByteString)
  toQueryItem name val = (name, Just . TE.encodeUtf8 . T.pack $ show val)

-- Type Int use default implementation of class ToQueryItem
instance ToQueryItem Int

-- Type Float use default implementation of class ToQueryItem
instance ToQueryItem Float

instance ToQueryItem T.Text where
  toQueryItem name text = (name, Just $ TE.encodeUtf8 text)

instance ToQueryItem String where
  toQueryItem name string = (name, Just . TE.encodeUtf8 $ T.pack string)

-- Given item converting to QueryItem (aka (ByteString, Maybe ByteString)) and appending to the Query (aka [QueryItem])
append :: (ToQueryItem a) => (ByteString, Maybe a) -> [(ByteString, Maybe ByteString)] -> [(ByteString, Maybe ByteString)]
append (name, optional) queryString =
  maybe queryString (\ value -> toQueryItem name value : queryString) optional

-- Given Just items converting to QueryItem (aka (ByteString, Maybe ByteString)) and appending to the Query (aka [QueryItem])
appendIsJust :: (ToQueryItem a) => [(ByteString, Maybe a)] -> [(ByteString, Maybe ByteString)] -> [(ByteString, Maybe ByteString)]
appendIsJust optionals queryString =
  queryString <> mapMaybe (\ (name, optional) -> fmap (toQueryItem name) optional) optionals

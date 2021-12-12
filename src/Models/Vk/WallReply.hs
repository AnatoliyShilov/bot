module Models.Vk.WallReply where

import Data.Aeson ( FromJSON (parseJSON), withObject )

-- TODO
-- | Комментарий на стене
data WallReply = WallReply {} deriving (Show)

instance FromJSON WallReply where
  parseJSON = withObject "WallReply" $ \ _ -> return WallReply

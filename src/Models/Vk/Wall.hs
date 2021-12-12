module Models.Vk.Wall where

import Data.Aeson ( FromJSON (parseJSON), withObject )

-- TODO
-- | Запись на стене
data Wall = Wall {} deriving (Show)

instance FromJSON Wall where
  parseJSON = withObject "Wall" $ \ _ -> return Wall

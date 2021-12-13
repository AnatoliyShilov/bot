{-# LANGUAGE OverloadedStrings #-}
module Models.Configuration where

import Data.Yaml ( FromJSON(parseJSON), withObject, (.:) )
import Text.Regex ( mkRegex, Regex )
import qualified Data.Text as T
import qualified Services.LoggerService.LoggerService as LoggerService

data Configuration = Configuration {
  vkApiUrl :: T.Text,
  logLevel :: LoggerService.Priority,
  vkBotList :: [VkBotConfiguration]
  } deriving (Show)

instance FromJSON Configuration where
  parseJSON = withObject "Configuration" $ \ v -> do Configuration
    <$> v .: "vk-api-url"
    <*> v .: "logger"
    <*> v .: "vk-bot-list"

data VkBotConfiguration = VkBotConfiguration {
  groupId :: Int,
  groupName :: T.Text,
  authToken :: T.Text,
  patterns :: [PatternAction]
  } deriving (Show)

instance FromJSON VkBotConfiguration where
  parseJSON = withObject "VkBotConfiguration" $ \ v -> do VkBotConfiguration
    <$> v .: "group-id"
    <*> v .: "group-name"
    <*> v .: "auth-token"
    <*> v .: "patterns"

data PatternAction = PatternAction {
  regex :: Regex,
  answer :: T.Text
  }

instance Show PatternAction where
  show patternAction = show $ answer patternAction

instance FromJSON PatternAction where
  parseJSON = withObject "Pattern" $ \ v -> do 
    regex <- v .: "regex"
    answer <- v .: "answer"
    return $ PatternAction { regex = mkRegex regex, answer = answer }

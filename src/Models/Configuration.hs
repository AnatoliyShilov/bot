{-# LANGUAGE OverloadedStrings #-}
module Models.Configuration where

import Data.Yaml ( FromJSON(parseJSON), withObject, (.:) )
import qualified Data.Text as T
import qualified Services.LoggerService.LoggerService as LoggerService
import qualified Services.PatternAIService as AIService

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
  patterns :: [Pattern]
  } deriving (Show)

instance FromJSON VkBotConfiguration where
  parseJSON = withObject "VkBotConfiguration" $ \ v -> do VkBotConfiguration
    <$> v .: "group-id"
    <*> v .: "group-name"
    <*> v .: "auth-token"
    <*> v .: "patterns"

data Pattern = Pattern {
  query :: [AIService.PatternGramm],
  answer :: T.Text
  } deriving (Show)

instance FromJSON Pattern where
  parseJSON = withObject "Pattern" $ \ v -> do 
    query <- v .: "query"
    answer <- v .: "answer"
    return $ Pattern { query = AIService.new query, answer = answer }

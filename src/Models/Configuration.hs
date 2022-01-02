{-# LANGUAGE OverloadedStrings #-}
module Models.Configuration where

import Data.Yaml ( FromJSON(parseJSON), withObject, (.:), (.:?), (.!=) )
import Text.Regex ( mkRegex, Regex )
import Data.Maybe ( maybe )
import qualified Data.Text as T
import qualified Services.LoggerService.LoggerService as LoggerService
import qualified Models.Vk.Methods.MessageSend as MessageSend

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
  token :: T.Text,
  patterns :: [PatternAction]
  } deriving (Show)

instance FromJSON VkBotConfiguration where
  parseJSON = withObject "VkBotConfiguration" $ \ v -> do VkBotConfiguration
    <$> v .: "id"
    <*> v .: "name"
    <*> v .: "token"
    <*> v .: "patterns"

data PatternAction = PatternAction {
  regex :: Regex,
  answer :: T.Text,
  attachment :: Maybe [MessageSend.AttachmentMeta]
  }

instance Show PatternAction where
  show patternAction = "PatternAction { answer = " <> show (answer patternAction) <> ", attachment = " <> show (attachment patternAction) <> "}"

instance FromJSON PatternAction where
  parseJSON = withObject "Pattern" $ \ v -> do
    regex <- v .: "regex"
    answer <- v .:? "answer" .!= ""
    attachment <- v .:? "attachment"
    return $ PatternAction { regex = mkRegex regex, answer = answer, attachment = (Just . read) =<< attachment }

{-# LANGUAGE OverloadedStrings #-}
module Services.ConfigService (
  load,
  Configuration (..),
  VkBotConfiguration (..)
  ) where

import Data.Yaml
import Models.Configuration
import qualified Data.Text as T
import qualified Services.LoggerService.LoggerService as LoggerService

-- Create configuration service with specific config-file
load :: IO Configuration
load = decodeFileThrow "app-settings.yaml"

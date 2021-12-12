module ConfigServiceSpec ( configServiceSpec ) where

import Test.Hspec ( hspec, describe, it, shouldBe )
import Models.Configuration ( Configuration(vkApiUrl) )
import qualified Data.Text as T
import qualified Services.ConfigService as ConfigService
import qualified Services.LoggerService.LoggerService as LoggerService
import qualified Services.LoggerService.ConsoleLoggerService as ConsoleLoggerService

configServiceSpec :: IO ()
configServiceSpec = hspec $ do
  describe "Configurator" $ do
    it "Read config" $ do
      -- Create logger service
      loggerService <- ConsoleLoggerService.new LoggerService.Debug
      -- Create configuration service
      config <- ConfigService.load
      LoggerService.logInfo loggerService $ T.pack $ show config
      vkApiUrl config `shouldBe` T.pack "https://api.vk.com/method"

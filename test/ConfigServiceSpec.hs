module ConfigServiceSpec ( configServiceSpec ) where

import Test.Hspec ( hspec, describe, it, shouldBe )
import Text.Regex ( matchRegex, mkRegex )
import Data.Maybe ( isJust )
import Models.Configuration ( Configuration(vkApiUrl) )
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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
    it "Regex \"0123456789\"" $ do
      isJust (matchRegex (mkRegex "^0123456789$") "0123456789") `shouldBe` True
    it "Regex \"abcdefghijklmnopqrstuvwxyz\"" $ do
      isJust (matchRegex (mkRegex "^abcdefghijklmnopqrstuvwxyz$") "abcdefghijklmnopqrstuvwxyz") `shouldBe` True
    it "Regex \"абвгдеёжзиклмнопрстуфхцчьыъшщэюя\"" $ do
      isJust (matchRegex (mkRegex "^абвгдеёжзиклмнопрстуфхцчьыъшщэюя$") "абвгдеёжзиклмнопрстуфхцчьыъшщэюя") `shouldBe` True
    it "Regex \"абвгдеёжзиклмнорстуфхцчьыъшщэюя\"" $ do
      isJust (matchRegex (mkRegex "^абвгдеёжзиклмнорстуфхцчьыъшщэюя$") "абвгдеёжзиклмнорстуфхцчьыъшщэюя") `shouldBe` True
    it "Regex \"абвгдеёжзиклмно\"" $ do
      isJust (matchRegex (mkRegex "^абвгдеёжзиклмно$") "абвгдеёжзиклмно") `shouldBe` True
    it "Regex \"рстуфхцчьыъшщэюя\"" $ do
      isJust (matchRegex (mkRegex "^рстуфхцчьыъшщэюя$") "рстуфхцчьыъшщэюя") `shouldBe` True
    it "Regex \"п\"" $ do
      isJust (matchRegex (mkRegex "^п$") "п") `shouldBe` True
    it "Regex \"ппп\"" $ do
      isJust (matchRegex (mkRegex "^ппп$") "ппп") `shouldBe` True

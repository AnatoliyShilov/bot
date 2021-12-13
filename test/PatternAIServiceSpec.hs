{-# LANGUAGE OverloadedStrings #-}
module PatternAIServiceSpec ( patternAIServiceSpec ) where

import Test.Hspec ( hspec, describe, it, shouldBe, shouldMatchList )
import Data.Maybe ( isJust, fromMaybe, mapMaybe )
import qualified Data.Text as T
import qualified Services.ConfigService as ConfigService
import qualified Services.PatternAIService as AIService
import qualified Models.Configuration as Config

patternAIServiceSpec :: IO ()
patternAIServiceSpec = hspec $ do
  describe "PatternAIService" $ do
    it "Empty pattern on empty data" $ do
      let regex = AIService.new "^$"
      isJust (AIService.match regex "") `shouldBe` True

    it "Any pattern on empty data" $ do
      let regex = AIService.new "^.*$"
      isJust (AIService.match regex "") `shouldBe` True

    it "Empty pattern on non empty data" $ do
      let regex = AIService.new "^$"
      isJust (AIService.match regex "example") `shouldBe` False

    it "Non empty pattern on empty data" $ do
      let regex = AIService.new "^example$"
      isJust (AIService.match regex "") `shouldBe` False

    it "One pattern on one data" $ do
      let regex = AIService.new "^\\w+\\s?$"
      isJust (AIService.match regex "example") `shouldBe` True

    it "Pattern equal data" $ do
      let regex = AIService.new "^example$"
      isJust (AIService.match regex "example") `shouldBe` True

    it "Pattern not equal data" $ do
      let regex = AIService.new "^elpmaxe$"
      isJust (AIService.match regex "example") `shouldBe` False

    it "Any pattern on one data" $ do
      let regex = AIService.new "^(\\w+\\s?)*$"
      isJust (AIService.match regex "example") `shouldBe` True

    it "Any pattern on 2 data" $ do
      let regex = AIService.new "^(\\w+\\s?)*$"
      isJust (AIService.match regex "example example") `shouldBe` True

    it "2 Any patterns on 2 data" $ do
      let regex = AIService.new "^(\\w+\\s?)*(\\w+\\s?)*$"
      isJust (AIService.match regex "example example") `shouldBe` True

    it "Any and One pattern on 2 data" $ do
      let regex = AIService.new "^(\\w+\\s?)*(\\w+\\s?)?$"
      isJust (AIService.match regex "example example") `shouldBe` True

    it "Rules from config" $ do
      config <- ConfigService.load
      let patternAcion = head . Config.patterns . head $ Config.vkBotList config
      let input = "Привет раршу ркгsf sfdf fsf wfef"
      let answer = if isJust $ AIService.match (Config.regex patternAcion) input then Config.answer patternAcion else ""
      answer `shouldBe` "Привет ;)"

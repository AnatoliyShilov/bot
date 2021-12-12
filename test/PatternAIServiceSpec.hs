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
      isJust (AIService.match [] []) `shouldBe` True

    it "Any pattern on empty data" $ do
      let pattern = AIService.new "?"
      isJust (AIService.match pattern []) `shouldBe` True

    it "Empty pattern on non empty data" $ do
      isJust (AIService.match [] ["example"]) `shouldBe` False

    it "Non empty pattern on empty data" $ do
      let pattern = AIService.new "example"
      isJust (AIService.match pattern []) `shouldBe` False

    it "One pattern on one data" $ do
      let pattern = AIService.new "!"
      isJust (AIService.match pattern ["example"]) `shouldBe` True

    it "Pattern equal data" $ do
      let pattern = AIService.new "example"
      isJust (AIService.match pattern ["example"]) `shouldBe` True

    it "Pattern not equal data" $ do
      let pattern = AIService.new "elpmaxe"
      isJust (AIService.match pattern ["example"]) `shouldBe` False

    it "Any pattern on one data" $ do
      let pattern = AIService.new "?"
      isJust (AIService.match pattern ["example"]) `shouldBe` True

    it "Any pattern on 2 data" $ do
      let pattern = AIService.new "?"
      isJust (AIService.match pattern ["example", "example"]) `shouldBe` True

    it "2 Any patterns on 2 data" $ do
      let pattern = AIService.new "? ?"
      isJust (AIService.match pattern ["example", "example"]) `shouldBe` True

    it "Any and One pattern on 2 data" $ do
      let pattern = AIService.new "? !"
      isJust (AIService.match pattern ["example", "example"]) `shouldBe` True

    it "Set variable" $ do
      let pattern = AIService.new "!some-var"
      let res = AIService.match pattern ["example"]
      fromMaybe [] res `shouldBe` [("some-var", "example")]

    it "Set variable list" $ do
      let pattern = AIService.new "?some-var"
      let res = AIService.match pattern ["example-1", "example-2"]
      fromMaybe [] res `shouldBe` [("some-var", "example-1 example-2")]

    it "Set single and list variables" $ do
      let pattern = AIService.new "?some-list !some-var"
      let res = AIService.match pattern ["example-1", "example-2", "example-3"]
      fromMaybe [] res `shouldMatchList` [("some-list", "example-1 example-2"), ("some-var", "example-3")]

    it "Rules from config" $ do
      config <- ConfigService.load
      let pattern = head . Config.patterns . head $ Config.vkBotList config
      let input = T.splitOn " " "Привет раршу ркгsf sfdf fsf wfef"
      let answer = if isJust $ AIService.match (Config.query pattern) input then Config.answer pattern else ""
      answer `shouldBe` "Привет ;)"

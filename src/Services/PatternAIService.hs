{-# LANGUAGE OverloadedStrings #-}
module Services.PatternAIService (
  new,
  match,
  PatternGramm
  ) where

-- TODO import Text.Regex
import Data.Maybe ( mapMaybe, catMaybes, listToMaybe )
import Control.Exception ( throw )
import Exceptions.PatternNotFoundException
import qualified Data.Text as T

-- TODO Remove to Models
data PatternGramm
  = One -- только 1 символ
  | Any -- 0, 1 и более символов
  | Pattern T.Text
  | Alt [T.Text]
  | Var T.Text
  | Vars T.Text
  deriving (Eq, Show)

-- TODO Remove to Models
new :: T.Text -> [PatternGramm]
new = map toPattern . T.splitOn " "

-- TODO Remove to Models
toPattern :: T.Text -> PatternGramm
toPattern "!" = One
toPattern "?" = Any
toPattern text
  | T.head text == '!' = Var $ T.tail text
  | T.head text == '?' = Vars $ T.tail text
  | T.head text == '^' = Alt . T.splitOn "|" $ T.tail text
  | otherwise = Pattern text

equal :: PatternGramm -> T.Text -> Bool
equal One _ = True
equal Any _ = True
equal (Var _) _ = False
equal (Vars _) _ = False
equal (Pattern pattern) input = pattern == input
equal (Alt patterns) input = input `elem` patterns

match :: [PatternGramm] -> [T.Text] -> Maybe [(T.Text, T.Text)]
match pattern input = matchHelper pattern input $ Just []

matchHelper :: [PatternGramm] -> [T.Text] -> Maybe [(T.Text, T.Text)] -> Maybe [(T.Text, T.Text)]
matchHelper [] []  varlist = varlist
matchHelper [Any] [] varlist = varlist
matchHelper [] _ _ = Nothing
matchHelper _ [] _ = Nothing
matchHelper (One : pattern) (_ : input) varlist =
  matchHelper pattern input varlist
matchHelper (Var name : pattern) (value : input) varlist =
  matchHelper pattern input $ fmap ((name, value) :) varlist
matchHelper pattern@(Any : _) input varlist =
  checkOther pattern input id varlist
matchHelper pattern@(Vars name : _) input varlist =
  checkOther pattern input (modify (name, head input)) varlist
matchHelper pattern input varlist
  | head pattern `equal` head input =
    matchHelper (tail pattern) (tail input) varlist
  | otherwise = Nothing

checkOther :: [PatternGramm] -> [T.Text] -> (Maybe [(T.Text, T.Text)] -> Maybe [(T.Text, T.Text)]) -> Maybe [(T.Text, T.Text)] -> Maybe [(T.Text, T.Text)]
checkOther pattern input apply varlist =
  listToMaybe $ catMaybes [
    matchHelper (tail pattern) input varlist,
    matchHelper (tail pattern) (tail input) (apply varlist),
    matchHelper pattern (tail input) (apply varlist)
    ]

append :: (T.Text, T.Text) -> [(T.Text, T.Text)] -> [(T.Text, T.Text)]
append var [] = [var]
append currentVar@(name, val) varlist@((existName, existVal) : vars)
  | existName == name = (name, existVal <> " " <> val) : vars
  | otherwise = currentVar : varlist

modify :: (T.Text, T.Text) -> Maybe [(T.Text, T.Text)] -> Maybe [(T.Text, T.Text)]
modify (name, value) = fmap (append (name, value))

module Services.PatternAIService (
  new,
  match
  ) where

import Text.Regex ( matchRegex, mkRegex, Regex )
import Data.Maybe ( mapMaybe, catMaybes, listToMaybe )
import qualified Data.Text as T

new :: T.Text -> Regex
new = mkRegex . T.unpack

match :: Regex -> T.Text -> Maybe [T.Text]
match regex input = map T.pack <$> matchRegex regex (T.unpack input)


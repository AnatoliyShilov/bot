module Exceptions.PatternNotFoundException ( PatternNotFoundException (PatternNotFoundException) ) where

import Control.Exception ( Exception )
import qualified Data.Text as T

newtype PatternNotFoundException = PatternNotFoundException { message :: T.Text }

instance Show PatternNotFoundException where
  show (PatternNotFoundException message) = "PatternNotFoundException: " <> show message

instance Exception PatternNotFoundException

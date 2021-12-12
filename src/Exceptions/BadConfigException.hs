module Exceptions.BadConfigException ( BadConfigException (BadConfigException) ) where

import Control.Exception ( Exception )
import qualified Data.Text as T

newtype BadConfigException = BadConfigException { message :: T.Text }

instance Show BadConfigException where
  show (BadConfigException message) = "BadConfigException: " <> show message

instance Exception BadConfigException

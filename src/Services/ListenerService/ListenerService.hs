module Services.ListenerService.ListenerService ( Handle (..) ) where

newtype Handle = Handle
  { listen :: IO () }

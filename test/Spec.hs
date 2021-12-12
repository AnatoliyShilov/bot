import LoggerServiceSpec ( loggerServiceSpec )
import VkServiceSpec ( vkServiceSpec )
import ConfigServiceSpec ( configServiceSpec )
import PatternAIServiceSpec ( patternAIServiceSpec )
import ListenerServiceSpec ( listenerServiceSpec )

main :: IO ()
main = do
  loggerServiceSpec
  configServiceSpec
  patternAIServiceSpec
  -- vkServiceSpec
  -- listenerServiceSpec

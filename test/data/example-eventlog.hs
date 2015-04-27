import           Control.Concurrent (threadDelay)
import           Debug.Trace (traceEvent, traceEventIO, traceMarker, traceMarkerIO)

main :: IO ()
main = do
  traceEventIO "START label 1"
  threadDelay 100000
  traceEventIO "STOP label 1"
  threadDelay 100000

  traceMarkerIO "START label 1"
  threadDelay 100000
  traceMarkerIO "STOP label 1"
  threadDelay 100000

  threadDelay 100000

  traceEvent "START label 2" $ threadDelay 100000
  traceEvent "STOP label 2" $ threadDelay 100000

  traceMarker "START label 2" $ threadDelay 100000
  traceMarker "STOP label 2" $ threadDelay 100000

{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

module GHC.Events.Time
  ( Duration
  , StartStopLabels
  , filterUserEvents
  , groupEventDurations
  , plotDistribution
  ) where


import           Data.Foldable (for_)
import           Data.List (stripPrefix, mapAccumL)
import           Data.Maybe (catMaybes, mapMaybe)
import           Data.Word (Word64)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Diagrams.Backend.Cairo.CmdLine (B)
import           Diagrams.Backend.CmdLine (mainRender, DiagramOpts(..), DiagramLoopOpts(..))
import           Diagrams.Prelude (Diagram, R2)
import           GHC.RTS.Events (Data(..), Event(..), EventLog(..), EventInfo(EventBlock, block_events, UserMessage, UserMarker), Timestamp)
import           Graphics.Rendering.Chart (vectorAlignmentFns)
import           Graphics.Rendering.Chart.Backend.Diagrams (defaultEnv)

import           GHC.Events.Time.Diagrams (doubleHistogramDiagram)


filterUserEvents :: [Event] -> [(Timestamp, String)]
filterUserEvents events =
  concat $ flip mapMaybe events $ \Event{ time, spec } -> case spec of
    UserMessage s -> Just [(time, s)]
    UserMarker  s -> Just [(time, s)]
    EventBlock{ block_events } -> Just (filterUserEvents block_events)
    _             -> Nothing


data Encounter a
  = JustOne !a
  | Bad !Int


makeBad :: Encounter a -> Encounter a -> Encounter a
makeBad JustOne{} _ = Bad 1
makeBad (Bad n)  _ = Bad (n + 1)


type Duration = Word64


type StartStopLabels = (String, String)


labeledEventsToDurations :: StartStopLabels -> [(Timestamp, String)] -> [(String, Timestamp, Duration)]
labeledEventsToDurations startStopLabels labeledEvents = durations
  where
    (start, stop) = startStopLabels
    f (m :: Map String (Encounter Timestamp)) (time, s)
      | Just label <- stripPrefix start s = (Map.insertWith makeBad label (JustOne time) m, Nothing)
      | Just label <- stripPrefix stop  s = case Map.lookup label m of
                                                 Nothing -> (m, Nothing) -- discard STOPs with missing START label
                                                 Just (JustOne startTime) -> (Map.delete label m, Just (label, startTime, time-startTime))
                                                 Just (Bad n) -> (Map.insert label (Bad (n-1)) m, Nothing)
      | otherwise = (m, Nothing)

    durations = catMaybes . snd $ mapAccumL f Map.empty labeledEvents


groupEventDurations :: StartStopLabels -> [(Timestamp, String)] -> Map String [(Timestamp, Duration)]
groupEventDurations startStopLabels labeledEvents = groupedDurations
  where
    durations = labeledEventsToDurations startStopLabels labeledEvents

    groupedDurations = Map.fromListWith (++) [ (label, [(startTime, d)])
                                             | (label, startTime, d) <- durations ]


renderHeader :: FilePath -> Diagram B R2 -> IO ()
renderHeader outputPath =
  mainRender ( DiagramOpts (Just 900) (Just 700) outputPath
             , DiagramLoopOpts False Nothing 0
             )


plotDistribution :: EventLog -> StartStopLabels -> IO ()
plotDistribution EventLog{ dat = Data{ events } } startStopLabels = do

  let userEvents       = filterUserEvents events
      groupedDurations = groupEventDurations startStopLabels userEvents

  denv <- defaultEnv vectorAlignmentFns 500 500

  for_ (Map.toList groupedDurations) $ \(label, durations) -> do
    let ds = map (\(_, z) -> fromIntegral z / 1e6) durations

    renderHeader (label ++ "-durations.svg") $
      doubleHistogramDiagram denv (label ++ " - Durations (milliseconds)") ds


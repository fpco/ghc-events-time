{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

module GHC.Events.Time
  ( Label
  , Duration
  , EventSpan
  , StartStopLabels
  , filterUserEvents
  , groupEventSpans
  , plotDistribution
  ) where


import           Control.Applicative
import qualified Data.DList as DList
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
import           Graphics.Rendering.Chart.Backend.Diagrams (DEnv, defaultEnv)

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


type Label = String


type Duration = Word64


type StartStopLabels = (String, String)


type EventSpan = (Timestamp, Duration)


labeledEventsToSpans :: StartStopLabels -> [(Timestamp, String)] -> [(Label, EventSpan)]
labeledEventsToSpans startStopLabels labeledEvents = labeledEventSpans
  where
    (start, stop) = startStopLabels
    f (m :: Map Label (Encounter Timestamp)) (time, s)
      | Just label <- stripPrefix start s = (Map.insertWith makeBad label (JustOne time) m, Nothing)
      | Just label <- stripPrefix stop  s = case Map.lookup label m of
                                                 Nothing -> (m, Nothing) -- discard STOPs with missing START label
                                                 Just (JustOne startTime) -> (Map.delete label m, Just (label, (startTime, time-startTime)))
                                                 Just (Bad n) -> (Map.insert label (Bad (n-1)) m, Nothing)
      | otherwise = (m, Nothing)

    labeledEventSpans = catMaybes . snd $ mapAccumL f Map.empty labeledEvents


fromListWithAppend :: Ord k => [(k, a)] -> Map k [a]
fromListWithAppend xs = DList.toList <$>
  Map.fromListWith (flip DList.append) [ (k, DList.singleton a) | (k, a) <- xs ]
{-# INLINABLE fromListWithAppend #-}


groupEventSpans :: StartStopLabels -> [(Timestamp, Label)] -> Map Label [EventSpan]
groupEventSpans startStopLabels labeledEvents =
  fromListWithAppend (labeledEventsToSpans startStopLabels labeledEvents)


renderHeader :: FilePath -> Diagram B R2 -> IO ()
renderHeader outputPath =
  mainRender ( DiagramOpts (Just 900) (Just 700) outputPath
             , DiagramLoopOpts False Nothing 0
             )


renderWithAllUserEvents ::
  Label ->
  (DEnv -> Label -> [(Timestamp, Duration)] -> Diagram B R2) ->
  EventLog ->
  StartStopLabels ->
  IO ()
renderWithAllUserEvents outFileInfix diagramFun eventLog startStopLabels = do

  let EventLog{ dat = Data{ events } } = eventLog
      userEvents   = filterUserEvents events
      groupedSpans = groupEventSpans startStopLabels userEvents

  denv <- defaultEnv vectorAlignmentFns 500 500

  for_ (Map.toList groupedSpans) $ \(label, eventSpans) -> do

    renderHeader (label ++ "-" ++ outFileInfix ++ ".svg") $
      diagramFun denv label eventSpans


nanoSecsToMillis :: Word64 -> Double
nanoSecsToMillis ns = fromIntegral ns * 1e-6


plotDistribution :: EventLog -> StartStopLabels -> IO ()
plotDistribution =
  renderWithAllUserEvents "durations" $ \denv label eventSpans ->
    let durations = map (nanoSecsToMillis . snd) eventSpans
    in doubleHistogramDiagram
         denv
         (label ++ " - Durations (milliseconds)")
         durations

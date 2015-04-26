{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}

module GHC.Events.Time
  ( Label
  , Duration
  , EventSpan
  , StartStopLabels
  , filterUserEvents
  , groupEventSpans
  , plotHistogram
  , plotOverTime
  , plotCumulativeFreq
  , plotCumulativeSum
  ) where


import           Control.Applicative
import qualified Data.DList as DList
import           Data.Foldable (for_)
import           Data.List (stripPrefix, mapAccumL, sort)
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

import           GHC.Events.Time.Diagrams (doubleHistogramDiagram, xyDiagram)



filterUserEvents :: [Event] -> [(Timestamp, String)]
filterUserEvents events =
  concat $ flip mapMaybe events $ \Event{ time, spec } -> case spec of
    UserMessage s -> Just [(time, s)]
    UserMarker  s -> Just [(time, s)]
    EventBlock{ block_events } -> Just (filterUserEvents block_events)
    _             -> Nothing


type Label = String


type Duration = Word64


type StartStopLabels = (String, String)


type EventSpan = (Timestamp, Duration)



-- | A value that we desire to encounter not more than once; otherwise we track
-- it as `Bad`, with a counter of how many more times than desired we have
-- encountered it (e.g. `Bad 1` means we've encountered it once, and that's
-- bad because it's one more time than we'd like).
--
-- After something has become `Bad`, it stays `Bad` until it disappears
-- completely. That's why a `Bad 0` isn't `JustOne`!
data Encounter a
  = JustOne !a
  | Bad !Int


-- | Combines two `Encounter`s; the result is `Bad` because two are already
-- more than we desire.
makeBad :: Encounter a -> Encounter a -> Encounter a
makeBad JustOne{} JustOne{} = Bad 1
makeBad JustOne{} (Bad m)   = Bad m
makeBad (Bad n)   JustOne{} = Bad n
makeBad (Bad n)   (Bad m)   = Bad (n + m)


-- | Groups a list of punctual string-annotated events to a list of
-- `EventSpan`s.
--
-- An event with annotation `"START " ++ label` is matched with the next event
-- annotated as `"STOP " ++ label` (the labels must match).
--
-- The start/stop prefixes can be customised via the passed-in
-- `StartStopLabels`.
--
-- Stop events without preceding start event are discarded, as are start events
-- that have no stop event following.
--
-- `EventSpan`s with the same label are allowed, but their durations must not
-- overlap - that would make matching ambiguous.
-- Events that lead to overlapping spans are ignored.
--
-- Example: For A=start, B=stop, and identical labels, the series
-- @B A B A A B B A B A@ will yield 2 `EventSpan`s.
-- The leading @B@, trailing @A@, and ambiguous @A A B B@ are discarded.
--
-- PRE: The events passed in are in ascending time order.
--
-- POST: The output `EventSpan`s are in ascending time order.
labeledEventsToSpans :: StartStopLabels -> [(Timestamp, String)] -> [(Label, EventSpan)]
labeledEventsToSpans startStopLabels labeledEvents = labeledEventSpans
  where
    (start, stop) = startStopLabels

    -- The Map keeps track of whether we've already encountered an event with
    -- the given `Label`. The good case is when we encounter exactly one START
    -- label (inserts `JustOne`) and then a STOP label (deletes `JustOne`).
    -- If we encounter more than one START label, matching is ambiguous and we
    -- go into `Bad` mode for that label; matching will only continue after that
    -- label has been completely "drained" from the Map with STOP labels.
    f (m :: Map Label (Encounter Timestamp)) (time, s)
      | Just label <- stripPrefix start s = (Map.insertWith makeBad label (JustOne time) m, Nothing)
      | Just label <- stripPrefix stop  s =
          case Map.lookup label m of
            Nothing                  -> (m,                              Nothing) -- discard STOPs with missing START label
            Just (JustOne startTime) -> (Map.delete label m,             Just (label, (startTime, time-startTime)))
            Just (Bad 0)             -> (Map.delete label m,             Nothing) -- done draining
            Just (Bad n)             -> (Map.insert label (Bad (n-1)) m, Nothing) -- drain overlap
      | otherwise = (m, Nothing) -- discard event that don't have a start/stop prefix

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
  String ->
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


nanoSecsToSecs :: Word64 -> Double
nanoSecsToSecs ns = fromIntegral ns * 1e-9


nanoSecsToMillis :: Word64 -> Double
nanoSecsToMillis ns = fromIntegral ns * 1e-6


plotHistogram :: EventLog -> StartStopLabels -> IO ()
plotHistogram =
  renderWithAllUserEvents "histogram" $ \denv label eventSpans ->
    let durations = map (nanoSecsToMillis . snd) eventSpans
    in doubleHistogramDiagram
         denv
         (label ++ " - Durations (milliseconds)")
         durations


plotOverTime :: EventLog -> StartStopLabels -> IO ()
plotOverTime =
  renderWithAllUserEvents "overtime" $ \denv label eventSpans ->
    xyDiagram
      denv
      (label ++ " - Durations over time")
      ("Event number", "Duration (milliseconds)")
      [ (nanoSecsToSecs time, nanoSecsToMillis dur) | (time, dur) <- eventSpans ]


plotCumulativeFreq :: EventLog -> StartStopLabels -> IO ()
plotCumulativeFreq =
  renderWithAllUserEvents "cumulative-freq" $ \denv label eventSpans ->
    let durations = map (nanoSecsToMillis . snd) eventSpans
    in xyDiagram
         denv
         (label ++ " - Durations cumulative frequency")
         ("Event number", "Duration (milliseconds)")
         (zip [(0::Int)..] (sort durations))


plotCumulativeSum :: EventLog -> StartStopLabels -> IO ()
plotCumulativeSum =
  renderWithAllUserEvents "cumulative-sum" $ \denv label eventSpans ->
    let cumulativeDurations =   map nanoSecsToSecs
                              . scanl (+) 0
                              . sort
                              . map snd
                              $ eventSpans
    in xyDiagram
         denv
         (label ++ " - Durations cumulative sum")
         ("Number of events (sorted by event duration, ascending)", "Accumulated duration (seconds)")
         (zip [(0::Int)..] cumulativeDurations)

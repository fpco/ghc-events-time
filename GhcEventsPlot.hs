{-# LANGUAGE LambdaCase, NamedFieldPuns, ScopedTypeVariables, DeriveGeneric #-}

module Main where

import           Data.Foldable (for_)
import           Data.List (stripPrefix, mapAccumL)
import           Data.Maybe (catMaybes, mapMaybe)
import           Data.Word (Word64)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Diagrams.Backend.Cairo.CmdLine (B)
import           Diagrams.Backend.CmdLine (mainRender, DiagramOpts(..), DiagramLoopOpts(..))
import           Diagrams.Prelude (Diagram, R2)
import           GHC.RTS.Events (Data(..), Event(..), EventLog(..), EventInfo(EventBlock, block_events, UserMessage, UserMarker), Timestamp, readEventLogFromFile)
import           GHC.Generics
import           Graphics.Rendering.Chart (vectorAlignmentFns)
import           Graphics.Rendering.Chart.Backend.Diagrams (defaultEnv)
import           Options.Applicative
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)

import           GHC.Events.Time.Diagrams (doubleHistogramDiagram)


data PlotOpts = PlotOpts
  { plotOptsMode :: !PlotMode
  , plotOptsEventLogPath :: !FilePath
  } deriving (Eq, Ord, Show, Generic)


data PlotMode
  = PlotDistribution
  | PlotOverTime
  deriving (Eq, Ord, Show, Generic)


plotModeParser :: Parser PlotMode
plotModeParser = subparser
  (
    metavar "MODE"

    <> command "distribution" (info
      (pure PlotDistribution)
      (progDesc "Plot the time distribution of events")
    )

    <> command "overtime" (info
      (pure PlotOverTime)
      (progDesc "Plot the time distribution of events over run-time of the program")
    )

  )


plotoptsParser :: Parser PlotOpts
plotoptsParser =
  PlotOpts
    <$> plotModeParser
    <*> argument str (metavar "<eventlog file>")


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


labeledEventsToDurations :: [(Timestamp, String)] -> [(String, Timestamp, Duration)]
labeledEventsToDurations labeledEvents = durations
  where
    f (m :: Map String (Encounter Timestamp)) (time, s)
      | Just label <- stripPrefix "START " s = (Map.insertWith makeBad label (JustOne time) m, Nothing)
      | Just label <- stripPrefix "STOP "  s = case Map.lookup label m of
                                                 Nothing -> (m, Nothing) -- discard STOPs with missing START label
                                                 Just (JustOne startTime) -> (Map.delete label m, Just (label, startTime, time-startTime))
                                                 Just (Bad n) -> (Map.insert label (Bad (n-1)) m, Nothing)
      | otherwise = (m, Nothing)

    durations = catMaybes . snd $ mapAccumL f Map.empty labeledEvents


groupEventDurations :: [(Timestamp, String)] -> Map String [(Timestamp, Duration)]
groupEventDurations labeledEvents = groupedDurations
  where
    durations = labeledEventsToDurations labeledEvents

    groupedDurations = Map.fromListWith (++) [ (label, [(startTime, d)])
                                             | (label, startTime, d) <- durations ]


renderHeader :: FilePath -> Diagram B R2 -> IO ()
renderHeader outputPath =
  mainRender ( DiagramOpts (Just 900) (Just 700) outputPath
             , DiagramLoopOpts False Nothing 0
             )


plotDistribution :: EventLog -> IO ()
plotDistribution EventLog{ dat = Data{ events } } = do
  let userEvents = filterUserEvents events

      groupedDurations = groupEventDurations userEvents

  denv <- defaultEnv vectorAlignmentFns 500 500

  for_ (Map.toList groupedDurations) $ \(label, durations) -> do
    let ds = map (\(_, z) -> fromIntegral z / 1e6) durations

    renderHeader (label ++ "-durations.svg") $
      doubleHistogramDiagram denv (label ++ " - Durations (milliseconds)") ds


die :: String -> IO a
die msg = do
  hPutStrLn stderr $ "ERROR: " ++ msg
  exitFailure


main :: IO ()
main = do
  PlotOpts mode eventLogPath <- execParser $ info (helper <*> plotoptsParser) fullDesc

  eventLog <- readEventLogFromFile eventLogPath >>= \case
    Left err -> die $ "Could not read eventlog file: " ++ err
    Right el -> return el

  case mode of
    PlotDistribution -> plotDistribution eventLog
    PlotOverTime -> error "overtime not yet implemented"

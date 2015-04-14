{-# LANGUAGE LambdaCase, NamedFieldPuns, ScopedTypeVariables, DeriveGeneric #-}

module Main where

import           Data.Foldable (for_)
import           Data.List (stripPrefix, mapAccumL)
import           Data.Maybe (catMaybes, mapMaybe)
import           Data.Word (Word64)
import           Data.Map (Map)
import qualified Data.Map as Map
import           GHC.RTS.Events (Data(..), Event(..), EventLog(..), EventInfo(EventBlock, block_events, UserMessage, UserMarker), Timestamp, readEventLogFromFile)
import           GHC.Generics
import           Options.Applicative
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)

import qualified Data.Vector.Unboxed as U

import Data.Histogram ( asList )
import Data.Histogram.Fill
import Data.Histogram.Generic ( Histogram )

import qualified Control.Foldl as F


import Data.Colour
import Diagrams.Prelude hiding ( Duration, sample, render, (<>) )

import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Backend.CmdLine

import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart hiding (label)

import Data.Default.Class

import Text.Printf

import System.IO.Unsafe


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


plotDistribution :: EventLog -> IO ()
plotDistribution EventLog{ dat = Data{ events } } = do
  let userEvents = filterUserEvents events

      groupedDurations = groupEventDurations userEvents

  for_ (Map.toList groupedDurations) $ \(label, durations) -> do
    let ds = map (\(_, z) -> fromIntegral z / 1e6) durations

    renderHeader (label ++ "-durations.svg") $
      doubleHistogramDiagram (label ++ " - Durations (milliseconds)") ds


doubleHistogramDiagram :: String -> [Double] -> Diagram B R2
doubleHistogramDiagram label ds = barDiag label (asList (hist ds))


denv :: DEnv
denv = unsafePerformIO $ defaultEnv vectorAlignmentFns 500 500
{-# NOINLINE denv #-}


numBins :: Int
numBins = 40

stats :: (F.Foldable f, Fractional a) =>
         f a -> (a, a, a)
stats v = F.fold stats v
  where
    stats = f <$> (F.premap (\x -> x * x) F.sum) <*> F.sum <*> F.genericLength
    f x2Sum xSum n = (var, mean, n)
      where
        mean = xSum / n
        mean2 = x2Sum / n
        var = n * (mean2 - mean * mean) / (n - 1)

hb :: F.Foldable f =>
      f Double -> HBuilder Double (Histogram U.Vector BinD Double)
hb xs = forceDouble -<< mkSimple (binD lower numBins upper)
  where
    (varX, xBar, _) = stats xs
    lower = xBar - 2.0 * sqrt varX
    upper = xBar + 2.0 * sqrt varX

hist :: F.Foldable f =>
        f Double -> Histogram U.Vector BinD Double
hist xs = fillBuilder (hb xs) xs

renderHeader :: FilePath -> Diagram B R2 -> IO ()
renderHeader fn =
  mainRender ( DiagramOpts (Just 900) (Just 700) fn
             , DiagramLoopOpts False Nothing 0
             )

barChart :: String ->
            [(Double, Double)] ->
            Graphics.Rendering.Chart.Renderable ()
barChart title bvs = toRenderable layout
  where
    layout =
      layout_title .~ title
      $ layout_x_axis . laxis_generate .~ autoIndexAxis (map (printf "%4.3f" . fst) bvs)

      $ layout_y_axis . laxis_title .~ "Frequency"
      $ layout_plots .~ (map plotBars $ [bars1])
      $ def

    bars1 =
      plot_bars_titles .~ [title]
      $ plot_bars_values .~ addIndexes (map return $ map snd bvs)
      $ plot_bars_style .~ BarsClustered
      $ plot_bars_item_styles .~ [(solidFillStyle (blue `withOpacity` 0.25), Nothing)]
      $ def

barDiag :: String ->
           [(Double, Double)] ->
           Diagram B R2
barDiag title bvs = fst $ runBackend denv (render (barChart title bvs) (500, 500))

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

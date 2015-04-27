{-# LANGUAGE LambdaCase, NamedFieldPuns, ScopedTypeVariables, DeriveGeneric #-}

module Main where

import           Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import           GHC.RTS.Events (EventLog(..), Data(..), readEventLogFromFile)
import           GHC.Generics
import           Options.Applicative
import           System.Exit (exitFailure)
import           System.FilePath (takeFileName)
import           System.IO (hPutStrLn, stderr)

import           GHC.Events.Time (filterUserEvents, groupEventSpans, makeChartEnv, renderLabelDiagrams, plotHistogram, plotOverTime, plotCumulativeFreq, plotCumulativeSum)


-- | Command line options for this program.
data Opts = Opts
  { optsPlotCommands :: ![PlotCommand] -- ^ types of plots to generate
  , optsEventLogPath :: !FilePath -- ^ input GHC *.eventlog file
  , optsStartLabel :: !String -- ^ prefix indicating start of an event (e.g. "START ")
  , optsStopLabel :: !String -- ^ prefix indicating stop of an event (e.g. "STOP ")
  } deriving (Eq, Ord, Show, Generic)


-- | The type of plot we want to generate.
data PlotCommand
  = PlotHistogram
  | PlotCumulativeFreq
  | PlotCumulativeSum
  | PlotOverTime
  deriving (Eq, Ord, Show, Generic)


-- | CLI parser the type of plot to generate.
plotModeParser :: Parser PlotCommand
plotModeParser = subparser
  -- Keeping PlotCommand as a subparser allows each command to have its own
  -- custom set of options.
  (
    metavar "COMMAND"

    <> command "histogram" (info
      (pure PlotHistogram)
      (progDesc "Plot a histogram of event durations")
    )

    <> command "cumulative-freq" (info
      (pure PlotCumulativeFreq)
      (progDesc "Plot cumulative frequency (how many event durations are shorter than X)")
    )

    <> command "cumulative-sum" (info
      (pure PlotCumulativeSum)
      (progDesc $ "Plot cumulative sum (how do event durations add up to total run time;"
                  ++ " integral of cumulative-freq)")
    )

    <> command "over-time" (info
      (pure PlotOverTime)
      (progDesc "Plot time durations of events over total run time of the program")
    )
  )


-- | Main program CLI parser.
optsParser :: Parser Opts
optsParser =
  Opts
    <$> many plotModeParser
    <*> argument str (metavar "<eventlog file>")
    <*> strOption
          (   long "start"
           <> metavar "STR"
           <> help "Use STR as the prefix for the start of user events (default: \"START \")"
           <> value "START "
          )
    <*> strOption
          (   long "stop"
           <> metavar "STR"
           <> help "Use STR as the prefix for the end of user events (default: \"STOP \")"
           <> value "STOP "
          )


-- | Terminates the program with an error message to stderr.
die :: String -> IO a
die msg = do
  hPutStrLn stderr $ "ERROR: " ++ msg
  exitFailure


main :: IO ()
main = do
  -- Parse CLI options.
  Opts
    { optsPlotCommands = commands
    , optsEventLogPath = eventLogPath
    , optsStartLabel   = startLabel
    , optsStopLabel    = stopLabel
    } <- execParser $ info (helper <*> optsParser) fullDesc

  let startStopLabels = (startLabel, stopLabel)

  -- Load *.eventlog file.
  eventLog <- readEventLogFromFile eventLogPath >>= \case
    Left err -> die $ "Could not read eventlog file: " ++ err
    Right el -> return el

  -- Find user annotated events, group durations by labels.
  let EventLog{ dat = Data{ events } } = eventLog
      userEvents   = filterUserEvents events
      groupedSpans = groupEventSpans startStopLabels userEvents

  -- Setup plotting.
  env <- makeChartEnv
  let plot name plotFun = renderLabelDiagrams
                            (takeFileName eventLogPath)
                            name
                            (Map.mapWithKey (plotFun env) groupedSpans)

  -- Generate the plot.
  for_ commands $ \comm -> case comm of
    PlotHistogram      -> plot "histogram" plotHistogram
    PlotCumulativeFreq -> plot "overtime" plotOverTime
    PlotCumulativeSum  -> plot "cumulative-freq" plotCumulativeFreq
    PlotOverTime       -> plot "cumulative-sum" plotCumulativeSum

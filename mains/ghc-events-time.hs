{-# LANGUAGE LambdaCase, NamedFieldPuns, ScopedTypeVariables, DeriveGeneric #-}

module Main where

import           GHC.RTS.Events (readEventLogFromFile)
import           GHC.Generics
import           Options.Applicative
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)

import           GHC.Events.Time (plotHistogram, plotOverTime, plotCumulativeFreq, plotCumulativeSum)


-- | Command line options for this program.
data Opts = Opts
  { optsPlotMode :: !PlotMode -- ^ type of plot to generate
  , optsEventLogPath :: !FilePath -- ^ input GHC *.eventlog file
  , optsStartLabel :: !String -- ^ prefix indicating start of an event (e.g. "START ")
  , optsStopLabel :: !String -- ^ prefix indicating stop of an event (e.g. "STOP ")
  } deriving (Eq, Ord, Show, Generic)


-- | The type of plot we want to generate.
data PlotMode
  = PlotHistogram
  | PlotCumulativeFreq
  | PlotCumulativeSum
  | PlotOverTime
  deriving (Eq, Ord, Show, Generic)


-- | CLI parser the type of plot to generate.
plotModeParser :: Parser PlotMode
plotModeParser = subparser
  (
    metavar "MODE"

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
plotoptsParser :: Parser Opts
plotoptsParser =
  Opts
    <$> plotModeParser
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
    { optsPlotMode     = mode
    , optsEventLogPath = eventLogPath
    , optsStartLabel   = startLabel
    , optsStopLabel    = stopLabel
    } <- execParser $ info (helper <*> plotoptsParser) fullDesc

  let startStopLabels = (startLabel, stopLabel)

  -- Load *.eventlog file.
  eventLog <- readEventLogFromFile eventLogPath >>= \case
    Left err -> die $ "Could not read eventlog file: " ++ err
    Right el -> return el

  -- Generate the plot.
  case mode of
    PlotHistogram -> plotHistogram eventLog startStopLabels
    PlotCumulativeFreq -> plotCumulativeFreq eventLog startStopLabels
    PlotCumulativeSum -> plotCumulativeSum eventLog startStopLabels
    PlotOverTime -> plotOverTime eventLog startStopLabels
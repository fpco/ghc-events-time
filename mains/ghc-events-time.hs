{-# LANGUAGE LambdaCase, NamedFieldPuns, ScopedTypeVariables, DeriveGeneric #-}

module Main where

import           GHC.RTS.Events (readEventLogFromFile)
import           GHC.Generics
import           Options.Applicative
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)

import           GHC.Events.Time (plotHistogram, plotOverTime, plotCumulativeFreq, plotCumulativeSum)


data PlotOpts = PlotOpts
  { plotOptsMode :: !PlotMode
  , plotOptsEventLogPath :: !FilePath
  , plotOptsStartLabel :: !String
  , plotOptsStopLabel :: !String
  } deriving (Eq, Ord, Show, Generic)


data PlotMode
  = PlotHistogram
  | PlotCumulativeFreq
  | PlotCumulativeSum
  | PlotOverTime
  deriving (Eq, Ord, Show, Generic)


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


plotoptsParser :: Parser PlotOpts
plotoptsParser =
  PlotOpts
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


die :: String -> IO a
die msg = do
  hPutStrLn stderr $ "ERROR: " ++ msg
  exitFailure


main :: IO ()
main = do
  PlotOpts
    { plotOptsMode         = mode
    , plotOptsEventLogPath = eventLogPath
    , plotOptsStartLabel   = startLabel
    , plotOptsStopLabel    = stopLabel
    } <- execParser $ info (helper <*> plotoptsParser) fullDesc

  let startStopLabels = (startLabel, stopLabel)

  eventLog <- readEventLogFromFile eventLogPath >>= \case
    Left err -> die $ "Could not read eventlog file: " ++ err
    Right el -> return el

  case mode of
    PlotHistogram -> plotHistogram eventLog startStopLabels
    PlotCumulativeFreq -> plotCumulativeFreq eventLog startStopLabels
    PlotCumulativeSum -> plotCumulativeSum eventLog startStopLabels
    PlotOverTime -> plotOverTime eventLog startStopLabels

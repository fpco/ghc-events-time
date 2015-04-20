{-# LANGUAGE LambdaCase, NamedFieldPuns, ScopedTypeVariables, DeriveGeneric #-}

module Main where

import           GHC.RTS.Events (readEventLogFromFile)
import           GHC.Generics
import           Options.Applicative
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)

import           GHC.Events.Time (plotDistribution, plotOverTime, plotPDF, plotCDF)


data PlotOpts = PlotOpts
  { plotOptsMode :: !PlotMode
  , plotOptsEventLogPath :: !FilePath
  , plotOptsStartLabel :: !String
  , plotOptsStopLabel :: !String
  } deriving (Eq, Ord, Show, Generic)


data PlotMode
  = PlotDistribution
  | PlotPDF
  | PlotCDF
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

    <> command "pdf" (info
      (pure PlotPDF)
      (progDesc "Plot the time distribution of events as a probability density function")
    )

    <> command "cdf" (info
      (pure PlotCDF)
      (progDesc "Plot the time distribution of events as a cumulative density function")
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
    PlotDistribution -> plotDistribution eventLog startStopLabels
    PlotPDF -> plotPDF eventLog startStopLabels
    PlotCDF -> plotCDF eventLog startStopLabels
    PlotOverTime -> plotOverTime eventLog startStopLabels

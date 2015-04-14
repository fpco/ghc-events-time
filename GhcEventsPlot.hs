{-# LANGUAGE LambdaCase, NamedFieldPuns, ScopedTypeVariables, DeriveGeneric #-}

module Main where

import           GHC.RTS.Events (readEventLogFromFile)
import           GHC.Generics
import           Options.Applicative
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)

import           GHC.Events.Time (plotDistribution)


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

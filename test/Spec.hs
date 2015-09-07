{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative
import           Control.Monad
import           Data.List (sort)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Test.Hspec
import           Test.QuickCheck
import           System.Directory (getCurrentDirectory, getDirectoryContents)
import           System.Exit (ExitCode(ExitSuccess))
import           System.FilePath ((</>))
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process (CreateProcess(cwd), createProcess, proc, waitForProcess)

import           GHC.Events.Time (Timestamp, labeledEventsToSpans, fromListWithAppend, sanitizeLabelForFilePath)


genLabeledEvents :: Gen [(Timestamp, String)]
genLabeledEvents = do
  times <- orderedList
  forM times $ \t -> do
    i :: Int <- choose (1, 5)
    return (t, "label " ++ show i)


newtype ExampleLabeledEvents = ExampleLabeledEvents [(Timestamp, String)]
  deriving (Eq, Ord, Show)


instance Arbitrary ExampleLabeledEvents where
  arbitrary = ExampleLabeledEvents <$> genLabeledEvents
  shrink (ExampleLabeledEvents xs) =
    -- Only shrink the number of entries, not the Timestamps or Labels
    -- (they are already quite minimal as generated by genLabeledEvents).
    ExampleLabeledEvents <$> shrinkList shrinkNothing xs


main :: IO ()
main = hspec $ do

  describe "fromListWithAppend" $ do

    it "works like fromListWith (flip (++))" $ do

      property $ \(kvs :: [(Int, Int)]) ->
        fromListWithAppend kvs
          `shouldBe`
          Map.fromListWith (flip (++)) (map (\(k, a) -> (k, [a])) kvs)


  describe "labeledEventsToSpans" $ do

    it "ignores unmatched and overlapping event spans" $ do

      -- In particular, this tests whether the function drains overlapping
      -- events correctly.

      let labeledEvents =
            [ ( 1, "START a")
            , ( 2, "START")
            , ( 3, "STOP no matching start")
            , ( 4, "START ok 1")
            , ( 5, "STOP ok 1")
            , ( 6, "START overlapping")
            , ( 7, "START overlapping")
            , ( 8, "STOP overlapping")
            , ( 9, "STOP overlapping")
            , (10, "START ok 2")
            , (20, "STOP ok 2")
            , (21, "START no matching stop")
            ]

      labeledEventsToSpans ("START ", "STOP ") labeledEvents
        `shouldBe`
        [ ("ok 1", (4, 1))
        , ("ok 2", (10, 10))
        ]

    it "returns only spans that exist in the input" $ do
      property $
        \startStopLabels (ExampleLabeledEvents labeledEvents) ->
          let spans = labeledEventsToSpans startStopLabels labeledEvents
              (start, stop) = startStopLabels
              set = Set.fromList labeledEvents
              inSet x = x `Set.member` set
          in
             flip all spans $ \(l, (time, duration)) ->
                  inSet (time,            start ++ l)
               && inSet (time + duration, stop ++ l)


  describe "sanitizeLabelForFilePath" $ do

    it "sanitizes labels" $ do
      sanitizeLabelForFilePath "" `shouldBe` ""
      sanitizeLabelForFilePath "ab" `shouldBe` "ab"
      sanitizeLabelForFilePath "a/b" `shouldBe` "a-b"
      sanitizeLabelForFilePath "a/../b" `shouldBe` "a-..-b"
      sanitizeLabelForFilePath "/a/b" `shouldBe` "-a-b"


  describe "diagram generation" $ do

    it "works on the example eventlog file" $ do

      -- Files we expect to be generated.
      let outfiles =
            [ "example-eventlog.eventlog-label 1-cumulative-freq.svg"
            , "example-eventlog.eventlog-label 1-cumulative-sum.svg"
            , "example-eventlog.eventlog-label 1-histogram.svg"
            , "example-eventlog.eventlog-label 1-overtime.svg"
            , "example-eventlog.eventlog-label 2-cumulative-freq.svg"
            , "example-eventlog.eventlog-label 2-cumulative-sum.svg"
            , "example-eventlog.eventlog-label 2-histogram.svg"
            , "example-eventlog.eventlog-label 2-overtime.svg"
            ]

      -- Use temp dir, gets cleaned up after run.
      withSystemTempDirectory "ghc-events-time-test" $ \tmpDir -> do

        projectDir <- getCurrentDirectory

        -- Run ghc-events-time.
        (_, _, _, pid) <-
          createProcess
            (proc
               ("ghc-events-time")
               [ "histogram"
               , "cumulative-freq"
               , "cumulative-sum"
               , "over-time"
               , projectDir </> "test/data/example-eventlog.eventlog"
               ]
            ){ cwd = Just tmpDir }

        ExitSuccess <- waitForProcess pid

        -- Check that all outfiles were generated, and no other files.
        generatedFiles <- filter (`notElem` [".", ".."]) <$> getDirectoryContents tmpDir
        sort generatedFiles `shouldBe` sort outfiles

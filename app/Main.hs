module Main where

import Aoc.Solution.Day01 qualified as Day01
import Control.Applicative ((<**>))
import Data.Text (Text, justifyRight, pack, unpack)
import Data.Text.IO qualified as Text.IO
import Options.Applicative (Parser, ParserInfo, auto, execParser, fullDesc, header, help, helper, info, long, option, progDesc, short)

data Args = Args
  { day :: Int,
    part :: Int
  }

parseArgs :: Parser Args
parseArgs =
  Args
    <$> option
      auto
      ( long "day"
          <> short 'd'
          <> help "day to run (1 - 25)"
      )
    <*> option
      auto
      ( long "part"
          <> short 'p'
          <> help "part to run (1 or 2)"
      )

parseInfoArgs :: ParserInfo Args
parseInfoArgs =
  info
    (parseArgs <**> helper)
    ( fullDesc
        <> progDesc "run aoc solution"
        <> header "aoc-exe - run aoc solution"
    )

readInput :: Args -> IO Text
readInput Args {day} =
  let dayText = justifyRight 2 '0' $ pack (show day)
   in Text.IO.readFile $ "./data/day" <> unpack dayText <> ".txt"

runSolution :: Text -> Args -> IO Text
runSolution input = \case
  Args {day = 1, part = 1} -> Day01.part1 input
  _ -> pure "unknown day and part"

main :: IO ()
main = do
  args <- execParser parseInfoArgs
  input <- readInput args
  result <- runSolution input args

  Text.IO.putStrLn result

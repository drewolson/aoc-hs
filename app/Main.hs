module Main where

import Aoc.Solution.Day01 qualified as Day01
import Aoc.Solution.Day02 qualified as Day02
import Aoc.Solution.Day03 qualified as Day03
import Aoc.Solution.Day04 qualified as Day04
import Control.Applicative ((<**>))
import Data.Text qualified as T
import Options.Applicative (Parser, ParserInfo)
import Options.Applicative qualified as A

data Args = Args
  { day :: Int,
    part :: Int
  }

parseArgs :: Parser Args
parseArgs =
  Args
    <$> A.option
      A.auto
      ( A.long "day"
          <> A.short 'd'
          <> A.help "day to run (1 - 25)"
      )
    <*> A.option
      A.auto
      ( A.long "part"
          <> A.short 'p'
          <> A.help "part to run (1 or 2)"
      )

parseInfoArgs :: ParserInfo Args
parseInfoArgs =
  A.info
    (parseArgs <**> A.helper)
    ( A.fullDesc
        <> A.progDesc "run aoc solution"
        <> A.header "aoc-exe - run aoc solution"
    )

readInput :: Args -> IO String
readInput Args {day} =
  let dayText = T.justifyRight 2 '0' $ T.pack $ show day
   in readFile $ "./data/day" <> T.unpack dayText <> ".txt"

runSolution :: String -> Args -> IO ()
runSolution input = \case
  Args {day = 1, part = 1} -> print $ Day01.part1 input
  Args {day = 1, part = 2} -> print $ Day01.part2 input
  Args {day = 2, part = 1} -> print $ Day02.part1 input
  Args {day = 2, part = 2} -> print $ Day02.part2 input
  Args {day = 3, part = 1} -> print $ Day03.part1 input
  Args {day = 3, part = 2} -> print $ Day03.part2 input
  Args {day = 4, part = 1} -> print $ Day04.part1 input
  Args {day = 4, part = 2} -> print $ Day04.part2 input
  _ -> fail "unknown day and part"

main :: IO ()
main = do
  args <- A.execParser parseInfoArgs
  input <- readInput args

  runSolution input args

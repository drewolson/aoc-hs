module Main where

import Aoc.Solution.Day01 qualified as Day01
import Aoc.Solution.Day02 qualified as Day02
import Aoc.Solution.Day03 qualified as Day03
import Aoc.Solution.Day04 qualified as Day04
import Aoc.Solution.Day05 qualified as Day05
import Aoc.Solution.Day06 qualified as Day06
import Aoc.Solution.Day07 qualified as Day07
import Aoc.Solution.Day08 qualified as Day08
import Aoc.Solution.Day09 qualified as Day09
import Aoc.Solution.Day10 qualified as Day10
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
  Args {day = 5, part = 1} -> print $ Day05.part1 input
  Args {day = 5, part = 2} -> print $ Day05.part2 input
  Args {day = 6, part = 1} -> print $ Day06.part1 input
  Args {day = 6, part = 2} -> print $ Day06.part2 input
  Args {day = 7, part = 1} -> print $ Day07.part1 input
  Args {day = 7, part = 2} -> print $ Day07.part2 input
  Args {day = 8, part = 1} -> print $ Day08.part1 input
  Args {day = 8, part = 2} -> print $ Day08.part2 input
  Args {day = 9, part = 1} -> print $ Day09.part1 input
  Args {day = 9, part = 2} -> print $ Day09.part2 input
  Args {day = 10, part = 1} -> print $ Day10.part1 input
  Args {day = 10, part = 2} -> print $ Day10.part2 input
  _ -> fail "unknown day and part"

main :: IO ()
main = do
  args <- A.execParser parseInfoArgs
  input <- readInput args

  runSolution input args

module Main where

import Aoc.TwentyOne.Day01 qualified as TwentyOne.Day01
import Aoc.TwentyOne.Day02 qualified as TwentyOne.Day02
import Aoc.TwentyOne.Day03 qualified as TwentyOne.Day03
import Aoc.TwentyOne.Day04 qualified as TwentyOne.Day04
import Aoc.TwentyOne.Day05 qualified as TwentyOne.Day05
import Aoc.TwentyOne.Day06 qualified as TwentyOne.Day06
import Aoc.TwentyOne.Day07 qualified as TwentyOne.Day07
import Aoc.TwentyOne.Day08 qualified as TwentyOne.Day08
import Aoc.TwentyOne.Day09 qualified as TwentyOne.Day09
import Aoc.TwentyOne.Day10 qualified as TwentyOne.Day10
import Aoc.TwentyOne.Day11 qualified as TwentyOne.Day11
import Aoc.TwentyOne.Day12 qualified as TwentyOne.Day12
import Aoc.TwentyOne.Day13 qualified as TwentyOne.Day13
import Control.Applicative ((<**>))
import Data.Text qualified as T
import Options.Applicative (Parser, ParserInfo)
import Options.Applicative qualified as A

data Args = Args
  { year :: Int,
    day :: Int,
    part :: Int
  }

parseArgs :: Parser Args
parseArgs =
  Args
    <$> A.option
      A.auto
      ( A.long "year"
          <> A.short 'y'
          <> A.help "year to run"
          <> A.showDefault
          <> A.value 2021
      )
    <*> A.option
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
readInput Args {year, day} =
  let dayText = T.justifyRight 2 '0' $ T.pack $ show day
   in readFile $ "./data/" <> show year <> "/day" <> T.unpack dayText <> ".txt"

run2021 :: String -> Args -> IO ()
run2021 input = \case
  Args {day = 1, part = 1} -> print $ TwentyOne.Day01.part1 input
  Args {day = 1, part = 2} -> print $ TwentyOne.Day01.part2 input
  Args {day = 2, part = 1} -> print $ TwentyOne.Day02.part1 input
  Args {day = 2, part = 2} -> print $ TwentyOne.Day02.part2 input
  Args {day = 3, part = 1} -> print $ TwentyOne.Day03.part1 input
  Args {day = 3, part = 2} -> print $ TwentyOne.Day03.part2 input
  Args {day = 4, part = 1} -> print $ TwentyOne.Day04.part1 input
  Args {day = 4, part = 2} -> print $ TwentyOne.Day04.part2 input
  Args {day = 5, part = 1} -> print $ TwentyOne.Day05.part1 input
  Args {day = 5, part = 2} -> print $ TwentyOne.Day05.part2 input
  Args {day = 6, part = 1} -> print $ TwentyOne.Day06.part1 input
  Args {day = 6, part = 2} -> print $ TwentyOne.Day06.part2 input
  Args {day = 7, part = 1} -> print $ TwentyOne.Day07.part1 input
  Args {day = 7, part = 2} -> print $ TwentyOne.Day07.part2 input
  Args {day = 8, part = 1} -> print $ TwentyOne.Day08.part1 input
  Args {day = 8, part = 2} -> print $ TwentyOne.Day08.part2 input
  Args {day = 9, part = 1} -> print $ TwentyOne.Day09.part1 input
  Args {day = 9, part = 2} -> print $ TwentyOne.Day09.part2 input
  Args {day = 10, part = 1} -> print $ TwentyOne.Day10.part1 input
  Args {day = 10, part = 2} -> print $ TwentyOne.Day10.part2 input
  Args {day = 11, part = 1} -> print $ TwentyOne.Day11.part1 input
  Args {day = 11, part = 2} -> print $ TwentyOne.Day11.part2 input
  Args {day = 12, part = 1} -> print $ TwentyOne.Day12.part1 input
  Args {day = 12, part = 2} -> print $ TwentyOne.Day12.part2 input
  Args {day = 13, part = 1} -> print $ TwentyOne.Day13.part1 input
  Args {day = 13, part = 2} -> putStrLn $ TwentyOne.Day13.part2 input
  _ -> fail "unknown year/day/part"

runSolution :: String -> Args -> IO ()
runSolution input args =
  case args of
    Args {year = 2021} -> run2021 input args
    _ -> fail "unknown year/day/part"

main :: IO ()
main = do
  args <- A.execParser parseInfoArgs
  input <- readInput args

  runSolution input args

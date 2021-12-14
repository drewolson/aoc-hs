module Main where

import Aoc.Year2021.Day01 qualified as Year2021.Day01
import Aoc.Year2021.Day02 qualified as Year2021.Day02
import Aoc.Year2021.Day03 qualified as Year2021.Day03
import Aoc.Year2021.Day04 qualified as Year2021.Day04
import Aoc.Year2021.Day05 qualified as Year2021.Day05
import Aoc.Year2021.Day06 qualified as Year2021.Day06
import Aoc.Year2021.Day07 qualified as Year2021.Day07
import Aoc.Year2021.Day08 qualified as Year2021.Day08
import Aoc.Year2021.Day09 qualified as Year2021.Day09
import Aoc.Year2021.Day10 qualified as Year2021.Day10
import Aoc.Year2021.Day11 qualified as Year2021.Day11
import Aoc.Year2021.Day12 qualified as Year2021.Day12
import Aoc.Year2021.Day13 qualified as Year2021.Day13
import Aoc.Year2021.Day14 qualified as Year2021.Day14
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
          <> A.metavar "YEAR"
          <> A.help "year to run"
          <> A.showDefault
          <> A.value 2021
      )
    <*> A.option
      A.auto
      ( A.long "day"
          <> A.short 'd'
          <> A.metavar "DAY"
          <> A.help "day to run (1 - 25)"
      )
    <*> A.option
      A.auto
      ( A.long "part"
          <> A.short 'p'
          <> A.metavar "PART"
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
  Args {day = 01, part = 1} -> print $ Year2021.Day01.part1 input
  Args {day = 01, part = 2} -> print $ Year2021.Day01.part2 input
  Args {day = 02, part = 1} -> print $ Year2021.Day02.part1 input
  Args {day = 02, part = 2} -> print $ Year2021.Day02.part2 input
  Args {day = 03, part = 1} -> print $ Year2021.Day03.part1 input
  Args {day = 03, part = 2} -> print $ Year2021.Day03.part2 input
  Args {day = 04, part = 1} -> print $ Year2021.Day04.part1 input
  Args {day = 04, part = 2} -> print $ Year2021.Day04.part2 input
  Args {day = 05, part = 1} -> print $ Year2021.Day05.part1 input
  Args {day = 05, part = 2} -> print $ Year2021.Day05.part2 input
  Args {day = 06, part = 1} -> print $ Year2021.Day06.part1 input
  Args {day = 06, part = 2} -> print $ Year2021.Day06.part2 input
  Args {day = 07, part = 1} -> print $ Year2021.Day07.part1 input
  Args {day = 07, part = 2} -> print $ Year2021.Day07.part2 input
  Args {day = 08, part = 1} -> print $ Year2021.Day08.part1 input
  Args {day = 08, part = 2} -> print $ Year2021.Day08.part2 input
  Args {day = 09, part = 1} -> print $ Year2021.Day09.part1 input
  Args {day = 09, part = 2} -> print $ Year2021.Day09.part2 input
  Args {day = 10, part = 1} -> print $ Year2021.Day10.part1 input
  Args {day = 10, part = 2} -> print $ Year2021.Day10.part2 input
  Args {day = 11, part = 1} -> print $ Year2021.Day11.part1 input
  Args {day = 11, part = 2} -> print $ Year2021.Day11.part2 input
  Args {day = 12, part = 1} -> print $ Year2021.Day12.part1 input
  Args {day = 12, part = 2} -> print $ Year2021.Day12.part2 input
  Args {day = 13, part = 1} -> print $ Year2021.Day13.part1 input
  Args {day = 13, part = 2} -> putStrLn $ Year2021.Day13.part2 input
  Args {day = 14, part = 1} -> print $ Year2021.Day14.part1 input
  Args {day = 14, part = 2} -> print $ Year2021.Day14.part2 input
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

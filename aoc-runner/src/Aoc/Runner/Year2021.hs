module Aoc.Runner.Year2021 where

import Aoc.Runner.Args (Args (..), Args')
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
import Aoc.Year2021.Day15 qualified as Year2021.Day15
import Aoc.Year2021.Day16 qualified as Year2021.Day16
import Aoc.Year2021.Day17 qualified as Year2021.Day17
import Aoc.Year2021.Day18 qualified as Year2021.Day18
import Aoc.Year2021.Day19 qualified as Year2021.Day19
import Aoc.Year2021.Day20 qualified as Year2021.Day20
import Aoc.Year2021.Day21 qualified as Year2021.Day21
import Aoc.Year2021.Day22 qualified as Year2021.Day22

run :: String -> Args' -> IO ()
run input = \case
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
  Args {day = 15, part = 1} -> print $ Year2021.Day15.part1 input
  Args {day = 15, part = 2} -> print $ Year2021.Day15.part2 input
  Args {day = 16, part = 1} -> print $ Year2021.Day16.part1 input
  Args {day = 16, part = 2} -> print $ Year2021.Day16.part2 input
  Args {day = 17, part = 1} -> print $ Year2021.Day17.part1 input
  Args {day = 17, part = 2} -> print $ Year2021.Day17.part2 input
  Args {day = 18, part = 1} -> print $ Year2021.Day18.part1 input
  Args {day = 18, part = 2} -> print $ Year2021.Day18.part2 input
  Args {day = 19, part = 1} -> print $ Year2021.Day19.part1 input
  Args {day = 19, part = 2} -> print $ Year2021.Day19.part2 input
  Args {day = 20, part = 1} -> print $ Year2021.Day20.part1 input
  Args {day = 20, part = 2} -> print $ Year2021.Day20.part2 input
  Args {day = 21, part = 1} -> print $ Year2021.Day21.part1 3 10
  Args {day = 21, part = 2} -> print $ Year2021.Day21.part2 3 10
  Args {day = 22, part = 1} -> print $ Year2021.Day22.part1 input
  Args {day = 22, part = 2} -> print $ Year2021.Day22.part2 input
  _ -> fail "unknown day/part"

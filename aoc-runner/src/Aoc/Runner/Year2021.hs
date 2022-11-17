module Aoc.Runner.Year2021 where

import Aoc.Runner.Args (Args (..), Args')
import Aoc.Year2021.Day01 qualified as Day01
import Aoc.Year2021.Day02 qualified as Day02
import Aoc.Year2021.Day03 qualified as Day03
import Aoc.Year2021.Day04 qualified as Day04
import Aoc.Year2021.Day05 qualified as Day05
import Aoc.Year2021.Day06 qualified as Day06
import Aoc.Year2021.Day07 qualified as Day07
import Aoc.Year2021.Day08 qualified as Day08
import Aoc.Year2021.Day09 qualified as Day09
import Aoc.Year2021.Day10 qualified as Day10
import Aoc.Year2021.Day11 qualified as Day11
import Aoc.Year2021.Day12 qualified as Day12
import Aoc.Year2021.Day13 qualified as Day13
import Aoc.Year2021.Day14 qualified as Day14
import Aoc.Year2021.Day15 qualified as Day15
import Aoc.Year2021.Day16 qualified as Day16
import Aoc.Year2021.Day17 qualified as Day17
import Aoc.Year2021.Day18 qualified as Day18
import Aoc.Year2021.Day19 qualified as Day19
import Aoc.Year2021.Day20 qualified as Day20
import Aoc.Year2021.Day21 qualified as Day21
import Aoc.Year2021.Day22 qualified as Day22

run :: String -> Args' -> IO ()
run input args =
  case args of
    Args {day = 01, part = 1} -> print $ Day01.part1 input
    Args {day = 01, part = 2} -> print $ Day01.part2 input
    Args {day = 02, part = 1} -> print $ Day02.part1 input
    Args {day = 02, part = 2} -> print $ Day02.part2 input
    Args {day = 03, part = 1} -> print $ Day03.part1 input
    Args {day = 03, part = 2} -> print $ Day03.part2 input
    Args {day = 04, part = 1} -> print $ Day04.part1 input
    Args {day = 04, part = 2} -> print $ Day04.part2 input
    Args {day = 05, part = 1} -> print $ Day05.part1 input
    Args {day = 05, part = 2} -> print $ Day05.part2 input
    Args {day = 06, part = 1} -> print $ Day06.part1 input
    Args {day = 06, part = 2} -> print $ Day06.part2 input
    Args {day = 07, part = 1} -> print $ Day07.part1 input
    Args {day = 07, part = 2} -> print $ Day07.part2 input
    Args {day = 08, part = 1} -> print $ Day08.part1 input
    Args {day = 08, part = 2} -> print $ Day08.part2 input
    Args {day = 09, part = 1} -> print $ Day09.part1 input
    Args {day = 09, part = 2} -> print $ Day09.part2 input
    Args {day = 10, part = 1} -> print $ Day10.part1 input
    Args {day = 10, part = 2} -> print $ Day10.part2 input
    Args {day = 11, part = 1} -> print $ Day11.part1 input
    Args {day = 11, part = 2} -> print $ Day11.part2 input
    Args {day = 12, part = 1} -> print $ Day12.part1 input
    Args {day = 12, part = 2} -> print $ Day12.part2 input
    Args {day = 13, part = 1} -> print $ Day13.part1 input
    Args {day = 13, part = 2} -> putStrLn $ Day13.part2 input
    Args {day = 14, part = 1} -> print $ Day14.part1 input
    Args {day = 14, part = 2} -> print $ Day14.part2 input
    Args {day = 15, part = 1} -> print $ Day15.part1 input
    Args {day = 15, part = 2} -> print $ Day15.part2 input
    Args {day = 16, part = 1} -> print $ Day16.part1 input
    Args {day = 16, part = 2} -> print $ Day16.part2 input
    Args {day = 17, part = 1} -> print $ Day17.part1 input
    Args {day = 17, part = 2} -> print $ Day17.part2 input
    Args {day = 18, part = 1} -> print $ Day18.part1 input
    Args {day = 18, part = 2} -> print $ Day18.part2 input
    Args {day = 19, part = 1} -> print $ Day19.part1 input
    Args {day = 19, part = 2} -> print $ Day19.part2 input
    Args {day = 20, part = 1} -> print $ Day20.part1 input
    Args {day = 20, part = 2} -> print $ Day20.part2 input
    Args {day = 21, part = 1} -> print $ Day21.part1 3 10
    Args {day = 21, part = 2} -> print $ Day21.part2 3 10
    Args {day = 22, part = 1} -> print $ Day22.part1 input
    Args {day = 22, part = 2} -> print $ Day22.part2 input
    _ -> fail "unknown day/part"

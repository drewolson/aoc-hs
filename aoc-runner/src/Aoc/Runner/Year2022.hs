module Aoc.Runner.Year2022
  ( run,
  )
where

import Aoc.Runner.Args (Args (..), Args')
import Aoc.Year2022.Day01 qualified as Day01
import Aoc.Year2022.Day02 qualified as Day02
import Aoc.Year2022.Day03 qualified as Day03
import Aoc.Year2022.Day04 qualified as Day04
import Aoc.Year2022.Day05 qualified as Day05
import Aoc.Year2022.Day06 qualified as Day06
import Aoc.Year2022.Day07 qualified as Day07
import Aoc.Year2022.Day08 qualified as Day08

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
    _ -> fail "unknown day/part"

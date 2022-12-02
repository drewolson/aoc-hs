module Aoc.Runner.Year2022 where

import Aoc.Runner.Args (Args (..), Args')
import Aoc.Year2022.Day01 qualified as Day01
import Aoc.Year2022.Day02 qualified as Day02

run :: String -> Args' -> IO ()
run input args =
  case args of
    Args {day = 01, part = 1} -> print $ Day01.part1 input
    Args {day = 01, part = 2} -> print $ Day01.part2 input
    Args {day = 02, part = 1} -> print $ Day02.part1 input
    Args {day = 02, part = 2} -> print $ Day02.part2 input
    _ -> fail "unknown day/part"

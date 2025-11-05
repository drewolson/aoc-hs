module Aoc.Runner.Year2025
  ( run,
  )
where

import Aoc.Runner.Args (Args (..))
import Aoc.Year2025.Day01 qualified as Day01

run :: String -> Args -> IO ()
run input args =
  case args of
    Args {day = 01, part = 1} -> print $ Day01.part1 input
    Args {day = 01, part = 2} -> print $ Day01.part2 input
    _ -> fail "unknown day/part"

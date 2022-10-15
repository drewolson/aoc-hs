module Aoc.Runner.Year2022 where

import Aoc.Runner.Args (Args (..), Args')
import Aoc.Year2022.Day01 qualified as Day01

run :: String -> Args' -> IO ()
run input = \case
  Args {day = 01, part = 1} -> print $ Day01.part1 input
  _ -> fail "unknown day/part"

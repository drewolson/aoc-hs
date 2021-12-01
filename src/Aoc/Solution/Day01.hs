module Aoc.Solution.Day01
  ( part1,
    part2,
  )
where

import Data.List.Split (divvy)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

readAll :: String -> [Int]
readAll = mapMaybe readMaybe . lines

part1 :: String -> String
part1 input =
  let ints = readAll input
      diffs = uncurry (-) <$> zip ints (drop 1 ints)
   in show $ length $ filter (< 0) diffs

part2 :: String -> String
part2 input =
  let ints = readAll input
      sums = sum <$> divvy 3 1 ints
      diffs = uncurry (-) <$> zip sums (drop 1 sums)
   in show $ length $ filter (< 0) diffs

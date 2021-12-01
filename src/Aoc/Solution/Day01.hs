module Aoc.Solution.Day01
  ( part1,
    part2,
  )
where

import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

readAll :: String -> [Int]
readAll = mapMaybe readMaybe . lines

part1 :: String -> String
part1 input =
  let l :: [Int] = readAll input
      diffs = uncurry (-) <$> zip l (drop 1 l)
   in show $ length $ filter (< 0) diffs

part2 :: String -> String
part2 input =
  let l :: [Int] = readAll input
      sums = (\(a, b, c) -> a + b + c) <$> zip3 l (drop 1 l) (drop 2 l)
      diffs = uncurry (-) <$> zip sums (drop 1 sums)
   in show $ length $ filter (< 0) diffs

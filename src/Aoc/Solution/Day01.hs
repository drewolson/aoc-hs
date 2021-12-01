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

part1 :: String -> Int
part1 input =
  let ints = readAll input
   in length $ filter (uncurry (<)) $ zip ints (tail ints)

part2 :: String -> Int
part2 input =
  let sums = fmap sum $ divvy 3 1 $ readAll input
   in length $ filter (uncurry (<)) $ zip sums (tail sums)

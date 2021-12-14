module Aoc.Year2021.Day01
  ( part1,
    part2,
  )
where

import Data.List.Split (divvy)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

readAll :: String -> [Int]
readAll = mapMaybe readMaybe . lines

numIncreasing :: [Int] -> Int
numIncreasing l = length $ filter (uncurry (<)) $ zip l (tail l)

part1 :: String -> Int
part1 = numIncreasing . readAll

part2 :: String -> Int
part2 = numIncreasing . fmap sum . divvy 3 1 . readAll

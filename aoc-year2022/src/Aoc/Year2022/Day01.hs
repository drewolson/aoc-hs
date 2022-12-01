module Aoc.Year2022.Day01
  ( part1,
    part2,
  )
where

import Data.List (sortOn)
import Data.List.Split (splitOn)
import Data.Ord (Down (..))

part1 :: String -> Int
part1 = maximum . fmap (sum . fmap read . lines) . splitOn "\n\n"

part2 :: String -> Int
part2 = sum . take 3 . sortOn Down . fmap (sum . fmap read . lines) . splitOn "\n\n"

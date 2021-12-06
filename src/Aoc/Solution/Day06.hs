module Aoc.Solution.Day06
  ( part1,
    part2,
  )
where

import Data.List.Split (splitOn)
import Data.MultiSet (MultiSet)
import Data.MultiSet qualified as MS

parseInput :: String -> [Int]
parseInput = fmap read . splitOn "," . head . lines

simulate :: MultiSet Int -> MultiSet Int
simulate = MS.concatMap decrease
  where
    decrease :: Int -> [Int]
    decrease 0 = [6, 8]
    decrease n = [n - 1]

part1 :: String -> Int
part1 = sum . MS.toMap . (!! 80) . iterate simulate . MS.fromList . parseInput

part2 :: String -> Int
part2 = sum . MS.toMap . (!! 256) . iterate simulate . MS.fromList . parseInput

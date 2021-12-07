module Aoc.Solution.Day07
  ( part1,
    part2,
  )
where

import Data.List.Split (splitOn)

parseInput :: String -> [Int]
parseInput = fmap read . splitOn "," . head . lines

diffs :: [Int] -> [Int]
diffs ints = diff <$> ints
  where
    diff :: Int -> Int
    diff int = sum $ fmap (abs . (int -)) ints

part1 :: String -> Int
part1 = minimum . diffs . parseInput

diffs' :: [Int] -> [Int]
diffs' ints = diff <$> [minimum ints .. maximum ints]
  where
    diff :: Int -> Int
    diff int = sum $ fmap (sum . (`take` [1 ..]) . abs . (int -)) ints

part2 :: String -> Int
part2 = minimum . diffs' . parseInput

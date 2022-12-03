module Aoc.Year2022.Day03
  ( part1,
    part2,
  )
where

import Data.List (foldl1')
import Data.List.Split (chunksOf)
import Data.Map.Strict (Map, (!))
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

priors :: Map Char Int
priors = Map.fromList $ zip (['a' .. 'z'] <> ['A' .. 'Z']) [1 ..]

split :: String -> (String, String)
split s = splitAt (length s `div` 2) s

collapse :: (String, String) -> Set Char
collapse (a, b) = Set.intersection (Set.fromList a) (Set.fromList b)

intersections :: Ord a => [Set a] -> Set a
intersections = foldl1' Set.intersection

part1 :: String -> Int
part1 = sum . fmap (sum . Set.map (priors !) . collapse . split) . lines

part2 :: String -> Int
part2 = sum . fmap (sum . Set.map (priors !) . intersections) . chunksOf 3 . fmap Set.fromList . lines

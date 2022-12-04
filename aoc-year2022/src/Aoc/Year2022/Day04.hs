module Aoc.Year2022.Day04
  ( part1,
    part2,
  )
where

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

overlapped :: (Set Int, Set Int) -> Bool
overlapped (a, b) = Set.isSubsetOf a b || Set.isSubsetOf b a

overlapped' :: (Set Int, Set Int) -> Bool
overlapped' (a, b) = not $ Set.null $ Set.intersection a b

makeSet :: String -> Maybe (Set Int)
makeSet range =
  case splitOn "-" range of
    [a, b] -> Just $ Set.fromList [read a .. read b]
    _ -> Nothing

parse :: String -> Maybe (Set Int, Set Int)
parse line =
  case splitOn "," line of
    [a, b] -> (,) <$> makeSet a <*> makeSet b
    _ -> Nothing

part1 :: String -> Int
part1 = length . filter overlapped . mapMaybe parse . lines

part2 :: String -> Int
part2 = length . filter overlapped' . mapMaybe parse . lines

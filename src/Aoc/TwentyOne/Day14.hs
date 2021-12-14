module Aoc.TwentyOne.Day14
  ( part1,
    part2,
  )
where

import Data.Bifunctor (Bifunctor (second))
import Data.List.Split (divvy, splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.MultiSet (MultiSet)
import Data.MultiSet qualified as MultiSet

type Mapping = Map String [String]

type Poly = MultiSet String

type Instructions = (Poly, Mapping, Char, Char)

parseInput :: String -> Instructions
parseInput input =
  let parts = splitOn "\n\n" input
      set = MultiSet.fromList $ divvy 2 1 $ head parts
      mapping = Map.fromList $ mapMaybe toTuple $ lines $ head $ tail parts
   in (set, mapping, head $ head parts, last $ head parts)
  where
    toTuple :: String -> Maybe (String, [String])
    toTuple line =
      case splitOn " -> " line of
        [[a, b], [c]] -> Just ([a, b], [[a, c], [c, b]])
        _ -> Nothing

evolve :: Mapping -> Poly -> Poly
evolve mapping = MultiSet.concatMap (flip (Map.findWithDefault []) mapping)

score :: Char -> Char -> Poly -> Int
score start end poly =
  let occurs = MultiSet.toOccurList $ MultiSet.concatMap split poly
      totals = fmap (incStartEnd . second (`div` 2)) occurs
   in maximum totals - minimum totals
  where
    incStartEnd :: (Char, Int) -> Int
    incStartEnd (c, n)
      | c `elem` [start, end] = n + 1
      | otherwise = n

    split :: String -> [Char]
    split [a, b] = [a, b]
    split _ = []

part1 :: String -> Int
part1 input =
  let (set, mapping, start, end) = parseInput input
   in score start end $ (!! 10) $ iterate (evolve mapping) set

part2 :: String -> Int
part2 input =
  let (set, mapping, start, end) = parseInput input
   in score start end $ (!! 40) $ iterate (evolve mapping) set

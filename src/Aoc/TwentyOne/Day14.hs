module Aoc.TwentyOne.Day14
  ( part1,
    part2,
  )
where

import Data.Bifunctor (Bifunctor (second))
import Data.List.Split (divvy, splitOn)
import Data.Map (Map, (!))
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
      mapping = Map.fromList $ mapMaybe (toTuple . splitOn " -> ") $ lines $ last parts
   in (set, mapping, head $ head parts, last $ head parts)
  where
    toTuple :: [String] -> Maybe (String, [String])
    toTuple [[l, r], [m]] = Just ([l, r], [[l, m], [m, r]])
    toTuple _ = Nothing

evolve :: Mapping -> Poly -> Poly
evolve = MultiSet.concatMap . (!)

score :: Char -> Char -> Poly -> Int
score start end poly =
  let occurs = MultiSet.toOccurList $ MultiSet.concatMap id poly
      totals = fmap (incStartEnd . second (`div` 2)) occurs
   in maximum totals - minimum totals
  where
    incStartEnd :: (Char, Int) -> Int
    incStartEnd (c, n)
      | c `elem` [start, end] = n + 1
      | otherwise = n

scoreDay :: Int -> String -> Int
scoreDay n input =
  let (set, mapping, start, end) = parseInput input
   in score start end $ (!! n) $ iterate (evolve mapping) set

part1 :: String -> Int
part1 = scoreDay 10

part2 :: String -> Int
part2 = scoreDay 40

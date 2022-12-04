{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Aoc.Year2022.Day04
  ( part1,
    part2,
  )
where

import Data.List.Split (splitOn)
import Data.Set (Set)
import Data.Set qualified as Set

overlapped :: (Set Int, Set Int) -> Bool
overlapped (a, b) = Set.isSubsetOf a b || Set.isSubsetOf b a

overlapped' :: (Set Int, Set Int) -> Bool
overlapped' (a, b) = not $ Set.null $ Set.intersection a b

makeSet :: String -> Set Int
makeSet range =
  let [a, b] = splitOn "-" range
   in Set.fromList [read a .. read b]

parse :: String -> (Set Int, Set Int)
parse line =
  let [a, b] = splitOn "," line
   in (makeSet a, makeSet b)

part1 :: String -> Int
part1 = length . filter overlapped . fmap parse . lines

part2 :: String -> Int
part2 = length . filter overlapped' . fmap parse . lines

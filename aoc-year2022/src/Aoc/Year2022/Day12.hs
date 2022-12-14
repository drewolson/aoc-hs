module Aoc.Year2022.Day12 where

import Algorithm.Search (bfs)
import Aoc.Parallel (pmap)
import Data.Char (ord)
import Data.Foldable (find)
import Data.Map.Strict (Map, (!))
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)

type Coord = (Int, Int)

type Graph = Map Coord Char

parseInput :: String -> Graph
parseInput = Map.fromList . foldMap makeRow . zip [0 ..] . lines
  where
    makeRow :: (Int, String) -> [((Int, Int), Char)]
    makeRow (y, l) = (\(x, c) -> ((x, y), c)) <$> zip [0 ..] l

shortestPath :: Graph -> Coord -> Coord -> Maybe [Coord]
shortestPath graph end = bfs neighbors (== end)
  where
    neighbors :: Coord -> [Coord]
    neighbors s@(x, y) =
      [d | d <- [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)], reachable s d]

    reachable :: Coord -> Coord -> Bool
    reachable s d
      | Map.notMember d graph = False
      | otherwise = canClimb (graph ! s) (graph ! d)

    canClimb :: Char -> Char -> Bool
    canClimb s d = asInt d - asInt s <= 1

    asInt :: Char -> Int
    asInt 'S' = asInt 'a'
    asInt 'E' = asInt 'z'
    asInt c = ord c

part1 :: String -> Maybe Int
part1 input = do
  let graph = parseInput input
  let items = Map.toList graph
  (start, _) <- find ((== 'S') . snd) items
  (end, _) <- find ((== 'E') . snd) items

  length <$> shortestPath graph end start

part2 :: String -> Maybe Int
part2 input = do
  let graph = parseInput input
  let items = Map.toList graph
  let starts = fst <$> filter ((== 'a') . snd) items
  (end, _) <- find ((== 'E') . snd) items

  pure $ minimum $ fmap length $ catMaybes $ pmap (shortestPath graph end) starts

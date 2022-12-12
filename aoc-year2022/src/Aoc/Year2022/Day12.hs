module Aoc.Year2022.Day12 where

import Algorithm.Search (dijkstra)
import Data.Char (ord)
import Data.Foldable (find)
import Data.Map.Strict (Map, (!))
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)

type Coord = (Int, Int)

type Graph = Map Coord Char

parseInput :: String -> Graph
parseInput = Map.fromList . foldMap makeRow . zip [0 ..] . lines
  where
    makeRow :: (Int, String) -> [((Int, Int), Char)]
    makeRow (y, l) = (\(x, c) -> ((x, y), c)) <$> zip [0 ..] l

shortestPath :: Graph -> Coord -> Coord -> Maybe (Int, [Coord])
shortestPath graph end = dijkstra neighbors (\_ _ -> 1) (== end)
  where
    neighbors :: Coord -> [Coord]
    neighbors s@(x, y) =
      [d | d <- [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)], reachable s d]

    reachable :: Coord -> Coord -> Bool
    reachable sc dc
      | Map.notMember dc graph = False
      | otherwise =
        let s = graph ! sc
            d = graph ! dc
         in canClimb s d

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

  fst <$> shortestPath graph end start

part2 :: String -> Maybe Int
part2 input = do
  let graph = parseInput input
  let items = Map.toList graph
  (end, _) <- find ((== 'E') . snd) items
  let starts = fst <$> filter ((== 'a') . snd) items

  pure $ minimum $ fst <$> mapMaybe (shortestPath graph end) starts

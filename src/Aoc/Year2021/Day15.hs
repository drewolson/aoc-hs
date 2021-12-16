module Aoc.Year2021.Day15
  ( part1,
    part2,
  )
where

import Algorithm.Search (dijkstra, pruning)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

type Coord = (Int, Int)

type Grid = Map Coord Int

parseInput :: String -> [[Int]]
parseInput = fmap (fmap (read . pure)) . lines

expand :: [[Int]] -> [[Int]]
expand ints = foldMap makeRow [0 .. 4]
  where
    makeRow :: Int -> [[Int]]
    makeRow n =
      let squares = fmap (makeSquare n) [0 .. 4]
       in fmap (\i -> foldMap (!! i) squares) [0 .. length (head squares) - 1]

    makeSquare :: Int -> Int -> [[Int]]
    makeSquare n m = fmap (fmap (bump n m)) ints

    bump :: Int -> Int -> Int -> Int
    bump n m i =
      case n + m + i of
        t | t > 9 -> t - 9
        t -> t

makeGrid :: [[Int]] -> Grid
makeGrid = Map.fromList . mconcat . zipWith makeRow [0 ..]
  where
    makeRow :: Int -> [Int] -> [(Coord, Int)]
    makeRow y = zipWith (\x n -> ((x, y), n)) [0 ..]

solve :: Grid -> Maybe Int
solve grid =
  let goal = maximum $ Map.keys grid
   in fst <$> dijkstra (neighbors `pruning` invalid) cost (== goal) (0, 0)
  where
    invalid :: Coord -> Bool
    invalid = (`Map.notMember` grid)

    neighbors :: Coord -> [Coord]
    neighbors (x, y) =
      [ (x, y + 1),
        (x, y - 1),
        (x + 1, y),
        (x - 1, y)
      ]

    cost :: Coord -> Coord -> Int
    cost _ c = Map.findWithDefault maxBound c grid

part1 :: String -> Maybe Int
part1 = solve . makeGrid . parseInput

part2 :: String -> Maybe Int
part2 = solve . makeGrid . expand . parseInput

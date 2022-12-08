module Aoc.Year2022.Day08 where

import Data.Map.Strict (Map, (!))
import Data.Map.Strict qualified as Map

type Grid = Map (Int, Int) Int

makeGrid :: String -> Grid
makeGrid = Map.fromList . foldMap makeRow . zip [0 ..] . lines
  where
    makeRow :: (Int, String) -> [((Int, Int), Int)]
    makeRow (y, l) = (\(x, c) -> ((x, y), read $ pure c)) <$> zip [0 ..] l

gridSize :: Grid -> (Int, Int)
gridSize g =
  let keys = Map.keys g
   in (maximum $ fmap fst keys, maximum $ fmap snd keys)

rays :: (Int, Int) -> (Int, Int) -> [[(Int, Int)]]
rays (maxX, maxY) (x, y) =
  [ zip [x - 1, x - 2 .. 0] (repeat y),
    zip [x + 1 .. maxX] (repeat y),
    zip (repeat x) [y - 1, y - 2 .. 0],
    zip (repeat x) [y + 1 .. maxY]
  ]

visible :: Grid -> ((Int, Int), Int) -> Bool
visible grid (coord, n) =
  any (all ((< n) . (grid !))) $ rays (gridSize grid) coord

scenicScore :: Grid -> ((Int, Int), Int) -> Int
scenicScore grid (coord, n) =
  product $ numLower <$> rays (gridSize grid) coord
  where
    numLower :: [(Int, Int)] -> Int
    numLower [] = 0
    numLower (h : t)
      | grid ! h < n = 1 + numLower t
      | otherwise = 1

part1 :: String -> Int
part1 input =
  let grid = makeGrid input
   in length $ filter (visible grid) $ Map.toList grid

part2 :: String -> Int
part2 input =
  let grid = makeGrid input
   in maximum $ scenicScore grid <$> Map.toList grid

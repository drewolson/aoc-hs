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

visible :: Int -> Int -> Grid -> ((Int, Int), Int) -> Bool
visible maxX maxY grid ((x, y), n) =
  any
    (all ((< n) . (grid !)))
    [ zip [0 .. x - 1] (repeat y),
      zip [x + 1 .. maxX] (repeat y),
      zip (repeat x) [0 .. y - 1],
      zip (repeat x) [y + 1 .. maxY]
    ]

scenicScore :: Int -> Int -> Grid -> ((Int, Int), Int) -> Int
scenicScore maxX maxY grid ((x, y), n) =
  product $
    fmap
      numLower
      [ zip [x - 1, x - 2 .. 0] (repeat y),
        zip [x + 1 .. maxX] (repeat y),
        zip (repeat x) [y - 1, y - 2 .. 0],
        zip (repeat x) [y + 1 .. maxY]
      ]
  where
    numLower :: [(Int, Int)] -> Int
    numLower [] = 0
    numLower (h : t)
      | grid ! h < n = 1 + numLower t
      | otherwise = 1

part1 :: String -> Int
part1 input =
  let grid = makeGrid input
      (x, y) = gridSize grid
   in length $ filter (visible x y grid) $ Map.toList grid

part2 :: String -> Int
part2 input =
  let grid = makeGrid input
      (x, y) = gridSize grid
   in maximum $ scenicScore x y grid <$> Map.toList grid

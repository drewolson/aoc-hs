module Aoc.Solution.TwentyOne.Day11
  ( part1,
    part2,
  )
where

import Control.Monad (guard)
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable (mapAccumL)

type Coord = (Int, Int)

type Grid = Map Coord Int

parseInput :: String -> [[Int]]
parseInput = fmap (fmap (read . pure)) . lines

makeGrid :: [[Int]] -> Grid
makeGrid = Map.fromList . mconcat . zipWith makeRow [0 ..]
  where
    makeRow :: Int -> [Int] -> [(Coord, Int)]
    makeRow y = zipWith (\x n -> ((x, y), n)) [0 ..]

neighbors :: Coord -> [Coord]
neighbors (x, y) = do
  x' <- [x - 1, x, x + 1]
  y' <- [y - 1, y, y + 1]

  guard $ (x, y) /= (x', y')

  pure (x', y')

resetFlash :: Int -> Int
resetFlash n
  | n > 9 = 0
  | otherwise = n

propagate :: Grid -> Set Coord -> [Coord] -> (Grid, Int)
propagate grid flashed [] = (Map.map resetFlash grid, length flashed)
propagate grid flashed (h : t) =
  let validNeighbors = filter (`notElem` flashed) $ neighbors h
      grid' = foldl' (flip $ Map.adjust (+ 1)) grid validNeighbors
      newFlashes = filter ((> 9) . flip (Map.findWithDefault 0) grid') validNeighbors
      flashed' = Set.union flashed $ Set.fromList newFlashes
   in propagate grid' flashed' (newFlashes <> t)

step :: Grid -> Int -> (Grid, Int)
step grid =
  let grid' = fmap (+ 1) grid
      flashed = fmap fst $ filter ((> 9) . snd) $ Map.assocs grid'
   in const $ propagate grid' (Set.fromList flashed) flashed

part1 :: String -> Int
part1 input =
  let grid = makeGrid $ parseInput input
   in sum $ snd $ mapAccumL step grid [1 .. 100]

part2 :: String -> Int
part2 input =
  let grid = makeGrid $ parseInput input
      size = length grid
   in (+ 1) $ length $ takeWhile (< size) $ snd $ mapAccumL step grid [1 ..]

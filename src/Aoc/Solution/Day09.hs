module Aoc.Solution.Day09
  ( part1,
    part2,
  )
where

import Data.List (sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set

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
neighbors (x, y) =
  [ (x + 1, y),
    (x - 1, y),
    (x, y + 1),
    (x, y - 1)
  ]

findNeighbors :: Grid -> Coord -> [(Coord, Int)]
findNeighbors grid = mapMaybe (\c -> (c,) <$> Map.lookup c grid) . neighbors

isLowPoint :: Grid -> Coord -> Int -> Bool
isLowPoint grid coord n = all (> n) $ snd <$> findNeighbors grid coord

lowPoints :: Grid -> Grid
lowPoints grid = Map.filterWithKey (isLowPoint grid) grid

part1 :: String -> Int
part1 input =
  let grid = makeGrid $ parseInput input
   in sum $ (+ 1) <$> lowPoints grid

basinSize :: Grid -> (Coord, Int) -> Int
basinSize grid = basinSize' Set.empty 0 . pure
  where
    validNeighbor :: Set Coord -> Int -> (Coord, Int) -> Bool
    validNeighbor seen val (c, v) = c `notElem` seen && v < 9 && v > val

    validNeighbors :: Set Coord -> Int -> Coord -> [(Coord, Int)]
    validNeighbors seen val =
      filter (validNeighbor seen val) . findNeighbors grid

    basinSize' :: Set Coord -> Int -> [(Coord, Int)] -> Int
    basinSize' _ acc [] = acc
    basinSize' seen acc ((coord, val) : r)
      | coord `elem` seen = basinSize' seen acc r
      | otherwise =
        let seen' = Set.insert coord seen
            n = validNeighbors seen' val coord
         in basinSize' seen' (acc + 1) (n <> r)

part2 :: String -> Int
part2 input =
  let grid = makeGrid $ parseInput input
      points = Map.toList $ lowPoints grid
   in product $ take 3 $ sortOn Down $ basinSize grid <$> points

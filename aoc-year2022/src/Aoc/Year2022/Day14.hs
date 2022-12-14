module Aoc.Year2022.Day14 where

import Aoc.Parser (Parser, runParser')
import Data.List (find)
import Data.Set (Set)
import Data.Set qualified as Set
import Text.Megaparsec (sepBy1, sepEndBy1)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Coord = (Int, Int)

type Grid = Set Coord

parseInput :: String -> [(Coord, Coord)]
parseInput = runParser' $ mconcat <$> sepEndBy1 coordsP newline
  where
    coordsP :: Parser [(Coord, Coord)]
    coordsP = do
      coords <- sepBy1 coordP (string " -> ")
      pure $ zip coords (tail coords)

    coordP :: Parser Coord
    coordP = (,) <$> (decimal <* char ',') <*> decimal

buildGrid :: [(Coord, Coord)] -> Grid
buildGrid = Set.fromList . foldMap expand
  where
    expand :: (Coord, Coord) -> [Coord]
    expand ((ax, ay), (bx, by))
      | ax /= bx = (,by) <$> [min ax bx .. max ax bx]
      | otherwise = (ax,) <$> [min ay by .. max ay by]

dropSand :: Int -> Grid -> Grid
dropSand maxY grid = Set.insert (fall (500, 0)) grid
  where
    next :: Coord -> [Coord]
    next (x, y) = [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]

    freeSlot :: Coord -> Bool
    freeSlot c@(_, y) = Set.notMember c grid && y < maxY

    fall :: Coord -> Coord
    fall c =
      case find freeSlot $ next c of
        Nothing -> c
        (Just n) -> fall n

part1 :: String -> Int
part1 input =
  let grid = buildGrid $ parseInput input
      maxY = 2 + maximum (Set.map snd grid)
   in pred $ length $ takeWhile (Set.notMember (maxY - 1) . Set.map snd) $ iterate (dropSand maxY) grid

part2 :: String -> Int
part2 input =
  let grid = buildGrid $ parseInput input
      maxY = 2 + maximum (Set.map snd grid)
   in length $ takeWhile (Set.notMember (500, 0)) $ iterate (dropSand maxY) grid

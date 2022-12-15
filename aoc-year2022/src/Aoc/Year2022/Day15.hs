module Aoc.Year2022.Day15 where

import Aoc.Parser (Parser, runParser')
import Data.List (find, genericLength)
import Data.Set qualified as Set
import Text.Megaparsec (sepEndBy1)
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)

type Interval = (Integer, Integer)

type Coord = (Integer, Integer)

type Pairs = [(Coord, Coord)]

parseInput :: String -> Pairs
parseInput = runParser' $ sepEndBy1 pairP newline
  where
    signedP :: Parser Integer
    signedP = signed (pure ()) decimal

    pairP :: Parser (Coord, Coord)
    pairP = do
      s <- (,) <$> (string "Sensor at x=" *> signedP) <*> (string ", y=" *> signedP <* string ": ")
      b <- (,) <$> (string "closest beacon is at x=" *> signedP) <*> (string ", y=" *> signedP)
      pure (s, b)

dist :: Coord -> Coord -> Integer
dist (xa, ya) (xb, yb) = abs (xa - xb) + abs (ya - yb)

findMinMaxX :: Pairs -> (Integer, Integer)
findMinMaxX pairs =
  let ends = foldMap xRange pairs
   in (minimum ends, maximum ends)
  where
    xRange :: (Coord, Coord) -> [Integer]
    xRange (s@(x, _), b) = [x - dist s b, x + dist s b]

isCovered :: Pairs -> Coord -> Bool
isCovered pairs coord = any (covered coord) pairs
  where
    covered :: Coord -> (Coord, Coord) -> Bool
    covered c (a, b) = dist a c <= dist a b

beaconsIn :: Integer -> Pairs -> Integer
beaconsIn y = genericLength . Set.toList . Set.filter ((== y) . snd) . Set.fromList . fmap snd

perimeter :: (Coord, Coord) -> [Coord]
perimeter (s@(sx, sy), b) =
  let d = dist s b
      pairs = zip [sx - d - 1 .. sx + d + 1] ([0 .. d + 1] <> [d, d - 1 .. 0])
   in foldMap (\(x, dy) -> [(x, sy + dy), (x, sy - dy)]) pairs

inBounds :: Integer -> Coord -> Bool
inBounds size (x, y) = x >= 0 && x <= size && y >= 0 && y <= size

part1 :: Integer -> String -> Integer
part1 y input =
  let pairs = parseInput input
      (minx, maxx) = findMinMaxX pairs
      count = genericLength [x | x <- [minx .. maxx], isCovered pairs (x, y)]
      beaconCount = beaconsIn y pairs
   in count - beaconCount

part2 :: Integer -> String -> Maybe Integer
part2 size input = do
  let pairs = parseInput input
  let candidates = filter (inBounds size) $ foldMap perimeter pairs
  (x, y) <- find (not . isCovered pairs) candidates

  Just $ x * 4000000 + y

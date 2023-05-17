module Aoc.Year2021.Day19
  ( part1,
    part2,
  )
where

import Aoc.Parser (Parser, runParser', signedIntP)
import Control.Monad (guard)
import Data.List qualified as List
import Data.Set qualified as Set
import Text.Megaparsec (sepEndBy1)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Coord = (Int, Int, Int)

type Scanner = [Coord]

parseCoord :: Parser Coord
parseCoord =
  (,,)
    <$> signedIntP
    <* char ','
    <*> signedIntP
    <* char ','
    <*> signedIntP

parseCoords :: Parser [Coord]
parseCoords = sepEndBy1 parseCoord newline

parseScannerNum :: Parser Int
parseScannerNum = decimal

parseScanner :: Parser Scanner
parseScanner =
  string "--- scanner " *> parseScannerNum *> string " ---" *> newline *> parseCoords

parseScanners :: Parser [Scanner]
parseScanners = sepEndBy1 parseScanner newline

parseInput :: String -> [Scanner]
parseInput = runParser' parseScanners

axisChanges :: [Coord -> Coord]
axisChanges =
  [ id,
    \(x, y, z) -> (-x, y, -z),
    \(x, y, z) -> (-z, y, x),
    \(x, y, z) -> (z, y, -x),
    \(x, y, z) -> (y, -x, z),
    \(x, y, z) -> (-y, x, z)
  ]

rotations :: [Coord -> Coord]
rotations =
  [ id,
    \(x, y, z) -> (x, z, -y),
    \(x, y, z) -> (x, -y, -z),
    \(x, y, z) -> (x, -z, y)
  ]

variantFunctions :: [Coord -> Coord]
variantFunctions = do
  a <- axisChanges
  b <- rotations

  pure (a . b)

variants :: Scanner -> [Scanner]
variants scanner = fmap (`fmap` scanner) variantFunctions

delta :: Int -> Int -> Int
delta n x
  | n < x = -(x - n)
  | otherwise = n - x

makeDiff :: Coord -> Coord -> Coord -> Coord
makeDiff (nx, ny, nz) (x, y, z) (a, b, c) =
  (a + delta nx x, b + delta ny y, c + delta nz z)

findMatches :: [Scanner] -> [Scanner] -> [(Scanner, Scanner, Coord)]
findMatches normalized scanners = do
  normal <- normalized
  scanner <- scanners
  variant <- variants scanner

  normalPos <- normal
  pos <- variant

  let diff = makeDiff normalPos pos
  let normalizedScanner = fmap diff variant
  let intersect = normal `List.intersect` normalizedScanner

  guard $ length intersect >= 12

  pure (normalizedScanner, scanner, diff (0, 0, 0))

normalize :: [Coord] -> [Scanner] -> [Scanner] -> ([Coord], [Scanner])
normalize positions normalized [] = (positions, normalized)
normalize positions normalized scanners =
  let (normal, match, position) = head $ findMatches normalized scanners
   in normalize (position : positions) (normal : normalized) (List.delete match scanners)

normalizeScanners :: [Scanner] -> ([Coord], [Scanner])
normalizeScanners scanners =
  normalize [(0, 0, 0)] (take 1 scanners) (tail scanners)

permutations :: [Coord] -> [(Coord, Coord)]
permutations coords = do
  a <- coords
  b <- coords

  guard $ a /= b

  pure (a, b)

manhattanDistance :: (Coord, Coord) -> Int
manhattanDistance ((x1, y1, z1), (x2, y2, z2)) =
  abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

part1 :: String -> Int
part1 input =
  let (_, normalized) = normalizeScanners $ parseInput input
   in length $ Set.unions $ Set.fromList <$> normalized

part2 :: String -> Int
part2 input =
  let (positions, _) = normalizeScanners $ parseInput input
   in maximum $ manhattanDistance <$> permutations positions

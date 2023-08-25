module Aoc.Year2021.Day05
  ( part1,
    part2,
  )
where

import Aoc.Core.Parser (Parser, runParser)
import Data.Map qualified as M
import Data.MultiSet (MultiSet)
import Data.MultiSet qualified as MS
import Text.Megaparsec (sepEndBy1)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Point = (Int, Int)

type Pair = (Point, Point)

parsePoint :: Parser Point
parsePoint = (,) <$> decimal <*> (char ',' *> decimal)

parsePair :: Parser Pair
parsePair = (,) <$> parsePoint <*> (string " -> " *> parsePoint)

parsePairs :: Parser [Pair]
parsePairs = sepEndBy1 parsePair newline

parseInput :: String -> Either String [Pair]
parseInput = runParser parsePairs

isStraight :: Pair -> Bool
isStraight ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

expandPair :: Pair -> [Point]
expandPair ((x1, y1), (x2, y2)) = zip (ints x1 x2) (ints y1 y2)
  where
    ints :: Int -> Int -> [Int]
    ints a b
      | a == b = repeat a
      | a > b = [a, (a - 1) .. b]
      | otherwise = [a .. b]

toLine :: Pair -> MultiSet Point
toLine = MS.fromList . expandPair

part2 :: String -> Either String Int
part2 =
  fmap (length . M.filter (> 1) . MS.toMap . foldMap toLine) . parseInput

part1 :: String -> Either String Int
part1 =
  fmap (length . M.filter (> 1) . MS.toMap . foldMap toLine . filter isStraight) . parseInput

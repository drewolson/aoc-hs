module Aoc.Year2021.Day13
  ( part1,
    part2,
  )
where

import Aoc.Parser (Parser, intP, runParser)
import Data.Bifunctor (first, second)
import Data.Foldable (Foldable (foldl'))
import Data.List (intercalate)
import Data.Set (Set)
import Data.Set qualified as Set
import Text.Megaparsec (choice, sepEndBy1)
import Text.Megaparsec.Char (char, newline, string)

type Coord = (Int, Int)

type Paper = Set Coord

data Fold
  = X Int
  | Y Int

type Instructions = (Paper, [Fold])

parseInstructions :: Parser Instructions
parseInstructions = (,) <$> parseCoords <*> (newline *> parseFolds)
  where
    parseDir :: Parser (Int -> Fold)
    parseDir = choice [X <$ char 'x', Y <$ char 'y']

    parseFold :: Parser Fold
    parseFold = (string "fold along " *> parseDir) <*> (char '=' *> intP)

    parseFolds :: Parser [Fold]
    parseFolds = sepEndBy1 parseFold newline

    parseCoord :: Parser Coord
    parseCoord = (,) <$> intP <*> (char ',' *> intP)

    parseCoords :: Parser (Set Coord)
    parseCoords = Set.fromList <$> sepEndBy1 parseCoord newline

foldPaper :: Paper -> Fold -> Paper
foldPaper paper fold = Set.map (foldCoord fold) paper
  where
    foldCoord :: Fold -> Coord -> Coord
    foldCoord (X n) = first (foldPoint n)
    foldCoord (Y n) = second (foldPoint n)

    foldPoint :: Int -> Int -> Int
    foldPoint n p
      | p < n = p
      | otherwise = n - (p - n)

part1 :: String -> Either String Int
part1 input = do
  (coords, folds) <- runParser parseInstructions input

  pure $ length $ foldl' foldPaper coords $ take 1 folds

showGrid :: Paper -> String
showGrid paper =
  let points = Set.toList paper
      max_x = maximum $ fst <$> points
      max_y = maximum $ snd <$> points
   in intercalate "\n" $ fmap (showRow max_x) [0 .. max_y]
  where
    showRow :: Int -> Int -> String
    showRow max_x y = foldMap (`showCoord` y) [0 .. max_x]

    showCoord :: Int -> Int -> String
    showCoord x y
      | (x, y) `elem` paper = "#"
      | otherwise = " "

part2 :: String -> String
part2 input =
  case runParser parseInstructions input of
    Left e -> e
    Right (coords, folds) ->
      showGrid $ foldl' foldPaper coords folds

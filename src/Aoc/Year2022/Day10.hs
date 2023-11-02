module Aoc.Year2022.Day10
  ( part1,
    part2,
  )
where

import Aoc.Core.Parser (Parser, runParser, signedIntP)
import Data.Function ((&))
import Data.List.Split (chunksOf)
import Text.Megaparsec (sepEndBy1, (<|>))
import Text.Megaparsec.Char (newline, string)

parseInput :: String -> [Int -> Int]
parseInput = runParser $ mconcat <$> sepEndBy1 cmdP newline
  where
    cmdP :: Parser [Int -> Int]
    cmdP = noopP <|> addxP

    noopP :: Parser [Int -> Int]
    noopP = [id] <$ string "noop"

    addxP :: Parser [Int -> Int]
    addxP = do
      n <- string "addx " *> signedIntP
      pure [id, (+ n)]

draw :: (Int, Int) -> Char
draw (c, i)
  | abs (c - i) <= 1 = '#'
  | otherwise = '.'

part1 :: String -> Int
part1 =
  sum
    . fmap (uncurry (*) . head)
    . chunksOf 40
    . drop 19
    . zip [1 ..]
    . scanl (&) 1
    . parseInput

part2 :: String -> String
part2 =
  unlines
    . take 6
    . chunksOf 40
    . fmap draw
    . zip ((`mod` 40) <$> [0 ..])
    . scanl (&) 1
    . parseInput

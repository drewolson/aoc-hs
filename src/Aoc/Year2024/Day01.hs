module Aoc.Year2024.Day01
  ( part1,
    part2,
  )
where

import Aoc.Core.Parser (Parser, runParser)
import Data.List (sort, transpose)
import Text.Megaparsec (sepEndBy1)
import Text.Megaparsec.Char (newline, space1)
import Text.Megaparsec.Char.Lexer (decimal)

parse :: String -> [(Int, Int)]
parse = runParser $ sepEndBy1 pairP newline
  where
    pairP :: Parser (Int, Int)
    pairP = (,) <$> (decimal <* space1) <*> decimal

part1 :: String -> Int
part1 =
  sum
    . map (abs . foldl1 (-))
    . transpose
    . map sort
    . transpose
    . map (\(a, b) -> [a, b])
    . parse

part2 :: String -> Int
part2 input =
  let pairs = parse input
      as = map fst pairs
      bs = map snd pairs
   in sum $ map (score bs) as
  where
    score :: [Int] -> Int -> Int
    score bs a =
      let count = length $ filter (== a) bs
       in count * a

module Aoc.Solution.Day02
  ( part1,
    part2,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, errorBundlePretty, parse, sepEndBy1, some)
import Text.Megaparsec.Char (digitChar, newline, space, string)

type Parser = Parsec Void String

data Dir = Forward | Up | Down

data Move = Move
  { dir :: Dir,
    dist :: Int
  }

parseDir :: Parser Dir
parseDir =
  choice
    [ Forward <$ string "forward",
      Up <$ string "up",
      Down <$ string "down"
    ]

parseDist :: Parser Int
parseDist = read <$> some digitChar

parseMove :: Parser Move
parseMove = Move <$> parseDir <*> (space *> parseDist)

parseMoves :: Parser [Move]
parseMoves = sepEndBy1 parseMove newline

parseInput :: String -> Either String [Move]
parseInput = first errorBundlePretty . parse parseMoves ""

move1 :: (Int, Int) -> Move -> (Int, Int)
move1 (x, y) Move {dir, dist} =
  case dir of
    Forward -> (x + dist, y)
    Up -> (x, y - dist)
    Down -> (x, y + dist)

part1 :: String -> Either String Int
part1 = fmap (uncurry (*) . foldl' move1 (0, 0)) . parseInput

move2 :: (Int, Int, Int) -> Move -> (Int, Int, Int)
move2 (x, y, aim) Move {dir, dist} =
  case dir of
    Forward -> (x + dist, y + dist * aim, aim)
    Up -> (x, y, aim - dist)
    Down -> (x, y, aim + dist)

part2 :: String -> Either String Int
part2 = fmap ((\(x, y, _) -> x * y) . foldl' move2 (0, 0, 0)) . parseInput

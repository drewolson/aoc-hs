module Aoc.Year2021.Day02
  ( part1,
    part2,
  )
where

import Aoc.Core.Parser (Parser, runParser)
import Data.Foldable (foldl')
import Text.Megaparsec (choice, sepEndBy1)
import Text.Megaparsec.Char (newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal)

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

parseMove :: Parser Move
parseMove = Move <$> parseDir <*> (space *> decimal)

parseMoves :: Parser [Move]
parseMoves = sepEndBy1 parseMove newline

parseInput :: String -> [Move]
parseInput = runParser parseMoves

move1 :: (Int, Int) -> Move -> (Int, Int)
move1 (x, y) Move {dir, dist} =
  case dir of
    Forward -> (x + dist, y)
    Up -> (x, y - dist)
    Down -> (x, y + dist)

part1 :: String -> Int
part1 = uncurry (*) . foldl' move1 (0, 0) . parseInput

move2 :: (Int, Int, Int) -> Move -> (Int, Int, Int)
move2 (x, y, aim) Move {dir, dist} =
  case dir of
    Forward -> (x + dist, y + dist * aim, aim)
    Up -> (x, y, aim - dist)
    Down -> (x, y, aim + dist)

part2 :: String -> Int
part2 = (\(x, y, _) -> x * y) . foldl' move2 (0, 0, 0) . parseInput

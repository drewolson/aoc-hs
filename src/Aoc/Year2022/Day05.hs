module Aoc.Year2022.Day05
  ( part1,
    part2,
  )
where

import Aoc.Core.Parser (Parser, dropLineP, runParser')
import Data.IntMap.Strict (IntMap, (!))
import Data.IntMap.Strict qualified as IntMap
import Data.List (foldl', transpose)
import Data.Maybe (catMaybes)
import Text.Megaparsec (anySingle, choice, sepBy1, sepEndBy1)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

data Move = Move
  { num :: Int,
    from :: Int,
    to :: Int
  }

data Input = Input
  { stacks :: IntMap [Char],
    moves :: [Move]
  }

stackItemP :: Parser (Maybe Char)
stackItemP =
  choice
    [ Just <$> (char '[' *> anySingle <* char ']'),
      Nothing <$ string "   "
    ]

stackP :: Parser [Maybe Char]
stackP = sepBy1 stackItemP (char ' ')

stacksP :: Parser [[Maybe Char]]
stacksP = sepEndBy1 stackP newline

moveP :: Parser Move
moveP =
  Move
    <$> (string "move " *> decimal)
    <*> (string " from " *> decimal)
    <*> (string " to " *> decimal)

movesP :: Parser [Move]
movesP = sepEndBy1 moveP newline

parseInput :: String -> Input
parseInput = runParser' $ do
  rawStacks <- stacksP
  dropLineP <* newline
  moves <- movesP
  let stacks = IntMap.fromList $ zip [1 ..] $ catMaybes <$> transpose rawStacks
  pure Input {stacks, moves}

checkTop :: IntMap [Char] -> String
checkTop = foldMap (pure . head) . IntMap.elems

performMoves :: ([Char] -> [Char]) -> Input -> IntMap [Char]
performMoves f Input {stacks, moves} = foldl' makeMove stacks moves
  where
    makeMove :: IntMap [Char] -> Move -> IntMap [Char]
    makeMove m Move {num, from, to} =
      let new = f $ take num $ m ! from
       in IntMap.adjust (drop num) from $ IntMap.adjust (new <>) to m

part1 :: String -> String
part1 = checkTop . performMoves reverse . parseInput

part2 :: String -> String
part2 = checkTop . performMoves id . parseInput

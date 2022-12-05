module Aoc.Year2022.Day05
  ( part1,
    part2,
  )
where

import Aoc.Parser (Parser, dropLineP, intP, runParser')
import Data.List (foldl', transpose)
import Data.Map.Strict (Map, (!))
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Text.Megaparsec (anySingle, choice, sepBy1, sepEndBy1)
import Text.Megaparsec.Char (char, newline, string)

data Move = Move
  { num :: Int,
    from :: Int,
    to :: Int
  }
  deriving (Show)

data Input = Input
  { stacks :: Map Int [Char],
    moves :: [Move]
  }
  deriving (Show)

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
    <$> (string "move " *> intP)
    <*> (string " from " *> intP)
    <*> (string " to " *> intP)

movesP :: Parser [Move]
movesP = sepEndBy1 moveP newline

parseInput :: String -> Input
parseInput = runParser' do
  rawStacks <- stacksP
  dropLineP <* newline
  moves <- movesP
  let stacks = Map.fromList $ zip [1 ..] $ catMaybes <$> transpose rawStacks
  pure Input {stacks, moves}

checkTop :: Map Int [Char] -> String
checkTop = foldMap safeHead . Map.elems
  where
    safeHead :: String -> String
    safeHead (h : _) = [h]
    safeHead _ = ""

performMoves :: ([Char] -> [Char]) -> Input -> Map Int [Char]
performMoves f Input {stacks, moves} = foldl' makeMove stacks moves
  where
    makeMove :: Map Int [Char] -> Move -> Map Int [Char]
    makeMove m Move {num, from, to} =
      let new = f $ take num $ m ! from
       in Map.adjust (drop num) from $ Map.adjust (new <>) to m

part1 :: String -> String
part1 = checkTop . performMoves reverse . parseInput

part2 :: String -> String
part2 = checkTop . performMoves id . parseInput

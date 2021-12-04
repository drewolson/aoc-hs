module Aoc.Solution.Day04
  ( part1,
    part2,
  )
where

import Control.Monad (join)
import Data.Bifunctor (first)
import Data.List (transpose, (\\))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void (Void)
import Text.Megaparsec (Parsec, count, errorBundlePretty, parse, sepBy1, some)
import Text.Megaparsec.Char (char, digitChar, hspace, newline, space1)

type Parser = Parsec Void String

type Board = [[Int]]

data Bingo = Bingo
  { guesses :: [Int],
    boards :: [Board]
  }
  deriving (Show)

parseInt :: Parser Int
parseInt = read <$> some digitChar

parseGuesses :: Parser [Int]
parseGuesses = sepBy1 parseInt (char ',')

parseRow :: Parser [Int]
parseRow = count 5 (hspace *> parseInt)

parseBoard :: Parser Board
parseBoard = count 5 (parseRow <* newline)

parseBoards :: Parser [Board]
parseBoards = sepBy1 parseBoard newline

parseBingo :: Parser Bingo
parseBingo = Bingo <$> parseGuesses <*> (space1 *> parseBoards)

parseInput :: String -> Either String Bingo
parseInput = first errorBundlePretty . parse parseBingo ""

solved :: Set Int -> Board -> Bool
solved called board =
  any (all (`elem` called)) board
    || any (all (`elem` called)) (transpose board)

findSolved :: Set Int -> [Board] -> [Board]
findSolved called = filter (solved called)

score :: Set Int -> Int -> Board -> Int
score called mul = (* mul) . sum . filter (`notElem` called) . join

solve :: Set Int -> [Board] -> [Int] -> [Int]
solve _ _ [] = []
solve called boards (h : t) =
  let called' = Set.insert h called
      justSolved = findSolved called' boards
      scores = score called' h <$> justSolved
      boards' = boards \\ justSolved
   in scores ++ solve called' boards' t

part1 :: String -> Either String Int
part1 input = do
  Bingo {guesses, boards} <- parseInput input

  pure $ head $ solve Set.empty boards guesses

part2 :: String -> Either String Int
part2 input = do
  Bingo {guesses, boards} <- parseInput input

  pure $ last $ solve Set.empty boards guesses

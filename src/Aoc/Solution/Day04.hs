module Aoc.Solution.Day04
  ( part1,
    part2,
  )
where

import Control.Monad (join)
import Data.Bifunctor (first)
import Data.Foldable (find)
import Data.List (transpose)
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

findSolved :: Set Int -> [Board] -> Maybe Board
findSolved called = find (solved called)

score :: Set Int -> Board -> Int
score called = sum . filter (`notElem` called) . join

solve1 :: Set Int -> [Int] -> [Board] -> Int
solve1 _ [] _ = 0
solve1 called (h : t) boards =
  let called' = Set.insert h called
   in case findSolved called' boards of
        Nothing -> solve1 called' t boards
        Just board -> score called' board * h

part1 :: String -> Either String Int
part1 input = do
  Bingo {guesses, boards} <- parseInput input

  pure $ solve1 Set.empty guesses boards

solve2 :: Set Int -> [Int] -> [Board] -> Maybe Int -> Either String Int
solve2 _ [] _ Nothing = Left "no solution found"
solve2 _ [] _ (Just n) = pure n
solve2 called (h : t) boards prevScore =
  let called' = Set.insert h called
   in case findSolved called' boards of
        Nothing -> solve2 called' t boards prevScore
        Just board ->
          let boards' = filter (/= board) boards
              prevScore' = Just (score called' board * h)
           in solve2 called' (h : t) boards' prevScore'

part2 :: String -> Either String Int
part2 input = do
  Bingo {guesses, boards} <- parseInput input

  solve2 Set.empty guesses boards Nothing

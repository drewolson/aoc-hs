module Aoc.Year2021.Day04
  ( part1,
    part2,
  )
where

import Aoc.Core.Parser (Parser, runParser)
import Control.Monad (join)
import Data.List (mapAccumL, transpose, (\\))
import Data.Set (Set)
import Data.Set qualified as Set
import Text.Megaparsec (count, sepBy1)
import Text.Megaparsec.Char (char, hspace, newline, space1)
import Text.Megaparsec.Char.Lexer (decimal)

type Board = [[Int]]

data Bingo = Bingo
  { guesses :: [Int],
    boards :: [Board]
  }

parseGuesses :: Parser [Int]
parseGuesses = sepBy1 decimal (char ',')

parseRow :: Parser [Int]
parseRow = count 5 (hspace *> decimal)

parseBoard :: Parser Board
parseBoard = count 5 (parseRow <* newline)

parseBoards :: Parser [Board]
parseBoards = sepBy1 parseBoard newline

parseBingo :: Parser Bingo
parseBingo = Bingo <$> parseGuesses <*> (space1 *> parseBoards)

parseInput :: String -> Bingo
parseInput = runParser parseBingo

isSolved :: Set Int -> Board -> Bool
isSolved called board =
  any (all (`elem` called)) board
    || any (all (`elem` called)) (transpose board)

findSolved :: Set Int -> [Board] -> [Board]
findSolved called = filter (isSolved called)

score :: Set Int -> Int -> Board -> Int
score called mul = (* mul) . sum . filter (`notElem` called) . join

solve :: Set Int -> [Board] -> [Int] -> [Int]
solve c b = join . snd . mapAccumL solveNext (c, b)
  where
    solveNext :: (Set Int, [Board]) -> Int -> ((Set Int, [Board]), [Int])
    solveNext (called, boards) guess =
      let called' = Set.insert guess called
          solved = findSolved called' boards
          scores = score called' guess <$> solved
          boards' = boards \\ solved
       in ((called', boards'), scores)

part1 :: String -> Int
part1 input =
  let Bingo {guesses, boards} = parseInput input
   in head $ solve Set.empty boards guesses

part2 :: String -> Int
part2 input = do
  let Bingo {guesses, boards} = parseInput input
   in last $ solve Set.empty boards guesses

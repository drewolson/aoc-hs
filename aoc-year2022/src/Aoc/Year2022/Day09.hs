module Aoc.Year2022.Day09 where

import Aoc.Parser (Parser, runParser')
import Control.Monad ((<=<))
import Data.List (foldl', mapAccumL)
import Data.Set (Set)
import Data.Set qualified as Set
import Text.Megaparsec (choice, sepEndBy1)
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

data Dir = R | L | U | D

type Cmd = (Dir, Int)

type Coord = (Int, Int)

dirP :: Parser Dir
dirP =
  choice
    [ R <$ string "R ",
      L <$ string "L ",
      U <$ string "U ",
      D <$ string "D "
    ]

cmdP :: Parser Cmd
cmdP = (,) <$> dirP <*> decimal

parseInput :: String -> [Cmd]
parseInput = runParser' $ sepEndBy1 cmdP newline

expand :: Cmd -> [Coord]
expand (dir, n) = replicate n step
  where
    step :: Coord
    step =
      case dir of
        R -> (1, 0)
        L -> (-1, 0)
        U -> (0, 1)
        D -> (0, -1)

data State = S {s :: Set Coord, h :: Coord, ks :: [Coord]}

makeSteps :: Int -> [Coord] -> Set Coord
makeSteps n = s . foldl' go (S (Set.singleton (0, 0)) (0, 0) (replicate n (0, 0)))
  where
    move :: Coord -> Coord -> Coord
    move (dx, dy) (x, y) = (x + dx, y + dy)

    dupe :: a -> (a, a)
    dupe a = (a, a)

    moveTail :: Coord -> Coord -> (Coord, Coord)
    moveTail (hX, hY) (tX, tY)
      | abs (hX - tX) >= 2 || abs (hY - tY) >= 2 =
        dupe (tX + signum (hX - tX), tY + signum (hY - tY))
      | otherwise = dupe (tX, tY)

    go :: State -> Coord -> State
    go S {s, h, ks} coord =
      let h' = move coord h
          ks' = snd $ mapAccumL moveTail h' ks
          s' = Set.insert (last ks') s
       in S s' h' ks'

part1 :: String -> Int
part1 = length . makeSteps 1 . (expand <=< parseInput)

part2 :: String -> Int
part2 = length . makeSteps 9 . (expand <=< parseInput)

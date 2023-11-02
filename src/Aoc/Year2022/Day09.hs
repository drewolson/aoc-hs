module Aoc.Year2022.Day09
  ( part1,
    part2,
  )
where

import Aoc.Core.Parser (Parser, runParser)
import Control.Monad ((<=<))
import Data.List (foldl', scanl')
import Data.Set (Set)
import Data.Set qualified as Set
import Text.Megaparsec (choice, sepEndBy1)
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Coord = (Int, Int)

parseInput :: String -> [(Int, Coord)]
parseInput = runParser $ sepEndBy1 cmdP newline
  where
    dirP :: Parser Coord
    dirP =
      choice
        [ (1, 0) <$ string "R ",
          (-1, 0) <$ string "L ",
          (0, 1) <$ string "U ",
          (0, -1) <$ string "D "
        ]

    cmdP :: Parser (Int, Coord)
    cmdP = flip (,) <$> dirP <*> decimal

data State = S {s :: Set Coord, h :: Coord, ks :: [Coord]}

takeSteps :: Int -> [Coord] -> Set Coord
takeSteps n = s . foldl' go S {s = Set.singleton (0, 0), h = (0, 0), ks = replicate n (0, 0)}
  where
    move :: Coord -> Coord -> Coord
    move (dx, dy) (x, y) = (x + dx, y + dy)

    moveTail :: Coord -> Coord -> Coord
    moveTail (hX, hY) (tX, tY)
      | abs (hX - tX) >= 2 || abs (hY - tY) >= 2 =
          (tX + signum (hX - tX), tY + signum (hY - tY))
      | otherwise = (tX, tY)

    go :: State -> Coord -> State
    go S {s, h, ks} coord =
      let h' = move coord h
          ks' = drop 1 $ scanl' moveTail h' ks
          s' = Set.insert (last ks') s
       in S s' h' ks'

part1 :: String -> Int
part1 = length . takeSteps 1 . (uncurry replicate <=< parseInput)

part2 :: String -> Int
part2 = length . takeSteps 9 . (uncurry replicate <=< parseInput)

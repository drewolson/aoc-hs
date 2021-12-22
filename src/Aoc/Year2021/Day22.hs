module Aoc.Year2021.Day22
  ( part1,
    part2,
  )
where

import Aoc.Parser (Parser, parseSignedInt, runParser')
import Control.Applicative ((<|>))
import Data.Foldable (Foldable (foldl'))
import Text.Megaparsec (sepEndBy1)
import Text.Megaparsec.Char (char, newline, space, string)

data Action = On | Off
  deriving (Eq, Show)

type Range = (Int, Int)

type Point = (Int, Int, Int)

data Step = Step
  { action :: Action,
    x :: Range,
    y :: Range,
    z :: Range
  }
  deriving (Eq, Show)

parseAction :: Parser Action
parseAction = On <$ string "on" <|> Off <$ string "off"

parseRange :: Parser (Int, Int)
parseRange = (,) <$> parseSignedInt <*> (string ".." *> parseSignedInt)

parseStep :: Parser Step
parseStep =
  Step
    <$> (parseAction <* space)
    <*> (string "x=" *> parseRange <* char ',')
    <*> (string "y=" *> parseRange <* char ',')
    <*> (string "z=" *> parseRange)

parseSteps :: Parser [Step]
parseSteps = sepEndBy1 parseStep newline

parseInput :: String -> [Step]
parseInput = runParser' parseSteps

isOn :: [Step] -> Point -> Bool
isOn steps point = foldl' (transform point) False steps
  where
    inRange :: Point -> Step -> Bool
    inRange (a, b, c) Step {x = (x1, x2), y = (y1, y2), z = (z1, z2)} =
      x1 <= a && a <= x2 && y1 <= b && b <= y2 && z1 <= c && c <= z2

    transform :: Point -> Bool -> Step -> Bool
    transform _ False Step {action = Off} = False
    transform _ True Step {action = On} = True
    transform p b s@Step {action}
      | inRange p s = action == On
      | otherwise = b

part1 :: String -> Int
part1 input =
  let steps = parseInput input
   in length $ filter (isOn steps) $ [(x, y, z) | x <- [-50 .. 50], y <- [-50 .. 50], z <- [-50 .. 50]]

part2 :: String -> Int
part2 input =
  let steps = parseInput input
      xmin = minimum $ fst . x <$> filter ((== On) . action) steps
      xmax = maximum $ snd . x <$> filter ((== On) . action) steps
      ymin = minimum $ fst . y <$> filter ((== On) . action) steps
      ymax = maximum $ snd . y <$> filter ((== On) . action) steps
      zmin = minimum $ fst . z <$> filter ((== On) . action) steps
      zmax = maximum $ snd . z <$> filter ((== On) . action) steps
   in length $ filter (isOn steps) $ [(x, y, z) | x <- [xmin .. xmax], y <- [ymin .. ymax], z <- [zmin .. zmax]]

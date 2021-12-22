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

type Point = (Int, Int, Int)

type Box = (Point, Point)

data Step = Step
  { action :: Action,
    box :: Box
  }
  deriving (Eq, Show)

parseAction :: Parser Action
parseAction = On <$ string "on" <|> Off <$ string "off"

parseRange :: Parser (Int, Int)
parseRange = (,) <$> parseSignedInt <*> (string ".." *> parseSignedInt)

parseBox :: Parser Box
parseBox = do
  (xmin, xmax) <- string "x=" *> parseRange <* char ','
  (ymin, ymax) <- string "y=" *> parseRange <* char ','
  (zmin, zmax) <- string "z=" *> parseRange

  pure ((xmin, ymin, zmin), (xmax, ymax, zmax))

parseStep :: Parser Step
parseStep =
  Step <$> (parseAction <* space) <*> parseBox

parseSteps :: Parser [Step]
parseSteps = sepEndBy1 parseStep newline

parseInput :: String -> [Step]
parseInput = runParser' parseSteps

isOn :: [Step] -> Point -> Bool
isOn steps point = foldl' (transform point) False steps
  where
    inRange :: Point -> Step -> Bool
    inRange (a, b, c) Step {box = ((xmin, ymin, zmin), (xmax, ymax, zmax))} =
      xmin <= a && a <= xmax && ymin <= b && b <= ymax && zmin <= c && c <= zmax

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
      onBoxes = box <$> filter ((== On) . action) steps
      xmin = minimum $ fmap (\((x, _, _), _) -> x) onBoxes
      xmax = maximum $ fmap (\((_, _, _), (x, _, _)) -> x) onBoxes
      ymin = minimum $ fmap (\((_, y, _), _) -> y) onBoxes
      ymax = maximum $ fmap (\((_, _, _), (_, y, _)) -> y) onBoxes
      zmin = minimum $ fmap (\((_, _, z), _) -> z) onBoxes
      zmax = maximum $ fmap (\((_, _, _), (_, _, z)) -> z) onBoxes
   in length $ filter (isOn steps) $ [(x, y, z) | x <- [xmin .. xmax], y <- [ymin .. ymax], z <- [zmin .. zmax]]

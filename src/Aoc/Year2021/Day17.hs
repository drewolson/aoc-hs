module Aoc.Year2021.Day17
  ( part1,
    part2,
  )
where

import Aoc.Parser (Parser, parseInt, runParser')
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Text.Megaparsec.Char (string)

type Coord = (Int, Int)

type Target = (Coord, Coord)

parseInput :: Parser Target
parseInput = do
  xMin <- string "target area: x=" *> parseInt
  xMax <- string ".." *> parseInt

  yMin <- string ", y=" *> parseInt
  yMax <- string ".." *> parseInt

  pure ((xMin, xMax), (yMin, yMax))

candidates :: Target -> [Coord]
candidates ((_, xMax), (yMin, _)) =
  let x_triangle = head $ filter ((> xMax) . triangle) [0 ..]
      candidate_y = abs (yMin - x_triangle `div` 2)
   in (,) <$> [0 .. xMax] <*> [yMin .. candidate_y]
  where
    triangle :: Int -> Int
    triangle n = n * (n + 1) `div` 2

drawPath :: Target -> Coord -> Maybe [Coord]
drawPath = go [] (0, 0)
  where
    inTarget :: Target -> Coord -> Bool
    inTarget ((xMin, xMax), (yMin, yMax)) (x, y) =
      (xMin <= x) && (x <= xMax) && (yMin <= y) && (y <= yMax)

    pastTarget :: Target -> Coord -> Bool
    pastTarget ((_, xMax), (yMin, _)) (x, y) =
      (x > xMax) || (y < yMin)

    updateVelocity :: Coord -> Coord
    updateVelocity (vx, vy)
      | vx < 0 = (vx + 1, vy - 1)
      | vx > 0 = (vx - 1, vy - 1)
      | otherwise = (vx, vy - 1)

    go :: [Coord] -> Coord -> Target -> Coord -> Maybe [Coord]
    go path coord@(x, y) target v@(vx, vy)
      | pastTarget target coord = Nothing
      | inTarget target coord = Just $ reverse (coord : path)
      | otherwise = go (coord : path) (x + vx, y + vy) target (updateVelocity v)

part1 :: String -> Int
part1 input =
  let target = runParser' parseInput input
   in maximum $ Set.map snd $ foldMap Set.fromList $ mapMaybe (drawPath target) $ candidates target

part2 :: String -> Int
part2 input =
  let target = runParser' parseInput input
   in length $ mapMaybe (drawPath target) $ candidates target

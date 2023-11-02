-- Solution liberally borrowed from
-- https://github.com/borjasotomayor/AoC/blob/main/2021/day22.py

module Aoc.Year2021.Day22
  ( part1,
    part2,
  )
where

import Aoc.Core.Parser (Parser, runParser, signedIntP)
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

data Cuboid = Cuboid
  { cbox :: Box,
    subs :: [Cuboid]
  }

parseAction :: Parser Action
parseAction = On <$ string "on" <|> Off <$ string "off"

parseRange :: Parser (Int, Int)
parseRange = (,) <$> signedIntP <*> (string ".." *> signedIntP)

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
parseInput = runParser parseSteps

intersect :: Box -> Box -> Maybe Box
intersect ((xmin1, ymin1, zmin1), (xmax1, ymax1, zmax1)) ((xmin2, ymin2, zmin2), (xmax2, ymax2, zmax2)) =
  let ixmin = max xmin1 xmin2
      iymin = max ymin1 ymin2
      izmin = max zmin1 zmin2
      ixmax = min xmax1 xmax2
      iymax = min ymax1 ymax2
      izmax = min zmax1 zmax2
   in if ixmin <= ixmax && iymin <= iymax && izmin <= izmax
        then Just ((ixmin, iymin, izmin), (ixmax, iymax, izmax))
        else Nothing

subBox :: Box -> Cuboid -> Cuboid
subBox box cube =
  case box `intersect` cbox cube of
    Nothing -> cube
    Just inter ->
      let subs' = subBox inter <$> subs cube
          iCube = Cuboid {cbox = inter, subs = []}
       in cube {subs = iCube : subs'}

runSteps :: [Cuboid] -> Step -> [Cuboid]
runSteps cubes Step {action, box} =
  let cubes' = fmap (subBox box) cubes
   in case action of
        Off -> cubes'
        On -> Cuboid {cbox = box, subs = []} : cubes'

cuboidSize :: Cuboid -> Int
cuboidSize Cuboid {cbox = ((xmin, ymin, zmin), (xmax, ymax, zmax)), subs} =
  let size = (xmax - xmin + 1) * (ymax - ymin + 1) * (zmax - zmin + 1)
   in size - sum (fmap cuboidSize subs)

smallStep :: Step -> Bool
smallStep Step {box = ((xmin, ymin, zmin), (xmax, ymax, zmax))} =
  all (>= -50) [xmin, ymin, zmin] && all (<= 50) [xmax, ymax, zmax]

part1 :: String -> Int
part1 = sum . fmap cuboidSize . foldl' runSteps [] . filter smallStep . parseInput

part2 :: String -> Int
part2 = sum . fmap cuboidSize . foldl' runSteps [] . parseInput

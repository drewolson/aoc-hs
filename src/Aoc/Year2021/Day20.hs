module Aoc.Year2021.Day20
  ( part1,
    part2,
  )
where

import Aoc.String (binToInt)
import Data.Array (Array, (!))
import Data.Array qualified as Array
import Data.Ix (inRange)
import Data.Set (Set)
import Data.Set qualified as Set

type Coord = (Int, Int)

type Algo = Set Int

type Image = Array Coord Bool

toBool :: Char -> Bool
toBool '#' = True
toBool _ = False

makeImage :: [String] -> Image
makeImage l =
  let y = length l
      x = length $ head l
   in Array.listArray ((0, 0), (y - 1, x - 1)) $ toBool <$> mconcat l

parseInput :: String -> (Algo, Image)
parseInput input =
  let l = lines input
      algo = Set.fromList $ fmap fst $ filter ((== '#') . snd) $ zip [0 ..] $ head l
      image = makeImage $ drop 2 l
   in (algo, image)

enhance :: Algo -> (Image, Bool) -> (Image, Bool)
enhance algo (image, def) = (enhanceImage, newDefault def)
  where
    newDefault :: Bool -> Bool
    newDefault b
      | 0 `elem` algo = not b
      | otherwise = b

    val :: Int -> Int -> Bool
    val x y
      | Array.bounds image `inRange` (y, x) = image ! (y, x)
      | otherwise = def

    windowInt :: Int -> Int -> Int
    windowInt x y = binToInt do
      y' <- [y - 1 .. y + 1]
      x' <- [x - 1 .. x + 1]

      if val x' y'
        then "1"
        else "0"

    enhanceImage :: Image
    enhanceImage =
      let ((yMin, xMin), (yMax, xMax)) = Array.bounds image
       in Array.listArray ((yMin - 1, xMin - 1), (yMax + 1, xMax + 1)) do
            y <- [yMin - 1 .. yMax + 1]
            x <- [xMin - 1 .. xMax + 1]

            pure $ windowInt x y `elem` algo

solve :: Int -> String -> Int
solve n input =
  let (algo, image) = parseInput input
   in length $ filter id $ Array.elems $ fst $ (!! n) $ iterate (enhance algo) (image, False)

part1 :: String -> Int
part1 = solve 2

part2 :: String -> Int
part2 = solve 50

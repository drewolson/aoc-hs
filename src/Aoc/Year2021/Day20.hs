module Aoc.Year2021.Day20
  ( part1,
    part2,
  )
where

import Aoc.String (binToInt)
import Data.Matrix (Matrix)
import Data.Matrix qualified as Matrix
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

type Algo = Set Int

type Image = Matrix Bool

toBool :: Char -> Bool
toBool '#' = True
toBool _ = False

makeImage :: [String] -> Image
makeImage l =
  let rows = length l
      cols = length $ head l
   in Matrix.fromList rows cols $ toBool <$> mconcat l

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
    val row col =
      fromMaybe def $ Matrix.safeGet row col image

    windowInt :: Int -> Int -> Int
    windowInt row col = binToInt do
      r <- [row - 1 .. row + 1]
      c <- [col - 1 .. col + 1]

      if val r c
        then "1"
        else "0"

    enhanceImage :: Image
    enhanceImage =
      let rows = Matrix.nrows image
          cols = Matrix.ncols image
       in Matrix.fromList (rows + 2) (cols + 2) do
            row <- [0 .. rows + 1]
            col <- [0 .. cols + 1]

            pure $ windowInt row col `elem` algo

solve :: Int -> String -> Int
solve n input =
  let (algo, image) = parseInput input
      (image', _) = (!! n) $ iterate (enhance algo) (image, False)
   in length $ filter id $ Matrix.toList image'

part1 :: String -> Int
part1 = solve 2

part2 :: String -> Int
part2 = solve 50

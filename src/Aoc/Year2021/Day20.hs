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
makeImage = Matrix.fromLists . fmap (fmap toBool)

parseInput :: String -> (Algo, Image)
parseInput input =
  let l = lines input
      algo = Set.fromList $ fmap fst $ filter ((== '#') . snd) $ zip [0 ..] $ head l
      image = makeImage $ drop 2 l
   in (algo, image)

enhance :: Algo -> (Image, Bool) -> (Image, Bool)
enhance algo (image, def) = (enhanceImage, newDefault)
  where
    nRows :: Int
    nRows = Matrix.nrows image

    nCols :: Int
    nCols = Matrix.ncols image

    newDefault :: Bool
    newDefault =
      if 0 `elem` algo
        then not def
        else def

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
      Matrix.fromList (nRows + 2) (nCols + 2) do
        row <- [0 .. nRows + 1]
        col <- [0 .. nCols + 1]

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

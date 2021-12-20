module Aoc.Year2021.Day20
  ( part1,
    part2,
  )
where

import Aoc.String (binToInt)
import Control.Monad (guard)
import Data.Foldable (foldl')
import Data.Set (Set)
import Data.Set qualified as Set

type Coord = (Int, Int)

type Algo = Set Int

type Image = Set Coord

type Bounds = ((Int, Int), (Int, Int))

makeImage :: [String] -> Image
makeImage = foldl' addRow Set.empty . zip [0 ..]
  where
    addRow :: Image -> (Int, String) -> Image
    addRow image (y, string) = foldl' (addCell y) image $ zip [0 ..] string

    addCell :: Int -> Image -> (Int, Char) -> Image
    addCell y image (x, '#') = Set.insert (x, y) image
    addCell _ image _ = image

parseInput :: String -> (Algo, Image)
parseInput input =
  let l = lines input
      algo = Set.fromList $ fmap fst $ filter ((== '#') . snd) $ zip [0 ..] $ head l
      image = makeImage $ drop 2 l
   in (algo, image)

enhance :: Algo -> (Image, Bool) -> (Image, Bool)
enhance algo (image, def) = (enhanceImage bounds, newDefault def)
  where
    newDefault :: Bool -> Bool
    newDefault b
      | 0 `elem` algo = not b
      | otherwise = b

    bounds :: Bounds
    bounds =
      let xs = Set.map fst image
          ys = Set.map snd image
       in ((minimum xs, maximum xs), (minimum ys, maximum ys))

    inBounds :: Int -> Int -> Bool
    inBounds x y =
      let ((xMin, xMax), (yMin, yMax)) = bounds
       in xMin <= x && x <= xMax && yMin <= y && y <= yMax

    active :: Int -> Int -> Bool
    active x y
      | inBounds x y = (x, y) `elem` image
      | otherwise = def

    windowInt :: Int -> Int -> Int
    windowInt x y = binToInt do
      y' <- [y - 1 .. y + 1]
      x' <- [x - 1 .. x + 1]

      pure $
        if active x' y'
          then '1'
          else '0'

    enhanceImage :: Bounds -> Image
    enhanceImage ((xMin, xMax), (yMin, yMax)) =
      Set.fromList do
        x <- [xMin - 2 .. xMax + 2]
        y <- [yMin - 2 .. yMax + 2]

        guard $ windowInt x y `elem` algo

        pure (x, y)

part1 :: String -> Int
part1 input =
  let (algo, image) = parseInput input
   in length $ fst $ (!! 2) $ iterate (enhance algo) (image, False)

part2 :: String -> Int
part2 input =
  let (algo, image) = parseInput input
   in length $ fst $ (!! 50) $ iterate (enhance algo) (image, False)

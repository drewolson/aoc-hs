module Aoc.Solution.Day03
  ( part1,
    part2,
  )
where

import Data.Char (digitToInt)
import Data.Foldable (foldl', maximumBy)
import Data.List (group, sort, transpose)
import Data.Ord (comparing)

binToInt :: String -> Int
binToInt = foldl' (\acc x -> acc * 2 + digitToInt x) 0

mostCommon :: String -> Char
mostCommon = head . maximumBy (comparing length) . group . sort

leastCommon :: String -> Char
leastCommon = flipBit . mostCommon

flipBit :: Char -> Char
flipBit '1' = '0'
flipBit _ = '1'

part1 :: String -> Int
part1 input =
  let d = transpose $ lines input
      g = binToInt $ fmap mostCommon d
      e = binToInt $ fmap leastCommon d
   in g * e

findRating :: ([Char] -> Char) -> Int -> [String] -> Int
findRating _ _ [s] = binToInt s
findRating p n l =
  let d = transpose l !! n
      c = p d
      l' = filter ((== c) . (!! n)) l
   in findRating p (n + 1) l'

part2 :: String -> Int
part2 input =
  let l = lines input
      o2 = findRating mostCommon 0 l
      co2 = findRating leastCommon 0 l
   in o2 * co2

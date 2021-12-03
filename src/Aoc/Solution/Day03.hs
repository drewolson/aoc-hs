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

flipBit :: Char -> Char
flipBit '1' = '0'
flipBit _ = '1'

part1 :: String -> Int
part1 input =
  let d = transpose $ lines input
      g = binToInt $ fmap mostCommon d
      e = binToInt $ fmap (flipBit . mostCommon) d
   in g * e

findRating :: ([Char] -> Char) -> [String] -> Int -> Int
findRating _ [s] _ = binToInt s
findRating p l n =
  let t = transpose l
      ns = t !! n
      d = p ns
      l' = filter ((== d) . (!! n)) l
   in findRating p l' (n + 1)

part2 :: String -> Int
part2 input =
  let l = lines input
      o2 = findRating mostCommon l 0
      co2 = findRating (flipBit . mostCommon) l 0
   in o2 * co2

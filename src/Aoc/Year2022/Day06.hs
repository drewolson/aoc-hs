module Aoc.Year2022.Day06
  ( part1,
    part2,
  )
where

import Data.List (nub)

findChunk :: Int -> Int -> String -> Int
findChunk _ _ [] = 0
findChunk n i l@(_ : t) =
  let c = take n l
   in if length c == n && nub c == c
        then i + n
        else findChunk n (i + 1) t

part1 :: String -> Int
part1 = findChunk 4 0

part2 :: String -> Int
part2 = findChunk 14 0

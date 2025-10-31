module Aoc.Core.String
  ( binToInt,
  )
where

import Data.Char (digitToInt)

binToInt :: String -> Int
binToInt = foldl' (\acc x -> acc * 2 + digitToInt x) 0

module Aoc.Core.String
  ( binToInt,
  )
where

import Data.Char (digitToInt)
import Data.Foldable (Foldable (foldl'))

binToInt :: String -> Int
binToInt = foldl' (\acc x -> acc * 2 + digitToInt x) 0

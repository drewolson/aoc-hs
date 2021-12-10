module Aoc.Solution.Day10
  ( part1,
    part2,
  )
where

import Data.List (sort)

isOpen :: Char -> Bool
isOpen = (`elem` "([{<")

toClose :: Char -> Char
toClose = \case
  '(' -> ')'
  '[' -> ']'
  '{' -> '}'
  _ -> '>'

syntaxScore :: String -> Int
syntaxScore = go []
  where
    go :: String -> String -> Int
    go _ [] = 0
    go [] (a : b)
      | isOpen a = go [toClose a] b
      | otherwise = 0
    go a@(ha : ta) (hb : tb)
      | isOpen hb = go (toClose hb : a) tb
      | ha == hb = go ta tb
      | otherwise =
        case hb of
          ')' -> 3
          ']' -> 57
          '}' -> 1197
          _ -> 25137

part1 :: String -> Int
part1 = sum . fmap syntaxScore . lines

correctScore :: String -> Int
correctScore = go []
  where
    charScore :: Char -> Int
    charScore = \case
      ')' -> 1
      ']' -> 2
      '}' -> 3
      _ -> 4

    score :: String -> Int
    score = foldl (\acc c -> acc * 5 + charScore c) 0

    go :: String -> String -> Int
    go acc [] = score acc
    go [] (a : b) = go [toClose a] b
    go a@(_ : ta) (hb : tb)
      | isOpen hb = go (toClose hb : a) tb
      | otherwise = go ta tb

middle :: [a] -> a
middle as = as !! (length as `div` 2)

part2 :: String -> Int
part2 = middle . sort . fmap correctScore . filter ((== 0) . syntaxScore) . lines

module Aoc.Solution.Day10
  ( part1,
    part2,
  )
where

import Data.Either (lefts, rights)
import Data.List (sort)

isOpen :: Char -> Bool
isOpen = (`elem` "([{<")

toClose :: Char -> Char
toClose = \case
  '(' -> ')'
  '[' -> ']'
  '{' -> '}'
  _ -> '>'

completionScore :: String -> Int
completionScore = foldl (\acc c -> acc * 5 + charScore c) 0
  where
    charScore :: Char -> Int
    charScore = \case
      ')' -> 1
      ']' -> 2
      '}' -> 3
      _ -> 4

errorScore :: Char -> Int
errorScore = \case
  ')' -> 3
  ']' -> 57
  '}' -> 1197
  _ -> 25137

syntaxScore :: String -> Either Int Int
syntaxScore = go []
  where
    go :: String -> String -> Either Int Int
    go acc [] = Left $ completionScore acc
    go [] (a : b) = go [toClose a] b
    go a@(ha : ta) (hb : tb)
      | isOpen hb = go (toClose hb : a) tb
      | ha == hb = go ta tb
      | otherwise = Right $ errorScore hb

middle :: [a] -> a
middle as = as !! (length as `div` 2)

part1 :: String -> Int
part1 = sum . rights . fmap syntaxScore . lines

part2 :: String -> Int
part2 = middle . sort . lefts . fmap syntaxScore . lines

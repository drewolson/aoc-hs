module Aoc.Year2022.Day02
  ( part1,
    part2,
  )
where

score :: String -> Int
score "A X" = 1 + 3
score "A Y" = 2 + 6
score "A Z" = 3 + 0
score "B X" = 1 + 0
score "B Y" = 2 + 3
score "B Z" = 3 + 6
score "C X" = 1 + 6
score "C Y" = 2 + 0
score "C Z" = 3 + 3
score _ = 0

score' :: String -> Int
score' "A X" = 3 + 0
score' "A Y" = 1 + 3
score' "A Z" = 2 + 6
score' "B X" = 1 + 0
score' "B Y" = 2 + 3
score' "B Z" = 3 + 6
score' "C X" = 2 + 0
score' "C Y" = 3 + 3
score' "C Z" = 1 + 6
score' _ = 0

part1 :: String -> Int
part1 = sum . fmap score . lines

part2 :: String -> Int
part2 = sum . fmap score' . lines

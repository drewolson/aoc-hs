module Aoc.Solution.Day08
  ( part1,
    part2,
  )
where

import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set, (\\))
import Data.Set qualified as Set

type Digit = Set Char

type Reading = ([Digit], [Digit])

find' :: (a -> Bool) -> [a] -> a
find' f = head . filter f

parseInput :: String -> [Reading]
parseInput = fmap parseReading . lines
  where
    toDigits :: String -> [Digit]
    toDigits = fmap Set.fromList . splitOn " "

    parseReading :: String -> Reading
    parseReading line =
      let parts = splitOn " | " line
       in (toDigits $ head parts, toDigits $ last parts)

part1 :: String -> Int
part1 = length . filter ((`elem` [2, 3, 4, 7]) . length) . foldMap snd . parseInput

solveReading :: Reading -> Int
solveReading (inputs, outputs) = read $ mconcat $ mapMaybe (`Map.lookup` mapping) outputs
  where
    mapping :: Map Digit String
    mapping =
      Map.fromList
        [ (zero, "0"),
          (one, "1"),
          (two, "2"),
          (three, "3"),
          (four, "4"),
          (five, "5"),
          (six, "6"),
          (seven, "7"),
          (eight, "8"),
          (nine, "9")
        ]

    zero :: Digit
    zero = find' (`notElem` [six, nine]) $ filter ((== 6) . length) inputs

    one :: Digit
    one = find' ((== 2) . length) inputs

    two :: Digit
    two = find' (`notElem` [zero, one, three, four, five, six, seven, eight, nine]) inputs

    three :: Digit
    three = nine \\ Set.intersection (four \\ seven) zero

    four :: Digit
    four = find' ((== 4) . length) inputs

    five :: Digit
    five = Set.union (nine \\ one) (Set.intersection six one)

    six :: Digit
    six = find' isInOneAndFour $ filter ((== 6) . length) inputs

    seven :: Digit
    seven = find' ((== 3) . length) inputs

    eight :: Digit
    eight = find' ((== 7) . length) inputs

    nine :: Digit
    nine = find' (not . isInFour) $ filter ((== 6) . length) inputs

    isInOneAndFour :: Digit -> Bool
    isInOneAndFour d = isInOne d && isInFour d

    isInFour :: Digit -> Bool
    isInFour = (not . Set.null) . (four \\)

    isInOne :: Digit -> Bool
    isInOne = (not . Set.null) . (one \\)

part2 :: String -> Int
part2 = sum . fmap solveReading . parseInput

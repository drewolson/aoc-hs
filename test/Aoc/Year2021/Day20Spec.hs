module Aoc.Year2021.Day20Spec
  ( spec,
  )
where

import Aoc.Year2021.Day20 qualified as Day20
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###
|]

spec :: Spec
spec = do
  describe "part1" $ do
    it "day 20 part 1 works" $ do
      let result = Day20.part1 input

      result `shouldBe` 35

  describe "part2" $ do
    it "day 20 part 2 works" $ do
      let result = Day20.part2 input

      result `shouldBe` 3351

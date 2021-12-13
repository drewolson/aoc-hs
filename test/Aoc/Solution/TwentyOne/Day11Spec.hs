module Aoc.Solution.TwentyOne.Day11Spec
  ( spec,
  )
where

import Aoc.Solution.TwentyOne.Day11 qualified as Day11
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
|]

spec :: Spec
spec = do
  describe "part1" do
    it "day 11 part 1 works" do
      let result = Day11.part1 input

      result `shouldBe` 1656

  describe "part2" do
    it "day 11 part 2 works" do
      let result = Day11.part2 input

      result `shouldBe` 195

module Aoc.Solution.Day09Spec
  ( spec,
  )
where

import Aoc.Solution.Day09 qualified as Day09
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|2199943210
3987894921
9856789892
8767896789
9899965678
|]

spec :: Spec
spec = do
  describe "part1" do
    it "day 9 part 1 works" do
      let result = Day09.part1 input

      result `shouldBe` 15

    it "day 9 part 2 works" do
      let result = Day09.part2 input

      result `shouldBe` 1134

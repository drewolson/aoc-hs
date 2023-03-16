module Aoc.Year2021.Day15Spec
  ( spec,
  )
where

import Aoc.Year2021.Day15 qualified as Day15
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
|]

spec :: Spec
spec = do
  describe "part1" do
    it "day 15 part 1 works" do
      let result = Day15.part1 input

      result `shouldBe` Just 40

  describe "part2" do
    it "day 15 part 2 works" do
      let result = Day15.part2 input

      result `shouldBe` Just 315

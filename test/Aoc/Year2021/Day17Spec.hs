module Aoc.Year2021.Day17Spec
  ( spec,
  )
where

import Aoc.Year2021.Day17 qualified as Day17
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|target area: x=20..30, y=-10..-5
|]

spec :: Spec
spec = do
  describe "part1" do
    it "day 17 part 1 works" do
      let result = Day17.part1 input

      result `shouldBe` 45

  describe "part2" do
    it "day 17 part 2 works" do
      let result = Day17.part2 input

      result `shouldBe` 112

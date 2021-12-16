module Aoc.Year2021.Day16Spec
  ( spec,
  )
where

import Aoc.Year2021.Day16 qualified as Day16
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|A0016C880162017C3686B18A3D4780
|]

input1 :: String
input1 =
  [r|D2FE28
|]

input2 :: String
input2 =
  [r|38006F45291200
|]

input3 :: String
input3 =
  [r|EE00D40C823060
|]

spec :: Spec
spec = do
  describe "part1" do
    it "day 16 part 1 works with input" do
      let result = Day16.part1 input

      result `shouldBe` 31

    it "day 16 part 1 works with input1" do
      let result = Day16.part1 input1

      result `shouldBe` 6

    it "day 16 part 1 works with input2" do
      let result = Day16.part1 input2

      result `shouldBe` 9

    it "day 16 part 1 works with input3" do
      let result = Day16.part1 input3

      result `shouldBe` 14

  describe "part2" do
    it "day 16 part 2 works with input" do
      let result = Day16.part2 input

      result `shouldBe` 54

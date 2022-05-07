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

spec :: Spec
spec = do
  describe "part1" do
    it "day 16 part 1 works" do
      let result = Day16.part1 input

      result `shouldBe` 31

  describe "part2" do
    it "day 16 part 2 works" do
      let result = Day16.part2 input

      result `shouldBe` 54

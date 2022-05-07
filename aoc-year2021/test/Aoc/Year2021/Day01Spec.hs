module Aoc.Year2021.Day01Spec
  ( spec,
  )
where

import Aoc.Year2021.Day01 qualified as Day01
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|199
200
208
210
200
207
240
269
260
263|]

spec :: Spec
spec = do
  describe "part1" do
    it "day 1 part 1 works" do
      let result = Day01.part1 input

      result `shouldBe` 7

  describe "part2" do
    it "day 1 part 2 works" do
      let result = Day01.part2 input

      result `shouldBe` 5

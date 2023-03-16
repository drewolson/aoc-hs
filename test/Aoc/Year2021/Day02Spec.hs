module Aoc.Year2021.Day02Spec
  ( spec,
  )
where

import Aoc.Year2021.Day02 qualified as Day02
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|forward 5
down 5
forward 8
up 3
down 8
forward 2|]

spec :: Spec
spec = do
  describe "part1" do
    it "day 2 part 1 works" do
      let result = Day02.part1 input

      result `shouldBe` Right 150

  describe "part2" do
    it "day 2 part 2 works" do
      let result = Day02.part2 input

      result `shouldBe` Right 900

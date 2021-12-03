module Aoc.Solution.Day02Spec
  ( spec,
  )
where

import Aoc.Solution.Day02 qualified as Day02
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
    it "returns the expected result" do
      let result = Day02.part1 input

      result `shouldBe` Right 150

  describe "part2" do
    it "returns the expected result" do
      let result = Day02.part2 input

      result `shouldBe` Right 900
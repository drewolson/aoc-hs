module Aoc.Solution.Day05Spec
  ( spec,
  )
where

import Aoc.Solution.Day05 qualified as Day05
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
|]

spec :: Spec
spec = do
  describe "part1" do
    it "returns the expected result" do
      let result = Day05.part1 input

      result `shouldBe` Right 5

  describe "part2" do
    it "returns the expected result" do
      let result = Day05.part2 input

      result `shouldBe` Right 12

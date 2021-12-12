module Aoc.Solution.Day12Spec
  ( spec,
  )
where

import Aoc.Solution.Day12 qualified as Day12
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|start-A
start-b
A-c
A-b
b-d
A-end
b-end
|]

spec :: Spec
spec = do
  describe "part1" do
    it "day 12 part 1 works" do
      let result = Day12.part1 input

      result `shouldBe` 10

  describe "part2" do
    it "day 12 part 2 works" do
      let result = Day12.part2 input

      result `shouldBe` 36

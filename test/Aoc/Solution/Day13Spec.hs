module Aoc.Solution.Day13Spec
  ( spec,
  )
where

import Aoc.Solution.Day13 qualified as Day13
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
|]

spec :: Spec
spec = do
  describe "part1" do
    it "day 13 part 1 works" do
      let result = Day13.part1 input

      result `shouldBe` Right 17

  describe "part2" do
    it "day 13 part 2 works" do
      let result = Day13.part2 input

      let expected =
            [r|#####
#   #
#   #
#   #
#####|]

      result `shouldBe` expected

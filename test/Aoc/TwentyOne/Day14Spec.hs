module Aoc.TwentyOne.Day14Spec
  ( spec,
  )
where

import Aoc.TwentyOne.Day14 qualified as Day14
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
|]

spec :: Spec
spec = do
  describe "part1" do
    it "day 14 part 1 works" do
      let result = Day14.part1 input

      result `shouldBe` 1588

  describe "part2" do
    it "day 14 part 2 works" do
      let result = Day14.part2 input

      result `shouldBe` 2188189693529

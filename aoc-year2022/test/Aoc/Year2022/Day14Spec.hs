module Aoc.Year2022.Day14Spec
  ( spec,
  )
where

import Aoc.Year2022.Day14 qualified as Day14
import Test.Hspec (Spec, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
|]

spec :: Spec
spec = do
  it "2022 day 14 part 1 works" do
    let result = Day14.part1 input

    result `shouldBe` 24

  it "2022 day 14 part 2 works" do
    let result = Day14.part2 input

    result `shouldBe` 93

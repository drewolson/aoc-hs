module Aoc.Year2022.Day02Spec
  ( spec,
  )
where

import Aoc.Year2022.Day02 qualified as Day02
import Test.Hspec (Spec, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|
A Y
B X
C Z|]

spec :: Spec
spec = do
  it "2022 day 2 part 1 works" do
    let result = Day02.part1 input

    result `shouldBe` 15

  it "2022 day 2 part 2 works" do
    let result = Day02.part2 input

    result `shouldBe` 12

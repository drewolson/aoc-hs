module Aoc.Year2022.Day12Spec
  ( spec,
  )
where

import Aoc.Year2022.Day12 qualified as Day12
import Test.Hspec (Spec, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
|]

spec :: Spec
spec = do
  it "2022 day 12 part 1 works" do
    let result = Day12.part1 input

    result `shouldBe` Just 31

  it "2022 day 12 part 2 works" do
    let result = Day12.part2 input

    result `shouldBe` Just 29

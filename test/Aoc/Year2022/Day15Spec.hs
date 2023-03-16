module Aoc.Year2022.Day15Spec
  ( spec,
  )
where

import Aoc.Year2022.Day15 qualified as Day15
import Test.Hspec (Spec, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3
|]

spec :: Spec
spec = do
  it "2022 day 15 part 1 works" do
    let result = Day15.part1 10 input

    result `shouldBe` 26

  it "2022 day 15 part 2 works" do
    let result = Day15.part2 20 input

    result `shouldBe` Just 56000011

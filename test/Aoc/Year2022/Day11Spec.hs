module Aoc.Year2022.Day11Spec
  ( spec,
  )
where

import Aoc.Year2022.Day11 qualified as Day11
import Test.Hspec (Spec, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
|]

spec :: Spec
spec = do
  it "2022 day 11 part 1 works" do
    let result = Day11.part1 input

    result `shouldBe` 10605

  it "2022 day 11 part 2 works" do
    let result = Day11.part2 input

    result `shouldBe` 2713310158

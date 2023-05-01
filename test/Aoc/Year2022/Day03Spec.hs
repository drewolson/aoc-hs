module Aoc.Year2022.Day03Spec
  ( spec,
  )
where

import Aoc.Year2022.Day03 qualified as Day03
import Test.Hspec (Spec, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
|]

spec :: Spec
spec = do
  it "2022 day 3 part 1 works" $ do
    let result = Day03.part1 input

    result `shouldBe` 157

  it "2022 day 3 part 2 works" $ do
    let result = Day03.part2 input

    result `shouldBe` 70

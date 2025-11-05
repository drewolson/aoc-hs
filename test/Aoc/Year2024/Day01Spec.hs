module Aoc.Year2024.Day01Spec
  ( spec,
  )
where

import Aoc.Year2024.Day01 qualified as Day01
import Test.Hspec (Spec, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|3   4
4   3
2   5
1   3
3   9
3   3|]

spec :: Spec
spec = do
  it "2024 day 1 part 1 works" $ do
    let result = Day01.part1 input

    result `shouldBe` 11

  it "2024 day 1 part 2 works" $ do
    let result = Day01.part2 input

    result `shouldBe` 31

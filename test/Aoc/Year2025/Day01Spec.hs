module Aoc.Year2025.Day01Spec
  ( spec,
  )
where

import Aoc.Year2025.Day01 qualified as Day01
import Test.Hspec (Spec, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|test
is
a
test|]

spec :: Spec
spec = do
  it "2025 day 1 part 1 works" $ do
    let result = Day01.part1 input

    result `shouldBe` 1

  it "2025 day 1 part 2 works" $ do
    let result = Day01.part2 input

    result `shouldBe` 2

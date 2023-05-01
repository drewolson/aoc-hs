module Aoc.Year2022.Day04Spec
  ( spec,
  )
where

import Aoc.Year2022.Day04 qualified as Day04
import Test.Hspec (Spec, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
|]

spec :: Spec
spec = do
  it "2022 day 4 part 1 works" $ do
    let result = Day04.part1 input

    result `shouldBe` 2

  it "2022 day 4 part 2 works" $ do
    let result = Day04.part2 input

    result `shouldBe` 4

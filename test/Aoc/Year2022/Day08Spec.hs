module Aoc.Year2022.Day08Spec
  ( spec,
  )
where

import Aoc.Year2022.Day08 qualified as Day08
import Test.Hspec (Spec, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|30373
25512
65332
33549
35390
|]

spec :: Spec
spec = do
  it "2022 day 8 part 1 works" $ do
    let result = Day08.part1 input

    result `shouldBe` 21

  it "2022 day 8 part 2 works" $ do
    let result = Day08.part2 input

    result `shouldBe` 8

module Aoc.Year2022.Day06Spec
  ( spec,
  )
where

import Aoc.Year2022.Day06 qualified as Day06
import Test.Hspec (Spec, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|mjqjpqmgbljsphdztnvjfqwrcgsmlb|]

spec :: Spec
spec = do
  it "2022 day 6 part 1 works" $ do
    let result = Day06.part1 input

    result `shouldBe` 7

  it "2022 day 6 part 2 works" $ do
    let result = Day06.part2 input

    result `shouldBe` 19

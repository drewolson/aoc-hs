module Aoc.Year2021.Day06Spec
  ( spec,
  )
where

import Aoc.Year2021.Day06 qualified as Day06
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|3,4,3,1,2
  |]

spec :: Spec
spec = do
  describe "part1" $ do
    it "day 6 part 1 works" $ do
      let result = Day06.part1 input

      result `shouldBe` 5934

  describe "part2" $ do
    it "day 6 part 2 works" $ do
      let result = Day06.part2 input

      result `shouldBe` 26984457539

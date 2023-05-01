module Aoc.Year2021.Day07Spec
  ( spec,
  )
where

import Aoc.Year2021.Day07 qualified as Day07
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|16,1,2,0,4,2,7,1,2,14
  |]

spec :: Spec
spec = do
  describe "part1" $ do
    it "day 7 part 1 works" $ do
      let result = Day07.part1 input

      result `shouldBe` 37

  describe "part2" $ do
    it "day 7 part 2 works" $ do
      let result = Day07.part2 input

      result `shouldBe` 168

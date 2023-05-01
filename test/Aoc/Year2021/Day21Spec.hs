module Aoc.Year2021.Day21Spec
  ( spec,
  )
where

import Aoc.Year2021.Day21 qualified as Day21
import Test.Hspec (Spec, describe, it, pendingWith, shouldBe)

spec :: Spec
spec = do
  describe "part1" $ do
    it "day 21 part 1 works" $ do
      let result = Day21.part1 4 8

      result `shouldBe` 739785

  describe "part2" $ do
    it "day 21 part 2 works" $ do
      pendingWith "slow"

      let result = Day21.part2 4 8

      result `shouldBe` 444356092776315

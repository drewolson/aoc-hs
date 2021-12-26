module Aoc.Year2021.Day23Spec
  ( spec,
  )
where

import Aoc.Year2021.Day23 qualified as Day23
import Test.Hspec (Spec, describe, it, pendingWith, shouldBe)

spec :: Spec
spec = do
  describe "part1" do
    it "day 23 part 1 works" do
      pendingWith "not solved"

      let result = Day23.part1 Day23.sample

      result `shouldBe` Just 12521

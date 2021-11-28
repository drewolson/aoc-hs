module Aoc.Solution.Day01Spec
  ( spec,
  )
where

import Aoc.Solution.Day01 qualified as Day01
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "part1" do
    it "returns the same text" do
      result <- Day01.part1 "Hello, world"

      result `shouldBe` "Hello, world"

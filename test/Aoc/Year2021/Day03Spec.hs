module Aoc.Year2021.Day03Spec
  ( spec,
  )
where

import Aoc.Year2021.Day03 qualified as Day03
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010
|]

spec :: Spec
spec = do
  describe "part1" $ do
    it "day 3 part 1 works" $ do
      let result = Day03.part1 input

      result `shouldBe` 198

  describe "part2" $ do
    it "day 3 part 2 works" $ do
      let result = Day03.part2 input

      result `shouldBe` 230

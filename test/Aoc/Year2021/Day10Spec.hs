module Aoc.Year2021.Day10Spec
  ( spec,
  )
where

import Aoc.Year2021.Day10 qualified as Day10
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
|]

spec :: Spec
spec = do
  describe "part1" $ do
    it "day 10 part 1 works" $ do
      let result = Day10.part1 input

      result `shouldBe` 26397

  describe "part2" $ do
    it "day 10 part 2 works" $ do
      let result = Day10.part2 input

      result `shouldBe` 288957

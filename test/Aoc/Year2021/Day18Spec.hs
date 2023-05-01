module Aoc.Year2021.Day18Spec
  ( spec,
  )
where

import Aoc.Year2021.Day18 qualified as Day18
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
|]

spec :: Spec
spec = do
  describe "part1" $ do
    it "day 18 part 1 works" $ do
      let result = Day18.part1 input

      result `shouldBe` 4140

  describe "part2" $ do
    it "day 18 part 2 works" $ do
      let result = Day18.part2 input

      result `shouldBe` 3993

module Aoc.Year2022.Day13Spec
  ( spec,
  )
where

import Aoc.Year2022.Day13 qualified as Day13
import Test.Hspec (Spec, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]
|]

spec :: Spec
spec = do
  it "2022 day 13 part 1 works" do
    let result = Day13.part1 input

    result `shouldBe` 13

  it "2022 day 13 part 2 works" do
    let result = Day13.part2 input

    result `shouldBe` Just 140

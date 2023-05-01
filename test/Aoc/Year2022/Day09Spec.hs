module Aoc.Year2022.Day09Spec
  ( spec,
  )
where

import Aoc.Year2022.Day09 qualified as Day09
import Test.Hspec (Spec, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
|]

input2 :: String
input2 =
  [r|R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
|]

spec :: Spec
spec = do
  it "2022 day 9 part 1 works" $ do
    let result = Day09.part1 input

    result `shouldBe` 13

  it "2022 day 9 part 2 works" $ do
    let result = Day09.part2 input

    result `shouldBe` 1

  it "2022 day 9 part 2 works for second input" $ do
    let result = Day09.part2 input2

    result `shouldBe` 36

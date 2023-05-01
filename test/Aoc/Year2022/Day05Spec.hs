module Aoc.Year2022.Day05Spec
  ( spec,
  )
where

import Aoc.Year2022.Day05 qualified as Day05
import Test.Hspec (Spec, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
|]

spec :: Spec
spec = do
  it "2022 day 5 part 1 works" $ do
    let result = Day05.part1 input

    result `shouldBe` "CMZ"

  it "2022 day 5 part 2 works" $ do
    let result = Day05.part2 input

    result `shouldBe` "MCD"

module Aoc.Year2022.Day01Spec
  ( spec,
  )
where

import Aoc.Year2022.Day01 qualified as Day01
import Test.Hspec (Spec, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|1000
2000
3000

4000

5000
6000

7000
8000
9000

10000|]

spec :: Spec
spec = do
  it "2022 day 1 part 1 works" $ do
    let result = Day01.part1 input

    result `shouldBe` 24000

  it "2022 day 1 part 2 works" $ do
    let result = Day01.part2 input

    result `shouldBe` 45000

module Aoc.Year2022.Day07Spec
  ( spec,
  )
where

import Aoc.Year2022.Day07 qualified as Day07
import Test.Hspec (Spec, it, shouldBe)
import Text.RawString.QQ (r)

input :: String
input =
  [r|$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
|]

spec :: Spec
spec = do
  it "2022 day 7 part 1 works" do
    let result = Day07.part1 input

    result `shouldBe` 95437

  it "2022 day 7 part 2 works" do
    let result = Day07.part2 input

    result `shouldBe` 24933642

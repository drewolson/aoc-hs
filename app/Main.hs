module Main
  ( main,
  )
where

import Aoc.Runner.Args (Args (..))
import Aoc.Runner.Args qualified as Args
import Aoc.Runner.Year2021 qualified as Year2021
import Aoc.Runner.Year2022 qualified as Year2022
import Text.Printf qualified as Printf

readInput :: Args -> IO String
readInput Args {year, day} = readFile $ Printf.printf "./data/%i/day%02i.txt" year day

runSolution :: String -> Args -> IO ()
runSolution input args =
  case args of
    Args {year = 2021} -> Year2021.run input args
    Args {year = 2022} -> Year2022.run input args
    _ -> fail "unknown year"

main :: IO ()
main = do
  args <- Args.parse
  input <- readInput args

  runSolution input args

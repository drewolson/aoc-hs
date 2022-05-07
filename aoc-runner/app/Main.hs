module Main
  ( main,
  )
where

import Aoc.Runner.Args (Args (..), Args')
import Aoc.Runner.Year2021 qualified as Year2021
import Options.Generic (unwrapRecord)

readInput :: Args' -> IO String
readInput Args {year, day} = readFile $ "./data/" <> show year <> "/day" <> paddedDay <> ".txt"
  where
    paddedDay :: String
    paddedDay =
      case show day of
        [d] -> ['0', d]
        d -> d

runSolution :: String -> Args' -> IO ()
runSolution input args =
  case args of
    Args {year = 2021} -> Year2021.run input args
    _ -> fail "unknown year"

main :: IO ()
main = do
  args <- unwrapRecord "run aoc solution"
  input <- readInput args

  runSolution input args

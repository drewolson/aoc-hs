module Main
  ( main,
  )
where

import Aoc.Runner.Args (Args (..), Args')
import Aoc.Runner.Year2021 qualified as Year2021
import Aoc.Runner.Year2022 qualified as Year2022
import Data.Text qualified as T
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
    Args {year = 2022} -> Year2022.run input args
    _ -> fail "unknown year"

main :: IO ()
main = do
  args <- unwrapRecord $ T.pack "run aoc solution"
  input <- readInput args

  runSolution input args

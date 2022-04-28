module Main
  ( main,
  )
where

import Args (Args (..), Args')
import Data.Text qualified as T
import Options.Generic (unwrapRecord)
import Runner.Year2021 qualified as Year2021

readInput :: Args' -> IO String
readInput Args {year, day} =
  let dayText = T.justifyRight 2 '0' $ T.pack $ show day
   in readFile $ "./data/" <> show year <> "/day" <> T.unpack dayText <> ".txt"

runSolution :: String -> Args' -> IO ()
runSolution input args =
  case args of
    Args {year = 2021} -> Year2021.run input args
    _ -> fail "unknown year"

main :: IO ()
main = do
  args <- unwrapRecord $ T.pack "run aoc solution"
  input <- readInput args

  runSolution input args

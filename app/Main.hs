module Main where

import Aoc.Prelude
import Aoc.Solution.Day01 qualified as Day01
import Control.Applicative ((<**>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as Text.IO
import Options.Applicative (Parser, ParserInfo)
import Options.Applicative qualified as A

data Args = Args
  { day :: Int,
    part :: Int
  }

parseArgs :: Parser Args
parseArgs =
  Args
    <$> A.option
      A.auto
      ( A.long "day"
          <> A.short 'd'
          <> A.help "day to run (1 - 25)"
      )
    <*> A.option
      A.auto
      ( A.long "part"
          <> A.short 'p'
          <> A.help "part to run (1 or 2)"
      )

parseInfoArgs :: ParserInfo Args
parseInfoArgs =
  A.info
    (parseArgs <**> A.helper)
    ( A.fullDesc
        <> A.progDesc "run aoc solution"
        <> A.header "aoc-exe - run aoc solution"
    )

readInput :: Args -> IO Text
readInput Args {day} =
  let dayText = T.justifyRight 2 '0' $ tshow day
   in Text.IO.readFile $ "./data/day" <> T.unpack dayText <> ".txt"

runSolution :: Text -> Args -> IO Text
runSolution input = \case
  Args {day = 1, part = 1} -> Day01.part1 input
  _ -> fail "unknown day and part"

main :: IO ()
main = do
  args <- A.execParser parseInfoArgs
  input <- readInput args
  result <- runSolution input args

  Text.IO.putStrLn result

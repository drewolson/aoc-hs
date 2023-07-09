module Aoc.Runner.Args
  ( Args (..),
    parse,
  )
where

import Options.Applicative
  ( Mod,
    OptionFields,
    Parser,
    ParserInfo,
    (<**>),
  )
import Options.Applicative qualified as OA

data Args = Args
  { year :: Int,
    day :: Int,
    part :: Int
  }

autoOpt :: (Read a) => Mod OptionFields a -> Parser a
autoOpt = OA.option OA.auto

argsP :: Parser Args
argsP =
  Args
    <$> autoOpt
      ( OA.long "year"
          <> OA.short 'y'
          <> OA.metavar "YEAR"
          <> OA.value 2022
          <> OA.showDefault
          <> OA.help "Year to run"
      )
    <*> autoOpt
      ( OA.long "day"
          <> OA.short 'd'
          <> OA.metavar "DAY"
          <> OA.help "Day to run (1 - 25)"
      )
    <*> autoOpt
      ( OA.long "part"
          <> OA.short 'p'
          <> OA.metavar "PART"
          <> OA.help "Part to run (1 or 2)"
      )

opts :: ParserInfo Args
opts =
  OA.info
    (argsP <**> OA.helper)
    ( OA.fullDesc
        <> OA.progDesc "Run aoc solution for YEAR, DAY, and PART"
        <> OA.header "aoc -- run an aoc solution"
    )

parse :: IO Args
parse = OA.execParser opts

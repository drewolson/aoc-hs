module Aoc.Runner.Args
  ( Args (..),
    parse,
  )
where

import Options.Applicative (Parser, ParserInfo, (<**>))
import Options.Applicative qualified as OA

data Args = Args
  { year :: Int,
    day :: Int,
    part :: Int
  }

argsP :: Parser Args
argsP =
  let yearOpt =
        OA.long "year"
          <> OA.short 'y'
          <> OA.metavar "YEAR"
          <> OA.value 2022
          <> OA.showDefault
          <> OA.help "Year to run"

      dayOpt =
        OA.long "day"
          <> OA.short 'd'
          <> OA.metavar "DAY"
          <> OA.help "Day to run (1 - 25)"

      partOpt =
        OA.long "part"
          <> OA.short 'p'
          <> OA.metavar "PART"
          <> OA.help "Part to run (1 or 2)"
   in Args
        <$> OA.option OA.auto yearOpt
        <*> OA.option OA.auto dayOpt
        <*> OA.option OA.auto partOpt

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

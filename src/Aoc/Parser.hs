module Aoc.Parser
  ( Parser,
    parseInt,
    runParser,
    runParser',
  )
where

import Data.Bifunctor (first)
import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, parse, some)
import Text.Megaparsec.Char (digitChar)

type Parser = Parsec Void String

parseInt :: Parser Int
parseInt = read <$> some digitChar

runParser :: Parser a -> String -> Either String a
runParser p = first errorBundlePretty . parse p ""

runParser' :: Parser a -> String -> a
runParser' p input =
  case runParser p input of
    Left e -> error e
    Right a -> a

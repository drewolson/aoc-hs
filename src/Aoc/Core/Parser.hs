module Aoc.Core.Parser
  ( Parser,
    dropLineP,
    signedIntP,
    runParser,
  )
where

import Control.Monad (void)
import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, parse, takeWhileP)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (decimal, signed)

type Parser = Parsec Void String

dropLineP :: Parser ()
dropLineP = void $ takeWhileP Nothing (/= '\n') *> newline

signedIntP :: Parser Int
signedIntP = signed (pure ()) decimal

runParser :: Parser a -> String -> a
runParser p input =
  case parse p "" input of
    Left e -> error $ errorBundlePretty e
    Right a -> a

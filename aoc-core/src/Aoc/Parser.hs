module Aoc.Parser
  ( Parser,
    dropLineP,
    intP,
    signedIntP,
    runParser,
    runParser',
  )
where

import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (takeWhileP), Parsec, errorBundlePretty, parse)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (decimal, signed)

type Parser = Parsec Void String

dropLineP :: Parser ()
dropLineP = void $ takeWhileP Nothing (/= '\n') *> newline

intP :: Parser Int
intP = decimal

signedIntP :: Parser Int
signedIntP = signed (pure ()) decimal

runParser :: Parser a -> String -> Either String a
runParser p = first errorBundlePretty . parse p ""

runParser' :: Parser a -> String -> a
runParser' p input =
  case runParser p input of
    Left e -> error e
    Right a -> a

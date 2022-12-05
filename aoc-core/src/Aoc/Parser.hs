module Aoc.Parser
  ( Parser,
    dropLineP,
    intP,
    signedIntP,
    runParser,
    runParser',
  )
where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (takeWhileP), Parsec, errorBundlePretty, option, parse, some)
import Text.Megaparsec.Char (char, digitChar, newline)

type Parser = Parsec Void String

dropLineP :: Parser ()
dropLineP = void $ takeWhileP Nothing (/= '\n') *> newline

intP :: Parser Int
intP = read <$> some digitChar

signedIntP :: Parser Int
signedIntP = do
  mult <- option 1 (-1 <$ char '-' <|> 1 <$ char '+')

  (* mult) <$> intP

runParser :: Parser a -> String -> Either String a
runParser p = first errorBundlePretty . parse p ""

runParser' :: Parser a -> String -> a
runParser' p input =
  case runParser p input of
    Left e -> error e
    Right a -> a

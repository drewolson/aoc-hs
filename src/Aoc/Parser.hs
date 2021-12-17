module Aoc.Parser
  ( Parser,
    parseInt,
    parseSignedInt,
    runParser,
    runParser',
  )
where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, option, parse, some)
import Text.Megaparsec.Char (char, digitChar)

type Parser = Parsec Void String

parseSignedInt :: Parser Int
parseSignedInt = do
  mult <- option 1 (-1 <$ char '-' <|> 1 <$ char '+')

  (* mult) <$> parseInt

parseInt :: Parser Int
parseInt = read <$> some digitChar

runParser :: Parser a -> String -> Either String a
runParser p = first errorBundlePretty . parse p ""

runParser' :: Parser a -> String -> a
runParser' p input =
  case runParser p input of
    Left e -> error e
    Right a -> a

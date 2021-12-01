module Aoc.Parser
  ( Parser,
    parse,
  )
where

import Data.Bifunctor (first)
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as M

type Parser = Parsec Void String

parse :: Parser a -> String -> Either String a
parse p = first M.errorBundlePretty . M.parse p ""

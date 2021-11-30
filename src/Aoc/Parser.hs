module Aoc.Parser
  ( Parser,
    parse,
  )
where

import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as M

type Parser = Parsec Void Text

parse :: Parser a -> Text -> Either Text a
parse p = first (T.pack . M.errorBundlePretty) . M.parse p ""

module Aoc.Parser
  ( Parser,
    parse,
  )
where

import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, ShowErrorComponent, TraversableStream, VisualStream)
import Text.Megaparsec qualified as M

type Parser = Parsec Void Text

parse ::
  ( TraversableStream s,
    VisualStream s,
    ShowErrorComponent e
  ) =>
  Parsec e s a ->
  s ->
  Either Text a
parse p s = first (T.pack . M.errorBundlePretty) $ M.parse p "" s

module Aoc.Prelude
  ( tshow,
  )
where

import Data.Text (Text)
import Data.Text qualified as T

tshow :: Show a => a -> Text
tshow = T.pack . show

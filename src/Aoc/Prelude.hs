module Aoc.Prelude
  ( readAll,
    tshow,
  )
where

import Data.Maybe qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read qualified as R

tshow :: Show a => a -> Text
tshow = T.pack . show

readAll :: Read a => String -> [a]
readAll = M.mapMaybe R.readMaybe . lines

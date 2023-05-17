module Aoc.Parallel
  ( pmap,
  )
where

import Control.Parallel.Strategies (parTraversable, rpar, withStrategy)

pmap :: Traversable f => (a -> b) -> f a -> f b
pmap f = withStrategy (parTraversable rpar) . fmap f

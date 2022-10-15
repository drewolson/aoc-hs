module Aoc.Runner.Args
  ( Args (..),
    Args',
  )
where

import GHC.Generics (Generic)
import Options.Generic
  ( ParseRecord,
    Unwrapped,
    Wrapped,
    type (:::),
    type (<!>),
    type (<#>),
    type (<?>),
  )

data Args w = Args
  { year :: w ::: Int <?> "year to run" <#> "y" <!> "2022",
    day :: w ::: Int <?> "day to run (1 - 25)" <#> "d",
    part :: w ::: Int <?> "part to run (1 or 2)" <#> "p"
  }
  deriving (Generic)

instance ParseRecord (Args Wrapped)

deriving instance Show (Args Unwrapped)

type Args' = Args Unwrapped

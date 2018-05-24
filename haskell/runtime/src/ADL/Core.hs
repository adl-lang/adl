module ADL.Core(
  module ADL.Core.Value,
  StringMap(..),
  stringMapFromList
  )
  where

import ADL.Core.Value
import ADL.Core.StringMap(StringMap, fromList)

stringMapFromList = fromList

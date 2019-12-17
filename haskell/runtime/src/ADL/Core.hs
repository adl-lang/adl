module ADL.Core(
  module ADL.Core.Value,
  StringMap(..),
  stringMapFromList,
  TypeToken(..),
  )
  where

import ADL.Core.Value
import ADL.Core.StringMap(StringMap, fromList)
import ADL.Core.TypeToken(TypeToken)

stringMapFromList = fromList

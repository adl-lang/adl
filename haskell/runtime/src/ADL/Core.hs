module ADL.Core(
  module ADL.Core.Value,
  mapFromMapEntryList,
  StringMap(..),
  stringMapFromList,
  TypeToken(..),
  )
  where

import ADL.Core.Value
import ADL.Core.StringMap(StringMap, fromList)
import ADL.Core.Map(mapFromMapEntryList)
import ADL.Core.TypeToken(TypeToken)
import qualified Data.Map as M

stringMapFromList = fromList


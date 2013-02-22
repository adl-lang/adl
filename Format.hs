module Format where

import qualified Data.Text as T

-- | Convert a value to a string for user presentation.
-- At least one of format, formatText must be defined.
class Format a where

  format :: a -> String
  format = T.unpack . formatText

  formatText :: a -> T.Text
  formatText = T.pack . format

instance Format T.Text where
  formatText = id

fshow :: (Show a) => a -> T.Text
fshow = T.pack . show

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

-- | `template src substs` will replace all occurences the string $i
-- in src with `substs !! i`
template :: T.Text -> [T.Text] -> T.Text
template t substs = foldr replace t (zip [1,2..] substs)
  where
    replace (i,s) t = T.replace (T.pack ('$':show i)) s t


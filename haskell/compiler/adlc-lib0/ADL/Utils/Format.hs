{-# LANGUAGE OverloadedStrings #-}
module ADL.Utils.Format where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe(fromMaybe)
import Data.Char(isDigit, ord)
import Data.List((!?))

-- | Convert a value to a string for user presentation.
-- At least one of format, formatText must be defined.
class Format a where

  format :: a -> String
  format = T.unpack . formatText

  formatText :: a -> T.Text
  formatText = T.pack . format

instance Format T.Text where
  formatText = id

instance Format LT.Text where
  formatText = LT.toStrict

instance Format BS.ByteString where
  formatText = T.decodeUtf8With TEE.lenientDecode

instance Format LBS.ByteString where
  formatText = T.decodeUtf8With TEE.lenientDecode . BS.concat . LBS.toChunks


fshow :: (Show a) => a -> T.Text
fshow = T.pack . show

-- | `template src substs` will replace all occurences the string $i
-- in src with `substs !! i`
template :: T.Text -> [T.Text] -> T.Text
template t substs = textFromTState (T.foldl' process initialTState t)
  where
    initialTState :: TState
    initialTState = TState mempty Nothing
    
    process :: (TState -> Char -> TState)
    process (TState lb Nothing) '$' = TState lb (Just 0)
    process (TState lb Nothing) c = TState (lb <> LB.singleton c) Nothing
    process (TState lb (Just n)) c
        | isDigit c = TState (lb) (Just (n*10 + (ord c - ord '0')))
        | c == '$' = TState (lb <> LB.fromText (substitution n)) (Just 0)
        | otherwise = TState (lb <> LB.fromText (substitution n) <> LB.singleton c) Nothing

    textFromTState :: TState -> T.Text
    textFromTState (TState lb Nothing) = LT.toStrict (LB.toLazyText lb)
    textFromTState (TState lb (Just n)) = LT.toStrict (LB.toLazyText lb) <> substitution n

    substitution :: Int -> T.Text
    substitution n
        | n == 0 = "$"
        | otherwise = fromMaybe (T.pack ('$':show n)) (substs !? (n-1))

data TState = TState LB.Builder (Maybe Int)

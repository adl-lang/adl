{-# LANGUAGE OverloadedStrings #-}
module ADL.Test where

import ADL.Core
import Control.Applicative( (<$>), (<*>) )
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Prelude
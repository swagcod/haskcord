module Haskcord.Resource.Base
where

import Data.Aeson
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

class Resource a where
    syncResource :: a -> H.HashMap T.Text Value -> a

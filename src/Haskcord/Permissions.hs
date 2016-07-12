{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Haskcord.Permissions
where

import Control.Lens
import Data.Aeson
import qualified Data.Text as T
import Data.Word

import Haskcord.Resource.Id

newtype PermissionBits = PermissionBits Word32
    deriving (Read, Show, Eq, Ord, FromJSON, ToJSON)

-- | Represents an overridden permission in a guild channel.
--
-- This is not a first-class resource and is not synced directly
-- (but is instead a component of GuildChannelResource)
data Overwrite = Overwrite {
      _overwriteId :: OverwriteId
    , _overwriteType :: T.Text
    , _overwriteAllow :: PermissionBits
    , _overwriteDeny :: PermissionBits
    }
    deriving (Show, Read, Eq)

makeLenses ''PermissionBits

instance FromJSON Overwrite where
    parseJSON = withObject "Overwrite" $ \v -> Overwrite
        <$> v .: "id"
        <*> v .: "type"
        <*> v .: "allow"
        <*> v .: "deny"
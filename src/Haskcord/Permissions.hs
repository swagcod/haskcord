{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Haskcord.Permissions
where

import Control.Lens
import Data.Aeson

import Haskcord.Resource.Id

newtype PermissionBits = PermissionBits Word32
    deriving (From, Show, Eq, Ord, FromJSON, ToJSON)

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

makeLenses ''permissionBits

instance FromJSON Overwrite where
    parseJSON (Object v) = Overwrite
        <$> v .: "id"
        <*> v .: "type"
        <*> v .: "allow"
        <*> v .: "deny"
    parseJSON _ = empty

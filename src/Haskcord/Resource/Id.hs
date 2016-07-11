{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | TODO Figure out a good spot for these types.
module Haskcord.Resource.Id
where

import Data.Aeson

newtype UserId = UserId Word64
    deriving (Show, Read, Eq, Ord, FromJSON, ToJSON)

newtype GuildId = GuildId Word64
    deriving (Show, Read, Eq, Ord, FromJSON, ToJSON)

newtype GuildChannelId = GuildChannelId Word64
    deriving (Show, Read, Eq, Ord, FromJSON, ToJSON)

newtype MessageId = MessageId Word64
    deriving (Show, Read, Eq, Ord, FromJSON, ToJSON)

-- | Either a UserId or a RoleId.
newtype OverrideId = OverrideId Word64
    deriving (Show, Read, Eq, Ord, FromJSON, ToJSON)

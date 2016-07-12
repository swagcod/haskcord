{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | TODO Figure out a good spot for these types.
module Haskcord.Resource.Id
where

import Data.Aeson
import Data.Aeson.Types
import Data.Word
import qualified Data.Text as T
import Text.Read

newtype UserId = UserId Word64
    deriving (Show, Read, Eq, Ord)

newtype GuildId = GuildId Word64
    deriving (Show, Read, Eq, Ord)

newtype GuildChannelId = GuildChannelId Word64
    deriving (Show, Read, Eq, Ord)

newtype MessageId = MessageId Word64
    deriving (Show, Read, Eq, Ord)

-- | Either a UserId or a RoleId.
newtype OverrideId = OverrideId Word64
    deriving (Show, Read, Eq, Ord)


-- | The builtin instance for Word64 tries to read Numbers.
parseId :: Value -> Parser Word64
parseId = withText "discord id" $ \text -> case readMaybe (T.unpack text) of
    Just val -> return val
    Nothing -> fail "failed to parse id"


instance FromJSON UserId where
    parseJSON = fmap UserId . parseId

instance FromJSON GuildId where
    parseJSON = fmap GuildId . parseId

instance FromJSON GuildChannelId where
    parseJSON = fmap GuildChannelId . parseId
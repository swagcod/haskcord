{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Haskcord.Resource.User
where

import Control.Lens
import Data.Aeson
import Data.Word

import Haskcord.User.Resource

data UserEntity = UserEntity {
      _userEntityId :: UserId
    , _userEntityName :: T.Text
    , _userEntityDiscriminator :: T.Text
    , _userEntityAvatarHash :: T.Text
    , _userEntityBot :: Bool
    , _userEntityMfaEnabled :: Bool
    , _userEntityVerifiedEmail :: Maybe Bool
    , _userEntityEmail :: Maybe T.Text
    }
    deriving (Show, Read, Eq)

class FromJSON UserEntity where
    parseJSON (Object v) = UserEntity
        <$> v .: "id"
        <*> v .: "username"
        <*> v .: "discriminator"
        <*> v .: "avatar"
        <*> v .: "bot"
        <*> v .: "mfa_enabled"
        <*> v .:? "verified"
        <*> v .:? "email"
    parseJSON _ = empty

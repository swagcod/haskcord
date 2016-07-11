{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Haskcord.Resource.User
where

import Control.Lens
import Data.Aeson
import Data.HashMap (lookup)
import Data.Word

import Haskcord.Resource.Base
import Haskcord.Resource.Id

data UserResource = UserResource {
      _userResourceId :: UserId
    , _userResourceName :: T.Text
    , _userResourceDiscriminator :: T.Text
    , _userResourceAvatarHash :: T.Text
    , _userResourceBot :: Bool
    , _userResourceMfaEnabled :: Bool
    , _userResourceVerifiedEmail :: Maybe Bool
    , _userResourceEmail :: Maybe T.Text
    }
    deriving (Show, Read, Eq)

class FromJSON UserResource where
    parseJSON (Object v) = UserResource
        <$> v .: "id"
        <*> v .: "username"
        <*> v .: "discriminator"
        <*> v .: "avatar"
        <*> v .: "bot"
        <*> v .: "mfa_enabled"
        <*> v .:? "verified"
        <*> v .:? "email"
    parseJSON _ = empty

instance Resource UserResource where
    syncResource user v = UserResource {
          _userResourceId = userResourceId user
        , _userResourceName = fromMaybe (view userResourceName user) (lookup "username" v)
        , _userResourceDiscriminator = fromMaybe (view userResourceDiscriminator user) (lookup "discriminator" v)
        , _userResourceAvatarHash = fromMaybe (view userResourceAvatarHash user) (lookup "avatar" v)
        , _userResourceBot = fromMaybe (view userResourceBot user) (lookup "bot" v)
        , _userResourceMfaEnabled = fromMaybe (view userResourceMfaEnabled user) (lookup "mfa_enabled" v)
        , _userResourceVerifiedEmail = fromMaybe (view userResourceVerifiedEmail user) (Just <$> lookup "verified" v)
        , _userResourceEmail = fromMaybe (view userEmail user) (Just <$> lookup "email" v)
        }

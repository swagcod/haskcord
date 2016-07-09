{-# LANGUAGE OverloadedStrings #-}
-- | Wrapper around Network.Wreq for servicing requests to Discord's API.
module Haskcord.Request
    ( get
    , W.Status
    )
where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Except
import qualified Data.Aeson as A
import qualified Network.Wreq as W

import Haskcord.Token

get :: A.FromJSON a => String -> Token -> ExceptT W.Status IO a
get url token = do
    wResponse <- liftIO $ W.getWith (authorizationOptions token) url
    if wResponse ^. W.responseStatus . W.statusCode == 200
        then case A.decode $ wResponse ^. W.responseBody of
                Just response -> return response
                Nothing -> error "Internal parse error. This is a bug in Haskcord."
        else throwError $ wResponse ^. W.responseStatus

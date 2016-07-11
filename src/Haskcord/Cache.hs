{-# LANGUAGE TemplateHaskell #-}
-- | Internal module that handles the caching of Discord resources.
--
-- Every object is indexed by its internal discord identifier.
module Haskcord.Cache
where

-- TODO Make a type synonym for ReaderT cache.

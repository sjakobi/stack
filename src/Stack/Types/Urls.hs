{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Types.Urls where

import Control.Applicative
import Data.Aeson.Extended
import Data.Monoid
import GHC.Generics (Generic)
import Generics.Deriving.Monoid (memptydefault, mappenddefault)
import Network.HTTP.URL
import Prelude

data Urls = Urls
    { urlsLatestSnapshot :: !HttpUrl
    , urlsLtsBuildPlans :: !HttpUrl
    , urlsNightlyBuildPlans :: !HttpUrl
    }
    deriving Show

data UrlsMonoid = UrlsMonoid
    { urlsMonoidLatestSnapshot :: !(First HttpUrl)
    , urlsMonoidLtsBuildPlans :: !(First HttpUrl)
    , urlsMonoidNightlyBuildPlans :: !(First HttpUrl)
    }
    deriving (Show, Generic)

instance FromJSON (WithJSONWarnings UrlsMonoid) where
    parseJSON = withObjectWarnings "UrlsMonoid" $ \o -> do
        UrlsMonoid
            <$> o ..: "latest-snapshot"
            <*> o ..: "lts-build-plans"
            <*> o ..: "nightly-build-plans"

instance Monoid UrlsMonoid where
    mempty = memptydefault
    mappend = mappenddefault

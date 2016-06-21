{-# LANGUAGE OverloadedStrings #-}

module Stack.Config.Urls (urlsFromMonoid) where

import           Stack.Types
import           Data.Maybe
import           Data.Monoid.Extra
import           Network.HTTP.URL

urlsFromMonoid :: UrlsMonoid -> Urls
urlsFromMonoid monoid =
    Urls
        (fromFirst defaultLatestSnapshot    $ urlsMonoidLatestSnapshot    monoid)
        (fromFirst defaultLtsBuildPlans     $ urlsMonoidLtsBuildPlans     monoid)
        (fromFirst defaultNightlyBuildPlans $ urlsMonoidNightlyBuildPlans monoid)
    where
    defaultLatestSnapshot =
        makeUrl "https://www.stackage.org/download/snapshots.json"
    defaultLtsBuildPlans =
        makeUrl "https://raw.githubusercontent.com/fpco/lts-haskell/master/"
    defaultNightlyBuildPlans =
        makeUrl "https://raw.githubusercontent.com/fpco/stackage-nightly/master/"
    makeUrl = fromJust . parseHttpUrl

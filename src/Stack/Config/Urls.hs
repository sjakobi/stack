{-# LANGUAGE OverloadedStrings #-}

module Stack.Config.Urls (urlsFromMonoid) where

import           Data.Monoid.Extra
import           Imports
import           Prelude ()
import           Stack.Types

urlsFromMonoid :: UrlsMonoid -> Urls
urlsFromMonoid monoid =
    Urls
        (fromFirst defaultLatestSnapshot    $ urlsMonoidLatestSnapshot    monoid)
        (fromFirst defaultLtsBuildPlans     $ urlsMonoidLtsBuildPlans     monoid)
        (fromFirst defaultNightlyBuildPlans $ urlsMonoidNightlyBuildPlans monoid)
    where
    defaultLatestSnapshot =
        "https://www.stackage.org/download/snapshots.json"
    defaultLtsBuildPlans =
        "https://raw.githubusercontent.com/fpco/lts-haskell/master/"
    defaultNightlyBuildPlans =
        "https://raw.githubusercontent.com/fpco/stackage-nightly/master/"

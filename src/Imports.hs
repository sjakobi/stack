module Imports
    ( -- * Commonly used modules
      module Prelude.Compat
    , module Control.Applicative
    , module Control.Arrow
    , module Control.Monad.Compat
    , module Control.Monad.IO.Class
    , module Control.Monad.Logger
    , module Data.Char
    , module Data.Either.Compat
    , module Data.Foldable.Compat
    , module Data.Function.Compat
    , module Data.IORef
    , module Data.List.Compat
    , module Data.Maybe
    , module Data.Monoid.Compat
    , module Data.Traversable
    , module Data.Tuple
    , module Data.Typeable
    , module Path
    , module Path.IO

      -- * Redefinitions for pre-AMP compatibility
    , when
    , unless
    , filterM

      -- * Selected re-exports from base and base-compat
    , assert
    , Exception(..)
    , SomeException(..)
    , Generic

      -- * Common type classes
    , MonadReader
    , ask
    , asks
    , runReaderT
    , MonadThrow(..)
    , MonadCatch(..)
    , MonadMask(..)
    , MonadBaseControl(..)
    , HasHttpManager(..)
    , Hashable

      -- * Common data types
    , ByteString
    , IntMap
    , Map
    , NonEmpty(..)
    , Set
    , Text
    , Vector
    ) where

import Control.Applicative
import Control.Arrow hiding (loop)
import Control.Exception
import Control.Monad.Catch
import Control.Monad.Compat hiding (when, unless, filterM)
import qualified Control.Monad as Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader (MonadReader(..), asks, runReaderT)
import Control.Monad.Trans.Control
import Data.ByteString
import Data.Char
import Data.Either.Compat
import Data.Foldable.Compat
import Data.Function.Compat
import Data.Hashable
import Data.IntMap
import Data.IORef
import Data.List.Compat
import Data.List.NonEmpty
import Data.Map
import Data.Maybe
import Data.Monoid.Compat
import Data.Orphans ()
import Data.Set
import Data.Text
import Data.Traversable
import Data.Tuple
import Data.Typeable
import Data.Vector (Vector)
import GHC.Generics
import Network.HTTP.Client.Conduit
import Path
import Path.IO
import Prelude.Compat

-- | The 'Monad' constraint is necessary to use these functions in monadic
-- environments under GHC-7.8. The alternative of adding an 'Applicative'
-- constraint to the context would result in a redundant constraint warning
-- under GHC-8.0.
when, unless :: Monad m => Bool -> m () -> m ()
when = Monad.when
unless = Monad.unless

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM = Monad.filterM

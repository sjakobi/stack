{-# LANGUAGE DeriveDataTypeable #-}

-- | Clean a project.
module Stack.Clean
    (clean
    ,CleanOpts(..)
    ,StackCleanException(..)
    ) where

import           Control.Exception (Exception)
import           Control.Monad.Catch (MonadCatch, throwM)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Logger (MonadLogger)
import           Control.Monad.Reader (MonadReader)
import           Data.Foldable (forM_)
import           Data.List ((\\),intercalate)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Typeable (Typeable)
import           Path (Path,Rel,Dir,(</>))
import           Path.IO (ignoringAbsence,removeDirRecur)
import           Stack.Build.Source (getLocalPackageViews)
import           Stack.Build.Target (LocalPackageView(..))
import           Stack.Constants (distRelativeDir)
import           Stack.Types (HasBuildConfig,HasEnvConfig,PackageName
                             ,configProjectWorkDir,getWorkDir)

-- | Clean a project according to the given 'CleanOpts'.
--
-- Throws 'StackCleanException'.
clean
    :: (MonadCatch m,MonadIO m,MonadReader env m,HasEnvConfig env,MonadLogger m)
    => CleanOpts
    -> m ()
clean (CleanTargets targets) = do
    distDir <- distRelativeDir
    cleanProjectPackages distDir targets
clean CleanFull = do
    workDir <- getWorkDir
    cleanProjectPackages workDir []
    deleteProjectWorkDir

cleanProjectPackages
    :: (MonadCatch m,MonadIO m,MonadReader env m,HasEnvConfig env,MonadLogger m)
    => Path Rel Dir -- ^ Directory to delete
    -> [PackageName] -- ^ Packages to clean. Cleans all project packages if empty.
    -> m ()
cleanProjectPackages deleteDir targets = do
    locals <- getLocalPackageViews
    let nonLocalTargets = targets \\ Map.keys locals
    case nonLocalTargets of
        [] -> do
            let targets' =
                    if null targets
                        then Map.elems locals
                        else mapMaybe (`Map.lookup` locals) targets
                pkgDirs = map (lpvRoot . fst) targets'
            forM_ pkgDirs $ \dir -> do
                (ignoringAbsence . removeDirRecur) (dir </> deleteDir)
        pkgs -> throwM (NonLocalPackages pkgs)

deleteProjectWorkDir
    :: (MonadCatch m,MonadIO m,MonadReader env m,HasBuildConfig env)
    => m ()
deleteProjectWorkDir = do
    workDir <- configProjectWorkDir
    ignoringAbsence (removeDirRecur workDir)

-- | Options for cleaning a project.
data CleanOpts
    = CleanTargets [PackageName]
    -- ^ Remove the @dist@ directories of the listed packages.
    -- If the list is empty, every local package should be cleaned.
    | CleanFull
    -- ^ Remove the entire work directories of all project packages
    -- as well as the main project work directory.
    deriving (Show)

-- | Exceptions during cleanup.
newtype StackCleanException
    = NonLocalPackages [PackageName]
    deriving (Typeable)

instance Show StackCleanException where
    show (NonLocalPackages pkgs) =
        "The following packages are not part of this project: " ++
        intercalate ", " (map show pkgs)

instance Exception StackCleanException

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Stack.Setup.Installed
    ( getCompilerVersion
    , markInstalled
    , unmarkInstalled
    , listInstalled
    , Tool (..)
    , toolString
    , toolNameString
    , parseToolText
    , ExtraDirs (..)
    , extraDirs
    , installDir
    ) where

import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Distribution.System (Platform (..))
import qualified Distribution.System as Cabal
import           Generics.Deriving.Monoid (mappenddefault, memptydefault)
import           Imports
import           Path.Extra (toFilePathNoTrailingSep)
import           Prelude ()
import           Stack.Types
import           System.Process.Read

data Tool
    = Tool PackageIdentifier -- ^ e.g. ghc-7.8.4, msys2-20150512
    | ToolGhcjs CompilerVersion -- ^ e.g. ghcjs-0.1.0_ghc-7.10.2

toolString :: Tool -> String
toolString (Tool ident) = packageIdentifierString ident
toolString (ToolGhcjs cv) = compilerVersionString cv

toolNameString :: Tool -> String
toolNameString (Tool ident) = packageNameString $ packageIdentifierName ident
toolNameString ToolGhcjs{} = "ghcjs"

parseToolText :: Text -> Maybe Tool
parseToolText (parseCompilerVersion -> Just (cv@GhcjsVersion{})) = Just (ToolGhcjs cv)
parseToolText (parsePackageIdentifierFromString . T.unpack -> Just pkgId) = Just (Tool pkgId)
parseToolText _ = Nothing

markInstalled :: (MonadIO m, MonadThrow m)
              => Path Abs Dir
              -> Tool
              -> m ()
markInstalled programsPath tool = do
    fpRel <- parseRelFile $ toolString tool ++ ".installed"
    liftIO $ writeFile (toFilePath $ programsPath </> fpRel) "installed"

unmarkInstalled :: (MonadIO m, MonadCatch m)
                => Path Abs Dir
                -> Tool
                -> m ()
unmarkInstalled programsPath tool = do
    fpRel <- parseRelFile $ toolString tool ++ ".installed"
    ignoringAbsence (removeFile $ programsPath </> fpRel)

listInstalled :: (MonadIO m, MonadThrow m)
              => Path Abs Dir
              -> m [Tool]
listInstalled programsPath = do
    ensureDir programsPath
    (_, files) <- listDir programsPath
    return $ mapMaybe toTool files
  where
    toTool fp = do
        x <- T.stripSuffix ".installed" $ T.pack $ toFilePath $ filename fp
        parseToolText x

getCompilerVersion :: (MonadLogger m, MonadCatch m, MonadBaseControl IO m, MonadIO m)
              => EnvOverride -> WhichCompiler -> m CompilerVersion
getCompilerVersion menv wc =
    case wc of
        Ghc -> do
            $logDebug "Asking GHC for its version"
            bs <- readProcessStdout Nothing menv "ghc" ["--numeric-version"]
            let (_, ghcVersion) = versionFromEnd bs
            GhcVersion <$> parseVersion (T.decodeUtf8 ghcVersion)
        Ghcjs -> do
            $logDebug "Asking GHCJS for its version"
            -- Output looks like
            --
            -- The Glorious Glasgow Haskell Compilation System for JavaScript, version 0.1.0 (GHC 7.10.2)
            bs <- readProcessStdout Nothing menv "ghcjs" ["--version"]
            let (rest, ghcVersion) = T.decodeUtf8 <$> versionFromEnd bs
                (_, ghcjsVersion) = T.decodeUtf8 <$> versionFromEnd rest
            GhcjsVersion <$> parseVersion ghcjsVersion <*> parseVersion ghcVersion
  where
    versionFromEnd = S8.spanEnd isValid . fst . S8.breakEnd isValid
    isValid c = c == '.' || ('0' <= c && c <= '9')

-- | Binary directories for the given installed package
extraDirs :: (MonadReader env m, HasConfig env, MonadThrow m, MonadLogger m)
          => Tool
          -> m ExtraDirs
extraDirs tool = do
    config <- asks getConfig
    dir <- installDir (configLocalPrograms config) tool
    case (configPlatform config, toolNameString tool) of
        (Platform _ Cabal.Windows, isGHC -> True) -> return mempty
            { edBins = goList
                [ dir </> $(mkRelDir "bin")
                , dir </> $(mkRelDir "mingw") </> $(mkRelDir "bin")
                ]
            }
        (Platform Cabal.I386 Cabal.Windows, "msys2") -> return mempty
            { edBins = goList
                [ dir </> $(mkRelDir "mingw32") </> $(mkRelDir "bin")
                , dir </> $(mkRelDir "usr") </> $(mkRelDir "bin")
                , dir </> $(mkRelDir "usr") </> $(mkRelDir "local") </> $(mkRelDir "bin")
                ]
            , edInclude = goList
                [ dir </> $(mkRelDir "mingw32") </> $(mkRelDir "include")
                ]
            , edLib = goList
                [ dir </> $(mkRelDir "mingw32") </> $(mkRelDir "lib")
                ]
            }
        (Platform Cabal.X86_64 Cabal.Windows, "msys2") -> return mempty
            { edBins = goList
                [ dir </> $(mkRelDir "mingw64") </> $(mkRelDir "bin")
                , dir </> $(mkRelDir "usr") </> $(mkRelDir "bin")
                , dir </> $(mkRelDir "usr") </> $(mkRelDir "local") </> $(mkRelDir "bin")
                ]
            , edInclude = goList
                [ dir </> $(mkRelDir "mingw64") </> $(mkRelDir "include")
                ]
            , edLib = goList
                [ dir </> $(mkRelDir "mingw64") </> $(mkRelDir "lib")
                ]
            }
        (_, isGHC -> True) -> return mempty
            { edBins = goList
                [ dir </> $(mkRelDir "bin")
                ]
            }
        (_, isGHCJS -> True) -> return mempty
            { edBins = goList
                [ dir </> $(mkRelDir "bin")
                ]
            }
        (Platform _ x, toolName) -> do
            $logWarn $ "binDirs: unexpected OS/tool combo: " <> T.pack (show (x, toolName))
            return mempty
  where
    goList = map toFilePathNoTrailingSep
    isGHC n = "ghc" == n || "ghc-" `isPrefixOf` n
    isGHCJS n = "ghcjs" == n

data ExtraDirs = ExtraDirs
    { edBins :: ![FilePath]
    , edInclude :: ![FilePath]
    , edLib :: ![FilePath]
    } deriving (Show, Generic)
instance Monoid ExtraDirs where
    mempty = memptydefault
    mappend = mappenddefault

installDir :: (MonadReader env m, MonadThrow m)
           => Path Abs Dir
           -> Tool
           -> m (Path Abs Dir)
installDir programsDir tool = do
    reldir <- parseRelDir $ toolString tool
    return $ programsDir </> reldir

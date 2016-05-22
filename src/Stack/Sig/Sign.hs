{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|
Module      : Stack.Sig.Sign
Description : Signing Packages
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Stack.Sig.Sign (sign, signPackage, signTarBytes) where

import Prelude ()
import Imports

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import           Network.HTTP.Conduit (Response(..), RequestBody(..),
                                       Request(..), httpLbs)
import           Network.HTTP.Client (Manager)
import           Network.HTTP.Download
import           Network.HTTP.Types (status200, methodPut)
import           Stack.Package
import           Stack.Sig.GPG
import           Stack.Types
import qualified System.FilePath as FP

-- | Sign a haskell package with the given url of the signature
-- service and a path to a tarball.
sign
    :: (MonadIO m, MonadLogger m, MonadMask m)
    => Manager -> String -> Path Abs File -> m Signature
sign manager url filePath =
    withSystemTempDir
        "stack"
        (\tempDir ->
              do bytes <-
                     liftIO
                         (fmap
                              GZip.decompress
                              (BS.readFile (toFilePath filePath)))
                 maybePath <- extractCabalFile tempDir (Tar.read bytes)
                 case maybePath of
                     Nothing -> throwM SigInvalidSDistTarBall
                     Just cabalPath -> do
                         pkg <- cabalFilePackageId (tempDir </> cabalPath)
                         signPackage manager url pkg filePath)
  where
    extractCabalFile tempDir (Tar.Next entry entries) =
        case Tar.entryContent entry of
            (Tar.NormalFile lbs _) ->
                case FP.splitFileName (Tar.entryPath entry) of
                    (folder,file)
                      | length (FP.splitDirectories folder) == 1 &&
                            FP.takeExtension file == ".cabal" -> do
                          cabalFile <- parseRelFile file
                          liftIO
                              (BS.writeFile
                                   (toFilePath (tempDir </> cabalFile))
                                   lbs)
                          return (Just cabalFile)
                    (_,_) -> extractCabalFile tempDir entries
            _ -> extractCabalFile tempDir entries
    extractCabalFile _ _ = return Nothing

-- | Sign a haskell package with the given url to the signature
-- service, a package tarball path (package tarball name) and a lazy
-- bytestring of bytes that represent the tarball bytestream.  The
-- function will write the bytes to the path in a temp dir and sign
-- the tarball with GPG.
signTarBytes
    :: (MonadIO m, MonadLogger m, MonadMask m)
    => Manager -> String -> Path Rel File -> L.ByteString -> m Signature
signTarBytes manager url tarPath bs =
    withSystemTempDir
        "stack"
        (\tempDir ->
              do let tempTarBall = tempDir </> tarPath
                 liftIO (L.writeFile (toFilePath tempTarBall) bs)
                 sign manager url tempTarBall)

-- | Sign a haskell package given the url to the signature service, a
-- @PackageIdentifier@ and a file path to the package on disk.
signPackage
    :: (MonadIO m, MonadLogger m, MonadThrow m)
    => Manager -> String -> PackageIdentifier -> Path Abs File -> m Signature
signPackage manager url pkg filePath = do
    sig@(Signature signature) <- gpgSign filePath
    let (PackageIdentifier name version) = pkg
    fingerprint <- gpgVerify sig filePath
    let fullUrl =
            url <> "/upload/signature/" <> show name <> "/" <> show version <>
            "/" <>
            show fingerprint
    req <- parseUrl fullUrl
    let put =
            req
            { method = methodPut
            , requestBody = RequestBodyBS signature
            }
    res <- liftIO (httpLbs put manager)
    when
        (responseStatus res /= status200)
        (throwM (GPGSignException "unable to sign & upload package"))
    $logInfo ("Signature uploaded to " <> T.pack fullUrl)
    return sig

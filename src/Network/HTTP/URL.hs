{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

-- | URLs with a HTTP or HTTPS scheme.
module Network.HTTP.URL where

import           Control.Exception
import           Control.Monad.Catch
import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import           Data.Text (Text)
import           Data.Typeable
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Lens.Micro
import           Network.HTTP.Client (Request)
import qualified Network.HTTP.Client as HTTPClient
import           Path
import qualified System.FilePath as FP
import           URI.ByteString (URIRef, Absolute, URIParseError)
import qualified URI.ByteString as URI

newtype HttpUrl = HttpUrl (URIRef Absolute)
    deriving (Eq, Ord, Show)

instance FromJSON HttpUrl where
    parseJSON = withText "HttpUrl" $ \t ->
        case parseHttpUrl t of
            Left e -> fail (show e)
            Right x -> return x

parseHttpUrl :: MonadThrow m => Text -> m HttpUrl
parseHttpUrl t =
    case URI.parseURI URI.strictURIParserOptions (Text.encodeUtf8 t) of
        Left e -> throwM (UriParseException t e)
        Right uri ->
            if URI.schemeBS (URI.uriScheme uri) `elem` ["http", "https"]
                then return (HttpUrl uri)
                else throwM (NonHttpScheme t)

appendPath :: HttpUrl -> Path Rel a -> HttpUrl
appendPath (HttpUrl uri) path = HttpUrl (over URI.pathL (`append` path) uri)
    where append bs p = BS.pack (BS.unpack bs FP.</> toFilePath p)

httpUrlText :: HttpUrl -> Text
httpUrlText (HttpUrl uri) = Text.decodeUtf8 (URI.serializeURIRef' uri)

httpUrlRequest :: HttpUrl -> Request
httpUrlRequest url =
    case HTTPClient.parseUrl (Text.unpack (httpUrlText url)) of
        Right req -> req
        Left e -> error $ concat
            [ "httpUrlRequest for "
            , show (httpUrlText url)
            , " went wrong: "
            , show e
            ]

data HttpUrlParseException
    = UriParseException Text URIParseError
    | NonHttpScheme Text
    deriving (Eq, Typeable)

instance Show HttpUrlParseException where
    show (UriParseException t e) =
        "Couldn't parse " ++ Text.unpack t ++ ": " ++ show e
    show (NonHttpScheme t) =
        "Expected \"http\" or \"https\" scheme: " ++ Text.unpack t

instance Exception HttpUrlParseException

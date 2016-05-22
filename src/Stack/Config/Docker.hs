{-# LANGUAGE CPP, DeriveDataTypeable, RecordWildCards, TemplateHaskell #-}

-- | Docker configuration
module Stack.Config.Docker where

import Control.Exception.Lifted
import Data.Monoid.Extra
import qualified Data.Text as T
import Distribution.Version (simplifyVersionRange)
import Imports
import Prelude ()
import Stack.Types

-- | Interprets DockerOptsMonoid options.
dockerOptsFromMonoid
    :: MonadThrow m
    => Maybe Project
    -> Path Abs Dir
    -> Maybe AbstractResolver
    -> DockerOptsMonoid
    -> m DockerOpts
dockerOptsFromMonoid mproject stackRoot maresolver DockerOptsMonoid{..} = do
    let dockerEnable =
            fromFirst (getAny dockerMonoidDefaultEnable) dockerMonoidEnable
        dockerImage =
            let mresolver =
                    case maresolver of
                        Just (ARResolver resolver) ->
                            Just resolver
                        Just aresolver ->
                            throw
                                (ResolverNotSupportedException $
                                 show aresolver)
                        Nothing ->
                            fmap projectResolver mproject
                defaultTag =
                    case mresolver of
                        Nothing -> ""
                        Just resolver ->
                            case resolver of
                                ResolverSnapshot n@(LTS _ _) ->
                                    ":" ++ T.unpack (renderSnapName n)
                                _ ->
                                    throw
                                        (ResolverNotSupportedException $
                                         show resolver)
            in case getFirst dockerMonoidRepoOrImage of
                   Nothing -> "fpco/stack-build" ++ defaultTag
                   Just (DockerMonoidImage image) -> image
                   Just (DockerMonoidRepo repo) ->
                       case find (`elem` (":@" :: String)) repo of
                           Just _    -- Repo already specified a tag or digest, so don't append default
                            ->
                               repo
                           Nothing -> repo ++ defaultTag
        dockerRegistryLogin =
            fromFirst
                (isJust (emptyToNothing (getFirst dockerMonoidRegistryUsername)))
                dockerMonoidRegistryLogin
        dockerRegistryUsername = emptyToNothing (getFirst dockerMonoidRegistryUsername)
        dockerRegistryPassword = emptyToNothing (getFirst dockerMonoidRegistryPassword)
        dockerAutoPull = fromFirst False dockerMonoidAutoPull
        dockerDetach = fromFirst False dockerMonoidDetach
        dockerPersist = fromFirst False dockerMonoidPersist
        dockerContainerName = emptyToNothing (getFirst dockerMonoidContainerName)
        dockerRunArgs = dockerMonoidRunArgs
        dockerMount = dockerMonoidMount
        dockerEnv = dockerMonoidEnv
        dockerSetUser = getFirst dockerMonoidSetUser
        dockerRequireDockerVersion =
            simplifyVersionRange (getIntersectingVersionRange dockerMonoidRequireDockerVersion)
    dockerDatabasePath <-
        case getFirst dockerMonoidDatabasePath of
            Nothing -> return $ stackRoot </> $(mkRelFile "docker.db")
            Just fp ->
                case parseAbsFile fp of
                    Left e -> throwM (InvalidDatabasePathException e)
                    Right p -> return p
    dockerStackExe <-
        case getFirst dockerMonoidStackExe of
            Just e -> liftM Just (parseDockerStackExe e)
            Nothing -> return Nothing
    return DockerOpts{..}
  where emptyToNothing Nothing = Nothing
        emptyToNothing (Just s) | null s = Nothing
                                | otherwise = Just s

-- | Exceptions thrown by Stack.Docker.Config.
data StackDockerConfigException
    = ResolverNotSupportedException String
    -- ^ Only LTS resolvers are supported for default image tag.
    | InvalidDatabasePathException SomeException
    -- ^ Invalid global database path.
    deriving (Typeable)

-- | Exception instance for StackDockerConfigException.
instance Exception StackDockerConfigException

-- | Show instance for StackDockerConfigException.
instance Show StackDockerConfigException where
    show (ResolverNotSupportedException resolver) =
        concat
            [ "Resolver not supported for Docker images:\n    "
            , resolver
            , "\nUse an LTS resolver, or set the '"
            , T.unpack dockerImageArgName
            , "' explicitly, in your configuration file."]
    show (InvalidDatabasePathException ex) =
        concat ["Invalid database path: ", show ex]

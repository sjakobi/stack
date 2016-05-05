data ConfigSource
    = CSCLI
    | CSEnvVars
    | CSSystemProperties
    | CSProjectConfig
    | CSGlobalConfig
    deriving (Eq, Ord, Show)

data WithSource a = WithSource
    { wsConfigMonoid :: a
    , wsSource :: ConfigSource
    }

consolidateConfig :: (Monoid a, MonadThrow m) => (a -> m b) -> [WithSource a] -> m b
consolidateConfig convert sources = do
    checkSourcesUnique sources
    (convert . map wsConfigMonoid . sortBy wsSource) sources


-----------------------------------------------------------------------------

getConfig

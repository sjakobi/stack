{-# LANGUAGE RecordWildCards, DeriveDataTypeable, OverloadedStrings #-}

-- | Nix configuration
module Stack.Config.Nix
       (nixOptsFromMonoid
       ,nixCompiler
       ,StackNixException(..)
       ) where

import Control.Exception.Lifted
import Data.Monoid.Extra
import qualified Data.Text as T
import Distribution.System (OS (..))
import Imports
import Prelude ()
import Stack.Types

-- | Interprets NixOptsMonoid options.
nixOptsFromMonoid
    :: (Monad m, MonadCatch m)
    => NixOptsMonoid
    -> OS
    -> m NixOpts
nixOptsFromMonoid NixOptsMonoid{..} os = do
    let nixEnable = fromFirst (getAny nixMonoidDefaultEnable) nixMonoidEnable
        defaultPure = case os of
          OSX -> False
          _ -> True
        nixPureShell = fromFirst defaultPure nixMonoidPureShell
        nixPackages = fromFirst [] nixMonoidPackages
        nixInitFile = getFirst nixMonoidInitFile
        nixShellOptions = fromFirst [] nixMonoidShellOptions
                          ++ prefixAll (T.pack "-I") (fromFirst [] nixMonoidPath)
    when (not (null nixPackages) && isJust nixInitFile) $
       throwM NixCannotUseShellFileAndPackagesException
    return NixOpts{..}
  where prefixAll p (x:xs) = p : x : prefixAll p xs
        prefixAll _ _      = []

nixCompiler :: Config -> Maybe Resolver -> Maybe CompilerVersion -> T.Text
nixCompiler config resolverOverride compilerOverride =
  let mproject = fst <$> configMaybeProject config
      mresolver = resolverOverride <|> fmap projectResolver mproject
      mcompiler = compilerOverride <|> join (fmap projectCompiler mproject)

      -- These are the latest minor versions for each respective major version available in nixpkgs
      fixMinor "8.0" = "8.0.1"
      fixMinor "7.10" = "7.10.3"
      fixMinor "7.8" = "7.8.4"
      fixMinor "7.6" = "7.6.3"
      fixMinor "7.4" = "7.4.2"
      fixMinor "7.2" = "7.2.2"
      fixMinor "6.12" = "6.12.3"
      fixMinor "6.10" = "6.10.4"
      fixMinor v = v
      nixCompilerFromVersion v = T.append (T.pack "haskell.compiler.ghc") (T.filter (/= '.') (fixMinor (versionText v)))
  in case (mresolver, mcompiler)  of
       (_, Just (GhcVersion v)) -> nixCompilerFromVersion v
       (Just (ResolverCompiler (GhcVersion v)), _) -> nixCompilerFromVersion v
       (Just (ResolverSnapshot (LTS x y)), _) ->
         T.pack ("haskell.packages.lts-" ++ show x ++ "_" ++ show y ++ ".ghc")
       _ -> T.pack "ghc"

-- Exceptions thown specifically by Stack.Nix
data StackNixException
  = NixCannotUseShellFileAndPackagesException
    -- ^ Nix can't be given packages and a shell file at the same time
    deriving (Typeable)

instance Exception StackNixException

instance Show StackNixException where
  show NixCannotUseShellFileAndPackagesException =
    "You cannot have packages and a shell-file filled at the same time in your nix-shell configuration."

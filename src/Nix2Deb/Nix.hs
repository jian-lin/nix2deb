{-# LANGUAGE QuasiQuotes #-}

-- | This module extracts info from nix packages.
module Nix2Deb.Nix
  ( getNixAttributeValue,
    getDependenciesForNixPackage,
  )
where

import Colog (Message, WithLog, logDebug)
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Nix2Deb.Effects (ExternProcessEffect (..), FileSystemEffect (..))
import Nix2Deb.Types
import Relude
import System.Exit (ExitCode (ExitSuccess))

-- TODO find a better replacement for NixEvalCommand (user-provided shell command).  maybe (flake-only) installable?
getNixAttributeValue ::
  (Monad m, ExternProcessEffect m) => NixEvalCommand -> NixAttribute -> m NixString
getNixAttributeValue evalCommand attribute = do
  (output, _err) <- readProcessShellEff_ [i|#{display evalCommand}.#{display attribute} --raw|]
  pure $ NixString output

getDependenciesForNixPackage ::
  (FileSystemEffect m, ExternProcessEffect m, WithLog env Message m) =>
  NixPackageOutputDirectory ->
  m (Set DependencyFile)
getDependenciesForNixPackage (NixPackageOutputDirectory nixPackageOutputDirectory) = do
  files <- listFilesRecursiveEff nixPackageOutputDirectory
  mconcat <$> traverse getDependencySonamesForFile files

getDependencySonamesForFile ::
  (ExternProcessEffect m, WithLog env Message m) => FilePath -> m (Set DependencyFile)
getDependencySonamesForFile file = do
  logDebug [i|find dependency soname for #{file}|]
  (exitCode, output, _err) <- readProcessEff "objdump" ["-p", file]
  if exitCode == ExitSuccess
    then do
      dependencySonames <- catMaybes <$> traverse (getDependencySoname . words) (lines output)
      logDebug [i|found #{display dependencySonames} needed by #{file}|]
      pure $ Set.fromList dependencySonames
    else do
      -- TODO ensure it is the wrong file type instead of other error
      logDebug [i|ignore #{file}: needs no soname|]
      pure Set.empty
  where
    getDependencySoname ["NEEDED", soname]
      | (not . isTrivialDependency) soname = pure . Just . DependencyFile . toString $ soname
      | otherwise = do
          logDebug [i|ignore trivial soname #{soname} needed by #{file}|]
          pure Nothing
    getDependencySoname _ = pure Nothing

    isTrivialDependency x = any (`T.isInfixOf` x) trivialDependencies

    -- provided by basic lib such as glibc
    trivialDependencies =
      [ "libc.so",
        "libdl.so",
        "libm.so",
        "libpthread.so",
        "librt.so"
      ]

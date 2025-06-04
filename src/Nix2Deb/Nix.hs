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

-- TODO can we make NixString more specific, like NixVersion, NixDescription?
getNixAttributeValue ::
  (Monad m, ExternProcessEffect m) => NixInstallable -> [NixAttribute] -> m NixString
getNixAttributeValue installable attributes = do
  (output, _err) <-
    readProcessEff_
      "nix"
      ["eval", toString $ display installable, "--raw", "--apply", [i|#{getNestedAttr} #{toNixList attributes}|]]
  pure $ NixString output

-- | Like @builtins.getAttr@ of Nix, but accepts a list of attributes and supports getting nested attribute.
-- @`getNestedAttr` [ "meta", "description" ] hello@ means @hello.meta.description@.
getNestedAttr :: Text
getNestedAttr = "(attrs: set: builtins.foldl' (result: attr: builtins.getAttr attr result) set attrs)"

toNixList :: [NixAttribute] -> Text
toNixList attributes =
  let nixAttributes = T.intercalate " " $ fmap (\attribute -> [i|"#{display attribute}"|]) attributes
   in [i|[#{nixAttributes}]|]

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

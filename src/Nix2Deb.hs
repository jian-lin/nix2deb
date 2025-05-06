{-# LANGUAGE QuasiQuotes #-}

-- | This module contains app logic.
module Nix2Deb
  ( app,
  )
where

import Colog (Message, WithLog, logDebug, logInfo)
import Control.Exception.Safe (MonadCatch, MonadThrow)
import Data.String.Interpolate (i)
import Nix2Deb.Deb (generateDebPackage)
import Nix2Deb.Effects (ExternProcessEffect, FileSystemEffect, NetworkEffect, SleepEffect)
import Nix2Deb.Map (findDebDependencyPackages)
import Nix2Deb.Nix (getDependenciesForNixPackage, getNixAttributeValue)
import Nix2Deb.Types
import Relude

-- TODO handle more error/exception

app ::
  ( NetworkEffect m,
    FileSystemEffect m,
    ExternProcessEffect m,
    WithLog env Message m,
    HasCliOptions env,
    MonadThrow m,
    SleepEffect m,
    MonadCatch m
  ) =>
  m ()
app = do
  logDebug "get deb dependency packages"
  Options {nixPackageOutputDirectory, nixEvalCommand, maintainerName, maintainerEmail, arch} <- asks getCliOptions
  dependencies <- getDependenciesForNixPackage nixPackageOutputDirectory
  logInfo [i|found dependencies: #{display dependencies}|]
  chosenDebDependencyPackages <- traverse findDebDependencyPackages (toList dependencies)
  logDebug "collect package meta info"
  debPackageName <- DebPackageName . toText <$> getNixAttributeValue nixEvalCommand "pname"
  logDebug [i|package name: #{display debPackageName}|]
  debVersion <- DebVersion . toText <$> getNixAttributeValue nixEvalCommand "version"
  logDebug [i|package version: #{display debVersion}|]
  debDescription <- DebDescription . toText <$> getNixAttributeValue nixEvalCommand "meta.description"
  logDebug [i|package description: #{display debDescription}|]
  let debMaintainer = DebMaintainer maintainerName maintainerEmail
  logDebug [i|package maintainer: #{display debMaintainer}|]
  logDebug "generate deb package"
  debPackage <-
    generateDebPackage
      debPackageName
      debVersion
      debMaintainer
      arch
      debDescription
      (fst <$> chosenDebDependencyPackages)
      nixPackageOutputDirectory
  logInfo [i|generated deb package: #{display debPackage}|]

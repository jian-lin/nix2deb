{-# LANGUAGE QuasiQuotes #-}

-- | This module contains app logic.
module Nix2Deb
  ( app,
  )
where

import Colog (Message, WithLog, logDebug, logInfo, logWarning)
import Control.Exception.Safe (MonadCatch, MonadThrow, throwM)
import Data.String.Interpolate (i)
import Nix2Deb.Deb (generateDebPackage)
import Nix2Deb.Effects (ExternProcessEffect, FileSystemEffect, NetworkEffect, SleepEffect)
import Nix2Deb.Exceptions (NixVersionNotValidAsDebVersionException (..))
import Nix2Deb.Map (chooseDebDependencyPackage, chooseDebDependencyPackageHeuristically, scrapeDebDependencyPackages)
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
  Options
    { nixPackageOutputDirectory,
      nixInstallable,
      maintainerName,
      maintainerEmail,
      arch,
      multipleDebDependencyPackageChooseStrategy
    } <-
    asks getCliOptions
  dependencies <- toList <$> getDependenciesForNixPackage nixPackageOutputDirectory
  logInfo [i|found dependencies: #{display dependencies}|]
  debDependencyPackages <- traverse scrapeDebDependencyPackages dependencies
  let heuristicChosenDebDependencyPackages = uncurry chooseDebDependencyPackageHeuristically <$> zip dependencies debDependencyPackages
  chosenDebDependencyPackages <- chooseDebDependencyPackage multipleDebDependencyPackageChooseStrategy (zip dependencies heuristicChosenDebDependencyPackages)
  forM_ (zip dependencies chosenDebDependencyPackages) \(dependency, (chosen, others)) ->
    let chosenLog = [i|choose deb dependency package #{display chosen} for #{display dependency}|]
     in if null others
          then logInfo chosenLog
          else logWarning [i|#{chosenLog}, other choices are #{display others}|]
  logDebug "collect package meta info"
  debPackageName <- DebPackageName . toText <$> getNixAttributeValue nixInstallable ["pname"]
  logDebug [i|package name: #{display debPackageName}|]
  nixVersion <- getNixAttributeValue nixInstallable ["version"]
  case mkDebVersion nixVersion of
    Left filteredNixVersion -> throwM $ NixVersionNotValidAsDebVersionException nixVersion filteredNixVersion
    Right debVersion -> do
      logDebug [i|package version: #{display debVersion}|]
      debDescription <- DebDescription . toText <$> getNixAttributeValue nixInstallable ["meta", "description"]
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

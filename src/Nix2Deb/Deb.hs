{-# LANGUAGE QuasiQuotes #-}

-- | This module is about generating deb packages.
module Nix2Deb.Deb
  ( generateDebPackage,
  )
where

import Colog (Message, WithLog, logDebug)
import Data.String.Interpolate (i, __i)
import Data.Text qualified as T
import Nix2Deb.Effects (ExternProcessEffect (..), FileSystemEffect (..))
import Nix2Deb.Types
import Relude
import System.FilePath ((</>))

generateDebPackage ::
  ( Monad m,
    FileSystemEffect m,
    ExternProcessEffect m,
    WithLog env Message m
  ) =>
  DebPackageName ->
  DebVersion ->
  DebMaintainer ->
  DebArch ->
  DebDescription ->
  [DebDependencyPackage] ->
  NixPackageOutputDirectory ->
  m DebPackage
generateDebPackage
  packageName
  version
  maintainer
  arch
  description
  debDependencyPackages
  nixPackageOutputDirectory = withTempDirectoryEff "/tmp" "nix2deb-" \workingDir -> do
    logDebug [i|working directory is #{workingDir}|]
    logDebug "write meta info"
    let debianDir = workingDir </> "DEBIAN"
        dependencies = T.intercalate ", " $ display . ddpPackage <$> debDependencyPackages
    createDirectoryIfMissingEff True debianDir
    -- TODO handle invalid deb version
    writeFileTextEff
      (debianDir </> "control")
      [__i|
        Package: #{display packageName}
        Version: #{display version}-1
        Maintainer: #{display maintainer}
        Architecture: #{display arch}
        Description: #{display description}
        Depends: #{dependencies}\n
      |]
    logDebug "copy/write actual pkg files"
    let usrDir = workingDir </> "usr"
    createDirectoryIfMissingEff True usrDir
    -- TODO switch to native hs
    (_output, _err) <- readProcessShellEff_ [i|cp -r #{display nixPackageOutputDirectory}/* #{usrDir}|]
    let nixSupportDir = usrDir </> "nix-support"
    whenM (doesDirectoryExistEff nixSupportDir)
      $ removeDirectoryRecursiveEff nixSupportDir
    let etcDir = usrDir </> "etc"
    whenM (doesDirectoryExistEff etcDir)
      $ renameDirectoryEff etcDir (workingDir </> "etc")
    logDebug "restore interpreter"
    allFiles <- listFilesRecursiveEff usrDir
    for_
      allFiles
      \file -> do
        (_exitCode, _output, _err) <- readProcessEff "chmod" ["u+w", file] -- TODO switch to native hs
        (_exitCode, _output, _err) <- readProcessEff "patchelf" ["--set-interpreter", "/lib64/ld-linux-x86-64.so.2", file]
        pure ()
    logDebug "build deb pkg"
    let debPackage = DebPackage [i|#{display packageName}.deb|]
    (_output, _err) <-
      readProcessEff_
        "dpkg-deb"
        ["--root-owner-group", "--build", workingDir, toString (display debPackage)]
    pure debPackage

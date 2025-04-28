{-# LANGUAGE QuasiQuotes #-}

module Nix2Deb.Deb
  ( generateDeb,
  )
where

import Data.String.Interpolate (i, __i)
import Data.Text qualified as T
import Nix2Deb.Types
import Relude
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive, renameDirectory)
import System.Directory.Extra (listFilesRecursive)
import System.FilePath ((</>))
import System.IO.Extra (withTempDir)
import System.Process.Typed (proc, runProcess, runProcess_)

generateDeb ::
  DebPackageName ->
  DebVersion ->
  DebMaintainer ->
  DebArch ->
  DebDescription ->
  DebDependedPkgs ->
  File ->
  IO File
generateDeb
  packageName
  version
  maintainer
  arch
  description
  (DebDependedPkgs dependedPkgs)
  nixOutPath = withTempDir $ \workingDir -> do
    putTextLn [i|workingDir is #{workingDir}|]
    putTextLn [i|write meta info|]
    let debianDir = workingDir </> "DEBIAN"
        version' :: Text = [i|#{toText version}-1|]
        maintainer' = toText maintainer -- TODO translate maintainer from nix to deb
        dependencies = T.intercalate ", " $ toText <$> dependedPkgs
    createDirectoryIfMissing True debianDir
    writeFileText
      (debianDir </> "control")
      [__i|
        Package: #{toText packageName}
        Version: #{version'}
        Maintainer: #{maintainer'}
        Architecture: #{toString arch}
        Description: #{toText description}
        Depends: #{dependencies}\n
      |]
    debControl <- readFile (debianDir </> "control")
    putTextLn
      [__i|
        deb control file:
        #{debControl}
      |]
    -- TODO find a easy-to-use file/dir lib to remove cp
    -- write actual pkg files
    let usrDir = workingDir </> "usr"
    createDirectoryIfMissing True usrDir
    runProcess_ [i|cp -r #{toString nixOutPath}/* #{usrDir}|]
    let nixSupportDir = usrDir </> "nix-support"
    whenM (doesDirectoryExist nixSupportDir)
      $ removeDirectoryRecursive nixSupportDir
    let etcDir = usrDir </> "etc"
    whenM (doesDirectoryExist etcDir)
      $ renameDirectory etcDir (workingDir </> "etc")
    let debFile = [i|#{toText packageName}.deb|]
    putTextLn [i|restore interpreter...|]
    allFiles <- listFilesRecursive usrDir
    traverse_
      ( \file -> do
          void $ runProcess $ proc "chmod" ["u+w", file]
          runProcess
            $ proc
              "patchelf"
              ["--set-interpreter", "/lib64/ld-linux-x86-64.so.2", file]
      )
      allFiles
    putTextLn [i|build deb pkg...|]
    runProcess_ $ proc "dpkg-deb" ["--root-owner-group", "--build", workingDir, debFile]
    pure $ File debFile

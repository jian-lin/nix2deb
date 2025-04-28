{-# LANGUAGE QuasiQuotes #-}

module Nix2Deb.Map
  ( findPackagesContainingFile,
    getRuntimeDependenciesForNixPkg,
    chooseDebPkgContainingFile,
  )
where

import Data.HashSet qualified as HashSet
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Nix2Deb.Types
import Relude
import System.Directory.Extra (listFilesRecursive)
import System.Process.Typed (ExitCode (ExitSuccess), proc, readProcess)
import Text.HTML.Scalpel ((//), (@:), (@=))
import Text.HTML.Scalpel qualified as S

-- TODO handle HTTP 500 (retry?)
-- TODO make it concurrent
findPackagesContainingFile :: DebArch -> Suite -> File -> IO (Maybe [(File, Package)])
findPackagesContainingFile arch (Suite suite) (File file) =
  S.scrapeURL
    [i|https://packages.ubuntu.com/search?searchon=contents&keywords=#{file}&mode=filename&suite=#{suite}&arch=#{toString arch}|]
    $ S.chroots ("div" @: ["id" @= "pcontentsres"] // "table" // "tr") do
      file' <- File <$> S.text ("td" @: [S.hasClass "file"])
      package <- Package . toText <$> S.text "a"
      pure (file', package)

-- for now, sort pkgs using lexical order and choose the first one
chooseDebPkgContainingFile :: [(File, Package)] -> Maybe (Package, [Package])
chooseDebPkgContainingFile xs =
  case sortWith snd (ordNubOn snd xs) of
    [] -> Nothing
    chosenPkg : otherPkgs -> Just (snd chosenPkg, snd <$> otherPkgs)

getRuntimeDependenciesForNixPkg :: File -> IO (HashSet File)
getRuntimeDependenciesForNixPkg (File file) = do
  files <- File <<$>> listFilesRecursive file
  mconcat <$> traverse getRuntimeDependenciesForFile files

-- TODO move to other module?
getRuntimeDependenciesForFile :: File -> IO (HashSet File)
getRuntimeDependenciesForFile (File file) = do
  (exitCode, outputBS, _errorBS) <- readProcess $ proc "objdump" ["-p", file]
  if exitCode /= ExitSuccess
    then pure HashSet.empty -- probably wrong file type
    else
      let output = decodeUtf8 outputBS -- TODO use decodeUtf8Strict
          runtimeDependencies = mapMaybe (getRuntimeDependency . words) (lines output)
       in pure $ HashSet.fromList runtimeDependencies
  where
    getRuntimeDependency ["NEEDED", file']
      | (not . isTrivialRuntimeDependency) file' = Just . File . toString $ file'
    getRuntimeDependency _ = Nothing
    isTrivialRuntimeDependency x = any (`T.isInfixOf` x) trivialRuntimeDependencies
    -- provided by basic lib such as glibc
    trivialRuntimeDependencies =
      [ "libc.so",
        "libdl.so",
        "libm.so",
        "libpthread.so",
        "librt.so"
      ]

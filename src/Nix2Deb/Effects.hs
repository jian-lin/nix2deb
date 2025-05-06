-- | This module contains app-specific effects.
module Nix2Deb.Effects where

import Nix2Deb.Types
import Relude
import System.Exit (ExitCode)

class FileSystemEffect effect where
  withTempDirectoryEff :: FilePath -> String -> (FilePath -> effect a) -> effect a
  createDirectoryIfMissingEff :: Bool -> FilePath -> effect ()
  writeFileTextEff :: FilePath -> Text -> effect ()
  doesDirectoryExistEff :: FilePath -> effect Bool
  removeDirectoryRecursiveEff :: FilePath -> effect ()
  renameDirectoryEff :: FilePath -> FilePath -> effect ()
  listFilesRecursiveEff :: FilePath -> effect [FilePath]

class NetworkEffect effect where
  fetchTagsEff :: Url -> effect Tags

class ExternProcessEffect effect where
  readProcessEff :: FilePath -> [String] -> effect (ExitCode, Text, Text)
  readProcessEff_ :: FilePath -> [String] -> effect (Text, Text)
  readProcessShellEff_ :: String -> effect (Text, Text)

class SleepEffect effect where
  sleepEff :: Int -> effect ()

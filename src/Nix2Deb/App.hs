-- | This module contains things about the App monad.
module Nix2Deb.App (run) where

import Control.Exception.Safe (MonadCatch, MonadThrow, handleAny, throwM)
import Nix2Deb.Effects
import Nix2Deb.Exceptions (FetchTagException (..))
import Nix2Deb.Types
import Relude
import System.Directory.Extra (listFilesRecursive)
import System.Process.Typed (proc, readProcess, readProcess_, shell)
import Text.HTML.Scalpel (fetchTags)
import UnliftIO (MonadUnliftIO, pooledMapConcurrentlyN, withTempDirectory)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory (createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive, renameDirectory)

newtype App (env :: (Type -> Type) -> Type) a = App {runApp :: ReaderT (env (App env)) IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader (env (App env)),
      MonadIO,
      MonadUnliftIO,
      MonadThrow,
      MonadCatch
    )

run :: App env a -> env (App env) -> IO a
run app = runReaderT (runApp app)

instance ExternProcessEffect (App env) where
  readProcessEff cmd args = do
    (exitCode, outputBS, errorBS) <- readProcess $ proc cmd args
    pure (exitCode, decodeUtf8 outputBS, decodeUtf8 errorBS)
  readProcessEff_ cmd args = do
    (outputBS, errorBS) <- readProcess_ $ proc cmd args
    pure (decodeUtf8 outputBS, decodeUtf8 errorBS)
  readProcessShellEff_ cmd = do
    (outputBS, errorBS) <- readProcess_ $ shell cmd
    pure (decodeUtf8 outputBS, decodeUtf8 errorBS)

instance FileSystemEffect (App env) where
  withTempDirectoryEff = withTempDirectory
  createDirectoryIfMissingEff = createDirectoryIfMissing
  writeFileTextEff = writeFileText
  doesDirectoryExistEff = doesDirectoryExist
  removeDirectoryRecursiveEff = removeDirectoryRecursive
  renameDirectoryEff = renameDirectory
  listFilesRecursiveEff = liftIO . listFilesRecursive

instance NetworkEffect (App env) where
  fetchTagsEff url =
    handleAny (throwM . FetchTagException url)
      $ liftIO (Tags <$> fetchTags (toString url))

instance SleepEffect (App env) where
  sleepEff = threadDelay

instance ConcurrentEffect (App env) where
  pooledMapConcurrentlyNEff threadNumber = pooledMapConcurrentlyN (unThreadNumber threadNumber)

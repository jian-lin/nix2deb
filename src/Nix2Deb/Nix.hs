{-# LANGUAGE QuasiQuotes #-}

-- | This module extracts info from nix packages.
module Nix2Deb.Nix
  ( getNixAttributeValue,
    getDependenciesForNixPackage,
  )
where

import Colog (Message, WithLog, logDebug)
import Control.Exception.Safe (MonadThrow, throwM)
import Data.Char qualified as Char
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Nix2Deb.Effects (ExternProcessEffect (..), FileSystemEffect (..))
import Nix2Deb.Exceptions (GetNixPnameForSonameException (..))
import Nix2Deb.Types
import Relude
import System.Exit (ExitCode (ExitSuccess))
import Prelude (lookup)

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
  (FileSystemEffect m, ExternProcessEffect m, WithLog env Message m, MonadThrow m) =>
  NixPackageOutputDirectory ->
  m (Set DependencyWithInfoFromNix)
getDependenciesForNixPackage (NixPackageOutputDirectory nixPackageOutputDirectory) = do
  files <- listFilesRecursiveEff nixPackageOutputDirectory
  mconcat <$> traverse getDependencySonamesForFile files

getDependencySonamesForFile ::
  (ExternProcessEffect m, WithLog env Message m, MonadThrow m) =>
  FilePath ->
  m (Set DependencyWithInfoFromNix)
getDependencySonamesForFile file = do
  logDebug [i|find dependency soname for #{file}|]
  (exitCode, output, _err) <- readProcessEff "objdump" ["-p", file]
  if exitCode == ExitSuccess
    then do
      dependencySonames <- catMaybes <$> traverse (getDependencySoname . words) (lines output)
      logDebug [i|found #{display dependencySonames} needed by #{file}|]
      nixPnames <- traverse (getNixPnameForSoname file) dependencySonames
      pure $ Set.fromList $ zipWith DependencyWithInfoFromNix dependencySonames nixPnames
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

getNixPnameForSoname ::
  (ExternProcessEffect m, MonadThrow m) =>
  FilePath ->
  DependencyFile ->
  m NixPname
getNixPnameForSoname file soname = do
  (output, _err) <- readProcessEff_ "ldd" [file]
  let meNixPname = lookup soname $ mapMaybe (getSonameAndNixPname . words) (lines output)
  case meNixPname of
    Just (Right nixPname) -> pure nixPname
    Just (Left nixPnameParseError) -> throwM $ NixPnameParseException nixPnameParseError
    Nothing -> throwM $ NixStorePathNotFoundForSonameException file soname
  where
    getSonameAndNixPname [soname', "=>", sonameInNixStore, _] =
      Just
        ( DependencyFile . toString $ soname',
          parseNixPnameFromStorePath . NixStorePath $ sonameInNixStore
        )
    getSonameAndNixPname _ = Nothing

-- |
-- >>> parseNixPnameFromStorePath $ NixStorePath "/nix/store/srby6wmvg7dp454pwb6qvaxdiri38sc1-zlib-1.3.1/lib/libz.so.1"
-- Right (NixPname "zlib")
parseNixPnameFromStorePath :: NixStorePath -> Either NixPnameParseError NixPname
parseNixPnameFromStorePath = parseNixNameFromStorePath >=> parseNixPnameFromName

-- | For now, we hardcode nix store dir as `/nix/store`.
-- >>> parseNixNameFromStorePath $ NixStorePath "/nix/store/srby6wmvg7dp454pwb6qvaxdiri38sc1-zlib-1.3.1/lib/libz.so.1"
-- Right (NixName "zlib-1.3.1")
parseNixNameFromStorePath :: NixStorePath -> Either NixPnameParseError NixName
parseNixNameFromStorePath nixStorePath@(NixStorePath nixStorePath') =
  case T.split (== '/') nixStorePath' of
    "" : "nix" : "store" : hashAndName : _ ->
      case T.split (== '-') hashAndName of
        _hash : nameElements -> Right $ NixName $ T.intercalate "-" nameElements
        _ -> Left $ CannotSplitHashAndName nixStorePath
    _ -> Left $ UnsupportedNixStoreDir nixStorePath

-- | Everything up to but not including the first dash (-) that is followed by a number.
-- https://github.com/NixOS/nixpkgs/blob/19059aa9c38d64f62f7e9828d4d0277debbacc19/pkgs/README.md?plain=1#L402-L421
-- https://nix.dev/manual/nix/2.29/command-ref/nix-env#selectors
-- >>> parseNixPnameFromName $ NixName "my-app-0.1.0"
-- Right (NixPname "my-app")
parseNixPnameFromName :: NixName -> Either NixPnameParseError NixPname
parseNixPnameFromName (NixName nixName) =
  case T.uncons nixName of
    Just (_firstChar, nixNameTail) ->
      Right $ NixPname $ T.concat $ one . fst <$> takeWhile (not . isDashFollewedByNumber) (T.zip nixName nixNameTail)
    Nothing -> Left EmptyNixName
  where
    isDashFollewedByNumber (currentChar, nextChar) = currentChar == '-' && Char.isDigit nextChar

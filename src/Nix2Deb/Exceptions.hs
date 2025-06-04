{-# LANGUAGE QuasiQuotes #-}

-- | This module contains exceptions.
module Nix2Deb.Exceptions where

import Data.String.Interpolate (i, __i)
import Data.Text qualified as T
import Nix2Deb.Types
import Relude

data ScrapeException = ScrapeException DependencyWithInfoFromNix Url ScrapeError
  deriving stock (Show)

instance Exception ScrapeException where
  displayException (ScrapeException dependency url scrapeError) =
    [__i|
      Fail to find deb dependency packages for #{display dependency}:
        #{display scrapeError}
        when scraping #{display url}
    |]

data FetchTagException = FetchTagException Url SomeException
  deriving stock (Show)

instance Exception FetchTagException where
  displayException (FetchTagException url exception) =
    [__i|
      Fail to fetch html tags:
        from #{display url}
        exception: #{displayException exception}
    |]

-- TODO add newtype wrappers for these two NixString to distinguish them at type level
data NixVersionNotValidAsDebVersionException = NixVersionNotValidAsDebVersionException NixString NixString
  deriving stock (Show)

instance Exception NixVersionNotValidAsDebVersionException where
  displayException (NixVersionNotValidAsDebVersionException (NixString originalNixVersion) (NixString filteredNixVersion)) =
    [__i|
      Nix version is not valid as a deb version:
        Original nix version: #{originalNixVersion}
        After filtering out invalid chars: #{filteredNixVersion}
    |]

data GetNixPnameForSonameException
  = NixPnameParseException NixPnameParseError
  | NixStorePathNotFoundForSonameException FilePath DependencyFile
  deriving stock (Show)

instance Exception GetNixPnameForSonameException where
  displayException (NixPnameParseException nixPnameParseError) =
    [__i|
      Fail to parse nix pname:
        #{display nixPnameParseError}
    |]
  displayException (NixStorePathNotFoundForSonameException file soname) =
    [__i|
      Fail to get nix pname for soname: nix store path not found
        file: #{file}
        soname: #{display soname}
    |]

newtype MultipleDebDependencyPackageChooseException
  = MultipleDebDependencyPackageChoicesExist (NonEmpty (DependencyWithInfoFromNix, (DebDependencyPackage, NonEmpty DebDependencyPackage)))
  deriving stock (Show)

instance Exception MultipleDebDependencyPackageChooseException where
  displayException (MultipleDebDependencyPackageChoicesExist dependencyAndDebDependencyPackagesList) =
    let format (dependency, (chosen, others)) = [i|For #{display dependency}: prefer #{display chosen}, other choices are #{display others}|]
        -- TODO use a pretty print lib instead of manually indent
        formatList = fmap format >>> toList >>> T.intercalate "\n  "
     in [__i|
          Multiple deb dependency package choices for one dependency exist:
            #{formatList dependencyAndDebDependencyPackagesList}
        |]

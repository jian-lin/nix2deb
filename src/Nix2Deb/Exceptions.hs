{-# LANGUAGE QuasiQuotes #-}

-- | This module contains exceptions.
module Nix2Deb.Exceptions where

import Data.String.Interpolate (__i)
import Nix2Deb.Types
import Relude

data ScrapeException = ScrapeException DependencyFile Url ScrapeError
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

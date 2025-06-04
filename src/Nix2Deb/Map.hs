{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | This modules is about the mapping between nix and deb packages.
module Nix2Deb.Map
  ( findDebDependencyPackages,
  )
where

import Colog (Message, WithLog, logDebug, logInfo, logWarning)
import Control.Exception.Safe (MonadCatch, MonadThrow, throwM, tryJust)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NL
import Data.Set qualified as Set
import Data.String.Interpolate (i, __i)
import Nix2Deb.Effects (NetworkEffect (..), SleepEffect (..))
import Nix2Deb.Exceptions (FetchTagException (..), ScrapeException (..))
import Nix2Deb.Types
import Relude
import Text.EditDistance qualified as ED
import Text.HTML.Scalpel (Scraper, (//), (@:), (@=))
import Text.HTML.Scalpel qualified as S

retryIntervals :: [Int]
retryIntervals = [200_000, 400_000, 1_000_000]

-- | Retry an action after some time if it raises an exception.
retry ::
  (Exception e, SleepEffect m, MonadCatch m, WithLog env Message m) =>
  [Int] ->
  (e -> Bool) ->
  m a ->
  m a
retry [] _predicate action = action
retry (x : xs) predicate action = do
  eResult <- tryJust (boolToMaybe predicate) action
  case eResult of
    Left exception -> do
      logDebug
        [__i|
          retry in #{x} us due to exception:
            #{displayException exception}
        |]
      sleepEff x
      retry xs predicate action
    Right result -> pure result
  where
    boolToMaybe predicate' e = bool Nothing (Just e) (predicate' e)

-- TODO make it concurrent (but not too concurrent to avoid rate-limit) to speed up
findDebDependencyPackages ::
  ( WithLog env Message m,
    HasCliOptions env,
    NetworkEffect m,
    MonadThrow m,
    SleepEffect m,
    MonadCatch m
  ) =>
  DependencyWithInfoFromNix ->
  m (DebDependencyPackage, [DebDependencyPackage])
findDebDependencyPackages dependency = retry retryIntervals isScrapeNetworkException do
  Options {suite, arch} <- asks getCliOptions
  let queryUrl = Url [i|https://packages.ubuntu.com/search?searchon=contents&keywords=#{display dependency}&mode=exactfilename&suite=#{display suite}&arch=#{display arch}|]
  logDebug [i|process #{display dependency} query #{display queryUrl}|]
  tags <- retry retryIntervals (\(FetchTagException _ _) -> True) (fetchTagsEff queryUrl)
  case scrapeDebDependencyPackages tags of
    Left scraperError -> throwM $ ScrapeException dependency queryUrl scraperError
    Right depDependenciesPackages -> do
      let result@(chosen, others) = chooseDebDependencyPackage dependency depDependenciesPackages
          chosenLog = [i|choose deb dependency package #{display chosen} for #{display dependency}|]
      if null others
        then logInfo chosenLog
        else logWarning [i|#{chosenLog}, other choices are #{display others}|]
      pure result
  where
    isScrapeNetworkException (ScrapeException _ _ NetworkError) = True
    isScrapeNetworkException _ = False

scrapeDebDependencyPackages :: Tags -> Either ScrapeError (NonEmpty DebDependencyPackage)
scrapeDebDependencyPackages tags
  | Just "500 Internal Server Error" <- S.scrape http500Scraper (unTags tags) = Left NetworkError
  | otherwise = toEither $ mergeByDebPackage <$> S.scrape debDependencyPackageScraper (unTags tags)
  where
    toEither Nothing = Left UnknownError
    toEither (Just []) = Left EmptyResult
    toEither (Just (x : xs)) = Right (x :| xs)

-- TODO more choosing strategies

-- | Choose a single deb package from many deb packages.
-- We choose the deb package whose name has the smallest edit distance with the nix pname.
chooseDebDependencyPackage ::
  DependencyWithInfoFromNix ->
  NonEmpty DebDependencyPackage ->
  (DebDependencyPackage, [DebDependencyPackage])
chooseDebDependencyPackage
  dependencyWithInfoFromNix
  (NL.sortBy (compareEditDistance dependencyWithInfoFromNix) -> x :| xs) = (x, xs)

compareEditDistance :: DependencyWithInfoFromNix -> DebDependencyPackage -> DebDependencyPackage -> Ordering
compareEditDistance (DependencyWithInfoFromNix _ nixPname) (DebDependencyPackage debPackage1 _) (DebDependencyPackage debPackage2 _) =
  compare (calcEditDistance nixPname debPackage1) (calcEditDistance nixPname debPackage2)

calcEditDistance :: NixPname -> DebPackage -> Int
calcEditDistance (NixPname nixPname) (DebPackage debPackage) =
  ED.levenshteinDistance ED.defaultEditCosts (toString nixPname) (toString debPackage)

http500Scraper :: Scraper Text Text
http500Scraper = S.text "title"

debDependencyPackageScraper :: Scraper Text [(DebPackage, DependencyWithInfoFromDeb)]
debDependencyPackageScraper =
  S.chroots ("div" @: ["id" @= "pcontentsres"] // "table" // "tr") do
    foundDependencyFile' <- DependencyWithInfoFromDeb . DependencyFile . toString <$> S.text ("td" @: [S.hasClass "file"])
    debPackage <- DebPackage . toText <$> S.text "a"
    pure (debPackage, foundDependencyFile')

-- maybe slow?
mergeByDebPackage :: [(DebPackage, DependencyWithInfoFromDeb)] -> [DebDependencyPackage]
mergeByDebPackage =
  fmap (second one)
    >>> HashMap.fromListWith Set.union
    >>> HashMap.toList
    >>> fmap (uncurry DebDependencyPackage)

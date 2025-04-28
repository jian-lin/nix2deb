{-# LANGUAGE QuasiQuotes #-}

module Nix2Deb
  ( demo,
  )
where

import Data.String.Interpolate (i)
import Nix2Deb.Deb (generateDeb)
import Nix2Deb.Map
  ( chooseDebPkgContainingFile,
    findPackagesContainingFile,
    getRuntimeDependenciesForNixPkg,
  )
import Nix2Deb.Nix (extractNixAttribute)
import Nix2Deb.Types
import Relude

-- TODO replace IO with explicit effects
-- TODO handle error properly
-- TODO restore interpreter for executables
-- TODO refine types

demo :: File -> NixEvalCommand -> IO ()
demo file nixEvalCommand = do
  putTextLn [i|getting deb deps...|]
  runtimeDependencies <- getRuntimeDependenciesForNixPkg file
  putTextLn [i|found runtime deps: #{runtimeDependencies}|]
  allRuntimeDependencies <-
    traverse (findPackagesContainingFile Amd64 "plucky") (toList runtimeDependencies)
  let allNonEmptyRuntimeDependencies = catMaybes allRuntimeDependencies
  when (length allNonEmptyRuntimeDependencies /= length allRuntimeDependencies)
    $ die [i|failed to find deb pkgs from some runtime deps: scrape error, please retry, TODO auto retry|]
  let chosenRuntimeDependencies = mapMaybe chooseDebPkgContainingFile allNonEmptyRuntimeDependencies
  when (length chosenRuntimeDependencies /= length allNonEmptyRuntimeDependencies)
    $ die [i|failed find deb pkgs from some runtime deps: scrape empty result, please retry, TODO auto retry|]
  traverse_ warnMultipleDebDepPkgChoices chosenRuntimeDependencies
  putTextLn [i|getting meta info...|]
  debPackageName <- DebPackageName . toText <$> extractNixAttribute nixEvalCommand "pname"
  debVersion <- DebVersion . toText <$> extractNixAttribute nixEvalCommand "version"
  let debMaintainer = DebMaintainer "TODO todo <todo@todo.org>"
  debDescription <- DebDescription . toText <$> extractNixAttribute nixEvalCommand "meta.description"
  putTextLn [i|generating deb pkg...|]
  debFile <- generateDeb debPackageName debVersion debMaintainer Amd64 debDescription (DebDependedPkgs $ fst <$> chosenRuntimeDependencies) file
  putTextLn [i|generated deb file at #{debFile}|]
  where
    warnMultipleDebDepPkgChoices (chosenPkg, otherPkgs) =
      unless (null otherPkgs)
        $ putTextLn [i|multiple deb dep choices: choose #{chosenPkg}, others are #{otherPkgs}|]

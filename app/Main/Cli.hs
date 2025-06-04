{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main.Cli
  ( parseCliOptions,
  )
where

import Colog (Severity)
import Colog qualified as Log
import Nix2Deb.Types
import Options.Applicative (Parser)
import Options.Applicative qualified as O
import Relude

-- TODO use optparse-generic

parseCliOptions :: IO Options
parseCliOptions =
  O.execParser
    ( O.info
        (optionsParser <**> O.helper)
        ( O.progDesc -- TODO get it from cabal file
            "Convert a Nix package to a deb package using other deb packages as dependencies"
        )
    )

optionsParser :: Parser Options
optionsParser = do
  nixPackageOutputDirectory <- nixPackageOutputDirectoryParser
  nixInstallable <- nixInstallableParser
  suite <- suiteParser
  arch <- archParser
  multipleDebDependencyPackageChooseStrategy <- multipleDebDependencyPackageChooseStrategyParser
  maintainerName <- maintainerNameParser
  maintainerEmail <- maintainerEmailParser
  scrapeThreads <- scrapeThreadsParser
  logLevel <- logLevelParser
  pure Options {..}

nixPackageOutputDirectoryParser :: Parser NixPackageOutputDirectory
nixPackageOutputDirectoryParser =
  O.strArgument
    ( O.metavar "DIRECTORY"
        <> O.help "Directory of your built nix package such as ./result"
    )

nixInstallableParser :: Parser NixInstallable
nixInstallableParser =
  NixInstallable
    <$> O.strOption
      ( O.long "nix-installable"
          <> O.short 'i'
          <> O.metavar "INSTALLABLE"
          <> O.help "Nix installable representing your nix package such as .#myPkg.  See https://nix.dev/manual/nix/latest/command-ref/new-cli/nix.html#installables."
      )

suiteParser :: Parser Suite
suiteParser =
  O.strOption
    ( O.long "suite"
        <> O.short 's'
        <> O.metavar "CODENAME"
        <> O.help "Version of the Ubuntu system such as plucky"
    )

archParser :: Parser DebArch
archParser =
  O.option
    O.auto
    ( O.long "arch"
        <> O.short 'a'
        <> O.metavar "ARCH"
        <> O.value Amd64
        <> O.showDefault
        <> O.help (show [minBound :: DebArch ..])
    )

multipleDebDependencyPackageChooseStrategyParser :: Parser MultipleDebDependencyPackageChooseStrategy
multipleDebDependencyPackageChooseStrategyParser =
  O.option
    O.auto
    ( O.long "multiple-deb-dependency-package-choose-strategy"
        <> O.long "choose-strategy"
        <> O.short 'c'
        <> O.metavar "STRATEGY"
        <> O.value Heuristic
        <> O.showDefault
        <> O.help (show [minBound :: MultipleDebDependencyPackageChooseStrategy ..])
    )

maintainerNameParser :: Parser MaintainerName
maintainerNameParser =
  O.strOption
    ( O.long "maintainer-name"
        <> O.long "name"
        <> O.short 'n'
        <> O.metavar "NAME"
        <> O.value "nix2deb user"
        <> O.showDefaultWith (display >>> toString)
        <> O.help "Maintainer name of this package"
    )

maintainerEmailParser :: Parser MaintainerEmail
maintainerEmailParser =
  O.strOption
    ( O.long "maintainer-email"
        <> O.long "email"
        <> O.short 'e'
        <> O.metavar "EMAIL"
        <> O.value "someone@example.com"
        <> O.showDefaultWith (display >>> toString)
        <> O.help "Maintainer email of this package"
    )

scrapeThreadsParser :: Parser ThreadNumber
scrapeThreadsParser =
  O.option
    (O.eitherReader threadNumberReader)
    ( O.long "scrape-threads"
        <> O.short 't'
        <> O.metavar "N"
        <> O.value $(mkThreadNumberTH 3)
        <> O.showDefaultWith (unThreadNumber >>> show)
        <> O.help "Number of threads to scrape concurrently"
    )

threadNumberReader :: String -> Either String ThreadNumber
threadNumberReader s = do
  threadNumber <- first toString $ readEither s
  first show $ mkThreadNumber threadNumber

logLevelParser :: Parser Severity
logLevelParser =
  O.option
    O.auto
    ( O.long "log-level"
        <> O.short 'l'
        <> O.metavar "LOG-LEVEL"
        <> O.value Log.Info
        <> O.showDefault
        <> O.help (show [minBound :: Severity ..])
    )

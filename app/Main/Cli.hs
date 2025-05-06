{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

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
  nixEvalCommand <- nixEvalCommandParser
  suite <- suiteParser
  arch <- archParser
  maintainerName <- maintainerNameParser
  maintainerEmail <- maintainerEmailParser
  logLevel <- logLevelParser
  pure Options {..}

nixPackageOutputDirectoryParser :: Parser NixPackageOutputDirectory
nixPackageOutputDirectoryParser =
  O.strArgument
    ( O.metavar "DIRECTORY"
        <> O.help "Directory of your built nix package such as ./result"
    )

nixEvalCommandParser :: Parser NixEvalCommand
nixEvalCommandParser =
  NixEvalCommand
    <$> O.strOption
      ( O.long "nix-eval-command"
          <> O.short 'e'
          <> O.metavar "COMMAND"
          <> O.help "Command to eval your nix package such as 'nix eval .#myPkg'"
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
        <> O.short 'm'
        <> O.metavar "EMAIL"
        <> O.value "someone@example.com"
        <> O.showDefaultWith (display >>> toString)
        <> O.help "Maintainer email of this package"
    )

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

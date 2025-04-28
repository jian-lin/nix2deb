module Main where

import Control.Exception.Uncaught (setDisplayExceptionHandler)
import Nix2Deb qualified
import Nix2Deb.Types
import Options.Applicative as O
import Relude

data Options = Options
  { nixOutPath :: File,
    nixEvalCommand :: NixEvalCommand
  }

parseCliOptions :: IO Options
parseCliOptions =
  O.execParser
    ( O.info
        (O.helper <*> optionsParser)
        (O.progDesc "Convert a Nix pkg to a deb pkg using other deb pkgs as dependencies")
    )

optionsParser :: Parser Options
optionsParser = Options <$> nixOutPathParser <*> nixEvalCommandParser

nixOutPathParser :: Parser File
nixOutPathParser =
  O.strOption
    ( O.long "nix-out-path"
        <> O.short 'p'
        <> O.metavar "NIX-OUT-PATH"
        <> O.help "Output path of nix build such as ./result"
    )

nixEvalCommandParser :: Parser NixEvalCommand
nixEvalCommandParser =
  NixEvalCommand
    <$> O.strOption
      ( O.long "nix-eval-command"
          <> O.short 'e'
          <> O.metavar "NIX-EVAL-COMMAND"
          <> O.help "Nix eval command for your derivation such as 'nix eval .#myPkg'"
      )

main :: IO ()
main = do
  setDisplayExceptionHandler
  Options {nixOutPath, nixEvalCommand} <- parseCliOptions
  Nix2Deb.demo nixOutPath nixEvalCommand

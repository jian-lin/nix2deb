module Main where

import Control.Exception.Uncaught (setDisplayExceptionHandler)
import Nix2Deb qualified
import Nix2Deb.Types
import Options.Applicative as O
import Relude

data Options = Options
  { nixOutPath :: File
  }

parseCliOptions :: IO Options
parseCliOptions =
  O.execParser
    ( O.info
        (O.helper <*> optionsParser)
        (O.progDesc "Convert a Nix pkg to a deb pkg using other deb pkgs as dependencies")
    )

optionsParser :: Parser Options
optionsParser = Options <$> nixOutPathParser

nixOutPathParser :: Parser File
nixOutPathParser =
  O.strOption
    ( O.long "nix-out-path"
        <> O.short 'p'
        <> O.metavar "NIX-OUT-PATH"
        <> O.help "Output path of nix build such as ./result"
    )

main :: IO ()
main = do
  setDisplayExceptionHandler
  Options {nixOutPath} <- parseCliOptions
  Nix2Deb.demo nixOutPath

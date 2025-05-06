module Main where

import Colog (HasLog (..), LogAction, Message)
import Control.Exception.Uncaught (setDisplayExceptionHandler)
import Main.Cli (parseCliOptions)
import Main.Log (mkLogAction)
import Nix2Deb (app)
import Nix2Deb.App (run)
import Nix2Deb.Types
import Relude

data Env m = Env
  { envLogAction :: LogAction m Message,
    envCliOptions :: Options
  }

instance HasLog (Env m) Message m where
  getLogAction = envLogAction
  {-# INLINE getLogAction #-}
  setLogAction newLogAction env = env {envLogAction = newLogAction}
  {-# INLINE setLogAction #-}

instance HasCliOptions (Env m) where
  getCliOptions = envCliOptions
  {-# INLINE getCliOptions #-}

main :: IO ()
main = do
  setDisplayExceptionHandler
  cliOptions <- parseCliOptions
  let logAction = mkLogAction (logLevel cliOptions)
      env = Env logAction cliOptions
  run app env

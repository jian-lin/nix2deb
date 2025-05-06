module Main.Log
  ( mkLogAction,
  )
where

import Colog (LogAction, Message, Severity)
import Colog qualified as Log
import Relude

mkLogAction :: (MonadIO m) => Severity -> LogAction m Message
mkLogAction severity = Log.filterBySeverity severity Log.msgSeverity Log.simpleMessageAction

-- TODO change format to exclude filename#line?

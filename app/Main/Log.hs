module Main.Log
  ( mkLogAction,
  )
where

import Colog (LogAction, Message, Msg (..), Severity)
import Colog qualified as Log
import Relude

mkLogAction :: (MonadIO m) => Severity -> LogAction m Message
mkLogAction severity = Log.filterBySeverity severity Log.msgSeverity (logAction severity)

-- | Action that prints `Message` to `stderr`.
logAction :: (MonadIO m) => Severity -> LogAction m Message
logAction severity = encodeUtf8 . fmtMessage severity >$< Log.logByteStringStderr
{-# INLINE logAction #-}
{-# SPECIALIZE logAction :: Severity -> LogAction IO Message #-}

fmtMessage :: Severity -> Message -> Text
fmtMessage severity message@Msg {msgSeverity, msgText}
  | severity <= Log.Debug = Log.fmtMessage message
  | otherwise = Log.showSeverity msgSeverity <> msgText

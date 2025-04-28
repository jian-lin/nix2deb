{-# LANGUAGE QuasiQuotes #-}

module Nix2Deb.Nix
  ( extractNixAttribute,
  )
where

import Data.String.Interpolate (i)
import Nix2Deb.Types
import Relude
import System.Process.Typed (readProcess_, shell)

-- TODO use json output
-- TODO find a better way for NixEvalCommand.  maybe (flake-only) installable?
extractNixAttribute :: NixEvalCommand -> NixAttribute -> IO NixString
extractNixAttribute evalCommand attribute = do
  (outputBS, _errorBS) <- readProcess_ $ shell [i|#{toText evalCommand}.#{toText attribute} --raw|]
  -- TODO use decodeUtf8Strict
  pure . NixString . decodeUtf8 $ outputBS

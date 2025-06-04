-- | This module contains type and smart constructor for DebVersion.
module Nix2Deb.Types.DebVersion
  ( DebVersion,
    mkDebVersion,
  )
where

import Data.Text qualified as T
import Nix2Deb.Types.Main (Display, NixString (..))
import Relude

newtype DebVersion = DebVersion Text
  deriving stock (Show)
  deriving newtype (Display)

-- | Turn a nix version to a deb version.
-- Nix version is used as the upstream_version of deb version.
-- A dummy debian_revision is added to the deb version to simplify this transformation.
-- https://www.debian.org/doc/debian-policy/ch-controlfields.html#version
mkDebVersion :: NixString -> Either NixString DebVersion
mkDebVersion (NixString nixVersion) =
  let filteredVersion = T.filter isValidChar nixVersion
      dummyDebRevision = "1"
   in if filteredVersion == nixVersion
        then Right $ DebVersion $ filteredVersion <> "-" <> dummyDebRevision
        else Left $ NixString filteredVersion

isValidChar :: Char -> Bool
isValidChar c
  | c `elem` ['.', '+', '-', '~'] = True
  | c `elem` ['0' .. '9'] = True
  | c `elem` ['a' .. 'z'] = True
  | c `elem` ['A' .. 'Z'] = True
  | otherwise = False

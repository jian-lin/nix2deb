module Nix2Deb.Types where

import Relude

newtype Package = Package Text
  deriving stock (Show)

newtype File = File FilePath
  deriving stock (Show, Eq)
  deriving newtype (ToString, IsString, Hashable)

data Arch = Amd64 | Arm64

instance ToString Arch where
  toString Amd64 = "amd64"
  toString Arm64 = "arm64"

newtype Suite = Suite Text
  deriving stock (Show)
  deriving newtype (ToString, IsString)

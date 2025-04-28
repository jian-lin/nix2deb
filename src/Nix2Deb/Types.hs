module Nix2Deb.Types where

import Relude

newtype Package = Package Text
  deriving stock (Show, Eq, Ord)
  deriving newtype (ToText)

newtype File = File FilePath
  deriving stock (Show, Eq)
  deriving newtype (ToString, IsString, Hashable)

data DebArch = Amd64 | Arm64

instance ToString DebArch where
  toString Amd64 = "amd64"
  toString Arm64 = "arm64"

newtype Suite = Suite Text
  deriving stock (Show)
  deriving newtype (ToString, IsString)

newtype NixString = NixString Text
  deriving stock (Show)
  deriving newtype (ToText)

newtype NixAttribute = NixAttribute Text
  deriving stock (Show)
  deriving newtype (IsString, ToText)

newtype NixEvalCommand = NixEvalCommand Text
  deriving stock (Show)
  deriving newtype (ToText)

-- TODO refactor deriving newtype (Show)
newtype DebPackageName = DebPackageName Text
  deriving stock (Show)
  deriving newtype (ToText)

newtype DebVersion = DebVersion Text
  deriving stock (Show)
  deriving newtype (ToText)

newtype DebMaintainer = DebMaintainer Text
  deriving stock (Show)
  deriving newtype (ToText)

newtype DebDescription = DebDescription Text
  deriving stock (Show)
  deriving newtype (ToText)

newtype DebDependedPkgs = DebDependedPkgs [Package]

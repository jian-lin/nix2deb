module Nix2Deb.Types.Main where

import Colog (Severity)
import Data.Text qualified as T
import Relude
import Text.HTML.TagSoup (Tag)

-- TODO move Display related-code to another module?
-- TODO use a pretty print lib?
class Display a where
  display :: a -> Text

instance {-# OVERLAPPABLE #-} (Foldable t, Display a) => Display (t a) where
  display = toList >>> fmap display >>> T.intercalate " "

instance Display FilePath where display = toText

instance Display Text where display = toText

-- instance Display a => Display [a] where display = fmap display >>> T.intercalate " "

newtype DependencyFile = DependencyFile FilePath
  deriving stock (Show, Eq, Ord)
  deriving newtype (Display)

newtype NixPname = NixPname Text
  deriving stock (Show)

newtype NixName = NixName Text
  deriving stock (Show)

newtype NixStorePath = NixStorePath Text
  deriving stock (Show)
  deriving newtype (Display)

data DependencyWithInfoFromNix = DependencyWithInfoFromNix DependencyFile NixPname
  deriving stock (Show)

instance Eq DependencyWithInfoFromNix where
  (DependencyWithInfoFromNix dependencyFile1 _) == (DependencyWithInfoFromNix dependencyFile2 _) =
    dependencyFile1 == dependencyFile2

instance Ord DependencyWithInfoFromNix where
  compare (DependencyWithInfoFromNix dependencyFile1 _) (DependencyWithInfoFromNix dependencyFile2 _) =
    compare dependencyFile1 dependencyFile2

instance Display DependencyWithInfoFromNix where
  display (DependencyWithInfoFromNix dependencyFile _) = display dependencyFile

newtype DependencyWithInfoFromDeb = DependencyWithInfoFromDeb DependencyFile
  deriving stock (Show, Eq, Ord)

newtype DebPackage = DebPackage Text
  deriving stock (Show, Eq, Ord)
  deriving newtype (Display, Hashable)

newtype NixPackageOutputDirectory = NixPackageOutputDirectory FilePath
  deriving stock (Show)
  deriving newtype (Display, IsString)

-- add more when needed
data DebArch = Amd64 | Arm64
  deriving stock (Show, Read, Enum, Bounded)

instance Display DebArch where
  display Amd64 = "amd64"
  display Arm64 = "arm64"

newtype Suite = Suite Text
  deriving stock (Show)
  deriving newtype (Display, IsString)

data DebDependencyPackage = DebDependencyPackage
  { ddpPackage :: DebPackage,
    ddpDependencyFile :: Set DependencyWithInfoFromDeb
  }
  deriving stock (Show)

instance Display DebDependencyPackage where
  display (DebDependencyPackage package _file) = display package

newtype NixString = NixString Text
  deriving stock (Show)
  deriving newtype (ToText)

newtype NixAttribute = NixAttribute Text
  deriving stock (Show)
  deriving newtype (IsString, Display)

newtype NixInstallable = NixInstallable Text
  deriving stock (Show)
  deriving newtype (Display)

newtype DebPackageName = DebPackageName Text
  deriving stock (Show)
  deriving newtype (Display)

data DebMaintainer = DebMaintainer MaintainerName MaintainerEmail
  deriving stock (Show)

instance Display DebMaintainer where
  display (DebMaintainer name email) = display name <> " <" <> display email <> ">"

newtype MaintainerName = MaintainerName Text
  deriving stock (Show)
  deriving newtype (Display, IsString)

newtype MaintainerEmail = MaintainerEmail Text
  deriving stock (Show)
  deriving newtype (IsString, Display)

newtype DebDescription = DebDescription Text
  deriving stock (Show)
  deriving newtype (Display)

newtype Url = Url Text
  deriving stock (Show)
  deriving newtype (Display, ToString)

newtype Tags = Tags {unTags :: [Tag Text]}
  deriving stock (Show)

data ScrapeError = NetworkError | EmptyResult | UnknownError
  deriving stock (Show)

instance Display ScrapeError where
  display NetworkError = "network error (try again later)"
  display EmptyResult = "empty result"
  display UnknownError = "unknown error"

data Options = Options
  { nixPackageOutputDirectory :: NixPackageOutputDirectory,
    nixInstallable :: NixInstallable,
    suite :: Suite,
    arch :: DebArch,
    maintainerName :: MaintainerName,
    maintainerEmail :: MaintainerEmail,
    logLevel :: Severity
  }

class HasCliOptions env where
  getCliOptions :: env -> Options

data NixPnameParseError
  = UnsupportedNixStoreDir NixStorePath
  | CannotSplitHashAndName NixStorePath
  | EmptyNixName
  deriving stock (Show)

instance Display NixPnameParseError where
  display (UnsupportedNixStoreDir nixStorePath) = "Unsupported nix store dir: " <> display nixStorePath
  display (CannotSplitHashAndName nixStorePath) = "Fail to split nix hash and name using dash (-): " <> display nixStorePath
  display EmptyNixName = "Empty Nix name"

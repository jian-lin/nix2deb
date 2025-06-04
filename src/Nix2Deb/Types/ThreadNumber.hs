module Nix2Deb.Types.ThreadNumber
  ( ThreadNumber,
    mkThreadNumber,
    unThreadNumber,
  )
where

import Language.Haskell.TH.Syntax (Lift)
import Relude

newtype ThreadNumber = ThreadNumber Int
  deriving stock (Show, Lift)

mkThreadNumber :: Int -> Either ThreadNumberError ThreadNumber
mkThreadNumber threadNumber =
  if threadNumber <= 0
    then Left NonPositiveThreadNumber
    else Right $ ThreadNumber threadNumber

unThreadNumber :: ThreadNumber -> Int
unThreadNumber (ThreadNumber threadNumber) = threadNumber

data ThreadNumberError = NonPositiveThreadNumber
  deriving stock (Show)

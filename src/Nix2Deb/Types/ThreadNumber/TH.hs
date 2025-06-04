module Nix2Deb.Types.ThreadNumber.TH
  ( mkThreadNumberTH,
  )
where

import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Syntax (lift)
import Nix2Deb.Types.ThreadNumber (mkThreadNumber)
import Relude hiding (lift)

-- | A type-safe way to write literal ThreadNumber values
mkThreadNumberTH :: Int -> Q Exp
mkThreadNumberTH threadNumber =
  either (fail . show) lift $ mkThreadNumber threadNumber

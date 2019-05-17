module Hkl.Python.QQ
    ( str ) where

import Language.Haskell.TH (stringE)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

str :: QuasiQuoter
str = QuasiQuoter { quoteExp = stringE
                  , quotePat = error "not used"
                  , quoteType = error "not used"
                  , quoteDec = error "not used"
                  }

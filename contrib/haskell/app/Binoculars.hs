{-# LANGUAGE GADTs              #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

{-
    Copyright  : Copyright (C) 2014-2020 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}
module Main where

import           Hkl.Binoculars

main :: IO ()
main = process

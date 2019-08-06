{-
    Copyright  : Copyright (C) 2014-2019 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Hkl.C.DArray
       (DArray(..)) where

import Foreign (peekArray)
import Foreign.C (CSize, CString)
import Foreign.Storable (Storable(..))

#include "hkl.h"

data DArray a = DArray CSize [a] deriving Show

instance Storable (DArray CString) where
  alignment _ = #{alignment darray_string}
  sizeOf _ = #{size darray_string}
  poke _ _ = undefined
  peek ptr = do
    n <- (#{peek darray_string, size} ptr)
    items <- #{peek darray_string ,item} ptr
    ss <- peekArray (fromEnum n) items
    return $ DArray n ss

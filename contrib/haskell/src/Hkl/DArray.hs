{-
    Copyright  : Copyright (C) 2014-2019, 2022 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.DArray
       ( DArray(..)
       , peek'darray_engine
       , peek'darray_string
       ) where

import           Foreign          (Ptr, peekArray)
import           Foreign.C        (CSize, CString)
import           Foreign.Storable (Storable (..))

import           Hkl.C.Hkl

data DArray a = DArray CSize [a] deriving Show

peek'darray_engine :: Ptr C'darray_engine -> IO (DArray (Ptr C'HklEngine))
peek'darray_engine ptr = do
  (C'darray_engine items n _) <- peek ptr
  DArray n
    <$> peekArray (fromEnum n) items

peek'darray_string :: Ptr C'darray_string -> IO (DArray CString)
peek'darray_string ptr = do
  (C'darray_string items n _) <- peek ptr
  DArray n
    <$> peekArray (fromEnum n) items

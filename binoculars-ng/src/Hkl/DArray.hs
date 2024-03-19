{-
    Copyright  : Copyright (C) 2014-2019, 2022, 2023 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.DArray
       ( DArray(..)
       , peekDarrayEngine
       , peekDarrayString
       ) where

import           Foreign          (Ptr, peekArray)
import           Foreign.C        (CSize, CString)
import           Foreign.Storable (Storable (..))

import           Hkl.C.Hkl

data DArray a = DArray CSize [a] deriving Show

peekDarrayEngine :: Ptr C'darray_engine -> IO (DArray (Ptr C'HklEngine))
peekDarrayEngine ptr = do
  (C'darray_engine items n _) <- peek ptr
  DArray n
    <$> peekArray (fromEnum n) items

peekDarrayString :: Ptr C'darray_string -> IO (DArray CString)
peekDarrayString ptr = do
  (C'darray_string items n _) <- peek ptr
  DArray n
    <$> peekArray (fromEnum n) items

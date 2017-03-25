{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Hkl.C.DArray
       (DArray(..)) where

import Foreign (peekArray)
import Foreign.C (CSize, CString)
import Foreign.Storable (Storable(..))

#include "hkl.h"

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data DArray a = DArray CSize [a] deriving Show

instance Storable (DArray CString) where
  alignment _ = #{alignment darray_string}
  sizeOf _ = #{size darray_string}
  peek ptr = do
    n <- (#{peek darray_string, size} ptr)
    items <- #{peek darray_string ,item} ptr
    ss <- peekArray (fromEnum n) items
    return $ DArray n ss

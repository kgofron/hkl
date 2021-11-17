{-# LANGUAGE UnicodeSyntax #-}

module Hkl.Image
    ( Image(..) )
    where

import           Data.Int           (Int32)
import           Data.Word          (Word16, Word32)
import           Foreign.ForeignPtr (ForeignPtr)

data Image = ImageInt32 (ForeignPtr Int32)
           | ImageWord16 (ForeignPtr Word16)
           | ImageWord32 (ForeignPtr Word32)
  deriving Show

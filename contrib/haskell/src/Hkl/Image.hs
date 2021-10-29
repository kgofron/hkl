{-# LANGUAGE UnicodeSyntax #-}

module Hkl.Image
    ( Image(..) )
    where

import           Data.Int           (Int32)
import           Data.Word          (Word16)
import           Foreign.ForeignPtr (ForeignPtr)

data Image = ImageWord16 (ForeignPtr Word16)
           | ImageInt32 (ForeignPtr Int32)
  deriving Show

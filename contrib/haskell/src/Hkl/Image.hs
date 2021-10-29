{-# LANGUAGE UnicodeSyntax #-}

module Hkl.Image
    ( Image(..) )
    where

import           Data.Word          (Word16)
import           Foreign.ForeignPtr (ForeignPtr)

data Image = ImageWord16 (ForeignPtr Word16)
  deriving Show

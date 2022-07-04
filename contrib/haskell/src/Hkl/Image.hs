{-# LANGUAGE UnicodeSyntax #-}

module Hkl.Image
    ( Image(..) )
    where

import           Data.Int                     (Int32)
import           Data.Vector.Storable.Mutable (IOVector)
import           Data.Word                    (Word16, Word32)

data Image = ImageInt32 (IOVector Int32)
           | ImageWord16 (IOVector Word16)
           | ImageWord32 (IOVector Word32)

instance Show Image where
  show (ImageInt32 _)  = "ImageInt32"
  show (ImageWord16 _) = "ImageWord16"
  show (ImageWord32 _) = "ImageWord32"

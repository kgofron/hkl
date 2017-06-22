{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hkl.Tiff
       ( ToTiff(..) ) where

import Codec.Picture ( DynamicImage )

class ToTiff a where
  toTiff ∷ a → IO DynamicImage

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hkl.Orphan where

import           Data.Array.Repa                 (Array, Shape, extent,
                                                  showShape)
import           Data.Array.Repa.Repr.ForeignPtr (F)
import           Foreign.Storable                (Storable)

instance (Shape sh, Storable e) => Show (Array F sh e) where
    show = showShape . extent

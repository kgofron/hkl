{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hkl.Orphan where

import           Data.Array.Repa                 (Array)
import           Data.Array.Repa.Repr.ForeignPtr (F)

instance Show (Array F sh e) where
    show _ = ""

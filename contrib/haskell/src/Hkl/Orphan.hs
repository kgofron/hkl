{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hkl.Orphan where

import           Data.Aeson                      (FromJSON (..), ToJSON (..),
                                                  Value (..))
import           Data.Array.Repa                 (Array, Shape, extent,
                                                  showShape)
import           Data.Array.Repa.Repr.ForeignPtr (F)
import           Data.ByteString.Char8           (ByteString)
import           Data.Text.Encoding              (decodeUtf8, encodeUtf8)
import           Foreign.Storable                (Storable)
import           GHC.Base                        (Alternative (..))
import           Path                            (Abs, Dir, Path, mkAbsDir)
import           Test.QuickCheck                 (Arbitrary (..), elements)

import           Prelude                         hiding (unwords)

import           Hkl.C.Binoculars

---------------
-- Arbitrary --
---------------

instance Arbitrary (Path Abs Dir) where
  arbitrary = pure $(mkAbsDir "/toto")

instance Arbitrary HklBinocularsSurfaceOrientationEnum where
  arbitrary = elements ([minBound .. maxBound] :: [HklBinocularsSurfaceOrientationEnum])

instance Arbitrary HklBinocularsQCustomSubProjectionEnum where
  arbitrary = elements ([minBound .. maxBound] :: [HklBinocularsQCustomSubProjectionEnum])

-----------------
-- From/ToJSON --
-----------------

instance ToJSON ByteString where
    toJSON = String . decodeUtf8
    {-# INLINE toJSON #-}

instance FromJSON ByteString where
    parseJSON (String t) = pure . encodeUtf8 $ t
    parseJSON _          = GHC.Base.empty
    {-# INLINE parseJSON #-}

----------
-- Show --
----------

instance (Shape sh, Storable e) => Show (Array F sh e) where
    show = showShape . extent

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}

{-
    Copyright  : Copyright (C) 2014-2022 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}
module Hkl.Binoculars.Projections
  ( Space(..)
  , newSpace
  , saveCube
  , withGeometry
  , withMaybeLimits
  , withMaybeMask
  , withNPixels
  , withPixelsDims
  , withResolutions
  , withSampleAxis
  ) where

import           Control.Monad                   (zipWithM)
import           Data.Array.Repa                 (Array, Shape, extent,
                                                  listOfShape, size)
import           Data.Array.Repa.Index           (DIM2, DIM3)
import           Data.Array.Repa.Repr.ForeignPtr (F, toForeignPtr)
import           Data.ByteString                 (useAsCString)
import           Data.Text.Encoding              (encodeUtf8)
import           Foreign.C.String                (CString, withCString)
import           Foreign.C.Types                 (CBool, CSize (..))
import           Foreign.ForeignPtr              (ForeignPtr, newForeignPtr,
                                                  withForeignPtr)
import           Foreign.Marshal.Array           (withArrayLen)
import           Foreign.Ptr                     (Ptr, nullPtr)
import           GHC.Exts                        (IsList (..))

import           Prelude                         hiding (drop)

import           Hkl.Binoculars.Config
import           Hkl.C
import           Hkl.Detector
import           Hkl.Geometry
import           Hkl.Orphan                      ()

--  Common

withNPixels :: Detector a DIM2 -> (CSize -> IO r) -> IO r
withNPixels d f = f (toEnum . size . shape $ d)

withPixelsDims :: Array F DIM3 Double -> (Int -> Ptr CSize -> IO r) -> IO r
withPixelsDims p = withArrayLen (map toEnum $ listOfShape . extent $ p)

saveCube :: Shape sh => FilePath -> [Cube sh] -> IO ()
saveCube o rs = do
  let c = (mconcat rs)
  case c of
    (Cube fp) ->
        withCString o $ \fn ->
        withForeignPtr fp $ \p ->
            c'hkl_binoculars_cube_save_hdf5 fn p
    EmptyCube -> return ()

withMaybeLimits :: Shape sh
                => Maybe (RLimits sh)
                -> Resolutions sh
                -> (Int -> Ptr (Ptr C'HklBinocularsAxisLimits) -> IO r)
                -> IO r
withMaybeLimits mls rs f = do
  case mls of
    Nothing   -> f 0 nullPtr
    (Just ls) -> do
      ptrs <- zipWithM newLimits (toList ls) (toList rs)
      withForeignPtrs ptrs $ \pts -> withArrayLen pts f

withMaybeMask :: Maybe Mask -> (Ptr CBool -> IO r) -> IO r
withMaybeMask mm f = case mm of
                       Nothing  -> f nullPtr
                       (Just m) -> withForeignPtr (toForeignPtr m) $ \ptr -> f ptr

withResolutions :: Shape sh => Resolutions sh -> (Int -> Ptr Double -> IO r) -> IO r
withResolutions r = withArrayLen . toList $ r

withSampleAxis :: SampleAxis -> (CString -> IO r) -> IO r
withSampleAxis (SampleAxis t) =  useAsCString (encodeUtf8 t)

-----------
-- Space --
-----------

newtype Space sh = Space (ForeignPtr C'HklBinocularsSpace)
  deriving Show

newSpace' :: Ptr C'HklBinocularsSpace -> IO (Space sh)
newSpace' p = Space <$> (newForeignPtr p'hkl_binoculars_space_free p)

newSpace :: (Shape sh1, Shape sh) => Detector a sh1 -> Int -> IO (Space sh)
newSpace d n = do
  let nPixels = toEnum . size . shape $ d
  let nDims = toEnum n
  newSpace' =<< c'hkl_binoculars_space_new nPixels nDims

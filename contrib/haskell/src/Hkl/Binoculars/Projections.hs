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
  ( DetectorPath(..)
  , Space(..)
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
import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.Array.Repa                 (Array, Shape, extent,
                                                  listOfShape, size)
import           Data.Array.Repa.Index           (DIM2, DIM3)
import           Data.Array.Repa.Repr.ForeignPtr (F, toForeignPtr)
import           Data.ByteString                 (useAsCString)
import           Data.Text.Encoding              (encodeUtf8)
import           Data.Word                       (Word16)
import           Foreign.C.String                (CString, withCString)
import           Foreign.C.Types                 (CBool, CSize (..))
import           Foreign.ForeignPtr              (ForeignPtr, newForeignPtr,
                                                  withForeignPtr)
import           Foreign.Marshal.Array           (withArrayLen)
import           Foreign.Ptr                     (Ptr, nullPtr)
import           GHC.Generics                    (Generic)

import           Prelude                         hiding (drop)

import           Hkl.Binoculars.Config
import           Hkl.C
import           Hkl.Detector
import           Hkl.Geometry
import           Hkl.H5                          hiding (File)
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

withMaybeLimits :: Maybe (RLimits a)
                -> Resolutions a
                -> (Int -> Ptr (Ptr C'HklBinocularsAxisLimits) -> IO r)
                -> IO r
withMaybeLimits mls rs f = do
  let rs' = case rs of
              (Resolutions2 r1 r2)    -> [r1, r2]
              (Resolutions3 r1 r2 r3) -> [r1, r2, r3]
  case mls of
    Nothing   -> f 0 nullPtr
    (Just ls) -> do
      let ls' = case ls of
                  (Limits2 l1 l2)    -> [l1, l2]
                  (Limits3 l1 l2 l3) -> [l1, l2, l3]
      ptrs <- zipWithM newLimits ls' rs'
      withForeignPtrs ptrs $ \pts ->
        withArrayLen pts f

withMaybeMask :: Maybe Mask -> (Ptr CBool -> IO r) -> IO r
withMaybeMask mm f = case mm of
                       Nothing  -> f nullPtr
                       (Just m) -> withForeignPtr (toForeignPtr m) $ \ptr -> f ptr

withResolutions :: Resolutions a -> (Int -> Ptr Double -> IO r) -> IO r
withResolutions (Resolutions2 r1 r2)    = withArrayLen [r1, r2]
withResolutions (Resolutions3 r1 r2 r3) = withArrayLen [r1, r2, r3]

withSampleAxis :: SampleAxis -> (CString -> IO r) -> IO r
withSampleAxis (SampleAxis t) =  useAsCString (encodeUtf8 t)

-- DetectorPath

newtype DetectorPath = DetectorPath
    { detectorPathImage    :: Hdf5Path DIM3 Word16
    } deriving (Eq, Generic, Show, FromJSON, ToJSON)


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

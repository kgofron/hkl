{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiWayIf            #-}

{-
    Copyright  : Copyright (C) 2014-2020 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}
module Hkl.Binoculars.Projections
  ( DataFrameHkl(..)
  , DataFrameQxQyQz(..)
  , FramesHklP(..)
  , FramesQxQyQzP(..)
  , InputQxQyQz(..)
  , LenP(..)
  , mkInputHkl
  , mkInputQxQyQz
  , processHkl
  , processQxQyQz
  ) where

import           Control.Concurrent.Async          (mapConcurrently)
import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Data.Array.Repa                   (Array, extent, listOfShape,
                                                    size)
import           Data.Array.Repa.Index             (DIM2, DIM3)
import           Data.Array.Repa.Repr.ForeignPtr   (F, toForeignPtr)
import           Data.Maybe                        (fromMaybe)
import           Data.Word                         (Word16, Word8)
import           Foreign.C.Types                   (CInt (..))
import           Foreign.ForeignPtr                (ForeignPtr, withForeignPtr)
import           Foreign.Marshal.Array             (withArrayLen)
import           Foreign.Ptr                       (Ptr)
import           Foreign.Storable                  (peek)
import           Numeric.Units.Dimensional.NonSI   (angstrom)
import           Numeric.Units.Dimensional.Prelude (Angle, Length, degree, (*~),
                                                    (/~))
import           Pipes                             (Pipe, each, runEffect,
                                                    (>->))
import           Pipes.Prelude                     (mapM)
import           Pipes.Safe                        (SafeT, runSafeT)

import           Prelude                           hiding (mapM)

import           Hkl.Binoculars.Common
import           Hkl.Binoculars.Config
import           Hkl.C.Binoculars
import           Hkl.C.Geometry
import           Hkl.C.Sample
import           Hkl.Detector
import           Hkl.H5                            hiding (File)
import           Hkl.Orphan                        ()
import           Hkl.Types

-- | Common

withK :: WaveLength -> (Double -> IO r) -> IO r
withK w f = f (2 * pi / (w /~ angstrom))

withNPixels :: Detector a DIM2 -> (CInt -> IO r) -> IO r
withNPixels d f = f (toEnum . size . shape $ d)

withPixelsDims :: Array F DIM3 Double -> (Int -> Ptr CInt -> IO r) -> IO r
withPixelsDims p = withArrayLen (map toEnum $ listOfShape . extent $ p)

saveCube :: FilePath -> [Cube' DIM3] -> IO ()
saveCube o rs = saveHdf5 o =<< toCube (mconcat rs)

-- | QxQyQz Projection

type Resolutions = [Double]

data InputQxQyQz a b =
  InputQxQyQz { detector     :: Detector b DIM2
              , filename     :: InputFn
              , h5dpath      :: a
              , output       :: FilePath
              , resolutions  :: [Double]
              , centralPixel :: (Int, Int)  -- x, y
              , sdd'         :: Length Double  -- sample to detector distance
              , detrot'      :: Angle Double
              , mask         :: Array F DIM2 Word8
              }
  deriving Show

data DataFrameQxQyQz
    = DataFrameQxQyQz
      Int -- n
      Geometry -- geometry
      (ForeignPtr Word16) -- image
    deriving Show

class LenP a => FramesQxQyQzP a where
  framesQxQyQzP :: a -> Detector b DIM2 -> Pipe (Chunk Int FilePath) DataFrameQxQyQz (SafeT IO) ()

{-# INLINE spaceQxQyQz #-}
spaceQxQyQz :: Detector a DIM2 -> Array F DIM3 Double -> Resolutions -> DataFrameQxQyQz -> IO (DataFrameSpace DIM3)
spaceQxQyQz det pixels rs (DataFrameQxQyQz _ g@(Geometry _ (Source w) _ _) img) =
  withK w $ \k ->
    withNPixels det $ \nPixels ->
    withGeometry g $ \geometry ->
    withForeignPtr (toForeignPtr pixels) $ \pix ->
    withArrayLen rs $ \nr r ->
    withPixelsDims pixels $ \ndim dims ->
    withForeignPtr img $ \i -> do
      p <- {-# SCC "hkl_binoculars_space_q" #-} hkl_binoculars_space_q geometry k i nPixels pix (toEnum ndim) dims r (toEnum nr)
      s <- peek p
      return (DataFrameSpace img s)

mkJobsQxQyQz :: LenP a => InputQxQyQz a b -> IO [[Chunk Int FilePath]]
mkJobsQxQyQz (InputQxQyQz _ fn h5d _ _ _ _ _ _) = mkJobs fn h5d

mkInputQxQyQz :: FramesQxQyQzP a => BinocularsConfig -> Detector b DIM2 -> (BinocularsConfig -> Maybe a) -> IO (InputQxQyQz a b)
mkInputQxQyQz c d f = do
  fs <- files c
  mask' <- getDetectorDefaultMask d (_binocularsInputMaskmatrix c)
  case f c of
    (Just h5dpath') -> pure $ InputQxQyQz
                      { detector = d
                      , filename = InputList fs
                      , h5dpath = h5dpath'
                      , output = case _binocularsInputInputrange c of
                                   Just r  -> destination' r (_binocularsDispatcherDestination c)
                                   Nothing -> destination' (ConfigRange []) (_binocularsDispatcherDestination c)
                      , resolutions = _binocularsProjectionResolution c
                      , centralPixel = _binocularsInputCentralpixel c
                      , sdd' = _binocularsInputSdd c
                      , detrot' = fromMaybe (0 *~ degree) ( _binocularsInputDetrot c)
                      , mask = mask'
                      }
    Nothing -> error "TODO"

processQxQyQz :: FramesQxQyQzP a => InputQxQyQz a b -> IO ()
processQxQyQz input@(InputQxQyQz det _ h5d o res cen d r _) = do
  pixels <- getPixelsCoordinates det cen d r
  jobs <- mkJobsQxQyQz input
  r' <- mapConcurrently (\job -> withCubeAccumulator $ \s ->
                           runSafeT $ runEffect $
                           each job
                           >-> framesQxQyQzP h5d det
                           >-> mapM (liftIO . spaceQxQyQz det pixels res)
                           >-> mkCube'P det s
                       ) jobs
  saveCube o r'

-- | Hkl Projection

data InputHkl a b =
  InputHkl { detector     :: Detector b DIM2
           , filename     :: InputFn
           , h5dpath      :: a
           , output       :: FilePath
           , resolutions  :: [Double]
           , centralPixel :: (Int, Int)  -- x, y
           , sdd'         :: Length Double  -- sample to detector distance
           , detrot'      :: Angle Double
           , config       :: BinocularsConfig
           , mask         :: Array F DIM2 Word8
           }
  deriving Show

data DataFrameHkl a
    = DataFrameHkl
      Int -- position in the stream
      (ForeignPtr Word16) -- image
      Geometry -- geometry
      (Sample Triclinic) --  the sample part
      -- (Array F DIM2 Double) -- ub

instance Show (DataFrameHkl a) where
  show (DataFrameHkl q _ _ _) = show q

class LenP a => FramesHklP a where
  framesHklP :: a -> Detector b DIM2 -> Pipe (Chunk Int FilePath) (DataFrameHkl b) (SafeT IO) ()

{-# INLINE spaceHkl #-}
spaceHkl :: BinocularsConfig -> Detector b DIM2 -> Array F DIM3 Double -> Resolutions -> DataFrameHkl b -> IO (DataFrameSpace DIM3)
spaceHkl config' det pixels rs (DataFrameHkl _ img g@(Geometry _ (Source w) _ _) samp) = do
  let sample' = overloadSampleWithConfig config' samp
  withK w $ \k ->
    withNPixels det $ \nPixels ->
    withGeometry g $ \geometry ->
    withSample sample' $ \sample ->
    withForeignPtr (toForeignPtr pixels) $ \pix ->
    withArrayLen rs $ \nr r ->
    withPixelsDims pixels $ \ndim dims ->
    withForeignPtr img $ \i -> do
      p <- {-# SCC "hkl_binoculars_space_hkl" #-} hkl_binoculars_space_hkl geometry sample k i nPixels pix (toEnum ndim) dims r (toEnum nr)
      s <- peek p
      return (DataFrameSpace img s)

mkJobsHkl :: LenP a => InputHkl a b -> IO [[Chunk Int FilePath]]
mkJobsHkl (InputHkl _ fn h5d _ _ _ _ _ _ _) = mkJobs fn h5d

mkInputHkl :: FramesHklP a => BinocularsConfig -> Detector b DIM2 -> (BinocularsConfig -> Maybe a) -> IO (InputHkl a b)
mkInputHkl c d f = do
  fs <- files c
  mask' <- getDetectorDefaultMask d (_binocularsInputMaskmatrix c)
  case f c of
    (Just h5dpath') -> pure $ InputHkl
                      { detector = d
                      , filename = InputList fs
                      , h5dpath = h5dpath'
                      , output = destination'
                                 (fromMaybe (ConfigRange []) (_binocularsInputInputrange c))
                                 (_binocularsDispatcherDestination c)
                      , resolutions = _binocularsProjectionResolution c
                      , centralPixel = _binocularsInputCentralpixel c
                      , sdd' = _binocularsInputSdd c
                      , detrot' = fromMaybe (0 *~ degree) (_binocularsInputDetrot c)
                      , config = c
                      , mask = mask'
                      }
    Nothing -> error "TODO"

processHkl :: FramesHklP a => InputHkl a b -> IO ()
processHkl input@(InputHkl det _ h5d o res cen d r config' _) = do
  pixels <- getPixelsCoordinates det cen d r

  jobs <- mkJobsHkl input
  r' <- mapConcurrently (\job -> withCubeAccumulator $ \s ->
                           runSafeT $ runEffect $
                           each job
                           >-> framesHklP h5d det
                           >-> mapM (liftIO . spaceHkl config' det pixels res)
                           >-> mkCube'P det s
                       ) jobs
  saveCube o r'

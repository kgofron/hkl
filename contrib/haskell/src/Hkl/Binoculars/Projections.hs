{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}

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
  ( DataFrameQxQyQz(..)
  , FramesQxQyQzP(..)
  , InputQxQyQz(..)
  , LenP(..)
  , mkInputQxQyQz
  , processQxQyQz
  , processHkl
  ) where

import           Control.Concurrent.Async          (mapConcurrently)
import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Data.Array.Repa                   (Array, extent, listOfShape,
                                                    size)
import           Data.Array.Repa.Index             (DIM2, DIM3)
import           Data.Array.Repa.Repr.ForeignPtr   (F, toForeignPtr)
import           Data.Maybe                        (fromMaybe)
import           Data.Word                         (Word16)
import           Foreign.C.Types                   (CInt (..))
import           Foreign.ForeignPtr                (ForeignPtr, withForeignPtr)
import           Foreign.Marshal.Array             (withArrayLen)
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
import           Hkl.Types

-- | QxQyQz Projection

type Resolutions = [Double]

data InputQxQyQz a =
  InputQxQyQz { filename     :: InputFn
              , h5dpath      :: a
              , output       :: FilePath
              , resolutions  :: [Double]
              , centralPixel :: (Int, Int)  -- x, y
              , sdd'         :: Length Double  -- sample to detector distance
              , detrot'      :: Angle Double
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
spaceQxQyQz detector pixels rs (DataFrameQxQyQz _ g@(Geometry _ (Source w) _ _) img) = do
  let k = 2 * pi / (w /~ angstrom)
  let nPixels = size . shape $ detector
  let pixelsDims = map toEnum $ listOfShape . extent $ pixels :: [CInt]
  withGeometry g $ \geometry ->
    withForeignPtr (toForeignPtr pixels) $ \pix ->
    withArrayLen rs $ \nr r ->
    withArrayLen pixelsDims $ \ndim dims ->
    withForeignPtr img $ \i -> do
      p <- {-# SCC "hkl_binoculars_space_q" #-} hkl_binoculars_space_q geometry k i (toEnum nPixels) pix (toEnum ndim) dims r (toEnum nr)
      s <- peek p
      return (DataFrameSpace img s)

mkJobsQxQyQz :: LenP a => InputQxQyQz a -> IO [[Chunk Int FilePath]]
mkJobsQxQyQz (InputQxQyQz fn h5d _ _ _ _ _) = mkJobs fn h5d

mkInputQxQyQz :: FramesQxQyQzP a => BinocularsConfig -> (InputType -> a) -> IO (InputQxQyQz a)
mkInputQxQyQz c' f = do
  fs <- files c'
  pure $ InputQxQyQz { filename = InputList fs
                     , h5dpath = f (itype . bInput $ c')
                     , output = case inputrange . bInput $ c' of
                                  Just r  -> destination' r (destination . bDispatcher $ c')
                                  Nothing -> destination' (ConfigRange []) (destination . bDispatcher $ c')
                     , resolutions = resolution . bProjection $ c'
                     , centralPixel = centralpixel . bInput $ c'
                     , sdd' = sdd . bInput $ c'
                     , detrot' = fromMaybe (0 *~ degree) (detrot . bInput $ c')
                     }

processQxQyQz :: FramesQxQyQzP a => InputQxQyQz a -> IO ()
processQxQyQz input@(InputQxQyQz _ h5d o res cen d r) = do
  let detector = ImXpadS140

  pixels <- getPixelsCoordinates detector cen d r

  jobs <- mkJobsQxQyQz input
  r' <- mapConcurrently (\job -> withCubeAccumulator $ \s ->
                           runSafeT $ runEffect $
                           each job
                           >-> framesQxQyQzP h5d detector
                           >-> mapM (liftIO . spaceQxQyQz detector pixels res)
                           >-> mkCube'P detector s
                       ) jobs
  let c' = mconcat r'
  c'' <- toCube c'
  saveHdf5 o c''
  print c''

  return ()

-- | Hkl

data InputHkl a b =
  InputHkl { filename     :: InputFn
           , h5dpath      :: a
           , output       :: FilePath
           , resolutions  :: [Double]
           , centralPixel :: (Int, Int)  -- x, y
           , sdd'         :: Length Double  -- sample to detector distance
           , detrot'      :: Angle Double
           , sample'      :: Sample b
           }
  deriving Show

data DataFrameHkl a
    = DataFrameHkl
      Int -- n
      Geometry -- geometry
      (ForeignPtr Word16) -- image
      (Sample a) -- sample
      (Array F DIM2 Double) -- ub

instance Show (DataFrameHkl a) where
  show (DataFrameHkl q _ _ _ _) = show q

class LenP a => FramesHklP a where
  framesHklP :: a -> Detector b DIM2 -> Pipe (Chunk Int FilePath) (DataFrameHkl b) (SafeT IO) ()

{-# INLINE spaceHkl #-}
spaceHkl :: Sample a -> Detector b DIM2 -> Array F DIM3 Double -> Resolutions -> (DataFrameHkl b) -> IO (DataFrameSpace DIM3)
spaceHkl samp detector pixels rs (DataFrameHkl _ g@(Geometry _ (Source w) _ _) img _sampl _ub) = do
  let k = 2 * pi / (w /~ angstrom)
  let nPixels = size . shape $ detector
  let pixelsDims = map toEnum $ listOfShape . extent $ pixels :: [CInt]
  withGeometry g $ \geometry ->
    withSample samp $ \sample ->
    withForeignPtr (toForeignPtr pixels) $ \pix ->
    withArrayLen rs $ \nr r ->
    withArrayLen pixelsDims $ \ndim dims ->
    withForeignPtr img $ \i -> do
      p <- {-# SCC "hkl_binoculars_space_q" #-} hkl_binoculars_space_hkl geometry sample k i (toEnum nPixels) pix (toEnum ndim) dims r (toEnum nr)
      s <- peek p
      return (DataFrameSpace img s)

mkJobsHkl :: LenP a => InputHkl a b -> IO [[Chunk Int FilePath]]
mkJobsHkl (InputHkl fn h5d _ _ _ _ _ _) = mkJobs fn h5d

processHkl :: FramesHklP a => InputHkl a b -> IO ()
processHkl input@(InputHkl _ h5d o res cen d r sample) = do
  let detector = ImXpadS140

  pixels <- getPixelsCoordinates detector cen d r

  jobs <- mkJobsHkl input
  r' <- mapConcurrently (\job -> withCubeAccumulator $ \s ->
                           runSafeT $ runEffect $
                           each job
                           >-> framesHklP h5d detector
                           >-> mapM (liftIO . spaceHkl sample detector pixels res)
                           >-> mkCube'P detector s
                       ) jobs
  let c' = mconcat r'
  c'' <- toCube c'
  saveHdf5 o c''
  print c''

  return ()

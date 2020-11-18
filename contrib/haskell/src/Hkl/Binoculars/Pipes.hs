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
module Hkl.Binoculars.Pipes
  ( Chunk(..)
  , InputFn(..)
  , DataFrameSpace(..)
  , LenP(..)
  , mkCube'P
  , mkJobs
  , mkInputHkl
  , mkInputQxQyQz
  , processHkl
  , processQxQyQz
  ) where

import           Bindings.HDF5.Core                (Location)
import           Control.Concurrent.Async          (mapConcurrently)
import           Control.Monad                     (forM_, forever)
import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Control.Monad.Trans.Cont          (cont, runCont)
import           Data.Array.Repa                   (Shape, size)
import           Data.Array.Repa.Index             (DIM1, DIM2)
import           Data.IORef                        (IORef, modifyIORef')
import           Data.Maybe                        (fromMaybe)
import           Data.Vector.Storable              (fromList)
import           Data.Word                         (Word16)
import           Foreign.ForeignPtr                (ForeignPtr)
import           GHC.Conc                          (getNumCapabilities)
import           Numeric.Units.Dimensional.NonSI   (angstrom)
import           Numeric.Units.Dimensional.Prelude (Quantity, Unit, degree,
                                                    (*~))
import           Pipes                             (Consumer, Pipe, await, each,
                                                    runEffect, yield, (>->))
import           Pipes.Prelude                     (print, tee, toListM)
import           Pipes.Safe                        (MonadSafe, SafeT, bracket,
                                                    runSafeT)

import           Hkl.Binoculars.Common
import           Hkl.Binoculars.Config
import           Hkl.Binoculars.Projections
import           Hkl.C.Binoculars
import           Hkl.C.Geometry
import           Hkl.Detector
import           Hkl.H5                            hiding (File)
import           Hkl.Pipes
import           Hkl.Types


-- LenP

class LenP a where
  lenP :: a -> Pipe FilePath Int (SafeT IO) ()


-- Jobs

mkJobs :: LenP a => InputFn -> a -> IO [[Chunk Int FilePath]]
mkJobs fn h5d = do
  let fns = concatMap (replicate 1) (toList fn)
  ns <- runSafeT $ toListM $ each fns >-> lenP h5d
  c' <- getNumCapabilities
  -- let c' = 1
  let ntot = sum ns
      c = if c' >= 2 then c' - 1 else c'
  return $ mkJobs' (quot ntot c) fns ns


-- Project

withSpace :: (MonadSafe m, Shape sh)
            => Detector a DIM2 -> Int -> (Space sh -> m r) -> m r
withSpace d n = bracket (liftIO $ newSpace d n) pure

project :: (MonadSafe m, Shape sh)
          => Detector a DIM2
        -> Int
        -> (Space sh -> b -> IO (DataFrameSpace sh))
        -> Pipe b (DataFrameSpace sh) m ()
project d n f = withSpace d n $ \s -> forever $ do
               df <- await
               yield =<< (liftIO $ f s df)


-- QxQyQz

class LenP a => FramesQxQyQzP a where
  framesQxQyQzP :: a -> Detector b DIM2 -> Pipe (Chunk Int FilePath) DataFrameQxQyQz (SafeT IO) ()

mkJobsQxQyQz :: LenP a => InputQxQyQz a -> IO [[Chunk Int FilePath]]
mkJobsQxQyQz (InputQxQyQz _ fn h5d _ _ _ _ _ _) = mkJobs fn h5d

mkInputQxQyQz :: FramesQxQyQzP a => BinocularsConfig -> (BinocularsConfig -> Maybe a) -> IO (InputQxQyQz a)
mkInputQxQyQz c f = do
  fs <- files c
  let d = fromMaybe defaultDetector (_binocularsInputDetector c)
  mask' <- getDetectorDefaultMask d (_binocularsInputMaskmatrix c)
  case f c of
    (Just h5dpath') -> pure $ InputQxQyQz
                      { detector = d
                      , filename = InputList fs
                      , h5dpath = h5dpath'
                      , output = case _binocularsInputInputRange c of
                                   Just r  -> destination' r (_binocularsDispatcherDestination c)
                                   Nothing -> destination' (ConfigRange []) (_binocularsDispatcherDestination c)
                      , resolutions = _binocularsProjectionResolution c
                      , centralPixel = _binocularsInputCentralpixel c
                      , sdd' = _binocularsInputSdd c
                      , detrot' = fromMaybe (0 *~ degree) ( _binocularsInputDetrot c)
                      , mask = mask'
                      }
    Nothing -> error "TODO"

processQxQyQz :: FramesQxQyQzP a => InputQxQyQz a -> IO ()
processQxQyQz input@(InputQxQyQz det _ h5d o res cen d r mask') = do
  pixels <- getPixelsCoordinates det cen d r
  jobs <- mkJobsQxQyQz input
  r' <- mapConcurrently (\job -> withCubeAccumulator $ \s ->
                           runSafeT $ runEffect $
                           each job
                           >-> framesQxQyQzP h5d det
                           >-> project det 3 (spaceQxQyQz det pixels res mask')
                           >-> mkCube'P det s
                       ) jobs
  saveCube o r'

-- Hkl

class LenP a => FramesHklP a where
  framesHklP :: a -> Detector b DIM2 -> Pipe (Chunk Int FilePath) (DataFrameHkl b) (SafeT IO) ()

mkJobsHkl :: LenP a => InputHkl a -> IO [[Chunk Int FilePath]]
mkJobsHkl (InputHkl _ fn h5d _ _ _ _ _ _ _) = mkJobs fn h5d

mkInputHkl :: FramesHklP a => BinocularsConfig -> (BinocularsConfig -> Maybe a) -> IO (InputHkl a)
mkInputHkl c f = do
  fs <- files c
  let d = fromMaybe defaultDetector (_binocularsInputDetector c)
  mask' <- getDetectorDefaultMask d (_binocularsInputMaskmatrix c)
  case f c of
    (Just h5dpath') -> pure $ InputHkl
                      { detector = d
                      , filename = InputList fs
                      , h5dpath = h5dpath'
                      , output = destination'
                                 (fromMaybe (ConfigRange []) (_binocularsInputInputRange c))
                                 (_binocularsDispatcherDestination c)
                      , resolutions = _binocularsProjectionResolution c
                      , centralPixel = _binocularsInputCentralpixel c
                      , sdd' = _binocularsInputSdd c
                      , detrot' = fromMaybe (0 *~ degree) (_binocularsInputDetrot c)
                      , config = c
                      , mask = mask'
                      }
    Nothing -> error "TODO"

processHkl :: FramesHklP a => InputHkl a -> IO ()
processHkl input@(InputHkl det _ h5d o res cen d r config' mask') = do
  pixels <- getPixelsCoordinates det cen d r

  jobs <- mkJobsHkl input
  r' <- mapConcurrently (\job -> withCubeAccumulator $ \s ->
                           runSafeT $ runEffect $
                           each job
                           >-> tee Pipes.Prelude.print
                           >-> framesHklP h5d det
                           >-> project det 3 (spaceHkl config' det pixels res mask')
                           >-> mkCube'P det s
                       ) jobs
  saveCube o r'

--  Create the Cube

mkCube'P :: (MonadIO m, Shape sh) => Detector a DIM2 -> IORef (Cube' sh) -> Consumer (DataFrameSpace sh) m ()
mkCube'P det ref = forever $ do
  s <- await
  c2 <- liftIO $ mkCube' det [s]
  liftIO $ modifyIORef' ref (c2 <>)

-- Instances

withDetectorPathP :: (MonadSafe m, Location l) => l -> Detector a DIM2 -> DetectorPath -> ((Int -> IO (ForeignPtr Word16)) -> m r) -> m r
withDetectorPathP f det (DetectorPath p) g = do
  let n = (size . shape $ det) * 2  -- hardcoded size
  withBytes n $ \buf ->
      withHdf5PathP f p $ \p' -> g (\j -> getArrayInBuffer buf det p' j)

nest :: [(r -> a) -> a] -> ([r] -> a) -> a
nest xs = runCont (Prelude.mapM cont xs)

withAxesPathP :: (MonadSafe m, Location l) => l -> [Hdf5Path DIM1 Double] -> ([Dataset] -> m a) -> m a
withAxesPathP f dpaths = nest (map (withHdf5PathP f) dpaths)

withGeometryPathP :: (MonadSafe m, Location l) => l -> GeometryPath -> ((Int -> IO Geometry) -> m r) -> m r
withGeometryPathP f (GeometryPathUhv w as) gg =
    withHdf5PathP f w $ \w' ->
    withAxesPathP f as $ \as' ->
        gg (\j -> Geometry
                 <$> pure Uhv
                 <*> (Source <$> getValueWithUnit w' 0 angstrom)
                 <*> (fromList <$> Prelude.mapM (`get_position` j) as')
                 <*> pure Nothing)
withGeometryPathP f (GeometryPathCristalK6C w m ko ka kp g d) gg =
    withHdf5PathP f w $ \w' ->
    withHdf5PathP f m $ \mu' ->
    withHdf5PathP f ko $ \komega' ->
    withHdf5PathP f ka $ \kappa' ->
    withHdf5PathP f kp $ \kphi' ->
    withHdf5PathP f g $ \gamma' ->
    withHdf5PathP f d $ \delta' -> do
      wavelength <- liftIO $ getValueWithUnit w' 0 angstrom
      mu <- liftIO $ get_position mu' 0
      komega <- liftIO $ get_position komega' 0
      kappa <- liftIO $ get_position kappa' 0
      gamma <- liftIO $ get_position gamma' 0
      delta <- liftIO $ get_position delta' 0
      gg (\j -> do
            kphi <- get_position kphi' j
            return (Geometry
                    K6c
                    (Source wavelength)
                    (fromList [mu, komega, kappa, kphi, gamma, delta])
                    Nothing))

--  FramesQxQyQzP

instance LenP QxQyQzPath where
  lenP (QxQyQzPath (DetectorPath i) _) = forever $ do
    fp <- await
    withFileP (openH5 fp) $ \f ->
      withHdf5PathP f i $ \i' -> do
      (_, ss) <- liftIO $ datasetShape i'
      case head ss of
        (Just n) -> yield (fromIntegral n)
        Nothing  -> error "can not extract length"

instance FramesQxQyQzP QxQyQzPath where
  framesQxQyQzP (QxQyQzPath d dif) det = forever $ do
    (Chunk fp from to) <- await
    withFileP (openH5 fp) $ \f ->
      withDetectorPathP f det d $ \getImage ->
      withGeometryPathP f dif $ \getDiffractometer ->
      forM_ [from..to-1] (\j -> yield =<< liftIO
                          (DataFrameQxQyQz
                           <$> pure j
                           <*> getDiffractometer j
                           <*> getImage j))

-- FramesHklP

getValueWithUnit :: Dataset -> Int -> Unit m d Double -> IO (Quantity d Double)
getValueWithUnit d j u = do
  v <- get_position d j
  return $ v *~ u

withSamplePathP :: (MonadSafe m, Location l) => l -> SamplePath -> (IO (Sample Triclinic) -> m r) -> m r
withSamplePathP f (SamplePath a b c alpha beta gamma ux uy uz) g =
    withHdf5PathP f a $ \a' ->
    withHdf5PathP f b $ \b' ->
    withHdf5PathP f c $ \c' ->
    withHdf5PathP f alpha $ \alpha' ->
    withHdf5PathP f beta $ \beta' ->
    withHdf5PathP f gamma $ \gamma' ->
    withHdf5PathP f ux $ \ux' ->
    withHdf5PathP f uy $ \uy' ->
    withHdf5PathP f uz $ \uz' ->
        g (Sample
           <$> pure "test"
           <*> (Triclinic
                <$> getValueWithUnit a' 0 angstrom
                <*> getValueWithUnit b' 0 angstrom
                <*> getValueWithUnit c' 0 angstrom
                <*> getValueWithUnit alpha' 0 degree
                <*> getValueWithUnit beta' 0 degree
                <*> getValueWithUnit gamma' 0 degree)
           <*> (Parameter
                <$> pure "ux"
                <*> get_position ux' 0
                <*> pure (Range 0 0))
           <*> (Parameter
                <$> pure "uy"
                <*> get_position uy' 0
                <*> pure (Range 0 0))
           <*> (Parameter
                <$> pure "uz"
                <*> get_position uz' 0
                <*> pure (Range 0 0)))


instance LenP HklPath where
  lenP (HklPath p _)           = lenP p
  lenP (HklPathFromQxQyQz p _) = lenP p

instance FramesHklP HklPath where
  framesHklP (HklPath (QxQyQzPath imgs dif) samp) det = forever $ do
    (Chunk fp from to) <- await
    withFileP (openH5 fp) $ \f ->
      withDetectorPathP f det imgs $ \getImage ->
      withGeometryPathP f dif $ \getDiffractometer ->
      withSamplePathP f samp $ \getSample ->
      forM_ [from..to-1] (\j -> yield =<< liftIO
                               (DataFrameHkl
                                <$> pure j
                                <*> getImage j
                                <*> getDiffractometer j
                                <*> getSample))
  framesHklP (HklPathFromQxQyQz (QxQyQzPath imgs dif) sample) det =
      forever $ do
        (Chunk fp from to) <- await
        withFileP (openH5 fp) $ \f ->
            withDetectorPathP f det imgs $ \getImage ->
            withGeometryPathP f dif $ \getDiffractometer ->
            forM_ [from..to-1] (\j -> yield =<< liftIO
                                     (DataFrameHkl
                                     <$> pure j
                                     <*> getImage j
                                     <*> getDiffractometer j
                                     <*> pure sample))

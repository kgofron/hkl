{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

{-
    Copyright  : Copyright (C) 2014-2020 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}
module Hkl.Binoculars.Conduit
  ( LenC(..)
  , FramesHklC(..)
  , FramesQxQyQzC(..)
  , mkJobsHklC
  , mkCube'C
  , processHklC
  ) where

import           Bindings.HDF5.Core                (Location)
import           Conduit                           (ConduitT, MonadResource,
                                                    Void, await, bracketP,
                                                    runConduitRes, yield, (.|))
import           Control.Concurrent.Async          (mapConcurrently)
import           Control.Monad                     (forM_)
import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Control.Monad.Trans.Cont          (cont, runCont)
import           Data.Array.Repa                   (Shape, size)
import           Data.Array.Repa.Index             (DIM1, DIM2)
import           Data.Conduit.Combinators          (sinkList)
import           Data.Conduit.List                 (sourceList)
import           Data.IORef                        (IORef, modifyIORef')
import           Data.Vector.Storable              (fromList)
import           Data.Word                         (Word16)
import           Foreign.ForeignPtr                (ForeignPtr)
import           GHC.Base                          (returnIO)
import           GHC.Conc                          (getNumCapabilities)
import           Numeric.Units.Dimensional.NonSI   (angstrom)
import           Numeric.Units.Dimensional.Prelude (Quantity, Unit, degree,
                                                    (*~))

import           Hkl.Binoculars.Common
import           Hkl.Binoculars.Projections
import           Hkl.C.Binoculars
import           Hkl.C.Geometry
import           Hkl.Conduit
import           Hkl.Detector
import           Hkl.H5                            hiding (File)
import           Hkl.Types

class LenC a where
  lenC :: MonadResource m => a -> ConduitT FilePath Int m ()

mkJobs :: LenC a => InputFn -> a -> IO [[Chunk Int FilePath]]
mkJobs fn h5d = do
  let fns = concatMap (replicate 1) (toList fn)
  ns <- runConduitRes $ sourceList fns .| lenC h5d .| sinkList
  -- Prelude.print ns
  c' <- getNumCapabilities
  let ntot = sum ns
      c = if c' >= 2 then c' - 1 else c'
  return $ mkJobs' (quot ntot c) fns ns

mkJobsHklC :: LenC a => InputHkl a -> IO [[Chunk Int FilePath]]
mkJobsHklC (InputHkl _ fn h5d _ _ _ _ _ _ _) = mkJobs fn h5d

--  Create the Cube

mkCube'C :: (MonadIO m, Shape sh) => Detector a DIM2 -> IORef (Cube' sh) -> ConduitT (DataFrameSpace sh) Void m ()
mkCube'C det ref = loop
  where
    loop = do
      ms <- await
      case ms of
        Nothing -> return ()
        (Just s) -> do
          c2 <- liftIO $ mkCube' det [s]
          liftIO $ modifyIORef' ref (c2 <>)
          loop

-- Instances
withSpaceC :: (MonadResource m, Shape sh)
             => Detector a DIM2
           -> Int
           -> (Space sh -> ConduitT i o m r)
           -> ConduitT i o m r
withSpaceC d n = bracketP (liftIO $ newSpace d n) (const $ return ())

projectC :: (MonadResource m, Shape sh)
       => Detector a DIM2
     -> Int
     -> (Space sh -> DataFrameHkl a -> IO (DataFrameSpace sh))
     -> ConduitT (DataFrameHkl a) (DataFrameSpace sh) m ()
projectC d n f = withSpaceC d n $ \s -> loop s
    where
      loop s = do
        mdf <- await
        case mdf of
          Nothing   -> return ()
          (Just df) -> do
                   yield =<< liftIO (f s df)
                   loop s

withDetectorPathC :: (MonadResource m, Location l)
                  => l
                  -> Detector a DIM2
                  -> DetectorPath
                  -> ((Int -> IO (ForeignPtr Word16)) -> ConduitT i o m r)
                  -> ConduitT i o m r
withDetectorPathC f det (DetectorPath p) g = do
  let n = (size . shape $ det) * 2
  withBytesC n $ \buf ->
      withHdf5PathC f p $ \p' -> g (getArrayInBuffer buf det p')

nest :: [(r -> a) -> a] -> ([r] -> a) -> a
nest xs = runCont (Prelude.mapM cont xs)

withAxesPathC :: (MonadResource m, Location l)
              => l
              -> [Hdf5Path DIM1 Double]
              -> ([Dataset] -> ConduitT i o m a)
              -> ConduitT i o m a
withAxesPathC f dpaths = nest (map (withHdf5PathC f) dpaths)

withGeometryPathC :: (MonadResource m, Location l)
                  => l
                  -> GeometryPath
                  -> ((Int -> IO Geometry) -> ConduitT i o m r)
                  -> ConduitT i o m r
withGeometryPathC f (GeometryPathMedV w as) gg =
    withHdf5PathC f w $ \w' ->
    withAxesPathC f as $ \as' ->
        gg (\j -> Geometry MedV
                 <$> (Source <$> getValueWithUnit w' 0 angstrom)
                 <*> (fromList <$> do
                        vs <- Prelude.mapM (`get_position` j) as'
                        return (0.0 : vs))
                 <*> pure Nothing)
withGeometryPathC f (GeometryPathUhv w as) gg =
    withHdf5PathC f w $ \w' ->
    withAxesPathC f as $ \as' ->
        gg (\j -> Geometry Uhv
                 <$> (Source <$> getValueWithUnit w' 0 angstrom)
                 <*> (fromList <$> Prelude.mapM (`get_position` j) as')
                 <*> pure Nothing)
withGeometryPathC f (GeometryPathCristalK6C w m ko ka kp g d) gg =
    withHdf5PathC f w $ \w' ->
    withHdf5PathC f m $ \mu' ->
    withHdf5PathC f ko $ \komega' ->
    withHdf5PathC f ka $ \kappa' ->
    withHdf5PathC f kp $ \kphi' ->
    withHdf5PathC f g $ \gamma' ->
    withHdf5PathC f d $ \delta' -> do
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

withAttenuationPathC :: (MonadResource m, Location l) =>
                       l
                     -> AttenuationPath
                     -> ((Int -> IO (Maybe Double)) -> ConduitT i o m r)
                     -> ConduitT i o m r
withAttenuationPathC f matt g =
    case matt of
      NoAttenuation -> g (const $ returnIO Nothing)
      (AttenuationPath p offset) ->
          withHdf5PathC f p $ \p' -> g (\j -> Just
                                            <$> get_position p' (j + offset))

withQxQyQzPathC :: (MonadResource m, Location l) =>
                 l
               -> Detector a DIM2
               -> QxQyQzPath
               -> ((Int -> IO DataFrameQxQyQz) -> ConduitT i o m r)
               -> ConduitT i o m r
withQxQyQzPathC f det (QxQyQzPath d dif att) g =
    withDetectorPathC f det d $ \getImage ->
    withGeometryPathC f dif $ \getDiffractometer ->
    withAttenuationPathC f att $ \getAttenuation -> g (\j ->
                                                         DataFrameQxQyQz j
                                                         <$> getDiffractometer j
                                                         <*> getImage j
                                                         <*> getAttenuation j)
--  FramesQxQyQzP

instance LenC QxQyQzPath where
  lenC (QxQyQzPath (DetectorPath i) _ ma) = loop
    where
      loop = do
        mfp <- await
        case mfp of
          Nothing -> return ()
          (Just fp) -> withFileC (openH5 fp) $ \f -> withHdf5PathC f i $ \i' -> do
            (_, ss) <- liftIO $ datasetShape i'
            case head ss of
              (Just n) -> do
                   yield $ fromIntegral n + case ma of
                                              NoAttenuation           -> 0
                                              (AttenuationPath _ off) -> off
                   loop
              Nothing  -> error "can not extract length"


class LenC a => FramesQxQyQzC a where
  framesQxQyQzC :: MonadResource m
                => a
                -> Detector b DIM2
                -> ConduitT (Chunk Int FilePath) DataFrameQxQyQz m ()

instance FramesQxQyQzC QxQyQzPath where
  framesQxQyQzC p det = loop
    where
      loop = do
        mc <- await
        case mc of
          Nothing -> return ()
          (Just (Chunk fp from to)) -> withFileC (openH5 fp) $ \f ->
            withQxQyQzPathC f det p $ \getDataFrameQxQyQz -> do
            forM_ [from..to-1] (\j -> yield =<< liftIO (getDataFrameQxQyQz j))
            loop

-- FramesHklP

class LenC a => FramesHklC a where
  framesHklC :: MonadResource m => a -> Detector b DIM2 -> ConduitT (Chunk Int FilePath) (DataFrameHkl b) m ()

getValueWithUnit :: Dataset -> Int -> Unit m d Double -> IO (Quantity d Double)
getValueWithUnit d j u = do
  v <- get_position d j
  return $ v *~ u

withSamplePathC :: (MonadResource m, Location l)
                => l
                -> SamplePath
                -> (IO (Sample Triclinic) -> ConduitT i o m r)
                -> ConduitT i o m r
withSamplePathC f (SamplePath a b c alpha beta gamma ux uy uz) g =
    withHdf5PathC f a $ \a' ->
    withHdf5PathC f b $ \b' ->
    withHdf5PathC f c $ \c' ->
    withHdf5PathC f alpha $ \alpha' ->
    withHdf5PathC f beta $ \beta' ->
    withHdf5PathC f gamma $ \gamma' ->
    withHdf5PathC f ux $ \ux' ->
    withHdf5PathC f uy $ \uy' ->
    withHdf5PathC f uz $ \uz' ->
        g (Sample "test"
           <$> (Triclinic
                <$> getValueWithUnit a' 0 angstrom
                <*> getValueWithUnit b' 0 angstrom
                <*> getValueWithUnit c' 0 angstrom
                <*> getValueWithUnit alpha' 0 degree
                <*> getValueWithUnit beta' 0 degree
                <*> getValueWithUnit gamma' 0 degree)
           <*> (Parameter "ux"
                <$> get_position ux' 0
                <*> pure (Range 0 0))
           <*> (Parameter "uy"
                <$> get_position uy' 0
                <*> pure (Range 0 0))
           <*> (Parameter "uz"
                <$> get_position uz' 0
                <*> pure (Range 0 0)))
withSamplePathC _ (SamplePath2 s) g = g (return s)

instance LenC HklPath where
  lenC (HklPath p _)           = lenC p

instance FramesHklC HklPath where
  framesHklC (HklPath qp samp) det = loop
    where
      loop = do
        mc <- await
        case mc of
          Nothing -> return ()
          (Just (Chunk fp from to)) -> withFileC (openH5 fp) $ \f ->
            withQxQyQzPathC f det qp $ \getDataFrameQxQyQz ->
            withSamplePathC f samp $ \getSample -> do
            forM_ [from..to-1] (\j -> yield =<< liftIO
                                 (DataFrameHkl
                                  <$> getDataFrameQxQyQz j
                                  <*> getSample
                                 ))
            loop

processHklC :: FramesHklC a => InputHkl a -> IO ()
processHklC input@(InputHkl det _ h5d o res cen d r config' mask') = do
  pixels <- getPixelsCoordinates det cen d r

  jobs <- mkJobsHklC input
  r' <- mapConcurrently (\job -> withCubeAccumulator $ \s ->
                           runConduitRes
                           $ sourceList job
                           -- .| Data.Conduit.Combinators.print
                           .| framesHklC h5d det
                           -- .| Data.Conduit.Combinators.print
                           .| projectC det 3 (spaceHkl config' det pixels res mask')
                           .| mkCube'C det s
                       ) jobs
  Prelude.print r'
  saveCube o r'

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-
    Copyright  : Copyright (C) 2014-2022 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.Binoculars.Pipes
  ( Chunk(..)
  , ChunkP(..)
  , accumulateP
  , condM
  , nest
  , progress
  , project
  , skipMalformed
  , tryYield
  , withAttenuationPathP
  , withAxesPathP
  , withDetectorPathP
  , withGeometryPathP
  , withProgressBar
  , withSpace
  ) where

import           Bindings.HDF5.Core                (Location)
import           Bindings.HDF5.Dataset             (getDatasetType)
import           Bindings.HDF5.Datatype            (getTypeSize, nativeTypeOf,
                                                    typeIDsEqual)
import           Control.Exception                 (throwIO)
import           Control.Monad                     (forever)
import           Control.Monad.Catch               (tryJust)
import           Control.Monad.Extra               (ifM)
import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Control.Monad.Trans.Cont          (cont, runCont)
import           Data.Array.Repa                   (Shape, size)
import           Data.Array.Repa.Index             (DIM1, DIM2)
import           Data.IORef                        (IORef, readIORef)
import           Data.Int                          (Int32)
import           Data.Vector.Storable              (fromList)
import           Data.Word                         (Word16, Word32)
import           GHC.Base                          (returnIO)
import           GHC.Float                         (float2Double)
import           Numeric.Units.Dimensional.NonSI   (angstrom)
import           Numeric.Units.Dimensional.Prelude (degree, (*~))
import           Pipes                             (Consumer, Pipe, Proxy,
                                                    await, yield)
import           Pipes.Prelude                     (mapM)
import           Pipes.Safe                        (MonadSafe, SomeException,
                                                    bracket, catchP,
                                                    displayException)
import           System.ProgressBar                (Progress (..), ProgressBar,
                                                    Style (..), defStyle,
                                                    elapsedTime, incProgress,
                                                    newProgressBar,
                                                    renderDuration,
                                                    updateProgress)

import           Prelude                           hiding (filter)

import           Hkl.Binoculars.Common
import           Hkl.Binoculars.Config
import           Hkl.Binoculars.Projections
import           Hkl.C.Binoculars
import           Hkl.C.Geometry
import           Hkl.Detector
import           Hkl.H5                            hiding (File)
import           Hkl.Image
import           Hkl.Pipes
import           Hkl.Types


-- ChunkP

class ChunkP a where
  chunkP :: MonadSafe m => a -> Pipe FilePath (Chunk Int FilePath) m ()

-- Project

withSpace :: (MonadSafe m, Shape sh)
            => Detector a DIM2 -> Int -> (Space sh -> m r) -> m r
withSpace d n = bracket (liftIO $ newSpace d n) pure

project :: (MonadSafe m, Shape sh)
          => Detector a DIM2
        -> Int
        -> (Space sh -> b -> IO (DataFrameSpace sh))
        -> Pipe b (DataFrameSpace sh) m ()
project d n f = withSpace d n $ \s -> Pipes.Prelude.mapM (liftIO . f s)


skipMalformed :: MonadSafe m
              => Proxy a' a b' b m r
              -> Proxy a' a b' b m r
skipMalformed p = loop
  where
    loop = catchP p $ \e -> do
        liftIO $ Prelude.print $ displayException (e :: SomeException)
        loop

-- ProgressBar

withProgressBar :: Int -> (ProgressBar () -> IO ()) -> IO ()
withProgressBar ntot f = do
  pb <- newProgressBar defStyle{ stylePostfix=elapsedTime renderDuration } 10 (Progress 0 ntot ())
  f pb
  updateProgress pb $ \p@(Progress _ t _) -> p{progressDone=t}

--  Create the Cube

accumulateP :: (MonadIO m, Shape sh)
            => IORef (Cube sh) -> Consumer (DataFrameSpace sh) m ()
accumulateP ref =
    forever $ do s <- await
                 liftIO $ addSpace s =<< readIORef ref

progress :: MonadIO m => ProgressBar s -> Consumer a m ()
progress p = forever $ do
  _ <- await
  liftIO $ p `incProgress` 1

-- Instances

condM :: (Monad m) => [(m Bool, m a)] -> m a
condM []          = undefined
condM ((p, v):ls) = ifM p v (condM ls)

withDetectorPathP :: (MonadSafe m, Location l) => l -> Detector a DIM2 -> DetectorPath -> ((Int -> IO Image) -> m r) -> m r
withDetectorPathP f det (DetectorPath p) g = do
  withHdf5PathP f p $ \p' -> do
    t <- liftIO $ getDatasetType p'
    s <- liftIO $ getTypeSize t
    let n = (size . shape $ det) * fromEnum s
    condM [ ((liftIO $ typeIDsEqual t (nativeTypeOf (undefined ::  Int32))), (withBytes n $ \buf -> g (\i -> ImageInt32 <$> getArrayInBuffer buf det p' i)))
          , ((liftIO $ typeIDsEqual t (nativeTypeOf (undefined :: Word16))), (withBytes n $ \buf -> g (\i -> ImageWord16 <$> getArrayInBuffer buf det p' i)))
          , ((liftIO $ typeIDsEqual t (nativeTypeOf (undefined :: Word32))), (withBytes n $ \buf -> g (\i -> ImageWord32 <$> getArrayInBuffer buf det p' i)))
          ]

nest :: [(r -> a) -> a] -> ([r] -> a) -> a
nest xs = runCont (Prelude.mapM cont xs)

withAxesPathP :: (MonadSafe m, Location l) => l -> [Hdf5Path DIM1 Double] -> ([Dataset] -> m a) -> m a
withAxesPathP f dpaths = nest (Prelude.map (withHdf5PathP f) dpaths)

instance Is1DStreamable Dataset Degree where
  extract1DStreamValue d i = Degree <$> do
    v <- extract1DStreamValue d i
    return $ v *~ degree

instance Is1DStreamable Dataset NanoMeter where
  extract1DStreamValue d i = NanoMeter <$> do
    v <- extract1DStreamValue d i
    return $ v *~ angstrom

instance Is1DStreamable Dataset WaveLength where
  extract1DStreamValue d i = do
    v <- extract1DStreamValue d i
    return $ v *~ angstrom

instance Is1DStreamable Dataset Source where
  extract1DStreamValue d i = Source <$> extract1DStreamValue d i

withGeometryPathP :: (MonadSafe m, Location l) => l -> GeometryPath -> ((Int -> IO Geometry) -> m r) -> m r
withGeometryPathP f (GeometryPathCristalK6C w m ko ka kp g d) gg =
    withHdf5PathP f w $ \w' ->
    withHdf5PathP f m $ \mu' ->
    withHdf5PathP f ko $ \komega' ->
    withHdf5PathP f ka $ \kappa' ->
    withHdf5PathP f kp $ \kphi' ->
    withHdf5PathP f g $ \gamma' ->
    withHdf5PathP f d $ \delta' -> do
      wavelength <- liftIO $ extract1DStreamValue w' 0
      mu <- liftIO $ extract1DStreamValue mu' 0
      komega <- liftIO $ extract1DStreamValue komega' 0
      kappa <- liftIO $ extract1DStreamValue kappa' 0
      gamma <- liftIO $ extract1DStreamValue gamma' 0
      delta <- liftIO $ extract1DStreamValue delta' 0
      gg (\j -> do
            kphi <- extract1DStreamValue kphi' j
            return (Geometry
                    K6c
                    (Source wavelength)
                    (fromList [mu, komega, kappa, kphi, gamma, delta])
                    Nothing))
withGeometryPathP f (GeometryPathFix w) gg =
  withHdf5PathP f w $ \w' ->
                        gg (const $
                             Geometry Fixe
                             <$> extract1DStreamValue w' 0
                             <*> pure (fromList [])
                             <*> pure Nothing)
withGeometryPathP f (GeometryPathMars as) gg =
    withAxesPathP f as $ \as' ->
        gg (\j -> Geometry Mars
                 <$> (Source <$> (return $ 1.537591 *~ angstrom))
                 <*> (fromList <$> do
                         vs <- Prelude.mapM (`extract1DStreamValue` j) as'
                         return (0.0 : vs))
                 <*> pure Nothing)
withGeometryPathP f (GeometryPathMedH w as) gg =
    withHdf5PathP f w $ \w' ->
    withAxesPathP f as $ \as' ->
        gg (\j -> Geometry MedH
                 <$> extract1DStreamValue w' 0
                 <*> (fromList <$> Prelude.mapM (`extract1DStreamValue` j) as')
                 <*> pure Nothing)
withGeometryPathP f (GeometryPathMedV w as) gg =
    withHdf5PathP f w $ \w' ->
    withAxesPathP f as $ \as' ->
        gg (\j -> Geometry MedV
                 <$> extract1DStreamValue w' 0
                 <*> (fromList <$> Prelude.mapM (`extract1DStreamValue` j) as')
                 <*> pure Nothing)
withGeometryPathP _f (GeometryPathMedVEiger _w _as _eix _eiz) _gg = undefined
withGeometryPathP f (GeometryPathUhv w as) gg =
    withHdf5PathP f w $ \w' ->
    withAxesPathP f as $ \as' ->
        gg (\j -> Geometry Uhv
                 <$> extract1DStreamValue w' 0
                 <*> (fromList <$> Prelude.mapM (`extract1DStreamValue` j) as')
                 <*> pure Nothing)
withGeometryPathP f (GeometryPathUhvTest w as) gg =
    withAxesPathP f as $ \as' ->
        gg (\j -> Geometry Uhv (Source (unAngstrom w))
                 <$> (fromList <$> Prelude.mapM (`extract1DStreamValue` j) as')
                 <*> pure Nothing)

withAttenuationPathP :: (MonadSafe m, Location l) =>
                       l
                     -> AttenuationPath
                     -> ((Int -> IO Double) -> m r)
                     -> m r
withAttenuationPathP f matt g =
  case matt of
    NoAttenuation -> g (const $ returnIO 1)
    (AttenuationPath p offset coef) ->
      withHdf5PathP f p $ \p' -> g (\j -> do
                                       v <-  extract1DStreamValue p' (j + offset)
                                       if v == badAttenuation
                                         then throwIO (WrongAttenuation "file" (j + offset) (float2Double v))
                                         else return  (coef ** float2Double v))
    (ApplyedAttenuationFactorPath p) ->
      withHdf5PathP f p $ \p' -> g (\j -> extract1DStreamValue p' j)


tryYield :: MonadSafe m
         => IO r -> Proxy x' x () r m ()
tryYield io = do
  edf <- liftIO $ tryJust selectHklBinocularsException io
  case edf of
    Left _   -> return ()
    Right df -> yield df
  where
    selectHklBinocularsException :: HklBinocularsException -> Maybe HklBinocularsException
    selectHklBinocularsException e = Just e

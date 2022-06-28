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
  , progress
  , project
  , skipMalformed
  , tryYield
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
import           Data.Array.Repa.Index             (DIM2)
import           Data.IORef                        (IORef, readIORef)
import           Data.Int                          (Int32)
import           Data.Vector.Storable              (fromList)
import           Data.Word                         (Word16, Word32)
import           GHC.Base                          (returnIO)
import           GHC.Float                         (float2Double)
import           Numeric.Units.Dimensional.NonSI   (angstrom)
import           Numeric.Units.Dimensional.Prelude ((*~))
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
import           Hkl.DataSource
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

withGeometryPathP :: (MonadSafe m, Location l) => l -> DataSourcePath Geometry -> ((Int -> IO Geometry) -> m r) -> m r
withGeometryPathP f p gg = withDataSourceP f p $ \r ->
  case r of
    (DataSourceAcq'Geometry'CristalK6C w' mu' komega' kappa' kphi' gamma' delta') -> do
      wavelength <- liftIO $ extract0DStreamValue w'
      mu <- liftIO $ extract0DStreamValue mu'
      komega <- liftIO $ extract0DStreamValue komega'
      kappa <- liftIO $ extract0DStreamValue kappa'
      gamma <- liftIO $ extract0DStreamValue gamma'
      delta <- liftIO $ extract0DStreamValue delta'
      gg (\j -> do
             kphi <- extract1DStreamValue kphi' j
             return (Geometry
                      K6c
                      (Source wavelength)
                      (fromList [mu, komega, kappa, kphi, gamma, delta])
                      Nothing))

    (DataSourceAcq'Geometry'Fix w') -> do
      gg (const $
          Geometry Fixe
          <$> extract0DStreamValue w'
          <*> pure (fromList [])
          <*> pure Nothing)


    (DataSourceAcq'Geometry'Mars w' as') -> do
      gg (\j -> Geometry Mars
               <$> extract0DStreamValue w'
               <*> (fromList <$> do
                       vs <- Prelude.mapM (`extract1DStreamValue` j) as'
                       return (0.0 : vs)) -- TODO check
               <*> pure Nothing)

    (DataSourceAcq'Geometry'MedH w' as') -> do
      gg (\j -> Geometry MedH
               <$> extract0DStreamValue w'
               <*> extract1DStreamValue as' j
               <*> pure Nothing)

    (DataSourceAcq'Geometry'MedV w' as') -> do
      gg (\j -> Geometry MedV
               <$> extract0DStreamValue w'
               <*> extract1DStreamValue as' j
               <*> pure Nothing)

    (DataSourceAcq'Geometry'MedVEiger _w' _as' _eix' _eyz') -> undefined

    (DataSourceAcq'Geometry'Uhv w' as') -> do
      gg (\j -> Geometry Uhv
               <$> extract0DStreamValue w'
               <*> extract1DStreamValue as' j
               <*> pure Nothing)

    (DataSourceAcq'Geometry'UhvTest w' as') -> do
      gg (\j -> Geometry Uhv
               <$> extract0DStreamValue w' -- (Source (unAngstrom w))
               <*> extract1DStreamValue as' j
               <*> pure Nothing)

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

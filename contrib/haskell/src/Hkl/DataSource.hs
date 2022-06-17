{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
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

module Hkl.DataSource
  ( DataSource(..)
  , Is1DStreamable(..)
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
import           Data.Array.Repa.Index             (DIM1, DIM2, Z)
import           Data.IORef                        (IORef, readIORef)
import           Data.Int                          (Int32)
import           Data.Vector.Storable              (Vector, fromList)
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
import           Hkl.Types


-- IsStreamable

class Is1DStreamable a e where
  extract1DStreamValue :: a -> Int -> IO e

-- IsStreamable (instances)

instance Is1DStreamable Dataset Attenuation where
  extract1DStreamValue d i = Attenuation <$> extract1DStreamValue d i

instance Is1DStreamable Dataset Degree where
  extract1DStreamValue d i = Degree <$> do
    v <- extract1DStreamValue d i
    return $ v *~ degree

instance Is1DStreamable Dataset Double where
  extract1DStreamValue = get_position


instance Is1DStreamable Dataset Float where
  extract1DStreamValue = get_position

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

instance Is1DStreamable  [Dataset] (Data.Vector.Storable.Vector Double) where
  extract1DStreamValue ds i = fromList <$> Prelude.mapM (`extract1DStreamValue` i) ds


class DataSource a where
  data DataSourcePath a :: *
  data DataSourceAcq a :: *

  dataSourceAcquire :: DataSourcePath a -> IO (DataSourceAcq a)

  dataSourceRelease :: DataSourceAcq a -> IO ()

{-# LANGUAGE GADTs              #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

{-
    Copyright  : Copyright (C) 2014-2020 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}
module Hkl.Binoculars.Sixs
  (process) where

import           Control.Monad                     (forM_, forever)
import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Data.Array.Repa.Index             (DIM1, DIM2, DIM3, Z)
import           Data.Vector.Storable              (concat, head)
import           Data.Word                         (Word16)
import           Numeric.Units.Dimensional.NonSI   (angstrom)
import           Numeric.Units.Dimensional.Prelude (degree, meter, (*~))
import           Pipes                             (await, yield)

import           Hkl.Binoculars.Common
import           Hkl.C.Geometry
import           Hkl.H5
import           Hkl.Pipes
import           Hkl.Types

data DataFrameHklH5Path
  = DataFrameHklH5Path
    (Hdf5Path DIM3 Word16) -- Image
    (Hdf5Path DIM1 Double) -- Mu
    (Hdf5Path DIM1 Double) -- Omega
    (Hdf5Path DIM1 Double) -- Delta
    (Hdf5Path DIM1 Double) -- Gamma
    (Hdf5Path DIM2 Double) -- UB
    (Hdf5Path Z Double) -- Wavelength
    (Hdf5Path DIM1 Char) -- DiffractometerType

instance FramesP DataFrameHklH5Path where
  lenP (DataFrameHklH5Path _ m _ _ _ _ _ _) = forever $ do
    fp <- await
    withFileP (openH5 fp) $ \f ->
      withHdf5PathP f m $ \m' -> do
      (Just n) <- liftIO $ lenH5Dataspace m'
      yield n

  framesP (DataFrameHklH5Path i m o d g u w t) det = forever $ do
    (Chunk fp from to) <- await
    withFileP (openH5 fp) $ \f ->
      withHdf5PathP f i $ \i' ->
      withHdf5PathP f m $ \m' ->
      withHdf5PathP f o $ \o' ->
      withHdf5PathP f d $ \d' ->
      withHdf5PathP f g $ \g' ->
      withHdf5PathP f u $ \u' ->
      withHdf5PathP f w $ \w' ->
      withHdf5PathP f t $ \_t' ->
      forM_ [from..to-1] (\j -> yield =<< liftIO
                       (do
                           mu <- get_position m' j
                           omega <- get_position o' j
                           delta <- get_position d' j
                           gamma <- get_position g' j
                           wavelength <- get_position w' 0
                           image <- get_image' det i' j
                           ub <- get_ub u'
                           let positions = Data.Vector.Storable.concat [mu, omega, delta, gamma]
                               source = Source (Data.Vector.Storable.head wavelength *~ angstrom)
                           pure $ DataFrame j (Geometry Uhv source positions Nothing) ub image))


_manip1 :: Input DataFrameHklH5Path
_manip1 = Input { filename = InputFn "/home/picca/align_FLY2_omega_00045.nxs"
                , h5dpath = DataFrameHklH5Path
                            (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image")
                            (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_MU")
                            (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_OMEGA")
                            (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_DELTA")
                            (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_GAMMA")
                            (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-UHV__#1/" $ datasetp "UB")
                            (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "Monochromator" $ datasetp "wavelength")
                            (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-UHV__#1" $ datasetp "type")
                , output = "test1.hdf5"
                , resolutions = [0.002, 0.002, 0.002]
                , centralPixel = (0, 0)
                , sdd = 1 *~ meter
                , detrot = 90 *~ degree
                }

_manip2 :: Input DataFrameHklH5Path
_manip2 = Input { filename = InputRange "/nfs/ruche-sixs/sixs-soleil/com-sixs/2019/Run3/FeSCO_Cu111/sample2_ascan_omega_%05d.nxs" 77 93
                , h5dpath = DataFrameHklH5Path
                            (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image")
                            (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu")
                            (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega")
                            (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta")
                            (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma")
                            (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx2-ex-diff-uhv" $ datasetp "UB")
                            (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda")
                            (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx2-ex-diff-uhv" $ datasetp "type")
                , output = "test2.hdf5"
                , resolutions = [0.003, 0.01, 0.003]
                , centralPixel = (352, 112)
                , sdd = 1.162 *~ meter
                , detrot = 90 *~ degree
                }

-- manip3 :: Input
-- manip3 = Input { filename = InputRange "/nfs/ruche-sixs/sixs-soleil/com-sixs/2018/Run3/Corentin/Al13Co4/Al13Co4_ascan_omega_%05d.nxs" 4 137

--                }

process :: IO ()
process = process' _manip1

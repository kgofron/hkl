{-# LANGUAGE GADTs             #-}
{-# LANGUAGE MultiWayIf        #-}
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
module Hkl.Binoculars.Sixs
  (process) where

import           Control.Monad                     (forM_, forever)
import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Data.Array.Repa.Index             (DIM1, DIM3, Z)
import           Data.Ini.Config                   (parseIniFile)
import           Data.Text.IO                      (readFile)
import           Data.Typeable                     (typeOf)
import           Data.Vector.Storable              (concat, head)
import           Data.Word                         (Word16)
import           Numeric.Units.Dimensional.NonSI   (angstrom)
import           Numeric.Units.Dimensional.Prelude ((*~))
import           Pipes                             (await, yield)

import           Prelude                           hiding (readFile)

import           Hkl.Binoculars.Common
import           Hkl.Binoculars.Config
import           Hkl.Binoculars.Projections
import           Hkl.C.Geometry
import           Hkl.H5                            hiding (File)
import           Hkl.Pipes
import           Hkl.Types
import           Paths_hkl

data SixsQxQyQzUhv
  = SixsQxQyQzUhv
    (Hdf5Path DIM3 Word16) -- Image
    (Hdf5Path DIM1 Double) -- Mu
    (Hdf5Path DIM1 Double) -- Omega
    (Hdf5Path DIM1 Double) -- Delta
    (Hdf5Path DIM1 Double) -- Gamma
    (Hdf5Path Z Double) -- Wavelength

instance Show SixsQxQyQzUhv where
  show = show . typeOf

instance LenP SixsQxQyQzUhv where
  lenP (SixsQxQyQzUhv _ m _ _ _ _) = forever $ do
    fp <- await
    withFileP (openH5 fp) $ \f ->
      withHdf5PathP f m $ \m' -> do
      (Just n) <- liftIO $ lenH5Dataspace m'
      yield n

instance FramesQxQyQzP SixsQxQyQzUhv where
  framesQxQyQzP (SixsQxQyQzUhv i m o d g w) det = forever $ do
    (Chunk fp from to) <- await
    withFileP (openH5 fp) $ \f ->
      withHdf5PathP f i $ \i' ->
      withHdf5PathP f m $ \m' ->
      withHdf5PathP f o $ \o' ->
      withHdf5PathP f d $ \d' ->
      withHdf5PathP f g $ \g' ->
      withHdf5PathP f w $ \w' ->
      forM_ [from..to-1] (\j -> yield =<< liftIO
                       (do
                           mu <- get_position m' j
                           omega <- get_position o' j
                           delta <- get_position d' j
                           gamma' <- get_position g' j
                           wavelength <- get_position w' 0
                           image <- get_image' det i' j
                           let positions = Data.Vector.Storable.concat [mu, omega, delta, gamma']
                               source = Source (Data.Vector.Storable.head wavelength *~ angstrom)
                           pure $ DataFrameQxQyQz j (Geometry Uhv source positions Nothing) image))

h5dpathQxQyQz :: InputType -> SixsQxQyQzUhv
h5dpathQxQyQz t = case t of
  SixsFlyScanUhv -> SixsQxQyQzUhv
                   (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image")
                   (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_MU")
                   (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_OMEGA")
                   (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_DELTA")
                   (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_GAMMA")
                   (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "Monochromator" $ datasetp "wavelength")
  SixsFlyScanUhv2 -> SixsQxQyQzUhv
                    (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image")
                    (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu")
                    (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega")
                    (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta")
                    (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma")
                    (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda")

data SixsHklUhv = SixsHklUhv
  deriving Show

instance LenP SixsHklUhv where
  lenP SixsHklUhv = undefined

instance FramesHklP SixsHklUhv where
  framesHklP SixsHklUhv _det = undefined

h5dpathHkl :: InputType -> SixsHklUhv
h5dpathHkl _t = undefined

process :: Maybe FilePath -> IO ()
process mf = do
  cfg <- readFile =<< case mf of
                       Nothing -> getDataFileName "data/test/config_manip1.cfg"
                       (Just f) -> pure f
  let r = parseIniFile cfg parseBinocularsConfig
  case r of
    Right c' -> case (ptype . bProjection $ c') of
      QxQyQzProjection -> do
        i <- mkInputQxQyQz c' h5dpathQxQyQz
        print i
        processQxQyQz i
      HklProjection -> do
        i <- mkInputHkl c' h5dpathHkl
        print i
        processHkl i
    Left e   -> print e

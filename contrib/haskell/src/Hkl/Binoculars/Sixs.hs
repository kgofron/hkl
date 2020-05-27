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

import           Bindings.HDF5.Core                (Location)
import           Control.Monad                     (forM_, forever)
import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Data.Array.Repa.Index             (DIM1, DIM3, Z)
import           Data.Typeable                     (typeOf)
import           Data.Vector.Storable              (fromList)
import           Data.Word                         (Word16)
import           Numeric.Units.Dimensional.NonSI   (angstrom)
import           Numeric.Units.Dimensional.Prelude (Quantity, Unit, degree,
                                                    (*~))
import           Pipes                             (await, yield)
import           Pipes.Safe                        (MonadSafe)

import           Hkl.Binoculars.Common
import           Hkl.Binoculars.Config
import           Hkl.Binoculars.Projections
import           Hkl.C.Geometry
import           Hkl.Detector
import           Hkl.H5                            hiding (File)
import           Hkl.Pipes
import           Hkl.Types


-- | Uhv Diffractometer

data UhvPath = UhvPath
    (Hdf5Path DIM1 Double) -- Mu
    (Hdf5Path DIM1 Double) -- Omega
    (Hdf5Path DIM1 Double) -- Delta
    (Hdf5Path DIM1 Double) -- Gamma
    (Hdf5Path Z Double) -- Wavelength
    deriving Show

withUhvPathP :: (MonadSafe m, Location l) => l -> UhvPath -> ((Int -> IO Geometry) -> m r) -> m r
withUhvPathP f (UhvPath m o d g w) gg =
    withHdf5PathP f m $ \m' ->
    withHdf5PathP f o $ \o' ->
    withHdf5PathP f d $ \d'->
    withHdf5PathP f g $ \g' ->
    withHdf5PathP f w $ \w' ->
        gg (\j -> Geometry
                 <$> pure Uhv
                 <*> (Source <$> getValueWithUnit w' 0 angstrom)
                 <*> (fromList <$> mapM (`get_position` j) [m', o', d', g'])
                 <*> pure Nothing)

-- | FramesQxQyQzP

data SixsQxQyQzUhvPath
  = SixsQxQyQzUhvPath
    (Hdf5Path DIM3 Word16) -- Image
    UhvPath -- diffractometer

instance Show SixsQxQyQzUhvPath where
  show = show . typeOf

instance LenP SixsQxQyQzUhvPath where
  lenP (SixsQxQyQzUhvPath i _) = forever $ do
    fp <- await
    withFileP (openH5 fp) $ \f ->
      withHdf5PathP f i $ \i' -> do
      mn <- liftIO $ lenH5Dataspace i'
      case mn of
        (Just n) -> yield n
        Nothing  -> error "can not extract length"

instance FramesQxQyQzP SixsQxQyQzUhvPath where
  framesQxQyQzP (SixsQxQyQzUhvPath i dif) det = forever $ do
    (Chunk fp from to) <- await
    withFileP (openH5 fp) $ \f ->
      withHdf5PathP f i $ \i' ->
      withUhvPathP f dif $ \getDiffractometer ->
      forM_ [from..to-1] (\j -> yield =<< liftIO
                          (DataFrameQxQyQz
                           <$> pure j
                           <*> getDiffractometer j
                           <*> get_image' det i' j))

h5dpathQxQyQz :: InputType -> SixsQxQyQzUhvPath
h5dpathQxQyQz t = case t of
  SixsFlyScanUhv -> SixsQxQyQzUhvPath
                   (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image")
                   (UhvPath
                    (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_MU")
                    (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_OMEGA")
                    (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_DELTA")
                    (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_GAMMA")
                    (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "Monochromator" $ datasetp "wavelength"))
  SixsFlyScanUhv2 -> SixsQxQyQzUhvPath
                    (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image")
                    (UhvPath
                     (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu")
                     (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega")
                     (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta")
                     (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda"))


-- | FramesHklP

data SamplePath = SamplePath
    (Hdf5Path Z Double) -- a
    (Hdf5Path Z Double) -- b
    (Hdf5Path Z Double) -- c
    (Hdf5Path Z Double) -- alpha
    (Hdf5Path Z Double) -- beta
    (Hdf5Path Z Double) -- gamma
    (Hdf5Path Z Double) -- ux
    (Hdf5Path Z Double) -- uy
    (Hdf5Path Z Double) -- yz
    deriving Show

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

data SixsHklUhvPath = SixsHklUhvPath
    (Hdf5Path DIM3 Word16) -- Image
    UhvPath -- diffractometer
    SamplePath -- sample
  deriving Show

instance LenP SixsHklUhvPath where
  lenP (SixsHklUhvPath p _ _) = forever $ do
    fp <- await
    withFileP (openH5 fp) $ \f ->
      withHdf5PathP f p $ \d -> do
      mn <- liftIO $ lenH5Dataspace d
      case mn of
        (Just n) -> yield n
        Nothing  -> error "Cannot extract frame lenght from the file"

instance FramesHklP SixsHklUhvPath where
  framesHklP (SixsHklUhvPath imgs dif samp) det = forever $ do
    (Chunk fp from to) <- await
    withFileP (openH5 fp) $ \f ->
      withHdf5PathP f imgs $ \dimgs ->
      withUhvPathP f dif $ \getDiffractometer ->
      withSamplePathP f samp $ \sample ->
      forM_ [from..to-1] (\j -> yield =<< liftIO
                               (DataFrameHkl
                                <$> pure j
                                <*> get_image' det dimgs j
                                <*> getDiffractometer j
                                <*> sample))

h5dpathHkl :: InputType -> SixsHklUhvPath
h5dpathHkl t =
    let uhvSamplePath = SamplePath
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-UHV__#1" $ datasetp "A")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-UHV__#1" $ datasetp "B")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-UHV__#1" $ datasetp "C")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-UHV__#1" $ datasetp "alpha")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-UHV__#1" $ datasetp "beta")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-UHV__#1" $ datasetp "gamma")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-UHV__#1" $ datasetp "Ux")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-UHV__#1" $ datasetp "Uy")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-UHV__#1" $ datasetp "Uz")
    in case t of
         SixsFlyScanUhv -> SixsHklUhvPath
                          (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image")
                          (UhvPath
                           (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_MU")
                           (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_OMEGA")
                           (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_DELTA")
                           (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_GAMMA")
                           (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "Monochromator" $ datasetp "wavelength"))
                          uhvSamplePath
         SixsFlyScanUhv2 -> SixsHklUhvPath
                           (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image")
                           (UhvPath
                            (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu")
                            (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega")
                            (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta")
                            (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma")
                            (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda"))
                           uhvSamplePath

process :: Maybe FilePath -> IO ()
process mf = do
  conf <- getConfig mf
  case conf of
    Right c -> case _binocularsProjectionPtype c of
                QxQyQzProjection -> do
                  i <- mkInputQxQyQz c ImXpadS140 h5dpathQxQyQz
                  print i
                  processQxQyQz i
                HklProjection -> do
                  i <- mkInputHkl c ImXpadS140 h5dpathHkl
                  print i
                  processHkl i
    Left e   -> print e

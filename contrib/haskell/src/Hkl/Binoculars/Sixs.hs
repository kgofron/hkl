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
import           Control.Monad.Trans.Cont          (cont, runCont)
import           Data.Array.Repa.Index             (DIM1, DIM2, DIM3, Z)
import           Data.Maybe                        (fromMaybe)
import           Data.Typeable                     (typeOf)
import           Data.Vector.Storable              (fromList)
import           Data.Word                         (Word16)
import           Foreign.ForeignPtr                (ForeignPtr)
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

-- DetectorPath

data DetectorPath = DetectorPath
    { detectorPathImage    :: Hdf5Path DIM3 Word16
    } deriving Show

withDetectorPathP :: (MonadSafe m, Location l) => l -> Detector a DIM2 -> DetectorPath -> ((Int -> IO (ForeignPtr Word16)) -> m r) -> m r
withDetectorPathP f det (DetectorPath p) g =
    withHdf5PathP f p $ \p' -> g (\j -> get_image' det p' j)

-- GeometryParth

data GeometryPath
  = GeometryPathUhv { geometryPathWavelength :: Hdf5Path Z Double
                    , geometryPathAxes       :: [Hdf5Path DIM1 Double]
                    }
  | GeometryPathCristalK6C { geometryPathWavelength :: Hdf5Path Z Double
                           , geometryPathMu         :: Hdf5Path DIM1 Double
                           , geometryPathKomega     :: Hdf5Path DIM1 Double
                           , geometryPathKappa      :: Hdf5Path DIM1 Double
                           , geometryPathKphi       :: Hdf5Path DIM1 Double
                           , geometryPathGamma      :: Hdf5Path DIM1 Double
                           , geometryPathDelta      :: Hdf5Path DIM1 Double
                           }

                  deriving Show

nest :: [(r -> a) -> a] -> ([r] -> a) -> a
nest xs = runCont (mapM cont xs)

withAxesPathP :: (MonadSafe m, Location l) => l -> [Hdf5Path DIM1 Double] -> ([Dataset] -> m a) -> m a
withAxesPathP f dpaths = nest (map (withHdf5PathP f) dpaths)

withGeometryPathP :: (MonadSafe m, Location l) => l -> GeometryPath -> ((Int -> IO Geometry) -> m r) -> m r
withGeometryPathP f (GeometryPathUhv w as) gg =
    withHdf5PathP f w $ \w' ->
    withAxesPathP f as $ \as' ->
        gg (\j -> Geometry
                 <$> pure Uhv
                 <*> (Source <$> getValueWithUnit w' 0 angstrom)
                 <*> (fromList <$> mapM (`get_position` j) as')
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

data QxQyQzPath = QxQyQzPath DetectorPath GeometryPath

instance Show QxQyQzPath where
  show = show . typeOf

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

h5dpathQxQyQz :: BinocularsConfig -> Maybe QxQyQzPath
h5dpathQxQyQz c = Just $ case _binocularsInputItype c of
  SixsFlyScanUhv -> QxQyQzPath
                   (DetectorPath
                    (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image"))
                   (GeometryPathUhv
                    (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "Monochromator" $ datasetp "wavelength")
                    [ hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_MU"
                    , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_OMEGA"
                    , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_DELTA"
                    , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_GAMMA"
                    ])
  SixsFlyScanUhv2 -> QxQyQzPath
                    (DetectorPath
                     (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image"))
                    (GeometryPathUhv
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda")
                     [ hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu"
                     , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega"
                     , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta"
                     , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma"
                     ])
  SixsSbsMedV -> QxQyQzPath
                (DetectorPath
                 (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "xpad_image"))
                (GeometryPathUhv
                 (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-c02-op-mono" $ datasetp "lambda")
                 [ hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu"
                 , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega"
                 , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta"
                 , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma"
                 ])
  CristalK6C -> QxQyQzPath
               (DetectorPath
                (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "data_05")) -- medipix
               (GeometryPathCristalK6C
                (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Monochromator" $ datasetp "lambda")
                (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Diffractometer" $ groupp "i06-c-c07-ex-dif-mu" $ datasetp "position")
                (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Diffractometer" $ groupp "i06-c-c07-ex-dif-komega" $ datasetp "position")
                (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Diffractometer" $ groupp "i06-c-c07-ex-dif-kappa" $ datasetp "position")
                (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "actuator_1_1")
                (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Diffractometer" $ groupp "i06-c-c07-ex-dif-gamma" $ datasetp "position")
                (hdf5p $ grouppat 0 $ groupp "CRISTAL" $ groupp "Diffractometer" $ groupp "i06-c-c07-ex-dif-delta" $ datasetp "position"))


--  FramesHklP

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

data HklPath = HklPath QxQyQzPath SamplePath
             | HklPathFromQxQyQz QxQyQzPath (Sample Triclinic)

  deriving Show

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

h5dpathHkl :: BinocularsConfig -> Maybe HklPath
h5dpathHkl c =
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
        medVSamplePath = SamplePath
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-MEDV__#1" $ datasetp "A")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-MEDV__#1" $ datasetp "B")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-MEDV__#1" $ datasetp "C")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-MEDV__#1" $ datasetp "alpha")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-MEDV__#1" $ datasetp "beta")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-MEDV__#1" $ datasetp "gamma")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-MEDV__#1" $ datasetp "Ux")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-MEDV__#1" $ datasetp "Uy")
                     (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "I14-C-CX2__EX__DIFF-MEDV__#1" $ datasetp "Uz")
    in case h5dpathQxQyQz c of
         (Just qxqyqz) -> case _binocularsInputItype c of
                           SixsFlyScanUhv -> Just (HklPath qxqyqz uhvSamplePath)
                           SixsFlyScanUhv2 -> Just (HklPath qxqyqz uhvSamplePath)
                           SixsSbsMedV -> undefined
                           CristalK6C -> fmap (HklPathFromQxQyQz qxqyqz) (sampleConfig c)
         Nothing -> Nothing

         -- SixsSbsMedV -> HklPath
         --               hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-c00/dt/xpad.1/image")  -- xpad
         --               (GeometryPath
         --                hdf5p -- TODO wavelength
         --                [ hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-ex-diff-med-tpp" $ groupp "TPP" $ groupp "Orientation" $ datasetp "pitch" -- beta
         --                , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/mu")
         --                , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/omega")
         --                , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/gamma")
         --                , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/delta")
         --                , hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/etaa")
         --                ])
         --               medVSamplePath
         --               -- "attenuation": DatasetPathWithAttribute("long_name", b"i14-c-c00/ex/roic/att"),
         --               -- "timestamp": HItem("sensors_timestamps", True),
process :: Maybe FilePath -> IO ()
process mf = do
  conf <- getConfig mf
  case conf of
    Right c -> do
               let d = fromMaybe ImXpadS140 (_binocularsInputDetector c)
               case _binocularsProjectionPtype c of
                 QxQyQzProjection -> do
                   i <- mkInputQxQyQz c d h5dpathQxQyQz
                   print i
                   processQxQyQz i
                 HklProjection -> do
                   i <- mkInputHkl c d h5dpathHkl
                   print i
                   processHkl i
    Left e   -> print e

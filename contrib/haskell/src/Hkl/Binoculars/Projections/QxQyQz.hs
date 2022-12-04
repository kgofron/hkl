{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-
    Copyright  : Copyright (C) 2014-2022 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.Binoculars.Projections.QxQyQz
    ( Config(..)
    , newQxQyQz
    , processQxQyQz
    , updateQxQyQz
    ) where

import           Control.Concurrent.Async           (mapConcurrently)
import           Control.Lens                       (makeLenses)
import           Control.Monad.Catch                (Exception, MonadThrow)
import           Control.Monad.IO.Class             (MonadIO (liftIO))
import           Control.Monad.Logger               (MonadLogger, logDebug,
                                                     logDebugSH, logErrorSH,
                                                     logInfo)
import           Control.Monad.Reader               (MonadReader, ask)
import           Control.Monad.Trans.Reader         (runReaderT)
import           Data.Array.Repa                    (Array)
import           Data.Array.Repa.Index              (DIM2, DIM3)
import           Data.Array.Repa.Repr.ForeignPtr    (F, toForeignPtr)
import           Data.Ini.Config.Bidir              (field, ini, section,
                                                     serializeIni, (.=), (.=?))
import           Data.Maybe                         (fromMaybe)
import           Data.Text                          (pack)
import           Data.Text.IO                       (putStr)
import           Data.Vector.Storable.Mutable       (unsafeWith)
import           Foreign.C.Types                    (CDouble (..))
import           Foreign.ForeignPtr                 (withForeignPtr)
import           Foreign.Marshal.Array              (withArrayLen)
import           GHC.Conc                           (getNumCapabilities)
import           Numeric.Units.Dimensional.Prelude  (degree, meter, (*~))
import           Path                               (Abs, Dir, Path)
import           Pipes                              (each, runEffect, (>->))
import           Pipes.Prelude                      (filter, map, tee, toListM)
import           Pipes.Safe                         (runSafeT)
import           Text.Printf                        (printf)

import           Hkl.Binoculars.Common
import           Hkl.Binoculars.Config
import           Hkl.Binoculars.Pipes
import           Hkl.Binoculars.Projections
import           Hkl.Binoculars.Projections.QCustom
import           Hkl.C.Binoculars
import           Hkl.DataSource
import           Hkl.Detector
import           Hkl.Image
import           Hkl.Orphan                         ()

------------
-- Config --
------------

data instance Config 'QxQyQzProjection = BinocularsConfigQxQyQz
    { _binocularsConfigQxQyQzNcore                  :: Maybe Int
    , _binocularsConfigQxQyQzDestination            :: DestinationTmpl
    , _binocularsConfigQxQyQzOverwrite              :: Bool
    , _binocularsConfigQxQyQzInputType              :: InputType
    , _binocularsConfigQxQyQzNexusdir               :: Maybe (Path Abs Dir)
    , _binocularsConfigQxQyQzTmpl                   :: Maybe InputTmpl
    , _binocularsConfigQxQyQzInputRange             :: Maybe ConfigRange
    , _binocularsConfigQxQyQzDetector               :: Maybe (Detector Hkl DIM2)
    , _binocularsConfigQxQyQzCentralpixel           :: (Int, Int)
    , _binocularsConfigQxQyQzSdd                    :: Meter
    , _binocularsConfigQxQyQzDetrot                 :: Maybe Degree
    , _binocularsConfigQxQyQzAttenuationCoefficient :: Maybe Double
    , _binocularsConfigQxQyQzAttenuationMax         :: Maybe Float
    , _binocularsConfigQxQyQzSurfaceOrientation     :: Maybe SurfaceOrientation
    , _binocularsConfigQxQyQzMaskmatrix             :: Maybe MaskLocation
    , _binocularsConfigQxQyQzWavelength             :: Maybe Angstrom
    , _binocularsConfigQxQyQzProjectionType         :: ProjectionType
    , _binocularsConfigQxQyQzProjectionResolution   :: [Double]
    , _binocularsConfigQxQyQzProjectionLimits       :: Maybe [Limits]
    , _binocularsConfigQxQyQzDataPath               :: Maybe (DataSourcePath DataFrameQCustom)
    , _binocularsConfigQxQyQzImageSumMax            :: Maybe Double
    } deriving (Eq, Show)

makeLenses 'BinocularsConfigQxQyQz

instance HasIniConfig 'QxQyQzProjection where

  defaultConfig = BinocularsConfigQxQyQz
    { _binocularsConfigQxQyQzNcore = Nothing
    , _binocularsConfigQxQyQzDestination = DestinationTmpl "."
    , _binocularsConfigQxQyQzOverwrite = False
    , _binocularsConfigQxQyQzInputType = SixsFlyScanUhv
    , _binocularsConfigQxQyQzNexusdir = Nothing
    , _binocularsConfigQxQyQzTmpl = Nothing
    , _binocularsConfigQxQyQzInputRange  = Nothing
    , _binocularsConfigQxQyQzDetector = Nothing
    , _binocularsConfigQxQyQzCentralpixel = (0, 0)
    , _binocularsConfigQxQyQzSdd = Meter (1 *~ meter)
    , _binocularsConfigQxQyQzDetrot = Nothing
    , _binocularsConfigQxQyQzAttenuationCoefficient = Nothing
    , _binocularsConfigQxQyQzAttenuationMax = Nothing
    , _binocularsConfigQxQyQzSurfaceOrientation = Just SurfaceOrientationVertical
    , _binocularsConfigQxQyQzMaskmatrix = Nothing
    , _binocularsConfigQxQyQzWavelength = Nothing
    , _binocularsConfigQxQyQzProjectionType = QxQyQzProjection
    , _binocularsConfigQxQyQzProjectionResolution = [0.01, 0.01, 0.01]
    , _binocularsConfigQxQyQzProjectionLimits  = Nothing
    , _binocularsConfigQxQyQzDataPath = (Just defaultDataSourcePath'DataFrameQCustom)
    , _binocularsConfigQxQyQzImageSumMax = Nothing
    }

  specConfig = do
    section "dispatcher" $ do
      binocularsConfigQxQyQzNcore .=? field "ncores" auto
      binocularsConfigQxQyQzDestination .= field "destination" auto
      binocularsConfigQxQyQzOverwrite .= field "overwrite" auto
    section "input" $ do
      binocularsConfigQxQyQzInputType .= field "type" auto
      binocularsConfigQxQyQzNexusdir .=? field "nexusdir" auto
      binocularsConfigQxQyQzTmpl .=? field "inputtmpl" auto
      binocularsConfigQxQyQzInputRange .=? field "inputrange" auto
      binocularsConfigQxQyQzDetector .=? field "detector" auto
      binocularsConfigQxQyQzCentralpixel .= field "centralpixel" auto
      binocularsConfigQxQyQzSdd .= field "sdd" auto
      binocularsConfigQxQyQzDetrot .=? field "detrot" auto
      binocularsConfigQxQyQzAttenuationCoefficient .=? field "attenuation_coefficient" auto
      binocularsConfigQxQyQzAttenuationMax .=? field "attenuation_max" auto
      binocularsConfigQxQyQzSurfaceOrientation .=? field "surface_orientation" auto
      binocularsConfigQxQyQzMaskmatrix .=? field "maskmatrix" auto
      binocularsConfigQxQyQzWavelength .=? field "wavelength" auto
      binocularsConfigQxQyQzDataPath .=? field "datapath" auto
      binocularsConfigQxQyQzImageSumMax .=? field "image_sum_max" auto
    section "projection" $ do
      binocularsConfigQxQyQzProjectionType .= field "type" auto
      binocularsConfigQxQyQzProjectionResolution .= field "resolution" auto
      binocularsConfigQxQyQzProjectionLimits .=? field "limits" auto

  overwriteInputRange mr c = case mr of
                               Nothing  -> c
                               (Just _) -> c{_binocularsConfigQxQyQzInputRange = mr}


------------------
-- Input Path's --
------------------

data HklBinocularsProjectionsQxQyQzException
    = MissingAttenuationCoefficient
    deriving (Show)

instance Exception HklBinocularsProjectionsQxQyQzException

getResolution' :: MonadThrow m => Config 'QxQyQzProjection -> m [Double]
getResolution' c = getResolution (_binocularsConfigQxQyQzProjectionResolution c) 3

-----------------------
-- QxQyQz Projection --
-----------------------

{-# INLINE spaceQxQyQz #-}
spaceQxQyQz :: Detector a DIM2 -> Array F DIM3 Double -> Resolutions -> Maybe Mask -> SurfaceOrientation -> Maybe [Limits] -> Space DIM3 -> DataFrameQCustom -> IO (DataFrameSpace DIM3)
spaceQxQyQz det pixels rs mmask' surf mlimits space@(Space fSpace) (DataFrameQCustom att g img _) =
  withNPixels det $ \nPixels ->
  withGeometry g $ \geometry ->
  withForeignPtr (toForeignPtr pixels) $ \pix ->
  withArrayLen rs $ \nr r ->
  withPixelsDims pixels $ \ndim dims ->
  withMaybeMask mmask' $ \ mask'' ->
  withMaybeLimits mlimits rs $ \nlimits limits ->
  withForeignPtr fSpace $ \pSpace -> do
  case img of
    (ImageInt32 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_qxqyqz_int32_t" #-} c'hkl_binoculars_space_qxqyqz_int32_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits)
    (ImageWord16 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_qxqyqz_uint16_t" #-} c'hkl_binoculars_space_qxqyqz_uint16_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits)
    (ImageWord32 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_qxqyqz_uint32_t" #-} c'hkl_binoculars_space_qxqyqz_uint32_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits)

  return (DataFrameSpace img space att)

----------
-- Pipe --
----------

class (FramesQCustomP a, Show a) => ProcessQxQyQzP a where
  processQxQyQzP :: (MonadIO m, MonadLogger m, MonadReader (Config 'QxQyQzProjection) m, MonadThrow m)
                 => m a -> m ()
  processQxQyQzP mkPaths = do
    (conf :: Config 'QxQyQzProjection) <- ask
    let det = fromMaybe defaultDetector (_binocularsConfigQxQyQzDetector conf)
    let mlimits = _binocularsConfigQxQyQzProjectionLimits conf
    let destination = _binocularsConfigQxQyQzDestination conf
    let output' = case _binocularsConfigQxQyQzInputRange conf of
                   Just r  -> destination' r mlimits destination
                   Nothing -> destination' (ConfigRange []) mlimits destination
    let centralPixel' = _binocularsConfigQxQyQzCentralpixel conf
    let (Meter sampleDetectorDistance) = _binocularsConfigQxQyQzSdd conf
    let (Degree detrot) = fromMaybe (Degree (0 *~ degree)) ( _binocularsConfigQxQyQzDetrot conf)
    let surfaceOrientation = fromMaybe SurfaceOrientationVertical (_binocularsConfigQxQyQzSurfaceOrientation conf)
    let mImageSumMax = _binocularsConfigQxQyQzImageSumMax conf

    h5d <- mkPaths
    filenames <- InputList
                <$> files (_binocularsConfigQxQyQzNexusdir conf)
                          (_binocularsConfigQxQyQzInputRange conf)
                          (_binocularsConfigQxQyQzTmpl conf)
    mask' <- getMask (_binocularsConfigQxQyQzMaskmatrix conf) det
    pixels <- liftIO $ getPixelsCoordinates det centralPixel' sampleDetectorDistance detrot
    res <- getResolution' conf

    -- compute the jobs

    let fns = concatMap (replicate 1) (toList filenames)
    chunks <- liftIO $ runSafeT $ toListM $ each fns >-> chunkP h5d
    cap' <-  liftIO $ getNumCapabilities
    let ntot = sum (Prelude.map clength chunks)
    let cap = if cap' >= 2 then cap' - 1 else cap'
    let jobs = chunk (quot ntot cap) chunks

    -- log parameters

    $(logDebugSH) filenames
    $(logDebugSH) h5d
    $(logDebugSH) chunks
    $(logDebug) "start gessing final cube size"

    -- guess the final cube dimensions (To optimize, do not create the cube, just extract the shape)

    guessed <- liftIO $ withCubeAccumulator EmptyCube $ \c ->
      runSafeT $ runEffect $
      each chunks
      >-> Pipes.Prelude.map (\(Chunk fn f t) -> (fn, [f, (quot (f + t) 4), (quot (f + t) 4) * 2, (quot (f + t) 4) * 3, t]))
      >-> framesQCustomP h5d
      >-> project det 3 (spaceQxQyQz det pixels res mask' surfaceOrientation mlimits)
      >-> accumulateP c

    $(logDebug) "stop gessing final cube size"

    -- do the final projection

    $(logInfo) (pack $ printf "let's do a QxQyQz projection of %d %s image(s) on %d core(s)" ntot (show det) cap)

    liftIO $ withProgressBar ntot $ \pb -> do
      r' <- mapConcurrently (\job -> withCubeAccumulator guessed $ \c ->
                               runSafeT $ runEffect $
                               each job
                               >-> Pipes.Prelude.map (\(Chunk fn f t) -> (fn, [f..t]))
                               >-> framesQCustomP h5d
                               >-> Pipes.Prelude.filter (\(DataFrameQCustom _ _ img _) -> filterSumImage mImageSumMax img)
                               >-> project det 3 (spaceQxQyQz det pixels res mask' surfaceOrientation mlimits)
                               >-> tee (accumulateP c)
                               >-> progress pb
                           ) jobs
      saveCube output' r'


instance ProcessQxQyQzP (DataSourcePath DataFrameQCustom)

---------
-- Cmd --
---------

process' :: (MonadLogger m, MonadThrow m, MonadIO m, MonadReader (Config 'QxQyQzProjection) m)
         => m ()
process' = do
  c <- ask
  processQxQyQzP (h5dpathQCustom
                  (_binocularsConfigQxQyQzInputType c)
                  (_binocularsConfigQxQyQzAttenuationCoefficient c)
                  (_binocularsConfigQxQyQzAttenuationMax c)
                  (_binocularsConfigQxQyQzDetector c)
                  (_binocularsConfigQxQyQzWavelength c)
                  Nothing
                 )

processQxQyQz :: (MonadLogger m, MonadThrow m, MonadIO m) => Maybe FilePath -> Maybe (ConfigRange) -> m ()
processQxQyQz mf mr = do
  econf <- liftIO $ getConfig mf
  case econf of
    Right conf -> do
      $(logDebug) "config red from the config file"
      $(logDebugSH) conf
      let conf' = overwriteInputRange mr conf
      $(logDebug) "config once overloaded with the command line arguments"
      $(logDebugSH) conf'
      runReaderT process' conf'
    Left e      -> $(logErrorSH) e

newQxQyQz :: (MonadIO m, MonadLogger m, MonadThrow m)
          => Path Abs Dir -> m ()
newQxQyQz cwd = do
  let conf = defaultConfig {_binocularsConfigQxQyQzNexusdir = Just cwd}
  liftIO $ Data.Text.IO.putStr $ serializeIni (ini conf specConfig)

updateQxQyQz :: (MonadIO m, MonadLogger m, MonadThrow m)
             => Maybe FilePath -> m ()
updateQxQyQz mf = do
  (conf :: Either String (Config 'QxQyQzProjection)) <- liftIO $ getConfig mf
  $(logDebug) "config red from the config file"
  $(logDebugSH) conf
  case conf of
    Left e      -> $(logErrorSH) e
    Right conf' -> liftIO $ Data.Text.IO.putStr $ serializeIni (ini conf' specConfig)

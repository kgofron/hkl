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
    Copyright  : Copyright (C) 2014-2023 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.Binoculars.Projections.QparQper
    ( newQparQper
    , processQparQper
    , updateQparQper
    ) where

import           Control.Applicative                ((<|>))
import           Control.Concurrent.Async           (mapConcurrently)
import           Control.Lens                       (makeLenses, over)
import           Control.Monad.Catch                (Exception, MonadThrow,
                                                     throwM)
import           Control.Monad.IO.Class             (MonadIO (liftIO), liftIO)
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

------------
-- Config --
------------

data instance Config 'QparQperProjection = BinocularsConfigQparQper
  { _binocularsConfigQparQperNcore                  :: Maybe Int
  , _binocularsConfigQparQperDestination            :: DestinationTmpl
  , _binocularsConfigQparQperOverwrite              :: Bool
  , _binocularsConfigQparQperInputType              :: InputType
  , _binocularsConfigQparQperNexusdir               :: Maybe (Path Abs Dir)
  , _binocularsConfigQparQperTmpl                   :: Maybe InputTmpl
  , _binocularsConfigQparQperInputRange             :: Maybe ConfigRange
  , _binocularsConfigQparQperDetector               :: Maybe (Detector Hkl DIM2)
  , _binocularsConfigQparQperCentralpixel           :: (Int, Int)
  , _binocularsConfigQparQperSdd                    :: Meter
  , _binocularsConfigQparQperDetrot                 :: Maybe Degree
  , _binocularsConfigQparQperAttenuationCoefficient :: Maybe Double
  , _binocularsConfigQparQperAttenuationMax         :: Maybe Float
  , _binocularsConfigQparQperSurfaceOrientation     :: Maybe SurfaceOrientation
  , _binocularsConfigQparQperMaskmatrix             :: Maybe MaskLocation
  , _binocularsConfigQparQperWavelength             :: Maybe Angstrom
  , _binocularsConfigQparQperProjectionType         :: ProjectionType
  , _binocularsConfigQparQperProjectionResolution   :: Resolutions DIM2
  , _binocularsConfigQparQperProjectionLimits       :: Maybe (RLimits DIM2)
  , _binocularsConfigQparQperDataPath               :: Maybe (DataSourcePath DataFrameQCustom)
  , _binocularsConfigQparQperImageSumMax            :: Maybe Double
  } deriving (Eq, Show)

makeLenses 'BinocularsConfigQparQper

instance HasIniConfig 'QparQperProjection where
  defaultConfig = BinocularsConfigQparQper
    { _binocularsConfigQparQperNcore = Nothing
    , _binocularsConfigQparQperDestination = DestinationTmpl "."
    , _binocularsConfigQparQperOverwrite = False
    , _binocularsConfigQparQperInputType = SixsFlyScanUhv
    , _binocularsConfigQparQperNexusdir = Nothing
    , _binocularsConfigQparQperTmpl = Nothing
    , _binocularsConfigQparQperInputRange  = Nothing
    , _binocularsConfigQparQperDetector = Nothing
    , _binocularsConfigQparQperCentralpixel = (0, 0)
    , _binocularsConfigQparQperSdd = Meter (1 *~ meter)
    , _binocularsConfigQparQperDetrot = Nothing
    , _binocularsConfigQparQperAttenuationCoefficient = Nothing
    , _binocularsConfigQparQperAttenuationMax = Nothing
    , _binocularsConfigQparQperSurfaceOrientation = Just SurfaceOrientationVertical
    , _binocularsConfigQparQperMaskmatrix = Nothing
    , _binocularsConfigQparQperWavelength = Nothing
    , _binocularsConfigQparQperProjectionType = QparQperProjection
    , _binocularsConfigQparQperProjectionResolution = Resolutions2 0.01 0.01
    , _binocularsConfigQparQperProjectionLimits  = Nothing
    , _binocularsConfigQparQperDataPath = Just defaultDataSourcePath'DataFrameQCustom
    , _binocularsConfigQparQperImageSumMax = Nothing
    }

  specConfig = do
    section "dispatcher" $ do
      binocularsConfigQparQperNcore .=? field "ncores" auto
      binocularsConfigQparQperDestination .= field "destination" auto
      binocularsConfigQparQperOverwrite .= field "overwrite" auto
    section "input" $ do
      binocularsConfigQparQperInputType .= field "type" auto
      binocularsConfigQparQperNexusdir .=? field "nexusdir" auto
      binocularsConfigQparQperTmpl .=? field "inputtmpl" auto
      binocularsConfigQparQperInputRange .=? field "inputrange" auto
      binocularsConfigQparQperDetector .=? field "detector" auto
      binocularsConfigQparQperCentralpixel .= field "centralpixel" auto
      binocularsConfigQparQperSdd .= field "sdd" auto
      binocularsConfigQparQperDetrot .=? field "detrot" auto
      binocularsConfigQparQperAttenuationCoefficient .=? field "attenuation_coefficient" auto
      binocularsConfigQparQperAttenuationMax .=? field "attenuation_max" auto
      binocularsConfigQparQperSurfaceOrientation .=? field "surface_orientation" auto
      binocularsConfigQparQperMaskmatrix .=? field "maskmatrix" auto
      binocularsConfigQparQperWavelength .=? field "wavelength" auto
      binocularsConfigQparQperDataPath .=? field "datapath" auto
      binocularsConfigQparQperImageSumMax .=? field "image_sum_max" auto
    section "projection" $ do
      binocularsConfigQparQperProjectionType .= field "type" auto
      binocularsConfigQparQperProjectionResolution .= field "resolution" auto
      binocularsConfigQparQperProjectionLimits .=? field "limits" auto

  overwriteWithCmd mr conf = return $ over binocularsConfigQparQperInputRange (mr <|>) conf

data HklBinocularsProjectionsQparQperException
    = MissingAttenuationCoefficient
    | MissingInputRange
    deriving (Show)

instance Exception HklBinocularsProjectionsQparQperException

-------------------------
-- QparQper Projection --
-------------------------

{-# INLINE spaceQparQper #-}
spaceQparQper :: Detector a DIM2 -> Array F DIM3 Double -> Resolutions DIM2 -> Maybe Mask -> SurfaceOrientation -> Maybe (RLimits DIM2) -> Space DIM2 -> DataFrameQCustom -> IO (DataFrameSpace DIM2)
spaceQparQper det pixels rs mmask' surf mlimits space@(Space fSpace) (DataFrameQCustom att g img _) =
  withNPixels det $ \nPixels ->
  withGeometry g $ \geometry ->
  withForeignPtr (toForeignPtr pixels) $ \pix ->
  withResolutions rs $ \nr r ->
  withPixelsDims pixels $ \ndim dims ->
  withMaybeMask mmask' $ \ mask'' ->
  withMaybeLimits mlimits rs $ \nlimits limits ->
  withForeignPtr fSpace $ \pSpace -> do
  case img of
    (ImageInt32 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_qparqper_int32_t" #-} c'hkl_binoculars_space_qparqper_int32_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits)
    (ImageWord16 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_qparqper_uint16_t" #-} c'hkl_binoculars_space_qparqper_uint16_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits)
    (ImageWord32 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_qparqper_uint32_t" #-} c'hkl_binoculars_space_qparqper_uint32_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits)

  return (DataFrameSpace img space att)

----------
-- Pipe --
----------

class (FramesQCustomP a, Show a) => ProcessQparQperP a where
  processQparQperP :: (MonadIO m, MonadLogger m, MonadReader (Config 'QparQperProjection) m, MonadThrow m)
                   => m a -> m ()
  processQparQperP mkPaths = do
    (conf :: (Config 'QparQperProjection)) <- ask
    let det = fromMaybe defaultDetector (_binocularsConfigQparQperDetector conf)
    let mlimits = _binocularsConfigQparQperProjectionLimits conf
    let destination = _binocularsConfigQparQperDestination conf
    let output' = case _binocularsConfigQparQperInputRange conf of
                   Just r  -> destination' r mlimits destination
                   Nothing -> throwM MissingInputRange
    let centralPixel' = _binocularsConfigQparQperCentralpixel conf
    let (Meter sampleDetectorDistance) = _binocularsConfigQparQperSdd conf
    let (Degree detrot) = fromMaybe (Degree (0 *~ degree)) ( _binocularsConfigQparQperDetrot conf)
    let surfaceOrientation = fromMaybe SurfaceOrientationVertical (_binocularsConfigQparQperSurfaceOrientation conf)
    let mImageSumMax = _binocularsConfigQparQperImageSumMax conf
    let res = _binocularsConfigQparQperProjectionResolution conf

    h5d <- mkPaths
    filenames <- InputFn'List
                <$> files (_binocularsConfigQparQperNexusdir conf)
                          (_binocularsConfigQparQperInputRange conf)
                          (_binocularsConfigQparQperTmpl conf)
    mask' <- getMask (_binocularsConfigQparQperMaskmatrix conf) det
    pixels <- liftIO $ getPixelsCoordinates det centralPixel' sampleDetectorDistance detrot

    -- compute the jobs

    let fns = concatMap (replicate 1) (toList filenames)
    chunks <- liftIO $ runSafeT $ toListM $ each fns >-> chunkP h5d
    cap' <-  liftIO getNumCapabilities
    let ntot = sum (Prelude.map clength chunks)
    let cap = if cap' >= 2 then cap' - 1 else cap'
    let jobs = chunk (quot ntot cap) chunks

    -- log parameters

    $(logDebugSH) filenames
    $(logDebugSH) h5d
    $(logDebug) "start gessing final cube size"

    -- guess the final cube dimensions (To optimize, do not create the cube, just extract the shape)

    guessed <- liftIO $ withCubeAccumulator EmptyCube $ \c ->
      runSafeT $ runEffect $
      each chunks
      >-> Pipes.Prelude.map (\(Chunk fn f t) -> (fn, [f, quot (f + t) 4, quot (f + t) 4 * 2, quot (f + t) 4 * 3, t]))
      >-> framesQCustomP h5d
      >-> project det 2 (spaceQparQper det pixels res mask' surfaceOrientation mlimits)
      >-> accumulateP c

    $(logDebug) "stop gessing final cube size"

    -- do the final projection

    $(logInfo) (pack $ printf "let's do a QparQper projection of %d %s image(s) on %d core(s)" ntot (show det) cap)

    liftIO $ withProgressBar ntot $ \pb -> do
      r' <- mapConcurrently (\job -> withCubeAccumulator guessed $ \c ->
                               runSafeT $ runEffect $
                               each job
                               >-> Pipes.Prelude.map (\(Chunk fn f t) -> (fn, [f..t]))
                               >-> framesQCustomP h5d
                               >-> Pipes.Prelude.filter (\(DataFrameQCustom _ _ img _) -> filterSumImage mImageSumMax img)
                               >-> project det 2 (spaceQparQper det pixels res mask' surfaceOrientation mlimits)
                               >-> tee (accumulateP c)
                               >-> progress pb
                           ) jobs
      saveCube output' r'

instance ProcessQparQperP (DataSourcePath DataFrameQCustom)

---------
-- Cmd --
---------

process' :: (MonadLogger m, MonadThrow m, MonadIO m, MonadReader (Config 'QparQperProjection) m)
         => m ()
process' = do
  c <- ask
  let i = _binocularsConfigQparQperInputType c
  let mc = _binocularsConfigQparQperAttenuationCoefficient c
  let mm = _binocularsConfigQparQperAttenuationMax c
  let mdet = _binocularsConfigQparQperDetector c
  let mw = _binocularsConfigQparQperWavelength c
  processQparQperP (h5dpathQCustom i mc mm mdet mw Nothing)

processQparQper :: (MonadLogger m, MonadThrow m, MonadIO m) => Maybe FilePath -> Maybe ConfigRange -> m ()
processQparQper mf mr = do
  econf <- liftIO $ getConfig mf
  case econf of
    Right conf -> do
      $(logDebug) "config red from the config file"
      $(logDebugSH) conf
      conf' <- overwriteWithCmd mr conf
      $(logDebug) "config once overloaded with the command line arguments"
      $(logDebugSH) conf'
      runReaderT process' conf'
    Left e      -> $(logErrorSH) e

newQparQper :: (MonadIO m, MonadLogger m, MonadThrow m)
            => Path Abs Dir -> m ()
newQparQper cwd = do
  let conf = defaultConfig {_binocularsConfigQparQperNexusdir = Just cwd}
  liftIO $ Data.Text.IO.putStr $ serializeIni (ini conf specConfig)

updateQparQper :: (MonadIO m, MonadLogger m, MonadThrow m)
               => Maybe FilePath -> m ()
updateQparQper mf = do
  (conf  :: Either String (Config 'QparQperProjection))<- liftIO $ getConfig mf
  $(logDebug) "config red from the config file"
  $(logDebugSH) conf
  case conf of
    Left e      -> $(logErrorSH) e
    Right conf' -> liftIO $ Data.Text.IO.putStr $ serializeIni (ini conf' specConfig)

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

module Hkl.Binoculars.Projections.QIndex
    ( newQIndex
    , processQIndex
    , updateQIndex
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
import           Hkl.Types


data HklBinocularsProjectionsQIndexException
    = MissingAttenuationCoefficient
    | MissingInputRange
    deriving (Show)

instance Exception HklBinocularsProjectionsQIndexException

------------
-- Config --
------------

data instance Config 'QIndexProjection = BinocularsConfigQIndex
  { _binocularsConfigQIndexNcore                  :: Maybe Int
  , _binocularsConfigQIndexDestination            :: DestinationTmpl
  , _binocularsConfigQIndexOverwrite              :: Bool
  , _binocularsConfigQIndexInputType              :: InputType
  , _binocularsConfigQIndexNexusdir               :: Maybe (Path Abs Dir)
  , _binocularsConfigQIndexTmpl                   :: Maybe InputTmpl
  , _binocularsConfigQIndexInputRange             :: Maybe ConfigRange
  , _binocularsConfigQIndexDetector               :: Maybe (Detector Hkl DIM2)
  , _binocularsConfigQIndexCentralpixel           :: (Int, Int)
  , _binocularsConfigQIndexSdd                    :: Meter
  , _binocularsConfigQIndexDetrot                 :: Maybe Degree
  , _binocularsConfigQIndexAttenuationCoefficient :: Maybe Double
  , _binocularsConfigQIndexAttenuationMax         :: Maybe Float
  , _binocularsConfigQIndexMaskmatrix             :: Maybe MaskLocation
  , _binocularsConfigQIndexWavelength             :: Maybe Angstrom
  , _binocularsConfigQIndexProjectionType         :: ProjectionType
  , _binocularsConfigQIndexProjectionResolution   :: Resolutions DIM2
  , _binocularsConfigQIndexProjectionLimits       :: Maybe (RLimits DIM2)
  , _binocularsConfigQIndexDataPath               :: Maybe (DataSourcePath DataFrameQCustom)
  , _binocularsConfigQIndexImageSumMax            :: Maybe Double
 } deriving (Eq, Show)

makeLenses 'BinocularsConfigQIndex

instance HasIniConfig 'QIndexProjection where
  defaultConfig = BinocularsConfigQIndex
    { _binocularsConfigQIndexNcore = Nothing
    , _binocularsConfigQIndexDestination = DestinationTmpl "."
    , _binocularsConfigQIndexOverwrite = False
    , _binocularsConfigQIndexInputType = SixsFlyScanUhv
    , _binocularsConfigQIndexNexusdir = Nothing
    , _binocularsConfigQIndexTmpl = Nothing
    , _binocularsConfigQIndexInputRange  = Nothing
    , _binocularsConfigQIndexDetector = Nothing
    , _binocularsConfigQIndexCentralpixel = (0, 0)
    , _binocularsConfigQIndexSdd = Meter (1 *~ meter)
    , _binocularsConfigQIndexDetrot = Nothing
    , _binocularsConfigQIndexAttenuationCoefficient = Nothing
    , _binocularsConfigQIndexAttenuationMax = Nothing
    , _binocularsConfigQIndexMaskmatrix = Nothing
    , _binocularsConfigQIndexWavelength = Nothing
    , _binocularsConfigQIndexProjectionType = QIndexProjection
    , _binocularsConfigQIndexProjectionResolution = Resolutions2 0.01 1
    , _binocularsConfigQIndexProjectionLimits  = Nothing
    , _binocularsConfigQIndexDataPath = Just defaultDataSourcePath'DataFrameQCustom
    , _binocularsConfigQIndexImageSumMax = Nothing
    }

  specConfig = do
    section "dispatcher" $ do
      binocularsConfigQIndexNcore .=? field "ncores" auto
      binocularsConfigQIndexDestination .= field "destination" auto
      binocularsConfigQIndexOverwrite .= field "overwrite" auto
    section "input" $ do
      binocularsConfigQIndexInputType .= field "type" auto
      binocularsConfigQIndexNexusdir .=? field "nexusdir" auto
      binocularsConfigQIndexTmpl .=? field "inputtmpl" auto
      binocularsConfigQIndexInputRange .=? field "inputrange" auto
      binocularsConfigQIndexDetector .=? field "detector" auto
      binocularsConfigQIndexCentralpixel .= field "centralpixel" auto
      binocularsConfigQIndexSdd .= field "sdd" auto
      binocularsConfigQIndexDetrot .=? field "detrot" auto
      binocularsConfigQIndexAttenuationCoefficient .=? field "attenuation_coefficient" auto
      binocularsConfigQIndexAttenuationMax .=? field "attenuation_max" auto
      binocularsConfigQIndexMaskmatrix .=? field "maskmatrix" auto
      binocularsConfigQIndexWavelength .=? field "wavelength" auto
      binocularsConfigQIndexDataPath .=? field "datapath" auto
      binocularsConfigQIndexImageSumMax .=? field "image_sum_max" auto
    section "projection" $ do
      binocularsConfigQIndexProjectionType .= field "type" auto
      binocularsConfigQIndexProjectionResolution .= field "resolution" auto
      binocularsConfigQIndexProjectionLimits .=? field "limits" auto

  overwriteWithCmd mr = over binocularsConfigQIndexInputRange (mr <|>)

-------------------------
-- QIndex Projection --
-------------------------

{-# INLINE spaceQIndex #-}
spaceQIndex :: Detector a DIM2 -> Array F DIM3 Double -> Resolutions DIM2 -> Maybe Mask -> Maybe (RLimits DIM2) -> Space DIM2 -> DataFrameQCustom -> IO (DataFrameSpace DIM2)
spaceQIndex det pixels rs mmask' mlimits space@(Space fSpace) (DataFrameQCustom att g img index) =
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
      {-# SCC "hkl_binoculars_space_angles_int32_t" #-} c'hkl_binoculars_space_qindex_int32_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits) (CDouble . unIndex $ index)
    (ImageWord16 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_angles_uint16_t" #-} c'hkl_binoculars_space_qindex_uint16_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits) (CDouble . unIndex $ index)
    (ImageWord32 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_angles_uint32_t" #-} c'hkl_binoculars_space_qindex_uint32_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits) (CDouble . unIndex $ index)

  return (DataFrameSpace img space att)

----------
-- Pipe --
----------

class (FramesQCustomP a, Show a) => ProcessQIndexP a where
  processQIndexP :: (MonadIO m, MonadLogger m, MonadReader (Config 'QIndexProjection) m, MonadThrow m)
                   => m a -> m ()
  processQIndexP mkPaths = do
    (conf :: (Config 'QIndexProjection)) <- ask
    let det = fromMaybe defaultDetector (_binocularsConfigQIndexDetector conf)
    let mlimits = _binocularsConfigQIndexProjectionLimits conf
    let destination = _binocularsConfigQIndexDestination conf
    let output' = case _binocularsConfigQIndexInputRange conf of
                   Just r  -> destination' r mlimits destination
                   Nothing -> throwM MissingInputRange
    let centralPixel' = _binocularsConfigQIndexCentralpixel conf
    let (Meter sampleDetectorDistance) = _binocularsConfigQIndexSdd conf
    let (Degree detrot) = fromMaybe (Degree (0 *~ degree)) ( _binocularsConfigQIndexDetrot conf)
    let mImageSumMax = _binocularsConfigQIndexImageSumMax conf
    let res = _binocularsConfigQIndexProjectionResolution conf

    h5d <- mkPaths
    filenames <- InputFn'List
                <$> files (_binocularsConfigQIndexNexusdir conf)
                          (_binocularsConfigQIndexInputRange conf)
                          (_binocularsConfigQIndexTmpl conf)
    mask' <- getMask (_binocularsConfigQIndexMaskmatrix conf) det
    pixels <- liftIO $ getPixelsCoordinates det centralPixel' sampleDetectorDistance detrot

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
    $(logDebug) "start gessing final cube size"

    -- guess the final cube dimensions (To optimize, do not create the cube, just extract the shape)

    guessed <- liftIO $ withCubeAccumulator EmptyCube $ \c ->
      runSafeT $ runEffect $
      each chunks
      >-> Pipes.Prelude.map (\(Chunk fn f t) -> (fn, [f, (quot (f + t) 4), (quot (f + t) 4) * 2, (quot (f + t) 4) * 3, t]))
      >-> framesQCustomP h5d
      >-> project det 2 (spaceQIndex det pixels res mask' mlimits)
      >-> accumulateP c

    $(logDebug) "stop gessing final cube size"

    -- do the final projection

    $(logInfo) (pack $ printf "let's do a QIndex projection of %d %s image(s) on %d core(s)" ntot (show det) cap)

    liftIO $ withProgressBar ntot $ \pb -> do
      r' <- mapConcurrently (\job -> withCubeAccumulator guessed $ \c ->
                               runSafeT $ runEffect $
                               each job
                               >-> Pipes.Prelude.map (\(Chunk fn f t) -> (fn, [f..t]))
                               >-> framesQCustomP h5d
                               >-> Pipes.Prelude.filter (\(DataFrameQCustom _ _ img _) -> filterSumImage mImageSumMax img)
                               >-> project det 2 (spaceQIndex det pixels res mask' mlimits)
                               >-> tee (accumulateP c)
                               >-> progress pb
                           ) jobs
      saveCube output' r'

instance ProcessQIndexP (DataSourcePath DataFrameQCustom)

---------
-- Cmd --
---------

process' :: (MonadLogger m, MonadThrow m, MonadIO m, MonadReader (Config 'QIndexProjection) m)
         => m ()
process' = do
  c <- ask
  let i = _binocularsConfigQIndexInputType c
  let mc = _binocularsConfigQIndexAttenuationCoefficient c
  let mm = _binocularsConfigQIndexAttenuationMax c
  let mdet = _binocularsConfigQIndexDetector c
  let mwavelength = _binocularsConfigQIndexWavelength c
  processQIndexP (h5dpathQCustom i mc mm mdet mwavelength Nothing)

processQIndex :: (MonadLogger m, MonadThrow m, MonadIO m) => Maybe FilePath -> Maybe (ConfigRange) -> m ()
processQIndex mf mr = do
  econf <- liftIO $ getConfig mf
  case econf of
    Right conf -> do
      $(logDebug) "config red from the config file"
      $(logDebugSH) conf
      let conf' = overwriteWithCmd mr conf
      $(logDebug) "config once overloaded with the command line arguments"
      $(logDebugSH) conf'
      runReaderT process' conf'
    Left e      -> $(logErrorSH) e

newQIndex :: (MonadIO m, MonadLogger m, MonadThrow m)
            => Path Abs Dir -> m ()
newQIndex cwd = do
  let conf = defaultConfig {_binocularsConfigQIndexNexusdir = Just cwd}
  liftIO $ Data.Text.IO.putStr $ serializeIni (ini conf specConfig)

updateQIndex :: (MonadIO m, MonadLogger m, MonadThrow m)
               => Maybe FilePath -> m ()
updateQIndex mf = do
  (conf  :: Either String (Config 'QIndexProjection))<- liftIO $ getConfig mf
  $(logDebug) "config red from the config file"
  $(logDebugSH) conf
  case conf of
    Left e      -> $(logErrorSH) e
    Right conf' -> liftIO $ Data.Text.IO.putStr $ serializeIni (ini conf' specConfig)

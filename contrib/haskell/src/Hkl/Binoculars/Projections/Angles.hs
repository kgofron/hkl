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

module Hkl.Binoculars.Projections.Angles
    ( newAngles
    , processAngles
    , updateAngles
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



data HklBinocularsProjectionsAnglesException
    = MissingInputRange
    deriving (Show)

instance Exception HklBinocularsProjectionsAnglesException

------------
-- Config --
------------

data instance Config 'AnglesProjection = BinocularsConfigAngles
  { _binocularsConfigAnglesNcore                  :: Maybe Int
  , _binocularsConfigAnglesDestination            :: DestinationTmpl
  , _binocularsConfigAnglesOverwrite              :: Bool
  , _binocularsConfigAnglesInputType              :: InputType
  , _binocularsConfigAnglesNexusdir               :: Maybe (Path Abs Dir)
  , _binocularsConfigAnglesTmpl                   :: Maybe InputTmpl
  , _binocularsConfigAnglesInputRange             :: Maybe ConfigRange
  , _binocularsConfigAnglesDetector               :: Maybe (Detector Hkl DIM2)
  , _binocularsConfigAnglesCentralpixel           :: (Int, Int)
  , _binocularsConfigAnglesSdd                    :: Meter
  , _binocularsConfigAnglesDetrot                 :: Maybe Degree
  , _binocularsConfigAnglesAttenuationCoefficient :: Maybe Double
  , _binocularsConfigAnglesAttenuationMax         :: Maybe Float
  , _binocularsConfigAnglesMaskmatrix             :: Maybe MaskLocation
  , _binocularsConfigAnglesWavelength             :: Maybe Angstrom
  , _binocularsConfigAnglesProjectionType         :: ProjectionType
  , _binocularsConfigAnglesProjectionResolution   :: Resolutions DIM3
  , _binocularsConfigAnglesProjectionLimits       :: Maybe (RLimits DIM3)
  , _binocularsConfigAnglesDataPath               :: Maybe (DataSourcePath DataFrameQCustom)
  , _binocularsConfigAnglesSampleAxis             :: Maybe SampleAxis
  , _binocularsConfigAnglesImageSumMax            :: Maybe Double
 } deriving (Eq, Show)

makeLenses 'BinocularsConfigAngles

instance HasIniConfig 'AnglesProjection where
  defaultConfig = BinocularsConfigAngles
    { _binocularsConfigAnglesNcore = Nothing
    , _binocularsConfigAnglesDestination = DestinationTmpl "."
    , _binocularsConfigAnglesOverwrite = False
    , _binocularsConfigAnglesInputType = SixsFlyScanUhv
    , _binocularsConfigAnglesNexusdir = Nothing
    , _binocularsConfigAnglesTmpl = Nothing
    , _binocularsConfigAnglesInputRange  = Nothing
    , _binocularsConfigAnglesDetector = Nothing
    , _binocularsConfigAnglesCentralpixel = (0, 0)
    , _binocularsConfigAnglesSdd = Meter (1 *~ meter)
    , _binocularsConfigAnglesDetrot = Nothing
    , _binocularsConfigAnglesAttenuationCoefficient = Nothing
    , _binocularsConfigAnglesAttenuationMax = Nothing
    , _binocularsConfigAnglesMaskmatrix = Nothing
    , _binocularsConfigAnglesWavelength = Nothing
    , _binocularsConfigAnglesProjectionType = AnglesProjection
    , _binocularsConfigAnglesProjectionResolution = Resolutions3 1 1 1
    , _binocularsConfigAnglesProjectionLimits  = Nothing
    , _binocularsConfigAnglesDataPath = Just defaultDataSourcePath'DataFrameQCustom
    , _binocularsConfigAnglesSampleAxis = Nothing
    , _binocularsConfigAnglesImageSumMax = Nothing
    }

  specConfig = do
    section "dispatcher" $ do
      binocularsConfigAnglesNcore .=? field "ncores" auto
      binocularsConfigAnglesDestination .= field "destination" auto
      binocularsConfigAnglesOverwrite .= field "overwrite" auto
    section "input" $ do
      binocularsConfigAnglesInputType .= field "type" auto
      binocularsConfigAnglesNexusdir .=? field "nexusdir" auto
      binocularsConfigAnglesTmpl .=? field "inputtmpl" auto
      binocularsConfigAnglesInputRange .=? field "inputrange" auto
      binocularsConfigAnglesDetector .=? field "detector" auto
      binocularsConfigAnglesCentralpixel .= field "centralpixel" auto
      binocularsConfigAnglesSdd .= field "sdd" auto
      binocularsConfigAnglesDetrot .=? field "detrot" auto
      binocularsConfigAnglesAttenuationCoefficient .=? field "attenuation_coefficient" auto
      binocularsConfigAnglesAttenuationMax .=? field "attenuation_max" auto
      binocularsConfigAnglesMaskmatrix .=? field "maskmatrix" auto
      binocularsConfigAnglesWavelength .=? field "wavelength" auto
      binocularsConfigAnglesDataPath .=? field "datapath" auto
      binocularsConfigAnglesSampleAxis .=? field "sample_axis" auto
      binocularsConfigAnglesImageSumMax .=? field "image_sum_max" auto
    section "projection" $ do
      binocularsConfigAnglesProjectionType .= field "type" auto
      binocularsConfigAnglesProjectionResolution .= field "resolution" auto
      binocularsConfigAnglesProjectionLimits .=? field "limits" auto

  overwriteWithCmd mr conf = return $ over binocularsConfigAnglesInputRange (mr <|>) conf

getSampleAxis :: Config 'AnglesProjection -> SampleAxis
getSampleAxis c = case _binocularsConfigAnglesSampleAxis c of
                    (Just n) -> n
                    Nothing -> case _binocularsConfigAnglesProjectionType c of
                                AnglesProjection   -> SampleAxis "omega"
                                Angles2Projection  -> SampleAxis "mu"
                                HklProjection      -> undefined
                                QCustomProjection  -> undefined
                                QIndexProjection   -> undefined
                                QparQperProjection -> undefined
                                QxQyQzProjection   -> undefined

-------------------------
-- Angles Projection --
-------------------------

{-# INLINE spaceAngles #-}
spaceAngles :: Detector a DIM2 -> Array F DIM3 Double -> Resolutions DIM3 -> Maybe Mask -> Maybe (RLimits DIM3) -> SampleAxis -> Space DIM2 -> DataFrameQCustom -> IO (DataFrameSpace DIM2)
spaceAngles det pixels rs mmask' mlimits sAxis space@(Space fSpace) (DataFrameQCustom att g img _) =
  withNPixels det $ \nPixels ->
  withGeometry g $ \geometry ->
  withForeignPtr (toForeignPtr pixels) $ \pix ->
  withResolutions rs $ \nr r ->
  withPixelsDims pixels $ \ndim dims ->
  withMaybeMask mmask' $ \ mask'' ->
  withMaybeLimits mlimits rs $ \nlimits limits ->
  withSampleAxis sAxis $ \sampleAxis ->
  withForeignPtr fSpace $ \pSpace -> do
  case img of
    (ImageInt32 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_angles_int32_t" #-} c'hkl_binoculars_space_angles_int32_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits) sampleAxis
    (ImageWord16 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_angles_uint16_t" #-} c'hkl_binoculars_space_angles_uint16_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits) sampleAxis
    (ImageWord32 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_angles_uint32_t" #-} c'hkl_binoculars_space_angles_uint32_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits) sampleAxis

  return (DataFrameSpace img space att)

----------
-- Pipe --
----------

class (FramesQCustomP a, Show a) => ProcessAnglesP a where
  processAnglesP :: (MonadIO m, MonadLogger m, MonadReader (Config 'AnglesProjection) m, MonadThrow m)
                   => m a -> m ()
  processAnglesP mkPaths = do
    (conf :: (Config 'AnglesProjection)) <- ask
    let det = fromMaybe defaultDetector (_binocularsConfigAnglesDetector conf)
    let mlimits = _binocularsConfigAnglesProjectionLimits conf
    let destination = _binocularsConfigAnglesDestination conf
    let output' = case _binocularsConfigAnglesInputRange conf of
                   Just r  -> destination' r mlimits destination
                   Nothing -> throwM MissingInputRange
    let centralPixel' = _binocularsConfigAnglesCentralpixel conf
    let (Meter sampleDetectorDistance) = _binocularsConfigAnglesSdd conf
    let (Degree detrot) = fromMaybe (Degree (0 *~ degree)) ( _binocularsConfigAnglesDetrot conf)
    let sAxis = getSampleAxis conf
    let mImageSumMax = _binocularsConfigAnglesImageSumMax conf
    let res = _binocularsConfigAnglesProjectionResolution conf

    h5d <- mkPaths
    filenames <- InputFn'List
                <$> files (_binocularsConfigAnglesNexusdir conf)
                          (_binocularsConfigAnglesInputRange conf)
                          (_binocularsConfigAnglesTmpl conf)
    mask' <- getMask (_binocularsConfigAnglesMaskmatrix conf) det
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
      >-> project det 3 (spaceAngles det pixels res mask' mlimits sAxis)
      >-> accumulateP c

    $(logDebug) "stop gessing final cube size"

    -- do the final projection

    $(logInfo) (pack $ printf "let's do a Angles projection of %d %s image(s) on %d core(s)" ntot (show det) cap)

    liftIO $ withProgressBar ntot $ \pb -> do
      r' <- mapConcurrently (\job -> withCubeAccumulator guessed $ \c ->
                               runSafeT $ runEffect $
                               each job
                               >-> Pipes.Prelude.map (\(Chunk fn f t) -> (fn, [f..t]))
                               >-> framesQCustomP h5d
                               >-> Pipes.Prelude.filter (\(DataFrameQCustom _ _ img _) -> filterSumImage mImageSumMax img)
                               >-> project det 3 (spaceAngles det pixels res mask' mlimits sAxis)
                               >-> tee (accumulateP c)
                               >-> progress pb
                           ) jobs
      saveCube output' r'

instance ProcessAnglesP (DataSourcePath DataFrameQCustom)

---------
-- Cmd --
---------

process' :: (MonadLogger m, MonadThrow m, MonadIO m, MonadReader (Config 'AnglesProjection) m)
         => m ()
process' = do
  c <- ask
  let i = _binocularsConfigAnglesInputType c
  let mc = _binocularsConfigAnglesAttenuationCoefficient c
  let mm = _binocularsConfigAnglesAttenuationMax c
  let det = fromMaybe defaultDetector (_binocularsConfigAnglesDetector c)
  processAnglesP (h5dpathQCustom i mc mm det Nothing Nothing)

processAngles :: (MonadLogger m, MonadThrow m, MonadIO m) => Maybe FilePath -> Maybe ConfigRange -> m ()
processAngles mf mr = do
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

newAngles :: (MonadIO m, MonadLogger m, MonadThrow m)
            => Path Abs Dir -> m ()
newAngles cwd = do
  let conf = defaultConfig {_binocularsConfigAnglesNexusdir = Just cwd}
  liftIO $ Data.Text.IO.putStr $ serializeIni (ini conf specConfig)

updateAngles :: (MonadIO m, MonadLogger m, MonadThrow m)
               => Maybe FilePath -> m ()
updateAngles mf = do
  (conf  :: Either String (Config 'AnglesProjection))<- liftIO $ getConfig mf
  $(logDebug) "config red from the config file"
  $(logDebugSH) conf
  case conf of
    Left e      -> $(logErrorSH) e
    Right conf' -> liftIO $ Data.Text.IO.putStr $ serializeIni (ini conf' specConfig)

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import           Control.Concurrent.Async           (mapConcurrently)
import           Control.Monad.Catch                (MonadThrow)
import           Control.Monad.IO.Class             (MonadIO (liftIO), liftIO)
import           Control.Monad.Logger               (MonadLogger, logDebugN,
                                                     logInfoN)
import           Control.Monad.Reader               (MonadReader, ask)
import           Data.Array.Repa                    (Array)
import           Data.Array.Repa.Index              (DIM2, DIM3)
import           Data.Array.Repa.Repr.ForeignPtr    (F, toForeignPtr)
import           Data.HashMap.Lazy                  (fromList)
import           Data.Ini                           (Ini (..))
import           Data.Text                          (pack, unpack)
import           Data.Text.IO                       (putStr)
import           Data.Vector.Storable.Mutable       (unsafeWith)
import           Foreign.C.Types                    (CDouble (..))
import           Foreign.ForeignPtr                 (withForeignPtr)
import           Path                               (Abs, Dir, Path)
import           Pipes                              (each, runEffect, (>->))
import           Pipes.Prelude                      (filter, map, tee, toListM)
import           Pipes.Safe                         (runSafeT)
import           Text.Printf                        (printf)

import           Hkl.Binoculars.Common
import           Hkl.Binoculars.Config
import           Hkl.Binoculars.Config.Common
import           Hkl.Binoculars.Pipes
import           Hkl.Binoculars.Projections
import           Hkl.Binoculars.Projections.QCustom
import           Hkl.C.Binoculars
import           Hkl.Detector
import           Hkl.Image
import           Hkl.Utils

------------
-- Config --
------------

data instance Config 'AnglesProjection = BinocularsConfig'Angles
  { binocularsConfig'Angles'Common                 :: BinocularsConfig'Common
  , binocularsConfig'Angles'ProjectionType         :: ProjectionType
  , binocularsConfig'Angles'ProjectionResolution   :: Resolutions DIM3
  , binocularsConfig'Angles'ProjectionLimits       :: Maybe (RLimits DIM3)
  , binocularsConfig'Angles'SampleAxis             :: SampleAxis
  , binocularsConfig'Angles'DataPath               :: DataSourcePath DataFrameQCustom
 } deriving (Show)


default'BinocularsConfig'Angles :: Config 'AnglesProjection
default'BinocularsConfig'Angles
  = BinocularsConfig'Angles
    { binocularsConfig'Angles'Common = default'BinocularsConfig'Common
    , binocularsConfig'Angles'ProjectionType = AnglesProjection
    , binocularsConfig'Angles'ProjectionResolution = Resolutions3 1 1 1
    , binocularsConfig'Angles'ProjectionLimits = Nothing
    , binocularsConfig'Angles'DataPath = default'DataSourcePath'DataFrameQCustom
    , binocularsConfig'Angles'SampleAxis = SampleAxis "omega"
    }

newtype instance Args 'AnglesProjection = Args'AnglesProjection (Maybe ConfigRange)

instance HasIniConfig 'AnglesProjection where

  getConfig (ConfigContent cfg) (Args'AnglesProjection mr) capabilities = do
    -- (ConfigContent cfg) <- liftIO $ readConfig mf

    let ecommon = parse'BinocularsConfig'Common cfg mr capabilities
    case ecommon of
      Left err -> error err
      Right common -> do

        -- section input
        mdatapath <- parseMb cfg "input" "datapath"

        -- section projection
        projectionType <- parseFDef cfg "projection" "type" (binocularsConfig'Angles'ProjectionType default'BinocularsConfig'Angles)
        resolution <- parseFDef cfg "projection" "resolution" (binocularsConfig'Angles'ProjectionResolution default'BinocularsConfig'Angles)
        limits <- parseMb cfg "projection" "limits"

        sampleAxis <- parseFDef cfg "input" "sample_axis" $ case projectionType of
                                                             AnglesProjection   -> SampleAxis "omega"
                                                             Angles2Projection  -> SampleAxis "mu"
                                                             HklProjection      -> undefined
                                                             QCustomProjection  -> undefined
                                                             QIndexProjection   -> undefined
                                                             QparQperProjection -> undefined
                                                             QxQyQzProjection   -> undefined
                                                             RealSpaceProjection -> undefined
                                                             PixelsProjection -> undefined
                                                             TestProjection  -> undefined

        -- compute the datatype
        let datapath = case mdatapath of
                         Nothing -> guess'DataSourcePath'DataFrameQCustom common Nothing
                         Just d  -> overload'DataSourcePath'DataFrameQCustom common Nothing d

        pure $ BinocularsConfig'Angles common projectionType resolution limits sampleAxis datapath


instance ToIni (Config 'AnglesProjection) where
  toIni c = toIni (binocularsConfig'Angles'Common c)
            `mergeIni`
            Ini { iniSections = fromList [ ("input", elemFDef' "datapath" binocularsConfig'Angles'DataPath c default'BinocularsConfig'Angles
                                                     <> elemFDef "sample_axis" binocularsConfig'Angles'SampleAxis c default'BinocularsConfig'Angles
                                                     [ "the name of the sample axis"
                                                     , ""
                                                     , "default value: `omega`"
                                                     ]
                                            )
                                         , ("projection", elemFDef' "type" binocularsConfig'Angles'ProjectionType c default'BinocularsConfig'Angles
                                                          <> elemFDef' "resolution" binocularsConfig'Angles'ProjectionResolution c default'BinocularsConfig'Angles
                                                          <> elemFMbDef' "limits" binocularsConfig'Angles'ProjectionLimits c default'BinocularsConfig'Angles
                                           )]
                , iniGlobals = []
                }

-------------------------
-- Angles Projection --
-------------------------

{-# INLINE spaceAngles #-}
spaceAngles :: Detector a DIM2 -> Array F DIM3 Double -> Resolutions DIM3 -> Maybe Mask -> Maybe (RLimits DIM3) -> SampleAxis -> Space DIM2 -> DataFrameQCustom -> IO (DataFrameSpace DIM2)
spaceAngles det pixels rs mmask' mlimits sAxis space@(Space fSpace) (DataFrameQCustom att g img _) =
  withNPixels det $ \nPixels ->
  withForeignPtr g $ \geometry ->
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

processAnglesP :: (MonadIO m, MonadLogger m, MonadReader (Config 'AnglesProjection) m, MonadThrow m)
               => m ()
processAnglesP = do
  conf :: Config 'AnglesProjection <- ask

  -- directly from the common config
  let common = binocularsConfig'Angles'Common conf

  let overwrite = binocularsConfig'Common'Overwrite common
  let det = binocularsConfig'Common'Detector common
  let (NCores cap) =  binocularsConfig'Common'NCores common
  let destination = binocularsConfig'Common'Destination common
  let centralPixel' = binocularsConfig'Common'Centralpixel common
  let (Meter sampleDetectorDistance) = binocularsConfig'Common'Sdd common
  let (Degree detrot) = binocularsConfig'Common'Detrot common
  let mImageSumMax = binocularsConfig'Common'ImageSumMax common
  let inputRange = binocularsConfig'Common'InputRange common
  let nexusDir = binocularsConfig'Common'Nexusdir common
  let tmpl = binocularsConfig'Common'Tmpl common
  let maskMatrix = binocularsConfig'Common'Maskmatrix common

  -- directly from the specific config
  let mlimits = binocularsConfig'Angles'ProjectionLimits conf
  let res = binocularsConfig'Angles'ProjectionResolution conf
  let datapaths = binocularsConfig'Angles'DataPath conf
  let projectionType = binocularsConfig'Angles'ProjectionType conf
  let sampleAxis = binocularsConfig'Angles'SampleAxis conf

  -- built from the config
  output' <- liftIO $ destination' projectionType Nothing inputRange mlimits destination overwrite
  filenames <- InputFn'List <$> files nexusDir (Just inputRange) tmpl
  mask' <- getMask maskMatrix det
  pixels <- liftIO $ getPixelsCoordinates det centralPixel' sampleDetectorDistance detrot Normalisation
  let fns = concatMap (replicate 1) (toList filenames)
  chunks <- liftIO $ runSafeT $ toListM $ each fns >-> chunkP datapaths
  let ntot = sum (Prelude.map clength chunks)
  let jobs = chunk (quot ntot cap) chunks

  -- log parameters

  logDebugNSH filenames
  logDebugNSH datapaths
  logDebugNSH chunks
  logDebugN "start gessing final cube size"

  -- guess the final cube dimensions (To optimize, do not create the cube, just extract the shape)

  guessed <- liftIO $ withCubeAccumulator EmptyCube $ \c ->
    runSafeT $ runEffect $
    each chunks
    >-> Pipes.Prelude.map (\(Chunk fn f t) -> (fn, [f, quot (f + t) 4, quot (f + t) 4 * 2, quot (f + t) 4 * 3, t]))
    >-> framesQCustomP datapaths
    >-> project det 3 (spaceAngles det pixels res mask' mlimits sampleAxis)
    >-> accumulateP c

  logDebugN "stop gessing final cube size"

  -- do the final projection

  logInfoN (pack $ printf "let's do a Angles projection of %d %s image(s) on %d core(s)" ntot (show det) cap)

  liftIO $ withProgressBar ntot $ \pb -> do
    r' <- mapConcurrently (\job -> withCubeAccumulator guessed $ \c ->
                             runSafeT $ runEffect $
                             each job
                             >-> Pipes.Prelude.map (\(Chunk fn f t) -> (fn, [f..t]))
                             >-> framesQCustomP datapaths
                             >-> Pipes.Prelude.filter (\(DataFrameQCustom _ _ img _) -> filterSumImage mImageSumMax img)
                             >-> project det 3 (spaceAngles det pixels res mask' mlimits sampleAxis)
                             >-> tee (accumulateP c)
                             >-> progress pb
                         ) jobs
    saveCube output' (unpack . serializeConfig $ conf) r'

---------
-- Cmd --
---------

processAngles :: (MonadLogger m, MonadThrow m, MonadIO m) => Maybe FilePath -> Maybe ConfigRange -> m ()
processAngles mf mr = cmd processAnglesP mf (Args'AnglesProjection mr)

newAngles :: (MonadIO m, MonadLogger m, MonadThrow m)
          => Path Abs Dir -> m ()
newAngles cwd = do
  let conf = default'BinocularsConfig'Angles
             { binocularsConfig'Angles'Common = default'BinocularsConfig'Common
                                                { binocularsConfig'Common'Nexusdir = Just cwd }
             }
  liftIO $ Data.Text.IO.putStr $ serializeConfig conf

updateAngles :: (MonadIO m, MonadLogger m, MonadThrow m)
             => Maybe FilePath -> Maybe ConfigRange -> m ()
updateAngles mf mr = cmd (pure ()) mf (Args'AnglesProjection mr)

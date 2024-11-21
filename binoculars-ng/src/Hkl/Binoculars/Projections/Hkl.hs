{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-
    Copyright  : Copyright (C) 2014-2024 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.Binoculars.Projections.Hkl
    ( Config(..)
    , DataFrameHkl'(..)
    , newHkl
    , processHkl
    , updateHkl
    ) where


import           Control.Concurrent.Async           (mapConcurrently)
import           Control.Monad                      (forM_, forever)
import           Control.Monad.Catch                (MonadThrow)
import           Control.Monad.IO.Class             (MonadIO (liftIO))
import           Control.Monad.Logger               (MonadLogger, logDebugN,
                                                     logInfoN)
import           Control.Monad.Reader               (MonadReader, ask)
import           Data.Aeson                         (FromJSON, ToJSON,
                                                     eitherDecode', encode)
import           Data.ByteString.Lazy               (fromStrict, toStrict)
import           Data.Functor.Identity              (Identity)
import           Data.HashMap.Strict                (fromList)
import           Data.Ini                           (Ini (..))
import           Data.Ini.Config.Bidir              (FieldValue (..))
import           Data.Text                          (pack, unpack)
import           Data.Text.Encoding                 (decodeUtf8, encodeUtf8)
import           Data.Text.IO                       (putStr)
import           Data.Vector.Storable.Mutable       (unsafeWith)
import           Foreign.C.Types                    (CDouble (..))
import           Foreign.ForeignPtr                 (withForeignPtr)
import           GHC.Generics                       (Generic)
import           Path                               (Abs, Dir, Path)
import           Pipes                              (await, each, runEffect,
                                                     (>->))
import           Pipes.Prelude                      (filter, map, tee, toListM)
import           Pipes.Safe                         (runSafeP, runSafeT)
import           Text.Printf                        (printf)

import           Hkl.Binoculars.Common
import           Hkl.Binoculars.Config
import           Hkl.Binoculars.Pipes
import           Hkl.Binoculars.Projections
import           Hkl.Binoculars.Projections.Config
import           Hkl.Binoculars.Projections.QCustom
import           Hkl.C.Binoculars
import           Hkl.DataSource
import           Hkl.Detector
import           Hkl.Geometry
import           Hkl.HKD
import           Hkl.Image
import           Hkl.Orphan                         ()
import           Hkl.Repa
import           Hkl.Sample
import           Hkl.Utils

----------------
-- DataPath's --
----------------

data DataFrameHkl' f
  = DataFrameHkl
    { dataFrameHkl'DataFrameQCustom :: HKD f DataFrameQCustom
    , dataFrameHkl'Sample           :: HKD f Sample
    }
  deriving (Generic)

deriving instance Show (DataFrameHkl' DataSourcePath)
instance FromJSON (DataFrameHkl' DataSourcePath)
instance ToJSON (DataFrameHkl' DataSourcePath)

defaultDataSourcePath'DataFrameHkl :: DataFrameHkl' DataSourcePath
defaultDataSourcePath'DataFrameHkl = DataFrameHkl
                                     default'DataSourcePath'DataFrameQCustom
                                     default'DataSourcePath'Sample

instance HasFieldComment (DataFrameHkl' DataSourcePath) where
  fieldComment _ = [ "`datapath` internal value used to find the data in the data file."
                   , ""
                   , "This value is for expert only."
                   , ""
                   , "default value: <not set>"
                   ]

instance HasFieldValue (DataFrameHkl' DataSourcePath) where
  fieldvalue = FieldValue
               { fvParse = eitherDecode' . fromStrict . encodeUtf8
               , fvEmit = decodeUtf8 . toStrict . encode
               }

------------
-- Config --
------------

overload'DataSourcePath'DataFrameHkl :: Config Common
                                     -> Config Sample
                                     -> DataFrameHkl' DataSourcePath
                                     -> DataFrameHkl' DataSourcePath
overload'DataSourcePath'DataFrameHkl common sample (DataFrameHkl qCustomPath samplePath)
  = DataFrameHkl newQCustomPath newSamplePath
  where
    newQCustomPath = overload'DataSourcePath'DataFrameQCustom common Nothing qCustomPath
    newSamplePath = overload'DataSourcePath'Sample sample samplePath

instance HasIniConfig 'HklProjection where
  data Config 'HklProjection
    = BinocularsConfig'Hkl
      { binocularsConfig'Hkl'Common                 :: Config Common
      , binocularsConfig'Hkl'Sample                 :: Config Sample
      , binocularsConfig'Hkl'ProjectionType         :: ProjectionType
      , binocularsConfig'Hkl'ProjectionResolution   :: Resolutions DIM3
      , binocularsConfig'Hkl'ProjectionLimits       :: Maybe (RLimits DIM3)
      , binocularsConfig'Hkl'DataPath               :: DataFrameHkl' DataSourcePath
      } deriving (Generic)

  newtype Args 'HklProjection
      = Args'HklProjection (Maybe ConfigRange)

  defaultConfig
      = BinocularsConfig'Hkl
        { binocularsConfig'Hkl'Common = defaultConfig
        , binocularsConfig'Hkl'Sample = defaultConfig
        , binocularsConfig'Hkl'ProjectionType = HklProjection
        , binocularsConfig'Hkl'ProjectionResolution = Resolutions3 0.01 0.01 0.01
        , binocularsConfig'Hkl'ProjectionLimits  = Nothing
        , binocularsConfig'Hkl'DataPath = defaultDataSourcePath'DataFrameHkl
        }

  getConfig content@(ConfigContent cfg) (Args'HklProjection mr) capabilities
      = do binocularsConfig'Hkl'Common <- getConfig content (Args'Common mr) capabilities
           binocularsConfig'Hkl'Sample <- getConfig content Args'Sample capabilities
           binocularsConfig'Hkl'ProjectionType <- parseFDef cfg "projection" "type" (binocularsConfig'Hkl'ProjectionType defaultConfig)
           binocularsConfig'Hkl'ProjectionResolution <- parseFDef cfg "projection" "resolution" (binocularsConfig'Hkl'ProjectionResolution defaultConfig)
           binocularsConfig'Hkl'ProjectionLimits <- parseMb cfg "projection" "limits"
           binocularsConfig'Hkl'DataPath <- pure $ eitherF (const $ guess'DataSourcePath'DataFrameHkl binocularsConfig'Hkl'Common binocularsConfig'Hkl'Sample content) (parse' cfg "input" "datapath")
                                           (\md -> case md of
                                                    Nothing -> guess'DataSourcePath'DataFrameHkl binocularsConfig'Hkl'Common binocularsConfig'Hkl'Sample content
                                                    Just d  ->  overload'DataSourcePath'DataFrameHkl binocularsConfig'Hkl'Common binocularsConfig'Hkl'Sample d)
           pure BinocularsConfig'Hkl{..}

  toIni c = toIni (binocularsConfig'Hkl'Common c)
            `mergeIni`
            toIni (binocularsConfig'Hkl'Sample c)
            `mergeIni`
            Ini { iniSections = fromList [ ("input", elemFDef' "datapath" binocularsConfig'Hkl'DataPath c defaultConfig)
                                         , ("projection", elemFDef' "type" binocularsConfig'Hkl'ProjectionType c defaultConfig
                                                          <> elemFDef' "resolution" binocularsConfig'Hkl'ProjectionResolution c defaultConfig
                                                          <> elemFMbDef' "limits" binocularsConfig'Hkl'ProjectionLimits c defaultConfig
                                           )
                                         ]
                , iniGlobals = []
                }


----------------
-- Projection --
----------------

{-# INLINE spaceHkl #-}
spaceHkl :: Detector b DIM2 -> Array F DIM3 Double -> Resolutions DIM3 -> Maybe (RLimits DIM3) -> Bool -> Space DIM3 -> DataFrameHkl' Identity -> IO (DataFrameSpace DIM3)
spaceHkl det pixels rs mlimits doPolarizationCorrection space@(Space fSpace) (DataFrameHkl (DataFrameQCustom att g img mmask _ _ _) samplePath) = do
  withNPixels det $ \nPixels ->
    withGeometry g $ \geometry ->
    withSample samplePath $ \sample ->
    withForeignPtr (toForeignPtr pixels) $ \pix ->
    withResolutions rs $ \nr r ->
    withMaybeMask mmask $ \ mask'' ->
    withPixelsDims pixels $ \ndim dims ->
    withMaybeLimits mlimits rs $ \nlimits limits ->
    withForeignPtr fSpace $ \pSpace -> do
    case img of
      (ImageInt32 arr) -> unsafeWith arr $ \i -> do
        {-# SCC "hkl_binoculars_space_hkl_int32_t" #-} c'hkl_binoculars_space_hkl_int32_t pSpace geometry sample i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits) (toEnum . fromEnum $ doPolarizationCorrection)
      (ImageWord16 arr) -> unsafeWith arr $ \i -> do
        {-# SCC "hkl_binoculars_space_hkl_uint16_t" #-} c'hkl_binoculars_space_hkl_uint16_t pSpace geometry sample i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits) (toEnum . fromEnum $ doPolarizationCorrection)
      (ImageWord32 arr) -> unsafeWith arr $ \i -> do
        {-# SCC "hkl_binoculars_space_hkl_uint32_t" #-} c'hkl_binoculars_space_hkl_uint32_t pSpace geometry sample i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits) (toEnum . fromEnum $ doPolarizationCorrection)
    return (DataFrameSpace img space att)

----------
-- Pipe --
----------

processHklP :: (MonadIO m, MonadLogger m, MonadReader (Config 'HklProjection) m, MonadThrow m)
            => m ()
processHklP = do
  conf :: Config 'HklProjection <- ask

  -- directly from the common config
  let common = binocularsConfig'Hkl'Common conf

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
  let mSkipFirstPoints = binocularsConfig'Common'SkipFirstPoints common
  let mSkipLastPoints = binocularsConfig'Common'SkipLastPoints common
  let doPolarizationCorrection = binocularsConfig'Common'PolarizationCorrection common

  -- directly from the specific config
  let mlimits = binocularsConfig'Hkl'ProjectionLimits conf
  let res = binocularsConfig'Hkl'ProjectionResolution conf
  let datapaths = binocularsConfig'Hkl'DataPath conf
  let projectionType = binocularsConfig'Hkl'ProjectionType conf


  -- built from the config
  output' <- liftIO $ destination' projectionType Nothing inputRange mlimits destination overwrite
  filenames <- InputFn'List <$> files nexusDir inputRange tmpl
  pixels <- liftIO $ getPixelsCoordinates det centralPixel' sampleDetectorDistance detrot NoNormalisation

  let fns = concatMap (replicate 1) (toList filenames)
  chunks <- liftIO $ runSafeT $ toListM $ each fns >-> chunkP mSkipFirstPoints mSkipLastPoints datapaths
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
    >-> framesP datapaths
    >-> project det 3 (spaceHkl det pixels res mlimits doPolarizationCorrection)
    >-> accumulateP c

  logDebugN "stop gessing final cube size"

  logInfoN (pack $ printf "let's do an Hkl projection of %d %s image(s) on %d core(s)" ntot (show det) cap)

  liftIO $ withProgressBar ntot $ \pb -> do
    r' <- mapConcurrently (\job -> withCubeAccumulator guessed $ \c ->
                             runEffect $ runSafeP $
                             each job
                             >-> Pipes.Prelude.map (\(Chunk fn f t) -> (fn, [f..t]))
                             -- >-> tee Pipes.Prelude.print
                             >-> framesP datapaths
                             >-> Pipes.Prelude.filter (\(DataFrameHkl (DataFrameQCustom _ _ img _ _ _ _) _) -> filterSumImage mImageSumMax img)
                             >-> project det 3 (spaceHkl det pixels res mlimits doPolarizationCorrection)
                             >-> tee (accumulateP c)
                             >-> progress pb
                         ) jobs
    saveCube output' (unpack . serializeConfig $ conf) r'

-- FramesHklP

instance ChunkP (DataFrameHkl' DataSourcePath) where
  chunkP sf sl (DataFrameHkl p _) = chunkP sf sl p

instance FramesP (DataFrameHkl' DataSourcePath) (DataFrameHkl' Identity) where
  framesP (DataFrameHkl qcustom sample) = skipMalformed $ forever $ do
    (fp, js) <- await
    withScanFileP fp $ \f ->
      withDataSourceP f qcustom $ \qcustomAcq ->
      withDataSourceP f sample $ \sampleAcq ->
      forM_ js (\j -> tryYield ( DataFrameHkl
                                <$> extract1DStreamValue qcustomAcq j
                                <*> extract0DStreamValue sampleAcq
                              ))

------------
-- Inputs --
------------

guess'DataSourcePath'DataFrameHkl :: Config Common
                                  -> Config Sample
                                  -> ConfigContent
                                  -> DataFrameHkl' DataSourcePath
guess'DataSourcePath'DataFrameHkl common sample content
  = DataFrameHkl
    (guess'DataSourcePath'DataFrameQCustom common Nothing content)
    (guess'DataSourcePath'Sample common sample)

---------
-- Cmd --
---------

processHkl :: (MonadLogger m, MonadThrow m, MonadIO m) => Maybe FilePath -> Maybe ConfigRange -> m ()
processHkl mf mr = cmd processHklP mf (Args'HklProjection mr)

newHkl :: (MonadIO m, MonadLogger m, MonadThrow m)
       => Path Abs Dir -> m ()
newHkl cwd = do
  let conf = defaultConfig
             { binocularsConfig'Hkl'Common = defaultConfig
                                             { binocularsConfig'Common'Nexusdir = Just cwd }
             }
  liftIO $ Data.Text.IO.putStr $ serializeConfig conf


updateHkl :: (MonadIO m, MonadLogger m, MonadThrow m)
          => Maybe FilePath -> Maybe ConfigRange -> m ()
updateHkl mf mr = cmd (pure ()) mf (Args'HklProjection mr)

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

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

module Hkl.Binoculars.Projections.Hkl
    ( Config(..)
    , DataFrameHkl(..)
    , defaultDataSourcePath'DataFrameHkl
    , newHkl
    , processHkl
    , spaceHkl
    , updateHkl
    ) where


import           Control.Concurrent.Async           (mapConcurrently)
import           Control.Monad.Catch                (Exception, MonadThrow)
import           Control.Monad.IO.Class             (MonadIO (liftIO))
import           Control.Monad.Logger               (MonadLogger, logDebugN,
                                                     logInfoN)
import           Control.Monad.Reader               (MonadReader, ask, forM_,
                                                     forever)
import           Control.Monad.Trans.Reader         (runReaderT)
import           Data.Aeson                         (FromJSON, ToJSON,
                                                     eitherDecode', encode)
import           Data.Array.Repa                    (Array)
import           Data.Array.Repa.Index              (DIM2, DIM3)
import           Data.Array.Repa.Repr.ForeignPtr    (F, toForeignPtr)
import           Data.ByteString.Lazy               (fromStrict, toStrict)
import           Data.HashMap.Lazy                  (fromList)
import           Data.Ini                           (Ini (..))
import           Data.Ini.Config.Bidir              (FieldValue (..))
import           Data.Text                          (pack)
import           Data.Text.Encoding                 (decodeUtf8, encodeUtf8)
import           Data.Text.IO                       (putStr)
import           Data.Vector.Storable.Mutable       (unsafeWith)
import           Foreign.C.Types                    (CDouble (..))
import           Foreign.ForeignPtr                 (withForeignPtr)
import           GHC.Generics                       (Generic)
import           Path                               (Abs, Dir, Path)
import           Pipes                              (Pipe, await, each,
                                                     runEffect, (>->))
import           Pipes.Prelude                      (filter, map, tee, toListM)
import           Pipes.Safe                         (MonadSafe, runSafeP,
                                                     runSafeT)
import           Test.QuickCheck                    (Arbitrary (..))
import           Text.Printf                        (printf)

import           Hkl.Binoculars.Common
import           Hkl.Binoculars.Config
import           Hkl.Binoculars.Config.Common
import           Hkl.Binoculars.Config.Sample
import           Hkl.Binoculars.Pipes
import           Hkl.Binoculars.Projections
import           Hkl.Binoculars.Projections.QCustom
import           Hkl.C.Binoculars
import           Hkl.DataSource
import           Hkl.Detector
import           Hkl.H5
import           Hkl.Image
import           Hkl.Orphan                         ()
import           Hkl.Pipes
import           Hkl.Sample
import           Hkl.Utils

----------------
-- Exceptions -
----------------

data HklBinocularsProjectionsHklException
    = MissingSampleParameters (Config 'HklProjection)
    | MissingInputRange
    deriving (Show)

instance Exception HklBinocularsProjectionsHklException

----------------
-- DataPath's --
----------------

data DataFrameHkl
  = DataFrameHkl DataFrameQCustom Sample
  deriving Show


data instance DataSourcePath DataFrameHkl = DataSourcePath'DataFrameHkl
  { dataSourcePath'DataFrameQCustom :: DataSourcePath DataFrameQCustom
  , dataSourcePath'Sample :: DataSourcePath Sample
  }
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

instance Arbitrary (DataSourcePath DataFrameHkl) where
  arbitrary = DataSourcePath'DataFrameHkl <$> arbitrary  <*> arbitrary

defaultDataSourcePath'DataFrameHkl :: DataSourcePath DataFrameHkl
defaultDataSourcePath'DataFrameHkl = DataSourcePath'DataFrameHkl
  { dataSourcePath'DataFrameQCustom = default'DataSourcePath'DataFrameQCustom
  , dataSourcePath'Sample = default'DataSourcePath'Sample
  }

instance HasFieldValue (DataSourcePath DataFrameHkl) where
  fieldvalue = FieldValue
               { fvParse = eitherDecode' . fromStrict . encodeUtf8
               , fvEmit = decodeUtf8 . toStrict . encode
               }

------------
-- Config --
------------

data instance Config 'HklProjection
  = BinocularsConfig'Hkl
    { binocularsConfig'Hkl'Common                 :: BinocularsConfig'Common
    , binocularsConfig'Hkl'Sample                 :: BinocularsConfig'Sample
    , binocularsConfig'Hkl'ProjectionType         :: ProjectionType
    , binocularsConfig'Hkl'ProjectionResolution   :: Resolutions DIM3
    , binocularsConfig'Hkl'ProjectionLimits       :: Maybe (RLimits DIM3)
    , binocularsConfig'Hkl'DataPath               :: DataSourcePath DataFrameHkl
    } deriving (Eq, Show, Generic)

newtype instance Args 'HklProjection = Args'HklProjection (Maybe ConfigRange)

default'BinocularsConfig'Hkl :: Config 'HklProjection
default'BinocularsConfig'Hkl
  = BinocularsConfig'Hkl
    { binocularsConfig'Hkl'Common = default'BinocularsConfig'Common
    , binocularsConfig'Hkl'Sample = default'BinocularsConfig'Sample
    , binocularsConfig'Hkl'ProjectionType = HklProjection
    , binocularsConfig'Hkl'ProjectionResolution = Resolutions3 0.01 0.01 0.01
    , binocularsConfig'Hkl'ProjectionLimits  = Nothing
    , binocularsConfig'Hkl'DataPath = defaultDataSourcePath'DataFrameHkl
    }

overload'DataSourcePath'DataFrameHkl :: BinocularsConfig'Common
                                     -> BinocularsConfig'Sample
                                     -> DataSourcePath DataFrameHkl
                                     -> DataSourcePath DataFrameHkl
overload'DataSourcePath'DataFrameHkl common sample (DataSourcePath'DataFrameHkl qCustomPath samplePath)
  = DataSourcePath'DataFrameHkl newQCustomPath newSamplePath
  where
    newQCustomPath = overload'DataSourcePath'DataFrameQCustom common Nothing qCustomPath
    newSamplePath = overload'DataSourcePath'Sample sample samplePath

instance HasIniConfig 'HklProjection where

  getConfig mf (Args'HklProjection mr) = do
    (ConfigContent cfg) <- liftIO $ readConfig mf

    ecommon <- parse'BinocularsConfig'Common cfg mr
    case ecommon of
      Left err -> error err
      Right common -> do

        -- section input
        sample <- parse'BinocularsConfig'Sample cfg
        mdatapath <- parseMb cfg "input" "datapath"

        -- section projection
        projectiontype <- parseFDef cfg "projection" "type" (binocularsConfig'Hkl'ProjectionType default'BinocularsConfig'Hkl)
        resolution <- parseFDef cfg "projection" "resolution" (binocularsConfig'Hkl'ProjectionResolution default'BinocularsConfig'Hkl)
        limits <- parseMb cfg "projection" "limits"

        -- customize a bunch of parameters

        -- compute the datatype
        datapath <- case mdatapath of
                     Nothing -> guess'DataSourcePath'DataFrameHkl common sample
                     Just d  -> pure $ overload'DataSourcePath'DataFrameHkl common sample d

        pure $ Right $ BinocularsConfig'Hkl common sample projectiontype resolution limits datapath


instance ToIni (Config 'HklProjection) where

  toIni c = Ini { iniSections = fromList [ ("dispatcher", elemF    "ncores" (binocularsConfig'Common'NCores . binocularsConfig'Hkl'Common $ c)
                                                          <> elemF "destination" (binocularsConfig'Common'Destination . binocularsConfig'Hkl'Common $ c)
                                                          <> elemF "overwrite" (binocularsConfig'Common'Overwrite . binocularsConfig'Hkl'Common $ c)
                                           )
                                         ,  ("input", elemF      "type" (binocularsConfig'Common'InputType . binocularsConfig'Hkl'Common $ c)
                                                      <> elemFMb "nexusdir" (binocularsConfig'Common'Nexusdir . binocularsConfig'Hkl'Common $ c)
                                                      <> elemFMb "inputtmpl" (binocularsConfig'Common'Tmpl . binocularsConfig'Hkl'Common $ c)
                                                      <> elemF   "inputrange" (binocularsConfig'Common'InputRange . binocularsConfig'Hkl'Common $ c)
                                                      <> elemF   "detector" (binocularsConfig'Common'Detector . binocularsConfig'Hkl'Common $ c)
                                                      <> elemF   "centralpixel" (binocularsConfig'Common'Centralpixel . binocularsConfig'Hkl'Common $ c)
                                                      <> elemF   "sdd" (binocularsConfig'Common'Sdd . binocularsConfig'Hkl'Common $ c)
                                                      <> elemF   "detrot" (binocularsConfig'Common'Detrot . binocularsConfig'Hkl'Common $ c)
                                                      <> elemFMb "attenuation_coefficient" (binocularsConfig'Common'AttenuationCoefficient . binocularsConfig'Hkl'Common $ c)
                                                      <> elemFMb "attenuation_max" (binocularsConfig'Common'AttenuationMax . binocularsConfig'Hkl'Common $ c)
                                                      <> elemFMb "maskmatrix" (binocularsConfig'Common'Maskmatrix . binocularsConfig'Hkl'Common $ c)
                                                      <> elemFMb "wavelength" (binocularsConfig'Common'Wavelength . binocularsConfig'Hkl'Common $ c)
                                                      <> elemF   "datapath" (binocularsConfig'Hkl'DataPath c)
                                                      <> elemFMb "image_sum_max" (binocularsConfig'Common'ImageSumMax . binocularsConfig'Hkl'Common $ c)
                                            )
                                         , ("projection", elemF      "type" (binocularsConfig'Hkl'ProjectionType c)
                                                          <> elemF   "resolution" (binocularsConfig'Hkl'ProjectionResolution c)
                                                          <> elemFMb "limits" (binocularsConfig'Hkl'ProjectionLimits c)
                                           )]

                , iniGlobals = []
                }


----------------
-- Projection --
----------------

{-# INLINE spaceHkl #-}
spaceHkl :: Detector b DIM2 -> Array F DIM3 Double -> Resolutions DIM3 -> Maybe Mask -> Maybe (RLimits DIM3) -> Space DIM3 -> DataFrameHkl -> IO (DataFrameSpace DIM3)
spaceHkl det pixels rs mmask' mlimits space@(Space fSpace) (DataFrameHkl (DataFrameQCustom att g img _) samplePath) = do
  withNPixels det $ \nPixels ->
    withGeometry g $ \geometry ->
    withSample samplePath $ \sample ->
    withForeignPtr (toForeignPtr pixels) $ \pix ->
    withResolutions rs $ \nr r ->
    withMaybeMask mmask' $ \ mask'' ->
    withPixelsDims pixels $ \ndim dims ->
    withMaybeLimits mlimits rs $ \nlimits limits ->
    withForeignPtr fSpace $ \pSpace -> do
    case img of
      (ImageInt32 arr) -> unsafeWith arr $ \i -> do
        {-# SCC "hkl_binoculars_space_hkl_int32_t" #-} c'hkl_binoculars_space_hkl_int32_t pSpace geometry sample i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits)
      (ImageWord16 arr) -> unsafeWith arr $ \i -> do
        {-# SCC "hkl_binoculars_space_hkl_uint16_t" #-} c'hkl_binoculars_space_hkl_uint16_t pSpace geometry sample i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits)
      (ImageWord32 arr) -> unsafeWith arr $ \i -> do
        {-# SCC "hkl_binoculars_space_hkl_uint32_t" #-} c'hkl_binoculars_space_hkl_uint32_t pSpace geometry sample i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits)
    return (DataFrameSpace img space att)

----------
-- Pipe --
----------

class ChunkP a => FramesHklP a where
  framesHklP :: MonadSafe m
             => a -> Pipe (FilePath, [Int]) DataFrameHkl m ()


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
  let maskMatrix = binocularsConfig'Common'Maskmatrix common

  -- directly from the specific config
  let mlimits = binocularsConfig'Hkl'ProjectionLimits conf
  let res = binocularsConfig'Hkl'ProjectionResolution conf
  let datapaths = binocularsConfig'Hkl'DataPath conf
  let projectionType = binocularsConfig'Hkl'ProjectionType conf


  -- built from the config
  output' <- liftIO $ destination' projectionType inputRange mlimits destination overwrite
  filenames <- InputFn'List <$> files nexusDir (Just inputRange) tmpl
  mask' <- getMask maskMatrix det
  pixels <- liftIO $ getPixelsCoordinates det centralPixel' sampleDetectorDistance detrot

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
    >-> framesHklP datapaths
    >-> project det 3 (spaceHkl det pixels res mask' mlimits)
    >-> accumulateP c

  logDebugN "stop gessing final cube size"

  logInfoN (pack $ printf "let's do an Hkl projection of %d %s image(s) on %d core(s)" ntot (show det) cap)

  liftIO $ withProgressBar ntot $ \pb -> do
    r' <- mapConcurrently (\job -> withCubeAccumulator guessed $ \c ->
                             runEffect $ runSafeP $
                             each job
                             >-> Pipes.Prelude.map (\(Chunk fn f t) -> (fn, [f..t]))
                             -- >-> tee Pipes.Prelude.print
                             >-> framesHklP datapaths
                             >-> Pipes.Prelude.filter (\(DataFrameHkl (DataFrameQCustom _ _ img _) _) -> filterSumImage mImageSumMax img)
                             >-> project det 3 (spaceHkl det pixels res mask' mlimits)
                             >-> tee (accumulateP c)
                             >-> progress pb
                         ) jobs
    saveCube output' r'

-- FramesHklP

instance ChunkP (DataSourcePath DataFrameHkl) where
  chunkP (DataSourcePath'DataFrameHkl p _) = chunkP p

instance FramesHklP (DataSourcePath DataFrameHkl) where
  framesHklP (DataSourcePath'DataFrameHkl qcustom sample) = skipMalformed $ forever $ do
    (fp, js) <- await
    withFileP (openFile' fp) $ \f ->
      withDataSourceP f qcustom $ \qcustomAcq ->
      withDataSourceP f sample $ \sampleAcq ->
      forM_ js (\j -> tryYield ( DataFrameHkl
                                <$> extract1DStreamValue qcustomAcq j
                                <*> extract0DStreamValue sampleAcq
                              ))

------------
-- Inputs --
------------

guess'DataSourcePath'DataFrameHkl :: (MonadLogger m, MonadThrow m)
                                  => BinocularsConfig'Common
                                  -> BinocularsConfig'Sample
                                  -> m (DataSourcePath DataFrameHkl)
guess'DataSourcePath'DataFrameHkl common sample
  = DataSourcePath'DataFrameHkl
    <$> guess'DataSourcePath'DataFrameQCustom common Nothing
    <*> guess'DataSourcePath'Sample common sample

---------
-- Cmd --
---------

processHkl :: (MonadLogger m, MonadThrow m, MonadIO m) => Maybe FilePath -> Maybe ConfigRange -> m ()
processHkl mf mr = do
  econf :: Either String (Config 'HklProjection) <- getConfig mf (Args'HklProjection mr)
  case econf of
    Right conf -> do
      logDebugN "config red from the config file"
      logDebugN $ serializeConfig conf
      runReaderT processHklP conf
    Left e      -> logErrorNSH e

newHkl :: (MonadIO m, MonadLogger m, MonadThrow m)
       => Path Abs Dir -> m ()
newHkl cwd = do
  let conf = default'BinocularsConfig'Hkl
             { binocularsConfig'Hkl'Common = default'BinocularsConfig'Common
                                             { binocularsConfig'Common'Nexusdir = Just cwd }
             }
  liftIO $ Data.Text.IO.putStr $ serializeConfig conf


updateHkl :: (MonadIO m, MonadLogger m, MonadThrow m)
          => Maybe FilePath -> m ()
updateHkl mf = do
  (conf :: Either String (Config 'HklProjection)) <- getConfig mf (Args'HklProjection Nothing)
  logDebugN "config red from the config file"
  logDebugNSH conf
  case conf of
    Left e      -> logErrorNSH e
    Right conf' -> liftIO $ Data.Text.IO.putStr $ serializeConfig conf'

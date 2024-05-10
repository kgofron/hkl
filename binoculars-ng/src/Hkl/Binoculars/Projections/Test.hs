{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
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

module Hkl.Binoculars.Projections.Test
    ( newTest
    , processTest
    , updateTest
    ) where

import           Control.Concurrent.Async           (mapConcurrently)
import           Control.Monad.Catch                (MonadThrow)
import           Control.Monad.IO.Class             (MonadIO (liftIO))
import           Control.Monad.Logger               (MonadLogger, logDebugN,
                                                     logInfoN)
import           Control.Monad.Reader               (MonadReader, ask, forM_,
                                                     forever)
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
import           Hkl.Binoculars.Config.Common
import           Hkl.Binoculars.Config.Sample
import           Hkl.Binoculars.Pipes
import           Hkl.Binoculars.Projections
import           Hkl.Binoculars.Projections.QCustom
import           Hkl.C.Binoculars
import           Hkl.DataSource
import           Hkl.Detector
import           Hkl.H5
import           Hkl.HKD
import           Hkl.Image
import           Hkl.Orphan                         ()
import           Hkl.Pipes
import           Hkl.Repa
import           Hkl.Sample
import           Hkl.Utils

----------------
-- DataPath's --
----------------

data DataFrameTest' f
  = DataFrameTest
    { dataFrameTest'DataFrameQCustom :: HKD f DataFrameQCustom
    , dataFrameTest'Sample           :: HKD f Sample
    }
  deriving (Generic)

deriving instance Show (DataFrameTest' DataSourcePath)
instance FromJSON (DataFrameTest' DataSourcePath)
instance ToJSON (DataFrameTest' DataSourcePath)

defaultDataSourcePath'DataFrameTest :: DataFrameTest' DataSourcePath
defaultDataSourcePath'DataFrameTest = DataFrameTest
                                     default'DataSourcePath'DataFrameQCustom
                                     default'DataSourcePath'Sample

instance HasFieldComment (DataFrameTest' DataSourcePath) where
  fieldComment _ = [ "`datapath` internal value used to find the data in the data file."
                   , ""
                   , "This value is for expert only."
                   , ""
                   , "default value: <not set>"
                   ]

instance HasFieldValue (DataFrameTest' DataSourcePath) where
  fieldvalue = FieldValue
               { fvParse = eitherDecode' . fromStrict . encodeUtf8
               , fvEmit = decodeUtf8 . toStrict . encode
               }

------------
-- Config --
------------

data instance Config 'TestProjection
  = BinocularsConfig'Test
    { binocularsConfig'Test'Common                 :: BinocularsConfig'Common
    , binocularsConfig'Test'Sample                 :: BinocularsConfig'Sample
    , binocularsConfig'Test'ProjectionType         :: ProjectionType
    , binocularsConfig'Test'ProjectionResolution   :: Resolutions DIM3
    , binocularsConfig'Test'ProjectionLimits       :: Maybe (RLimits DIM3)
    , binocularsConfig'Test'DataPath               :: DataFrameTest' DataSourcePath
    } deriving (Generic)

newtype instance Args 'TestProjection = Args'TestProjection (Maybe ConfigRange)

default'BinocularsConfig'Test :: Config 'TestProjection
default'BinocularsConfig'Test
  = BinocularsConfig'Test
    { binocularsConfig'Test'Common = default'BinocularsConfig'Common
    , binocularsConfig'Test'Sample = default'BinocularsConfig'Sample
    , binocularsConfig'Test'ProjectionType = TestProjection
    , binocularsConfig'Test'ProjectionResolution = Resolutions3 0.01 0.01 0.01
    , binocularsConfig'Test'ProjectionLimits  = Nothing
    , binocularsConfig'Test'DataPath = defaultDataSourcePath'DataFrameTest
    }

overload'DataSourcePath'DataFrameTest :: BinocularsConfig'Common
                                     -> BinocularsConfig'Sample
                                     -> DataFrameTest' DataSourcePath
                                     -> DataFrameTest' DataSourcePath
overload'DataSourcePath'DataFrameTest common sample (DataFrameTest qCustomPath samplePath)
  = DataFrameTest newQCustomPath newSamplePath
  where
    newQCustomPath = overload'DataSourcePath'DataFrameQCustom common Nothing qCustomPath
    newSamplePath = overload'DataSourcePath'Sample sample samplePath

instance HasIniConfig 'TestProjection where

  getConfig content@(ConfigContent cfg) (Args'TestProjection mr) capabilities = do
    common <- parse'BinocularsConfig'Common cfg mr capabilities
    sample <- parse'BinocularsConfig'Sample cfg
    BinocularsConfig'Test
      <$> pure common
      <*> pure sample
      <*> parseFDef cfg "projection" "type" (binocularsConfig'Test'ProjectionType default'BinocularsConfig'Test)
      <*> parseFDef cfg "projection" "resolution" (binocularsConfig'Test'ProjectionResolution default'BinocularsConfig'Test)
      <*> parseMb cfg "projection" "limits"
      <*> (pure $ eitherF (const $ guess'DataSourcePath'DataFrameTest common sample content) (parse' cfg "input" "datapath")
           (\md -> case md of
                    Nothing -> guess'DataSourcePath'DataFrameTest common sample content
                    Just d  ->  overload'DataSourcePath'DataFrameTest common sample d))


instance ToIni (Config 'TestProjection) where
  toIni c = toIni (binocularsConfig'Test'Common c)
            `mergeIni`
            toIni (binocularsConfig'Test'Sample c)
            `mergeIni`
            Ini { iniSections = fromList [ ("input", elemFDef' "datapath" binocularsConfig'Test'DataPath c default'BinocularsConfig'Test)
                                         , ("projection", elemFDef' "type" binocularsConfig'Test'ProjectionType c default'BinocularsConfig'Test
                                                          <> elemFDef' "resolution" binocularsConfig'Test'ProjectionResolution c default'BinocularsConfig'Test
                                                          <> elemFMbDef' "limits" binocularsConfig'Test'ProjectionLimits c default'BinocularsConfig'Test
                                           )
                                         ]
                , iniGlobals = []
                }


----------------
-- Projection --
----------------

{-# INLINE spaceTest #-}
spaceTest :: Detector b DIM2 -> Array F DIM3 Double -> Resolutions DIM3 -> Maybe Mask -> Maybe (RLimits DIM3) -> Bool -> Space DIM3 -> DataFrameTest' Identity -> IO (DataFrameSpace DIM3)
spaceTest det pixels rs mmask' mlimits doPolarizationCorrection space@(Space fSpace) (DataFrameTest (DataFrameQCustom att g img _) samplePath) = do
  withNPixels det $ \nPixels ->
    withForeignPtr g $ \geometry ->
    withSample samplePath $ \sample ->
    withForeignPtr (toForeignPtr pixels) $ \pix ->
    withResolutions rs $ \nr r ->
    withMaybeMask mmask' $ \ mask'' ->
    withPixelsDims pixels $ \ndim dims ->
    withMaybeLimits mlimits rs $ \nlimits limits ->
    withForeignPtr fSpace $ \pSpace -> do
    case img of
      (ImageInt32 arr) -> unsafeWith arr $ \i -> do
        {-# SCC "test_binoculars_space_test_int32_t" #-} c'hkl_binoculars_space_test_int32_t pSpace geometry sample i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits) (toEnum . fromEnum $ doPolarizationCorrection)
      (ImageWord16 arr) -> unsafeWith arr $ \i -> do
        {-# SCC "test_binoculars_space_test_uint16_t" #-} c'hkl_binoculars_space_test_uint16_t pSpace geometry sample i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits) (toEnum . fromEnum $ doPolarizationCorrection)
      (ImageWord32 arr) -> unsafeWith arr $ \i -> do
        {-# SCC "test_binoculars_space_test_uint32_t" #-} c'hkl_binoculars_space_test_uint32_t pSpace geometry sample i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits) (toEnum . fromEnum $ doPolarizationCorrection)
    return (DataFrameSpace img space att)

----------
-- Pipe --
----------

processTestP :: (MonadIO m, MonadLogger m, MonadReader (Config 'TestProjection) m, MonadThrow m)
            => m ()
processTestP = do
  conf :: Config 'TestProjection <- ask

  -- directly from the common config
  let common = binocularsConfig'Test'Common conf

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
  let mSkipFirstPoints = binocularsConfig'Common'SkipFirstPoints common
  let mSkipLastPoints = binocularsConfig'Common'SkipLastPoints common
  let doPolarizationCorrection = binocularsConfig'Common'PolarizationCorrection common

  -- directly from the specific config
  let mlimits = binocularsConfig'Test'ProjectionLimits conf
  let res = binocularsConfig'Test'ProjectionResolution conf
  let datapaths = binocularsConfig'Test'DataPath conf
  let projectionType = binocularsConfig'Test'ProjectionType conf


  -- built from the config
  output' <- liftIO $ destination' projectionType Nothing inputRange mlimits destination overwrite
  filenames <- InputFn'List <$> files nexusDir (Just inputRange) tmpl
  mask' <- getMask maskMatrix det
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
    >-> project det 3 (spaceTest det pixels res mask' mlimits doPolarizationCorrection)
    >-> accumulateP c

  logDebugN "stop gessing final cube size"

  logInfoN (pack $ printf "let's do a Test projection of %d %s image(s) on %d core(s)" ntot (show det) cap)

  liftIO $ withProgressBar ntot $ \pb -> do
    r' <- mapConcurrently (\job -> withCubeAccumulator guessed $ \c ->
                             runEffect $ runSafeP $
                             each job
                             >-> Pipes.Prelude.map (\(Chunk fn f t) -> (fn, [f..t]))
                             -- >-> tee Pipes.Prelude.print
                             >-> framesP datapaths
                             >-> Pipes.Prelude.filter (\(DataFrameTest (DataFrameQCustom _ _ img _) _) -> filterSumImage mImageSumMax img)
                             >-> project det 3 (spaceTest det pixels res mask' mlimits doPolarizationCorrection)
                             >-> tee (accumulateP c)
                             >-> progress pb
                         ) jobs
    saveCube output' (unpack . serializeConfig $ conf) r'

-- FramesTestP

instance ChunkP (DataFrameTest' DataSourcePath) where
  chunkP sf sl (DataFrameTest p _) = chunkP sf sl p

instance FramesP (DataFrameTest' DataSourcePath) (DataFrameTest' Identity) where
  framesP (DataFrameTest qcustom sample) = skipMalformed $ forever $ do
    (fp, js) <- await
    withFileP (openFile' fp) $ \f ->
      withDataSourceP f qcustom $ \qcustomAcq ->
      withDataSourceP f sample $ \sampleAcq ->
      forM_ js (\j -> tryYield ( DataFrameTest
                                <$> extract1DStreamValue qcustomAcq j
                                <*> extract0DStreamValue sampleAcq
                              ))

------------
-- Inputs --
------------

guess'DataSourcePath'DataFrameTest :: BinocularsConfig'Common
                                  -> BinocularsConfig'Sample
                                  -> ConfigContent
                                  -> DataFrameTest' DataSourcePath
guess'DataSourcePath'DataFrameTest common sample content
  = DataFrameTest
    (guess'DataSourcePath'DataFrameQCustom common Nothing content)
    (guess'DataSourcePath'Sample common sample)

---------
-- Cmd --
---------

processTest :: (MonadLogger m, MonadThrow m, MonadIO m) => Maybe FilePath -> Maybe ConfigRange -> m ()
processTest mf mr = cmd processTestP mf (Args'TestProjection mr)

newTest :: (MonadIO m, MonadLogger m, MonadThrow m)
       => Path Abs Dir -> m ()
newTest cwd = do
  let conf = default'BinocularsConfig'Test
             { binocularsConfig'Test'Common = default'BinocularsConfig'Common
                                             { binocularsConfig'Common'Nexusdir = Just cwd }
             }
  liftIO $ Data.Text.IO.putStr $ serializeConfig conf


updateTest :: (MonadIO m, MonadLogger m, MonadThrow m)
          => Maybe FilePath -> Maybe ConfigRange -> m ()
updateTest mf mr = cmd (pure ()) mf (Args'TestProjection mr)

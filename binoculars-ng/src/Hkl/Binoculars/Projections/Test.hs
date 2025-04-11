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
    Copyright  : Copyright (C) 2014-2025 Synchrotron SOLEIL
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
import           Control.Monad                      (forM_, forever)
import           Control.Monad.Catch                (MonadThrow)
import           Control.Monad.IO.Class             (MonadIO (liftIO))
import           Control.Monad.Logger               (MonadLogger, logDebugN,
                                                     logInfoN)
import           Control.Monad.Reader               (MonadReader, ask)
import           Data.Aeson                         (FromJSON, ToJSON)
import           Data.HashMap.Strict                (fromList)
import           Data.Ini                           (Ini (..))
import           Data.Maybe                         (fromMaybe)
import           Data.Text                          (pack, unpack)
import           Data.Text.IO                       (putStr)
import           Data.Vector.Storable.Mutable       (unsafeWith)
import           Foreign.C.Types                    (CDouble (..))
import           Foreign.ForeignPtr                 (withForeignPtr)
import           GHC.Generics                       (Generic)
import           Path                               (Abs, Dir, Path)
import           Pipes                              (await, each, runEffect,
                                                     yield, (>->))
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
import           Hkl.Image
import           Hkl.Orphan                         ()
import           Hkl.Repa
import           Hkl.Sample
import           Hkl.Types
import           Hkl.Utils

----------------
-- DataPath's --
----------------

data DataFrameTest
    = DataFrameTest
      DataFrameQCustom
      Sample

data DSDataFrameTest (k :: DSKind)
  = DSDataFrameTest
    { dataFrameTest'DataFrameQCustom :: DSWrap_ DSDataFrameQCustom k
    , dataFrameTest'Sample           :: DSWrap_ DSSample k
    }
  deriving (Generic)

deriving instance Show (DSDataFrameTest DSPath)
instance FromJSON (DSDataFrameTest DSPath)
instance ToJSON (DSDataFrameTest DSPath)
instance DataSource DSDataFrameTest

instance Is1DStreamable (DSDataFrameTest DSAcq) DataFrameTest where
    extract1DStreamValue (DSDataFrameTest q s) i =
      DataFrameTest
      <$> extract1DStreamValue q i
      <*> extract0DStreamValue s

defaultDataSource'DataFrameTest :: DSWrap_ DSDataFrameTest DSPath
defaultDataSource'DataFrameTest
    = [ DSDataFrameTest
        default'DataSource'DataFrameQCustom
        default'DataSource'Sample
      ]

instance HasFieldComment [DSDataFrameTest DSPath] where
  fieldComment _ = [ "`datapath` internal value used to find the data in the data file."
                   , ""
                   , "This value is for expert only."
                   , ""
                   , "default value: <not set>"
                   ]

instance HasFieldValue [DSDataFrameTest DSPath] where
  fieldvalue = autoJSON

------------
-- Config --
------------

overload'DataSourcePath'DataFrameTest :: Config Common
                                      -> Config Sample
                                      -> DSWrap_ DSDataFrameTest DSPath
                                      -> DSWrap_ DSDataFrameTest DSPath
overload'DataSourcePath'DataFrameTest common sample dfs
    = Prelude.map
      ( \(DSDataFrameTest qCustomPath samplePath) ->
            let newQCustomPath = overload'DataSource'DataFrameQCustom common Nothing qCustomPath
                newSamplePath = overload'DataSource'Sample sample samplePath
            in DSDataFrameTest newQCustomPath newSamplePath
      ) dfs

instance HasIniConfig 'TestProjection where
  data Config 'TestProjection
    = BinocularsConfig'Test
      { binocularsConfig'Test'Common                 :: Config Common
      , binocularsConfig'Test'Sample                 :: Config Sample
      , binocularsConfig'Test'ProjectionType         :: ProjectionType
      , binocularsConfig'Test'ProjectionResolution   :: Resolutions DIM3
      , binocularsConfig'Test'ProjectionLimits       :: Maybe (RLimits DIM3)
      , binocularsConfig'Test'DataPath               :: DSWrap_ DSDataFrameTest DSPath
      } deriving (Generic)

  newtype Args 'TestProjection = Args'TestProjection (Maybe ConfigRange)

  defaultConfig
      = BinocularsConfig'Test
        { binocularsConfig'Test'Common = defaultConfig
        , binocularsConfig'Test'Sample = defaultConfig
        , binocularsConfig'Test'ProjectionType = TestProjection
        , binocularsConfig'Test'ProjectionResolution = Resolutions3 0.01 0.01 0.01
        , binocularsConfig'Test'ProjectionLimits  = Nothing
        , binocularsConfig'Test'DataPath = defaultDataSource'DataFrameTest
        }

  getConfig content@(ConfigContent cfg) (Args'TestProjection mr) capabilities
      = do binocularsConfig'Test'Common <- getConfig content (Args'Common mr) capabilities
           binocularsConfig'Test'Sample <- getConfig content Args'Sample capabilities
           binocularsConfig'Test'ProjectionType <- parseFDef cfg "projection" "type" (binocularsConfig'Test'ProjectionType defaultConfig)
           binocularsConfig'Test'ProjectionResolution <- parseFDef cfg "projection" "resolution" (binocularsConfig'Test'ProjectionResolution defaultConfig)
           binocularsConfig'Test'ProjectionLimits <- parseMb cfg "projection" "limits"
           binocularsConfig'Test'DataPath <- (pure $ eitherF (const $ guess'DataSourcePath'DataFrameTest binocularsConfig'Test'Common binocularsConfig'Test'Sample content) (parse' cfg "input" "datapath")
                                             (\md -> case md of
                                                      Nothing -> guess'DataSourcePath'DataFrameTest binocularsConfig'Test'Common binocularsConfig'Test'Sample content
                                                      Just d  ->  overload'DataSourcePath'DataFrameTest binocularsConfig'Test'Common binocularsConfig'Test'Sample d))
           pure BinocularsConfig'Test{..}

  toIni c = toIni (binocularsConfig'Test'Common c)
            `mergeIni`
            toIni (binocularsConfig'Test'Sample c)
            `mergeIni`
            Ini { iniSections = fromList [ ("input", elemFDef' "datapath" binocularsConfig'Test'DataPath c defaultConfig)
                                         , ("projection", elemFDef' "type" binocularsConfig'Test'ProjectionType c defaultConfig
                                                          <> elemFDef' "resolution" binocularsConfig'Test'ProjectionResolution c defaultConfig
                                                          <> elemFMbDef' "limits" binocularsConfig'Test'ProjectionLimits c defaultConfig
                                           )
                                         ]
                , iniGlobals = []
                }


----------------
-- Projection --
----------------

{-# INLINE spaceTest #-}
spaceTest :: Detector b DIM2 -> Array F DIM3 Double -> Resolutions DIM3 -> Maybe (RLimits DIM3) -> Bool -> Space DIM3 -> DataFrameTest -> IO (DataFrameSpace DIM3)
spaceTest det pixels rs mlimits doPolarizationCorrection space@(Space fSpace) (DataFrameTest (DataFrameQCustom att g img mmask _ _ _) samplePath) = do
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
      (ImageDouble arr) -> unsafeWith arr $ \i -> do
        {-# SCC "test_binoculars_space_test_double" #-} c'hkl_binoculars_space_test_double pSpace geometry sample i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' limits (toEnum nlimits) (toEnum . fromEnum $ doPolarizationCorrection)
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
    >-> project det 3 (spaceTest det pixels res mlimits doPolarizationCorrection)
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
                             >-> Pipes.Prelude.filter (\(DataFrameTest (DataFrameQCustom _ _ img _ _ _ _) _) -> filterSumImage mImageSumMax img)
                             >-> project det 3 (spaceTest det pixels res mlimits doPolarizationCorrection)
                             >-> tee (accumulateP c)
                             >-> progress pb
                         ) jobs
    saveCube output' (unpack . serializeConfig $ conf) r'

-- FramesTestP

instance ChunkP [DSDataFrameTest DSPath] where
    chunkP mSkipFirst mSkipLast p =
      skipMalformed $ forever $ do
      sfp <- await
      withScanFileP sfp $ \f' ->
        withDataSourcesP f' p $ \p' -> do
          (DataSourceShape'Range (Z :. f) (Z :. t)) <- ds'Shape p'
          yield $ cclip (fromMaybe 0 mSkipFirst) (fromMaybe 0 mSkipLast) (Chunk sfp f (t - 1))

instance FramesP [DSDataFrameTest DSPath] DataFrameTest where
  framesP p = skipMalformed $ forever $ do
    (fp, js) <- await
    withScanFileP fp $ \f ->
      withDataSourcesP f p $ \p' ->
      forM_ js (tryYield . extract1DStreamValue p')

------------
-- Inputs --
------------

guess'DataSourcePath'DataFrameTest :: Config Common
                                   -> Config Sample
                                   -> ConfigContent
                                   -> DSWrap_ DSDataFrameTest DSPath
guess'DataSourcePath'DataFrameTest common sample content
  = [ DSDataFrameTest
      (guess'DataSource'DataFrameQCustom common Nothing content)
      (guess'DataSource'Sample common sample)
    ]

---------
-- Cmd --
---------

processTest :: (MonadLogger m, MonadThrow m, MonadIO m) => Maybe FilePath -> Maybe ConfigRange -> m ()
processTest mf mr = cmd processTestP mf (Args'TestProjection mr)

newTest :: (MonadIO m, MonadLogger m, MonadThrow m)
       => Path Abs Dir -> m ()
newTest cwd = do
  let conf = defaultConfig
             { binocularsConfig'Test'Common = defaultConfig
                                             { binocularsConfig'Common'Nexusdir = Just cwd }
             }
  liftIO $ Data.Text.IO.putStr $ serializeConfig conf


updateTest :: (MonadIO m, MonadLogger m, MonadThrow m)
          => Maybe FilePath -> Maybe ConfigRange -> m ()
updateTest mf mr = cmd (pure ()) mf (Args'TestProjection mr)

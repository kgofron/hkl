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


import           Control.Applicative                ((<|>))
import           Control.Concurrent.Async           (mapConcurrently)
import           Control.Lens                       (makeLenses, over)
import           Control.Monad.Catch                (Exception, MonadThrow,
                                                     throwM)
import           Control.Monad.IO.Class             (MonadIO (liftIO))
import           Control.Monad.Logger               (MonadLogger, logDebug,
                                                     logDebugSH, logErrorSH,
                                                     logInfo)
import           Control.Monad.Reader               (MonadReader, ask, forM_,
                                                     forever)
import           Control.Monad.Trans.Reader         (runReaderT)
import           Data.Aeson                         (FromJSON, ToJSON,
                                                     eitherDecode', encode)
import           Data.Array.Repa                    (Array)
import           Data.Array.Repa.Index              (DIM2, DIM3)
import           Data.Array.Repa.Repr.ForeignPtr    (F, toForeignPtr)
import           Data.ByteString.Lazy               (fromStrict, toStrict)
import           Data.Ini.Config.Bidir              (FieldValue (..), field,
                                                     ini, section, serializeIni,
                                                     (.=), (.=?))
import           Data.Maybe                         (fromMaybe)
import           Data.Text                          (pack)
import           Data.Text.Encoding                 (decodeUtf8, encodeUtf8)
import           Data.Text.IO                       (putStr)
import           Data.Vector.Storable.Mutable       (unsafeWith)
import           Foreign.C.Types                    (CDouble (..))
import           Foreign.ForeignPtr                 (withForeignPtr)
import           GHC.Conc                           (getNumCapabilities)
import           GHC.Generics                       (Generic)
import           Numeric.Units.Dimensional.Prelude  (degree, meter, (*~), (/~))
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
import           Hkl.Binoculars.Pipes
import           Hkl.Binoculars.Projections
import           Hkl.Binoculars.Projections.QCustom
import           Hkl.C.Binoculars
import           Hkl.DataSource
import           Hkl.Detector
import           Hkl.H5
import           Hkl.Image
import           Hkl.Parameter
import           Hkl.Pipes
import           Hkl.Sample
import           Hkl.Types

----------------
-- Exceptions --
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

data instance DataSourcePath Sample
    = DataSourcePath'Sample
      (DataSourcePath NanoMeter) -- a
      (DataSourcePath NanoMeter) -- b
      (DataSourcePath NanoMeter) -- c
      (DataSourcePath Degree) -- alpha
      (DataSourcePath Degree) -- beta
      (DataSourcePath Degree) -- gamma
      (DataSourcePath Degree) -- ux
      (DataSourcePath Degree) -- uy
      (DataSourcePath Degree) -- uz
    | DataSourcePath'Sample'Const
      Sample
    deriving (Eq, FromJSON, Generic, Show, ToJSON)

data instance DataSourceAcq Sample
  = DataSourceAcq'Sample
    (DataSourceAcq NanoMeter)
    (DataSourceAcq NanoMeter)
    (DataSourceAcq NanoMeter)
    (DataSourceAcq Degree)
    (DataSourceAcq Degree)
    (DataSourceAcq Degree)
    (DataSourceAcq Degree)
    (DataSourceAcq Degree)
    (DataSourceAcq Degree)
  | DataSourceAcq'Sample'Const
    Sample

instance DataSource Sample where
  withDataSourceP f (DataSourcePath'Sample a b c alpha beta gamma ux uy uz) g =
    withDataSourceP f a $ \a' ->
    withDataSourceP f b $ \b' ->
    withDataSourceP f c $ \c' ->
    withDataSourceP f alpha $ \alpha' ->
    withDataSourceP f beta $ \beta' ->
    withDataSourceP f gamma $ \gamma' ->
    withDataSourceP f ux $ \ux' ->
    withDataSourceP f uy $ \uy' ->
    withDataSourceP f uz $ \uz' -> g (DataSourceAcq'Sample a' b' c' alpha' beta' gamma' ux' uy' uz')
  withDataSourceP _ (DataSourcePath'Sample'Const s) g = g (DataSourceAcq'Sample'Const s)

instance Is0DStreamable (DataSourceAcq Sample) Sample where
  extract0DStreamValue (DataSourceAcq'Sample a b c alpha beta gamma ux uy uz) =
    Sample "test"
    <$> (Triclinic
         <$> extract0DStreamValue a
         <*> extract0DStreamValue b
         <*> extract0DStreamValue c
         <*> extract0DStreamValue alpha
         <*> extract0DStreamValue beta
         <*> extract0DStreamValue gamma)
    <*> (Parameter "ux"
         <$> extract0DStreamValue ux
         <*> pure (Range 0 0))
    <*> (Parameter "uy"
         <$> extract0DStreamValue uy
         <*> pure (Range 0 0))
    <*> (Parameter "uz"
         <$> extract0DStreamValue uz
         <*> pure (Range 0 0))
  extract0DStreamValue (DataSourceAcq'Sample'Const s) = pure s

instance Arbitrary (DataSourcePath Sample) where
  arbitrary = DataSourcePath'Sample <$> arbitrary  <*> arbitrary <*> arbitrary <*> arbitrary  <*> arbitrary <*> arbitrary <*> arbitrary  <*> arbitrary <*> arbitrary

defaultDataSourcePath'Sample :: DataSourcePath Sample
defaultDataSourcePath'Sample = DataSourcePath'Sample
  (DataSourcePath'NanoMeter(hdf5p $ grouppat 0 $ datasetp "SIXS/I14-C-CX2__EX__DIFF-UHV__#1/A"))
  (DataSourcePath'NanoMeter(hdf5p $ grouppat 0 $ datasetp "SIXS/I14-C-CX2__EX__DIFF-UHV__#1/B"))
  (DataSourcePath'NanoMeter(hdf5p $ grouppat 0 $ datasetp "SIXS/I14-C-CX2__EX__DIFF-UHV__#1/C"))
  (DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "SIXS/I14-C-CX2__EX__DIFF-UHV__#1/Alpha"))
  (DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "SIXS/I14-C-CX2__EX__DIFF-UHV__#1/Beta"))
  (DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "SIXS/I14-C-CX2__EX__DIFF-UHV__#1/Gamma"))
  (DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "SIXS/I14-C-CX2__EX__DIFF-UHV__#1/Ux"))
  (DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "SIXS/I14-C-CX2__EX__DIFF-UHV__#1/Uy"))
  (DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "SIXS/I14-C-CX2__EX__DIFF-UHV__#1/Uz"))

data instance DataSourcePath DataFrameHkl = DataSourcePath'DataFrameHkl
  { dataSourcePath'DataFrameQCustom :: DataSourcePath DataFrameQCustom
  , dataSourcePath'Sample :: DataSourcePath Sample
  }
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

instance Arbitrary (DataSourcePath DataFrameHkl) where
  arbitrary = DataSourcePath'DataFrameHkl <$> arbitrary  <*> arbitrary

defaultDataSourcePath'DataFrameHkl :: DataSourcePath DataFrameHkl
defaultDataSourcePath'DataFrameHkl = DataSourcePath'DataFrameHkl
  { dataSourcePath'DataFrameQCustom = defaultDataSourcePath'DataFrameQCustom
  , dataSourcePath'Sample = defaultDataSourcePath'Sample
  }

instance HasFieldValue (DataSourcePath DataFrameHkl) where
  fieldvalue = FieldValue
               { fvParse = eitherDecode' . fromStrict . encodeUtf8
               , fvEmit = decodeUtf8 . toStrict . encode
               }

------------
-- Config --
------------



data instance Config 'HklProjection = BinocularsConfigHkl
  { _binocularsConfigHklNcore                  :: Maybe Int
  , _binocularsConfigHklDestination            :: DestinationTmpl
  , _binocularsConfigHklOverwrite              :: Bool
  , _binocularsConfigHklInputType              :: InputType
  , _binocularsConfigHklNexusdir               :: Maybe (Path Abs Dir)
  , _binocularsConfigHklTmpl                   :: Maybe InputTmpl
  , _binocularsConfigHklInputRange             :: Maybe ConfigRange
  , _binocularsConfigHklDetector               :: Maybe (Detector Hkl DIM2)
  , _binocularsConfigHklCentralpixel           :: (Int, Int)
  , _binocularsConfigHklSdd                    :: Meter
  , _binocularsConfigHklDetrot                 :: Maybe Degree
  , _binocularsConfigHklAttenuationCoefficient :: Maybe Double
  , _binocularsConfigHklAttenuationMax         :: Maybe Float
  , _binocularsConfigHklMaskmatrix             :: Maybe MaskLocation
  , _binocularsConfigHklA                      :: Maybe Angstrom
  , _binocularsConfigHklB                      :: Maybe Angstrom
  , _binocularsConfigHklC                      :: Maybe Angstrom
  , _binocularsConfigHklAlpha                  :: Maybe Degree
  , _binocularsConfigHklBeta                   :: Maybe Degree
  , _binocularsConfigHklGamma                  :: Maybe Degree
  , _binocularsConfigHklUx                     :: Maybe Degree
  , _binocularsConfigHklUy                     :: Maybe Degree
  , _binocularsConfigHklUz                     :: Maybe Degree
  , _binocularsConfigHklWavelength             :: Maybe Angstrom
  , _binocularsConfigHklProjectionType         :: ProjectionType
  , _binocularsConfigHklProjectionResolution   :: Resolutions DIM3
  , _binocularsConfigHklProjectionLimits       :: Maybe (RLimits DIM3)
  , _binocularsConfigHklDataPath               :: Maybe (DataSourcePath DataFrameHkl)
  , _binocularsConfigHklImageSumMax            :: Maybe Double
  } deriving (Eq, Show)

makeLenses 'BinocularsConfigHkl

instance HasIniConfig 'HklProjection where
  defaultConfig = BinocularsConfigHkl
    { _binocularsConfigHklNcore = Nothing
    , _binocularsConfigHklDestination = DestinationTmpl "."
    , _binocularsConfigHklOverwrite = False
    , _binocularsConfigHklInputType = SixsFlyScanUhv
    , _binocularsConfigHklNexusdir = Nothing
    , _binocularsConfigHklTmpl = Nothing
    , _binocularsConfigHklInputRange  = Nothing
    , _binocularsConfigHklDetector = Nothing
    , _binocularsConfigHklCentralpixel = (0, 0)
    , _binocularsConfigHklSdd = Meter (1 *~ meter)
    , _binocularsConfigHklDetrot = Nothing
    , _binocularsConfigHklAttenuationCoefficient = Nothing
    , _binocularsConfigHklAttenuationMax = Nothing
    , _binocularsConfigHklMaskmatrix = Nothing
    , _binocularsConfigHklA  = Nothing
    , _binocularsConfigHklB = Nothing
    , _binocularsConfigHklC = Nothing
    , _binocularsConfigHklAlpha  = Nothing
    , _binocularsConfigHklBeta = Nothing
    , _binocularsConfigHklGamma  = Nothing
    , _binocularsConfigHklUx = Nothing
    , _binocularsConfigHklUy = Nothing
    , _binocularsConfigHklUz = Nothing
    , _binocularsConfigHklWavelength = Nothing
    , _binocularsConfigHklProjectionType = HklProjection
    , _binocularsConfigHklProjectionResolution = Resolutions3 0.01 0.01 0.01
    , _binocularsConfigHklProjectionLimits  = Nothing
    , _binocularsConfigHklDataPath = Just defaultDataSourcePath'DataFrameHkl
    , _binocularsConfigHklImageSumMax = Nothing
    }

  specConfig = do
    section "dispatcher" $ do
      binocularsConfigHklNcore .=? field "ncores" auto
      binocularsConfigHklDestination .= field "destination" auto
      binocularsConfigHklOverwrite .= field "overwrite" auto
    section "input" $ do
      binocularsConfigHklInputType .= field "type" auto
      binocularsConfigHklNexusdir .=? field "nexusdir" auto
      binocularsConfigHklTmpl .=? field "inputtmpl" auto
      binocularsConfigHklInputRange .=? field "inputrange" auto
      binocularsConfigHklDetector .=? field "detector" auto
      binocularsConfigHklCentralpixel .= field "centralpixel" auto
      binocularsConfigHklSdd .= field "sdd" auto
      binocularsConfigHklDetrot .=? field "detrot" auto
      binocularsConfigHklAttenuationCoefficient .=? field "attenuation_coefficient" auto
      binocularsConfigHklAttenuationMax .=? field "attenuation_max" auto
      binocularsConfigHklMaskmatrix .=? field "maskmatrix" auto
      binocularsConfigHklA .=? field "a" auto
      binocularsConfigHklB .=? field "b" auto
      binocularsConfigHklC .=? field "c" auto
      binocularsConfigHklAlpha .=?field "alpha" auto
      binocularsConfigHklBeta .=? field "beta" auto
      binocularsConfigHklGamma .=? field "gamma" auto
      binocularsConfigHklUx .=? field "ux" auto
      binocularsConfigHklUy .=? field "uy" auto
      binocularsConfigHklUz .=? field "uz" auto
      binocularsConfigHklWavelength .=? field "wavelength" auto
      binocularsConfigHklDataPath .=? field "datapath" auto
      binocularsConfigHklImageSumMax .=? field "image_sum_max" auto
    section "projection" $ do
      binocularsConfigHklProjectionType .= field "type" auto
      binocularsConfigHklProjectionResolution .= field "resolution" auto
      binocularsConfigHklProjectionLimits .=? field "limits" auto

  overwriteWithCmd mr conf = return $ over binocularsConfigHklInputRange (mr <|>) conf


overloadSampleWithConfig :: Config 'HklProjection -> Sample -> Sample
overloadSampleWithConfig conf (Sample
                               name
                               (Triclinic a b c alpha beta gamma)
                               ux uy uz) =
    Sample name nlat nux nuy nuz
        where
          nlat = Triclinic
                 (maybe a (NanoMeter . unAngstrom) (_binocularsConfigHklA conf))
                 (maybe b (NanoMeter . unAngstrom) (_binocularsConfigHklB conf))
                 (maybe c (NanoMeter . unAngstrom) (_binocularsConfigHklC conf))
                 (fromMaybe alpha (_binocularsConfigHklAlpha conf))
                 (fromMaybe beta (_binocularsConfigHklBeta conf))
                 (fromMaybe gamma (_binocularsConfigHklGamma conf))

          go :: Parameter -> Maybe Double -> Parameter
          go p@(Parameter _ v _) nv = p{parameterValue=fromMaybe v nv}

          nux = go ux ((/~ degree) . unDegree <$> _binocularsConfigHklUx conf)
          nuy = go uy ((/~ degree) . unDegree <$> _binocularsConfigHklUy conf)
          nuz = go uz ((/~ degree) . unDegree <$> _binocularsConfigHklUz conf)

sampleConfig ::  Config 'HklProjection -> Maybe Sample
sampleConfig cf = do
  (Angstrom a) <- _binocularsConfigHklA cf
  (Angstrom b) <- _binocularsConfigHklB cf
  (Angstrom c) <- _binocularsConfigHklC cf
  alpha <- _binocularsConfigHklAlpha cf
  beta <- _binocularsConfigHklBeta cf
  gamma <- _binocularsConfigHklGamma cf
  (Degree ux) <- _binocularsConfigHklUx cf
  (Degree uy) <- _binocularsConfigHklUy cf
  (Degree uz) <- _binocularsConfigHklUz cf
  let pux = Parameter "ux" (ux /~ degree) (Range 0  360)
  let puy = Parameter "uy" (uy /~ degree) (Range 0  360)
  let puz = Parameter "uz" (uz /~ degree) (Range 0  360)
  return $ Sample "triclinic" (Triclinic (NanoMeter a) (NanoMeter b) (NanoMeter c)
                                alpha beta gamma) pux puy puz

----------------
-- Projection --
----------------

{-# INLINE spaceHkl #-}
spaceHkl :: Config 'HklProjection -> Detector b DIM2 -> Array F DIM3 Double -> Resolutions DIM3 -> Maybe Mask -> Maybe (RLimits DIM3) -> Space DIM3 -> DataFrameHkl -> IO (DataFrameSpace DIM3)
spaceHkl config' det pixels rs mmask' mlimits space@(Space fSpace) (DataFrameHkl (DataFrameQCustom att g img _) samp) = do
  let sample' = overloadSampleWithConfig config' samp
  withNPixels det $ \nPixels ->
    withGeometry g $ \geometry ->
    withSample sample' $ \sample ->
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


class (FramesHklP a, Show a) => ProcessHklP a where
  processHklP :: (MonadIO m, MonadLogger m, MonadReader (Config 'HklProjection) m, MonadThrow m)
              => m a -> m ()
  processHklP mkPaths = do
    conf :: Config 'HklProjection <- ask
    let det = fromMaybe defaultDetector (_binocularsConfigHklDetector conf)
    let mlimits = _binocularsConfigHklProjectionLimits conf
    let destination = _binocularsConfigHklDestination conf
    let centralPixel' = _binocularsConfigHklCentralpixel conf
    let (Meter sampleDetectorDistance) = _binocularsConfigHklSdd conf
    let (Degree detrot) = fromMaybe (Degree (0 *~ degree)) ( _binocularsConfigHklDetrot conf)
    let mImageSumMax = _binocularsConfigHklImageSumMax conf
    let res = _binocularsConfigHklProjectionResolution conf

    output' <- case _binocularsConfigHklInputRange conf of
                Just r  -> liftIO $ destination' r mlimits destination (_binocularsConfigHklOverwrite conf)
                Nothing -> throwM MissingInputRange
    filenames <- InputFn'List
                <$> files (_binocularsConfigHklNexusdir conf)
                          (_binocularsConfigHklInputRange conf)
                          (_binocularsConfigHklTmpl conf)
    mask' <- getMask (_binocularsConfigHklMaskmatrix conf) det
    h5d <- mkPaths
    pixels <- liftIO $ getPixelsCoordinates det centralPixel' sampleDetectorDistance detrot

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
      >-> framesHklP h5d
      >-> project det 3 (spaceHkl conf det pixels res mask' mlimits)
      >-> accumulateP c

    $(logDebug) "stop gessing final cube size"

    $(logInfo) (pack $ printf "let's do an Hkl projection of %d %s image(s) on %d core(s)" ntot (show det) cap)

    liftIO $ withProgressBar ntot $ \pb -> do
      r' <- mapConcurrently (\job -> withCubeAccumulator guessed $ \c ->
                               runEffect $ runSafeP $
                               each job
                               >-> Pipes.Prelude.map (\(Chunk fn f t) -> (fn, [f..t]))
                               -- >-> tee Pipes.Prelude.print
                               >-> framesHklP h5d
                               >-> Pipes.Prelude.filter (\(DataFrameHkl (DataFrameQCustom _ _ img _) _) -> filterSumImage mImageSumMax img)
                               >-> project det 3 (spaceHkl conf det pixels res mask' mlimits)
                               >-> tee (accumulateP c)
                               >-> progress pb
                           ) jobs
      saveCube output' r'

instance ProcessHklP (DataSourcePath DataFrameHkl)

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

h5dpathHkl :: (MonadLogger m, MonadThrow m)
           => Config 'HklProjection
           -> m (DataSourcePath DataFrameHkl)
h5dpathHkl c =
  do let i = _binocularsConfigHklInputType c
     let ma = _binocularsConfigHklAttenuationCoefficient c
     let det = fromMaybe defaultDetector (_binocularsConfigHklDetector c)
     let mm = _binocularsConfigHklAttenuationMax c
     let mw = _binocularsConfigHklWavelength c
     let samplePath beamline device =
           DataSourcePath'Sample
           (DataSourcePath'NanoMeter(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "A"))
           (DataSourcePath'NanoMeter(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "B"))
           (DataSourcePath'NanoMeter(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "C"))
           (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "alpha"))
           (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "beta"))
           (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "gamma"))
           (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "Ux"))
           (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "Uy"))
           (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "Uz"))
     let sampleMarsPath beamline device =
           DataSourcePath'Sample
           (DataSourcePath'NanoMeter(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "a"))
           (DataSourcePath'NanoMeter(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "b"))
           (DataSourcePath'NanoMeter(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "c"))
           (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "alpha"))
           (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "beta"))
           (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "gamma"))
           (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "u_x"))
           (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "u_y"))
           (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "u_z"))
     let marsSamplePath = sampleMarsPath "MARS" "d03-1-cx2__ex__dif-cm_#1"
     let medHSamplePath = samplePath "SIXS" "i14-c-cx1-ex-cm-med.h"
     let medVSamplePath = samplePath "SIXS" "i14-c-cx1-ex-cm-med.v"
     let uhvSamplePath  = samplePath "SIXS" "I14-C-CX2__EX__DIFF-UHV__#1"
     let uhvSamplePath2 = samplePath "SIXS" "i14-c-cx2-ex-diff-uhv"
     let uhvSamplePath3 = samplePath "SIXS" "i14-c-cx2-ex-cm-uhv"
     qcustom <- h5dpathQCustom i ma mm det mw Nothing
     case i of
       CristalK6C -> do
         let ms = sampleConfig c
         case ms of
           (Just s) -> return (DataSourcePath'DataFrameHkl qcustom (DataSourcePath'Sample'Const s))
           Nothing  -> throwM (MissingSampleParameters c)
       MarsFlyscan -> return $ DataSourcePath'DataFrameHkl qcustom marsSamplePath
       MarsSbs -> return $ DataSourcePath'DataFrameHkl qcustom marsSamplePath
       SixsFlyMedH -> return $ DataSourcePath'DataFrameHkl qcustom medHSamplePath
       SixsFlyMedV -> return $ DataSourcePath'DataFrameHkl qcustom medVSamplePath
       SixsFlyMedVEiger -> return $ DataSourcePath'DataFrameHkl qcustom medVSamplePath
       SixsFlyMedVS70 -> return $ DataSourcePath'DataFrameHkl qcustom medVSamplePath
       SixsFlyScanUhv -> return $ DataSourcePath'DataFrameHkl qcustom uhvSamplePath
       SixsFlyScanUhv2 -> return $ DataSourcePath'DataFrameHkl qcustom uhvSamplePath3
       SixsFlyScanUhvTest -> return $ DataSourcePath'DataFrameHkl qcustom uhvSamplePath3
       SixsFlyScanUhvUfxc -> return $ DataSourcePath'DataFrameHkl qcustom uhvSamplePath
       SixsSbsFixedDetector -> undefined -- TODO this must not be possible.
       SixsSbsMedH -> return $ DataSourcePath'DataFrameHkl qcustom medHSamplePath
       SixsSbsMedV -> return $ DataSourcePath'DataFrameHkl qcustom medVSamplePath
       SixsSbsMedVFixDetector -> return $ DataSourcePath'DataFrameHkl qcustom medVSamplePath
       SixsSbsUhv -> return $ DataSourcePath'DataFrameHkl qcustom uhvSamplePath3

         -- SixsSbsMedV -> DataSourcePath'DataFrameHkl
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

---------
-- Cmd --
---------

process' :: (MonadLogger m, MonadThrow m, MonadIO m, MonadReader (Config 'HklProjection) m)
         => m ()
process' = do
  c <- ask
  processHklP (h5dpathHkl c)

processHkl :: (MonadLogger m, MonadThrow m, MonadIO m) => Maybe FilePath -> Maybe ConfigRange -> m ()
processHkl mf mr = do
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

newHkl :: (MonadIO m, MonadLogger m, MonadThrow m)
       => Path Abs Dir -> m ()
newHkl cwd = do
  let conf = defaultConfig {_binocularsConfigHklNexusdir = Just cwd}
  liftIO $ Data.Text.IO.putStr $ serializeIni (ini conf specConfig)


updateHkl :: (MonadIO m, MonadLogger m, MonadThrow m)
          => Maybe FilePath -> m ()
updateHkl mf = do
  (conf :: Either String (Config 'HklProjection)) <- liftIO $ getConfig mf
  $(logDebug) "config red from the config file"
  $(logDebugSH) conf
  case conf of
    Left e      -> $(logErrorSH) e
    Right conf' -> liftIO $ Data.Text.IO.putStr $ serializeIni (ini conf' specConfig)

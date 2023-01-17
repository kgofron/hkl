{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
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

module Hkl.Binoculars.Projections.QCustom
    ( Args(..)
    , Config(..)
    , DataFrameQCustom(..)
    , FramesQCustomP(..)
    , defaultDataSourcePath'DataFrameQCustom
    , h5dpathQCustom
    , newQCustom
    , processQCustom
    , updateQCustom
    ) where

import           Control.Applicative               ((<|>))
import           Control.Concurrent.Async          (mapConcurrently)
import           Control.Monad.Catch               (Exception, MonadThrow,
                                                    throwM)
import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Control.Monad.Logger              (MonadLogger, logDebug,
                                                    logDebugN, logDebugSH,
                                                    logErrorSH, logInfo,
                                                    logWarn, logWarnN)
import           Control.Monad.Reader              (MonadReader, ask, forM_,
                                                    forever)
import           Control.Monad.Trans.Reader        (runReaderT)
import           Data.Aeson                        (FromJSON, ToJSON,
                                                    eitherDecode', encode)
import           Data.Array.Repa                   (Array)
import           Data.Array.Repa.Index             (DIM2, DIM3)
import           Data.Array.Repa.Repr.ForeignPtr   (F, toForeignPtr)
import           Data.ByteString.Lazy              (fromStrict, toStrict)
import           Data.HashMap.Lazy                 (fromList)
import           Data.Ini                          (Ini (..))
import           Data.Ini.Config                   (fieldMbOf, parseIniFile,
                                                    section)
import           Data.Ini.Config.Bidir             (FieldValue (..))
import           Data.Maybe                        (fromJust, fromMaybe)
import           Data.Text                         (Text, pack)
import           Data.Text.Encoding                (decodeUtf8, encodeUtf8)
import           Data.Text.IO                      (putStr)
import           Data.Vector.Storable.Mutable      (unsafeWith)
import           Foreign.C.Types                   (CDouble (..))
import           Foreign.ForeignPtr                (withForeignPtr)
import           GHC.Conc                          (getNumCapabilities)
import           GHC.Generics                      (Generic)
import           Generic.Random                    (genericArbitraryU)
import           Numeric.Units.Dimensional.NonSI   (angstrom)
import           Numeric.Units.Dimensional.Prelude (degree, meter, (*~))
import           Path                              (Abs, Dir, Path)
import           Pipes                             (Pipe, await, each,
                                                    runEffect, yield, (>->))
import           Pipes.Prelude                     (filter, map, tee, toListM)
import           Pipes.Safe                        (MonadSafe, runSafeT)
import           Test.QuickCheck                   (Arbitrary (..))
import           Text.Printf                       (printf)

import           Hkl.Binoculars.Common
import           Hkl.Binoculars.Config
import           Hkl.Binoculars.Pipes
import           Hkl.Binoculars.Projections
import           Hkl.C.Binoculars
import           Hkl.DataSource
import           Hkl.Detector
import           Hkl.Geometry
import           Hkl.H5
import           Hkl.Image
import           Hkl.Orphan                        ()
import           Hkl.Pipes
import           Hkl.Types

-----------------------
-- QCustom Projection --
-----------------------

data DataFrameQCustom
    = DataFrameQCustom
      Attenuation -- attenuation
      Geometry -- geometry
      Image -- image
      Index -- timestamp in double
    deriving Show

data instance DataSourcePath DataFrameQCustom
  = DataSourcePath'DataFrameQCustom
    (DataSourcePath Attenuation)
    (DataSourcePath Geometry)
    (DataSourcePath Image)
    (DataSourcePath Index)
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

data instance DataSourceAcq DataFrameQCustom
  = DataSourceAcq'DataFrameQCustom
    (DataSourceAcq Attenuation)
    (DataSourceAcq Geometry)
    (DataSourceAcq Image)
    (DataSourceAcq Index)

instance Arbitrary (DataSourcePath DataFrameQCustom) where
  arbitrary = DataSourcePath'DataFrameQCustom <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance DataSource DataFrameQCustom where
  withDataSourceP f (DataSourcePath'DataFrameQCustom a g i idx) gg =
    withDataSourceP f a $ \a' ->
    withDataSourceP f g $ \g' ->
    withDataSourceP f i $ \i' ->
    withDataSourceP f idx $ \idx' -> gg (DataSourceAcq'DataFrameQCustom a' g' i' idx')

instance Is1DStreamable (DataSourceAcq DataFrameQCustom) DataFrameQCustom where
    extract1DStreamValue (DataSourceAcq'DataFrameQCustom att geom img idx) i =
      DataFrameQCustom
      <$> extract1DStreamValue att i
      <*> extract1DStreamValue geom i
      <*> extract1DStreamValue img i
      <*> extract1DStreamValue idx i


defaultDataSourcePath'DataFrameQCustom :: DataSourcePath DataFrameQCustom
defaultDataSourcePath'DataFrameQCustom
  = DataSourcePath'DataFrameQCustom
    (DataSourcePath'Attenuation
      (DataSourcePath'Float (hdf5p $ grouppat 0 $ datasetp "scan_data/attenuation"))
      2 0 Nothing)
    (DataSourcePath'Geometry'Uhv
      (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ datasetp "SIXS/Monochromator/wavelength"))
      [ DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/UHV_MU")
      , DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/UHV_OMEGA")
      , DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/UHV_DELTA")
      , DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/UHV_GAMMA")
      ])
    (DataSourcePath'Image
      (hdf5p $ grouppat 0 $ datasetp "scan_data/xpad_image")
      defaultDetector)
    (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/epoch"))

instance HasFieldValue (DataSourcePath DataFrameQCustom) where
  fieldvalue = FieldValue
               { fvParse = eitherDecode' . fromStrict . encodeUtf8
               , fvEmit = decodeUtf8 . toStrict . encode
               }

------------
-- Config --
------------

data instance Config 'QCustomProjection
  = BinocularsConfigQCustom
    { _binocularsConfigQCustomNCores                 :: NCores
    , _binocularsConfigQCustomDestination            :: DestinationTmpl
    , _binocularsConfigQCustomOverwrite              :: Bool
    , _binocularsConfigQCustomInputType              :: InputType
    , _binocularsConfigQCustomNexusdir               :: Maybe (Path Abs Dir)
    , _binocularsConfigQCustomTmpl                   :: Maybe InputTmpl
    , _binocularsConfigQCustomInputRange             :: Maybe ConfigRange
    , _binocularsConfigQCustomDetector               :: Maybe (Detector Hkl DIM2)
    , _binocularsConfigQCustomCentralpixel           :: (Int, Int)
    , _binocularsConfigQCustomSdd                    :: Meter
    , _binocularsConfigQCustomDetrot                 :: Maybe Degree
    , _binocularsConfigQCustomAttenuationCoefficient :: Maybe Double
    , _binocularsConfigQCustomAttenuationMax         :: Maybe Float
    , _binocularsConfigQCustomSurfaceOrientation     :: Maybe SurfaceOrientation
    , _binocularsConfigQCustomMaskmatrix             :: Maybe MaskLocation
    , _binocularsConfigQCustomWavelength             :: Maybe Angstrom
    , _binocularsConfigQCustomProjectionType         :: ProjectionType
    , _binocularsConfigQCustomProjectionResolution   :: Resolutions DIM3
    , _binocularsConfigQCustomProjectionLimits       :: Maybe (RLimits DIM3)
    , _binocularsConfigQCustomDataPath               :: Maybe (DataSourcePath DataFrameQCustom)
    , _binocularsConfigQCustomImageSumMax            :: Maybe Double
    , _binocularsConfigQCustomSubProjection          :: Maybe QCustomSubProjection
    } deriving (Eq, Show, Generic)

instance Arbitrary (Config 'QCustomProjection) where
  arbitrary = genericArbitraryU

newtype instance Args 'QCustomProjection = Args'QCustomProjection
  {
    argsQCustomInputRange :: Maybe ConfigRange
  }

defaultConfig' :: Config 'QCustomProjection
defaultConfig'
  = BinocularsConfigQCustom
    { _binocularsConfigQCustomNCores = NCores 4
    , _binocularsConfigQCustomDestination = DestinationTmpl "."
    , _binocularsConfigQCustomOverwrite = False
    , _binocularsConfigQCustomInputType = SixsFlyScanUhv
    , _binocularsConfigQCustomNexusdir = Nothing
    , _binocularsConfigQCustomTmpl = Nothing
    , _binocularsConfigQCustomInputRange  = Nothing
    , _binocularsConfigQCustomDetector = Just defaultDetector
    , _binocularsConfigQCustomCentralpixel = (0, 0)
    , _binocularsConfigQCustomSdd = Meter (1 *~ meter)
    , _binocularsConfigQCustomDetrot = Just (Degree (0 *~ degree))
    , _binocularsConfigQCustomAttenuationCoefficient = Nothing
    , _binocularsConfigQCustomAttenuationMax = Nothing
    , _binocularsConfigQCustomSurfaceOrientation = Just SurfaceOrientationVertical
    , _binocularsConfigQCustomMaskmatrix = Nothing
    , _binocularsConfigQCustomWavelength = Nothing
    , _binocularsConfigQCustomProjectionType = QCustomProjection
    , _binocularsConfigQCustomProjectionResolution = Resolutions3 0.01 0.01 0.01
    , _binocularsConfigQCustomProjectionLimits  = Nothing
    , _binocularsConfigQCustomDataPath = Just defaultDataSourcePath'DataFrameQCustom
    , _binocularsConfigQCustomImageSumMax = Nothing
    , _binocularsConfigQCustomSubProjection = Just QCustomSubProjection'QxQyQz
    }


parse' :: HasFieldValue b => Text -> Text -> Text -> Either String (Maybe b)
parse' c s f = parseIniFile c $ section s (fieldMbOf f auto')

eitherF :: (t1 -> p) -> Either t1 t2 -> (t2 -> p) -> p
eitherF fa (Left a)  _ = fa a
eitherF _ (Right b) fb = fb b

parseF :: (MonadLogger m, MonadIO m, HasFieldValue b)
       => Text -> Text -> Text -> (Maybe b -> m r) -> m r
parseF c s f io = eitherF error (parse' c s f) io

parseFDef :: (MonadLogger m, MonadIO m, HasFieldValue p)
          => Text -> Text -> Text -> p -> m p
parseFDef c s f def = parseF c s f (\mb -> pure $ maybe def id mb)

parseMb ::  (MonadLogger m, MonadIO m, HasFieldValue r)
        => Text -> Text -> Text -> m (Maybe r)
parseMb c s f = eitherF error (parse' c s f) pure

parseMbDef :: (MonadLogger m, MonadIO m, HasFieldValue r)
           => Text -> Text -> Text -> Maybe r -> m (Maybe r)
parseMbDef c s f def = eitherF error (parse' c s f) (\mb -> pure $ mb <|> def)

instance HasIniConfig' 'QCustomProjection where

  getConfig' mf (Args'QCustomProjection mr) = do
    (ConfigContent cfg) <- liftIO $ readConfig mf

    -- section dispatcher
    ncores <- eitherF error (parse' cfg "dispatcher" "ncores") $ \mb -> do
      nmax <- liftIO getNumCapabilities
      let n = case mb of
                Nothing -> nmax
                Just b  -> max nmax b
      pure $ NCores (if n >= 2 then n - 1 else n)
    destination <- parseFDef cfg "dispatcher" "destination" (DestinationTmpl ".")
    overwrite <- parseFDef cfg "dispatcher" "overwrite" False

    -- section input
    inputtype <- parseFDef cfg "input" "type" SixsFlyScanUhv
    nexusdir <- parseMb cfg "input" "nexusdir"
    inputtmpl <- parseMb cfg "input" "inputtmpl"
    inputrange <- parseMbDef cfg "input" "inputrange" mr
    detector <- parseMbDef cfg "input" "detector" (Just defaultDetector)
    centralpixel <- parseFDef cfg "input" "centralpixel" (0, 0)
    sdd <- parseFDef cfg "input" "sdd" (Meter (1 *~ meter))
    detrot <- parseMbDef cfg "input" "detrot" (Just (Degree (0 *~ degree)))
    attenuation_coefficient <- parseMb cfg "input" "attenuation_coefficient"
    attenuation_max <- parseMb cfg "input" "attenuation_max"
    surface_orientation <- parseMbDef cfg "input" "surface_orientation" (Just SurfaceOrientationVertical)
    maskmatrix <-parseMb cfg "input" "maskmatrix"
    wavelength <- parseMb cfg "input" "wavelength"
    mdatapath <- parseMb cfg "input" "datapath"
    image_sum_max <- parseMb cfg "input" "image_sum_max"

    -- section projection
    projectiontype <- parseFDef cfg "projection" "type" QCustomProjection
    resolution <- parseFDef cfg "projection" "resolution" (Resolutions3 0.01 0.01 0.01)
    limits <- parseMb cfg "projection" "limits"
    msubprojection <- parseMbDef cfg "projection" "subprojection" (Just QCustomSubProjection'QxQyQz)
    let subprojection = case projectiontype of
                          QxQyQzProjection -> Just QCustomSubProjection'QxQyQz
                          _                -> msubprojection

    -- customize a bunch of parameters
    datapath <- case mdatapath of
                 Nothing -> do
                   p <- h5dpathQCustom inputtype attenuation_coefficient attenuation_max detector wavelength subprojection
                   pure $ Just p
                 (Just d) -> pure $ Just d

    pure $ Right $ BinocularsConfigQCustom ncores destination overwrite inputtype nexusdir inputtmpl inputrange detector centralpixel sdd detrot attenuation_coefficient attenuation_max surface_orientation maskmatrix wavelength projectiontype resolution limits datapath image_sum_max subprojection


  toIni c = Ini { iniSections = ss
                , iniGlobals = []
                }
    where
      otua :: HasFieldValue a => a -> Text
      otua = fvEmit fieldvalue

      elemF :: HasFieldValue a => Text -> a -> [(Text, Text)]
      elemF k v = [(k, otua v)]

      elemFMb :: HasFieldValue a => Text -> Maybe a -> [(Text, Text)]
      elemFMb k = maybe [("# " <> k, "")] (\v -> [(k, otua v)])

      ss = fromList [ ("dispatcher",    elemF "ncores" (_binocularsConfigQCustomNCores c)
                                     <> elemF "destination" (_binocularsConfigQCustomDestination c)
                                     <> elemF "overwrite" (_binocularsConfigQCustomOverwrite c)
                      )
                    ,  ("input",    elemF   "type" (_binocularsConfigQCustomInputType c)
                                 <> elemFMb "nexusdir" (_binocularsConfigQCustomNexusdir c)
                                 <> elemFMb "inputtmpl" (_binocularsConfigQCustomTmpl c)
                                 <> elemFMb "inputrange" (_binocularsConfigQCustomInputRange c)
                                 <> elemFMb "detector" (_binocularsConfigQCustomDetector c)
                                 <> elemF   "centralpixel" (_binocularsConfigQCustomCentralpixel c)
                                 <> elemF   "sdd" (_binocularsConfigQCustomSdd c)
                                 <> elemFMb "detrot" (_binocularsConfigQCustomDetrot c)
                                 <> elemFMb "attenuation_coefficient" (_binocularsConfigQCustomAttenuationCoefficient c)
                                 <> elemFMb "attenuation_max" (_binocularsConfigQCustomAttenuationMax c)
                                 <> elemFMb "surface_orientation" (_binocularsConfigQCustomSurfaceOrientation c)
                                 <> elemFMb "maskmatrix" (_binocularsConfigQCustomMaskmatrix c)
                                 <> elemFMb "wavelength" (_binocularsConfigQCustomWavelength c)
                                 <> elemFMb "datapath" (_binocularsConfigQCustomDataPath c)
                                 <> elemFMb "image_sum_max" (_binocularsConfigQCustomImageSumMax c)
                       )
                    , ("projection",    elemF   "type" (_binocularsConfigQCustomProjectionType c)
                                     <> elemF   "resolution" (_binocularsConfigQCustomProjectionResolution c)
                                     <> elemFMb "limits" (_binocularsConfigQCustomProjectionLimits c)
                                     <> elemFMb "subprojection" (_binocularsConfigQCustomSubProjection c)
                      )]

------------------
-- Input Path's --
------------------

data HklBinocularsProjectionsQCustomException
    = MissingAttenuationCoefficient
    | MissingInputRange
    deriving (Show)

instance Exception HklBinocularsProjectionsQCustomException


mkAttenuation :: (MonadLogger m, MonadThrow m) => Maybe Double -> DataSourcePath Attenuation -> m (DataSourcePath Attenuation)
mkAttenuation ma att =
    case ma of
      Nothing -> case att of
                  DataSourcePath'NoAttenuation     -> return DataSourcePath'NoAttenuation
                  DataSourcePath'Attenuation{} -> do
                           $(logWarn) ("The current configuration extract the attenuation from the data files." :: Text)
                           logWarnN "You forgot to provide the attenuation coefficient in the config file."
                           logWarnN ("I continue without attenuation correction" :: Text)
                           logWarnN ("Add attenuation_coefficient=<something> under the [input] section, to fix this" :: Text)
                           return DataSourcePath'NoAttenuation
                  applyed@DataSourcePath'ApplyedAttenuationFactor{} -> return applyed
      (Just coef) -> return $ case att of
                               DataSourcePath'NoAttenuation           -> DataSourcePath'NoAttenuation
                               (DataSourcePath'Attenuation p o _ m) -> DataSourcePath'Attenuation p o coef m
                               (DataSourcePath'ApplyedAttenuationFactor _) -> undefined

mkDetector'Sixs'Fly :: Detector Hkl DIM2 -> DataSourcePath Image
mkDetector'Sixs'Fly det@(Detector2D n _ _)
  | n == c'HKL_BINOCULARS_DETECTOR_IMXPAD_S140 =
      DataSourcePath'Image
      (hdf5p $ grouppat 0 (datasetp "scan_data/xpad_image"
                           `H5Or`
                           datasetp "scan_data/xpad_s140_image"))
      det
  | n == c'HKL_BINOCULARS_DETECTOR_XPAD_FLAT_CORRECTED = undefined
  | n == c'HKL_BINOCULARS_DETECTOR_IMXPAD_S70 =
      DataSourcePath'Image
      (hdf5p $ grouppat 0 $ datasetp "scan_data/xpad_s70_image")
      det
  | n == c'HKL_BINOCULARS_DETECTOR_DECTRIS_EIGER1M =
      DataSourcePath'Image
      (hdf5p $ grouppat 0 $ datasetp "scan_data/eiger_image")
      det
  | n == c'HKL_BINOCULARS_DETECTOR_UFXC =
      DataSourcePath'Image
      (hdf5p $ grouppat 0 $ datasetp "scan_data/ufxc_sixs_image")
      det
  | n == c'HKL_BINOCULARS_DETECTOR_MERLIN = undefined
  | n == c'HKL_BINOCULARS_DETECTOR_MERLIN_MEDIPIX_3RX_QUAD = undefined
  | otherwise = undefined

mkDetector'Sixs'Sbs :: Detector Hkl DIM2 -> DataSourcePath Image
mkDetector'Sixs'Sbs det@(Detector2D n _ _)
  | n == c'HKL_BINOCULARS_DETECTOR_IMXPAD_S140 =
      DataSourcePath'Image
      (hdf5p (datasetpattr ("long_name", "i14-c-c00/dt/xpad.s140/image")
              `H5Or`
              datasetpattr ("long_name", "i14-c-c00/dt/xpad.1/image")))
      det
  | n == c'HKL_BINOCULARS_DETECTOR_XPAD_FLAT_CORRECTED = undefined
  | n == c'HKL_BINOCULARS_DETECTOR_IMXPAD_S70 =
      DataSourcePath'Image
      (hdf5p $ datasetpattr ("long_name", "i14-c-c00/dt/xpad.s70/image"))
      det
  | n == c'HKL_BINOCULARS_DETECTOR_DECTRIS_EIGER1M =
      DataSourcePath'Image
      (hdf5p $ datasetpattr ("long_name", "i14-c-c00/dt/eiger.1/image"))
      det
  | n == c'HKL_BINOCULARS_DETECTOR_UFXC = undefined
  | n == c'HKL_BINOCULARS_DETECTOR_MERLIN = undefined
  | n == c'HKL_BINOCULARS_DETECTOR_MERLIN_MEDIPIX_3RX_QUAD = undefined
  | otherwise = undefined

mkTimeStamp :: Maybe QCustomSubProjection -> DataSourcePath Index -> DataSourcePath Index
mkTimeStamp msub idx =
  case msub of
    Nothing -> DataSourcePath'Index'NoIndex
    (Just sub) -> case sub of
                   QCustomSubProjection'QxQyQz -> DataSourcePath'Index'NoIndex
                   QCustomSubProjection'QTthTimestamp -> idx
                   QCustomSubProjection'QparQperTimestamp -> idx
                   QCustomSubProjection'QPhiQx -> DataSourcePath'Index'NoIndex
                   QCustomSubProjection'QPhiQy -> DataSourcePath'Index'NoIndex
                   QCustomSubProjection'QPhiQz -> DataSourcePath'Index'NoIndex
                   QCustomSubProjection'QStereo -> DataSourcePath'Index'NoIndex

mkWaveLength :: Maybe Angstrom -> DataSourcePath WaveLength -> DataSourcePath WaveLength
mkWaveLength ma wp =
    case ma of
      Nothing  -> wp
      (Just a) -> DataSourcePath'WaveLength'Const a


h5dpathQCustom :: (MonadLogger m, MonadThrow m)
              => InputType
              -> Maybe Double
              -> Maybe Float
              -> Maybe (Detector Hkl DIM2)
              -> Maybe Angstrom
              -> Maybe QCustomSubProjection
              -> m (DataSourcePath DataFrameQCustom)
h5dpathQCustom i ma mMaxAtt mdet mw msub =
    do let det = fromMaybe defaultDetector mdet

       -- attenuation
       let dataSourcePath'Attenuation'Sixs :: DataSourcePath Attenuation
           dataSourcePath'Attenuation'Sixs =
             DataSourcePath'Attenuation
             (DataSourcePath'Float (hdf5p $ grouppat 0 $ groupp "scan_data" (datasetp "attenuation"
                                                                             `H5Or`
                                                                             datasetp "attenuation_old")))
             2 0 mMaxAtt

       let dataSourcePath'Attenuation'SixsSBS :: DataSourcePath Attenuation
           dataSourcePath'Attenuation'SixsSBS =
             DataSourcePath'Attenuation
             (DataSourcePath'Float (hdf5p (datasetpattr ("long_name", "i14-c-c00/ex/roic/att")
                                           `H5Or`
                                           datasetpattr ("long_name", "i14-c-c00/ex/roic-s140/att")
                                           `H5Or`
                                           datasetpattr ("long_name", "i14-c-c00/ex/roic-s140/att_old")
                                           `H5Or`
                                           datasetpattr ("long_name", "i14-c-c00/ex/roic-s70/att")
                                           `H5Or`
                                           datasetpattr ("long_name", "i14-c-c00/ex/roic-s70/att_old"))))
             0 0 mMaxAtt

       -- wavelength
       let dataSourcePath'WaveLength'Sixs ::  DataSourcePath WaveLength
           dataSourcePath'WaveLength'Sixs
             = DataSourcePath'WaveLength (hdf5p $ grouppat 0 (datasetp "SIXS/Monochromator/wavelength"
                                                              `H5Or`
                                                              datasetp "SIXS/i14-c-c02-op-mono/lambda"))

       -- geometry
       let dataSourcePaths'Sixs'Uhv'Axes :: [DataSourcePath Degree]
           dataSourcePaths'Sixs'Uhv'Axes
             = [ DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" (datasetp "mu"
                                                                                `H5Or`
                                                                                datasetpattr ("long_name", "i14-c-cx2/ex/uhv-dif-group/mu")
                                                                                `H5Or`
                                                                                datasetp "UHV_MU"))

               , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" (datasetp "omega"
                                                                                `H5Or`
                                                                                datasetp "UHV_OMEGA"
                                                                                `H5Or`
                                                                                datasetpattr ("long_name", "i14-c-cx2/ex/uhv-dif-group/omega")))

               , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" (datasetp "delta"
                                                                                `H5Or`
                                                                                datasetp "UHV_DELTA"
                                                                                `H5Or`
                                                                                datasetpattr ("long_name", "i14-c-cx2/ex/uhv-dif-group/delta")))

               , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" (datasetp "gamma"
                                                                                `H5Or`
                                                                                datasetp "UHV_GAMMA"
                                                                                `H5Or`
                                                                                datasetpattr ("long_name", "i14-c-cx2/ex/uhv-dif-group/gamma")))
               ]

       let dataSourcePath'Geometry'Uhv'Sixs :: DataSourcePath Geometry
           dataSourcePath'Geometry'Uhv'Sixs
             = DataSourcePath'Geometry'Uhv
               (mkWaveLength mw dataSourcePath'WaveLength'Sixs)
               dataSourcePaths'Sixs'Uhv'Axes

       let dataSourcePath'Geometry'MedH'Sixs ::  DataSourcePath Geometry
           dataSourcePath'Geometry'MedH'Sixs
             = DataSourcePath'Geometry'MedH
               (mkWaveLength mw dataSourcePath'WaveLength'Sixs)
               [ DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" (datasetp "beta"
                                                                                `H5Or`
                                                                                datasetpattr ("long_name", "i14-c-cx1/ex/diff-med-tpp/pitch")))

               , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" (datasetp "scan_data/mu"
                                                                                `H5Or`
                                                                                datasetpattr ("long_name", "i14-c-cx1/ex/med-h-dif-group.1/mu")))

               , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" (datasetp "scan_data/gamma"
                                                                                `H5Or`
                                                                                datasetpattr ("long_name", "i14-c-cx1/ex/med-h-dif-group.1/gamma")))

               , DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "scan_data" (datasetp "scan_data/delta"
                                                                                `H5Or`
                                                                                datasetpattr ("long_name", "i14-c-cx1/ex/med-h-dif-group.1/delta")))
               ]

       -- timestamp
       let mkTimeStamp'Sbs :: Maybe QCustomSubProjection -> DataSourcePath Index
           mkTimeStamp'Sbs msub'
             = mkTimeStamp msub' (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/sensors_timestamps"))

       let mkTimeStamp'Fly :: Maybe QCustomSubProjection -> DataSourcePath Index
           mkTimeStamp'Fly msub'
             = mkTimeStamp msub' (DataSourcePath'Index(hdf5p $ grouppat 0 $ datasetp "scan_data/epoch"))

       case i of
         CristalK6C -> DataSourcePath'DataFrameQCustom
                      <$> mkAttenuation ma DataSourcePath'NoAttenuation
                      <*> pure (DataSourcePath'Geometry'CristalK6C
                                 (mkWaveLength mw (DataSourcePath'WaveLength (hdf5p $ grouppat 0 $ datasetp "CRISTAL/Monochromator/lambda")))
                                 (DataSourcePath'Degree (hdf5p $ grouppat 0 $ datasetp "CRISTAL/Diffractometer/i06-c-c07-ex-dif-mu/position"))
                                 (DataSourcePath'Degree (hdf5p $ grouppat 0 $ datasetp "CRISTAL/Diffractometer/i06-c-c07-ex-dif-komega/position"))
                                 (DataSourcePath'Degree (hdf5p $ grouppat 0 $ datasetp "CRISTAL/Diffractometer/i06-c-c07-ex-dif-kappa/position"))
                                 (DataSourcePath'Degree (hdf5p $ grouppat 0 $ datasetp "scan_data/actuator_1_1"))
                                 (DataSourcePath'Degree (hdf5p $ grouppat 0 $ datasetp "CRISTAL/Diffractometer/i06-c-c07-ex-dif-gamma/position"))
                                 (DataSourcePath'Degree (hdf5p $ grouppat 0 $ datasetp "CRISTAL/Diffractometer/i06-c-c07-ex-dif-delta/position")))
                      <*> pure (DataSourcePath'Image
                                (hdf5p $ grouppat 0 $ datasetp "scan_data/data_05")
                                det) -- medipix
                      <*> pure (mkTimeStamp'Sbs msub)
         MarsFlyscan -> DataSourcePath'DataFrameQCustom
                       <$> mkAttenuation ma (DataSourcePath'ApplyedAttenuationFactor
                                             (DataSourcePath'Float (hdf5p $ grouppat 0 $ datasetp "scan_data/applied_att")))
                       <*> pure (DataSourcePath'Geometry'Mars
                                  (mkWaveLength mw (DataSourcePath'WaveLength'Const (Angstrom (1.537591 *~ angstrom))))
                                  [ DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/omega")
                                  , DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/chi")
                                  , DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/phi")
                                  , DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/tth")
                                  ])
                       <*> pure (DataSourcePath'Image
                                  (hdf5p $ grouppat 0 (datasetp "scan_data/merlin_image"
                                                       `H5Or`
                                                       datasetp "scan_data/merlin_quad_image"))
                                 det)
                       <*> pure (mkTimeStamp'Fly msub)
         MarsSbs -> DataSourcePath'DataFrameQCustom
                   <$> mkAttenuation ma DataSourcePath'NoAttenuation
                   <*> pure (DataSourcePath'Geometry'Mars
                              (mkWaveLength mw (DataSourcePath'WaveLength'Const (Angstrom (1.537591 *~ angstrom))))
                              [ DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/omega")
                              , DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/chi")
                              , DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/phi")
                              , DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/tth")
                              ])
                   <*> pure (DataSourcePath'Image
                             (hdf5p $ datasetpattr ("long_name", "d03-1-c00/dt/merlin-quad/image"))
                             det)
                   <*> pure (mkTimeStamp'Sbs msub)
         SixsFlyMedH -> DataSourcePath'DataFrameQCustom
                       <$> mkAttenuation ma dataSourcePath'Attenuation'Sixs
                       <*> pure dataSourcePath'Geometry'MedH'Sixs
                       <*> pure (mkDetector'Sixs'Fly det)
                       <*> pure (mkTimeStamp'Fly msub)
         SixsFlyMedV -> DataSourcePath'DataFrameQCustom
                       <$> mkAttenuation ma dataSourcePath'Attenuation'Sixs
                       <*> pure (DataSourcePath'Geometry'MedV
                                  (mkWaveLength mw dataSourcePath'WaveLength'Sixs)
                                  (DataSourcePath'Degree'Const (Degree (0 *~ degree)))
                                -- (DataSourcePath'Degree(H5Or
                                --                         (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "beta")
                                --                         (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-ex-diff-med-tpp" $ groupp "TPP" $ groupp "Orientation" $ datasetp "pitch")))
                                  (DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/mu"))
                                  (DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/omega"))
                                  (DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/gamma"))
                                  (DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/delta"))
                                  (DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/etaa")))
                       <*> pure (mkDetector'Sixs'Fly det)
                       <*> pure (mkTimeStamp'Fly msub)
         SixsFlyMedVEiger -> DataSourcePath'DataFrameQCustom
                            <$> mkAttenuation ma dataSourcePath'Attenuation'Sixs
                            <*> pure (DataSourcePath'Geometry'MedVEiger
                                       (mkWaveLength mw dataSourcePath'WaveLength'Sixs)
                                       [ DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/beta") -- maybe nothing
                                       , DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/mu")
                                       , DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/omega")
                                       , DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/gamma")
                                       , DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/delta")
                                       , DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/etaa")
                                       ]
                                       (DataSourcePath'Degree(hdf5p (grouppat 0 $ groupp "scan_data" $ datasetp "eix")
                                                              `H5Or`
                                                              hdf5p (grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-dt-det_tx.1" $ datasetp "position_pre")))
                                       (DataSourcePath'Degree(hdf5p (grouppat 0 $ groupp "scan_data" $ datasetp "eiz")
                                                              `H5Or`
                                                              hdf5p (grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-dt-det_tz.1" $ datasetp "position_pre"))))
                            <*> pure (mkDetector'Sixs'Fly det)
                            <*> pure (mkTimeStamp'Fly msub)
         SixsFlyMedVS70 -> DataSourcePath'DataFrameQCustom
                          <$> mkAttenuation ma dataSourcePath'Attenuation'Sixs
                          <*> pure (DataSourcePath'Geometry'MedV
                                     (mkWaveLength mw dataSourcePath'WaveLength'Sixs)
                                     (DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/beta"))
                                     (DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/mu"))
                                     (DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/omega"))
                                     (DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/gamma"))
                                     (DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/delta"))
                                     (DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "scan_data/etaa")))
                          <*> pure (mkDetector'Sixs'Fly det)
                          <*> pure (mkTimeStamp'Fly msub)
         SixsFlyScanUhv -> DataSourcePath'DataFrameQCustom
                          <$> mkAttenuation ma dataSourcePath'Attenuation'Sixs
                          <*> pure dataSourcePath'Geometry'Uhv'Sixs
                          <*> pure (mkDetector'Sixs'Fly det)
                          <*> pure (mkTimeStamp'Fly msub)
         SixsFlyScanUhv2 -> DataSourcePath'DataFrameQCustom
                           <$> mkAttenuation ma dataSourcePath'Attenuation'Sixs
                           <*> pure dataSourcePath'Geometry'Uhv'Sixs
                           <*> pure (mkDetector'Sixs'Fly det)
                           <*> pure (mkTimeStamp'Fly msub)
         SixsFlyScanUhvTest -> DataSourcePath'DataFrameQCustom
                              <$> mkAttenuation ma dataSourcePath'Attenuation'Sixs
                              <*> pure (DataSourcePath'Geometry'UhvTest
                                        (mkWaveLength mw (DataSourcePath'WaveLength'Const (Angstrom (0.672494 *~ angstrom))))
                                        dataSourcePaths'Sixs'Uhv'Axes)
                              <*> pure (mkDetector'Sixs'Fly det)
                              <*> pure (mkTimeStamp'Fly msub)
         SixsFlyScanUhvUfxc -> DataSourcePath'DataFrameQCustom
                              <$> mkAttenuation ma dataSourcePath'Attenuation'Sixs
                              <*> pure dataSourcePath'Geometry'Uhv'Sixs
                              <*> pure (mkDetector'Sixs'Fly det)
                              <*> pure (mkTimeStamp'Fly msub)
         SixsSbsFixedDetector -> DataSourcePath'DataFrameQCustom
                                <$> mkAttenuation ma dataSourcePath'Attenuation'SixsSBS
                                <*> pure (DataSourcePath'Geometry'Fix
                                          (mkWaveLength mw dataSourcePath'WaveLength'Sixs))
                                <*> pure (mkDetector'Sixs'Sbs det)
                                <*> pure (mkTimeStamp'Sbs msub)
         SixsSbsMedH -> DataSourcePath'DataFrameQCustom
                       <$> mkAttenuation ma dataSourcePath'Attenuation'SixsSBS
                       <*> pure dataSourcePath'Geometry'MedH'Sixs
                       <*> pure (mkDetector'Sixs'Sbs det)
                       <*> pure (mkTimeStamp'Sbs msub)
         SixsSbsMedV -> DataSourcePath'DataFrameQCustom
                       <$> mkAttenuation ma dataSourcePath'Attenuation'SixsSBS
                       <*> pure (DataSourcePath'Geometry'MedV
                                  (mkWaveLength mw dataSourcePath'WaveLength'Sixs)
                                  (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-ex-diff-med-tpp" $ groupp "TPP" $ groupp "Orientation" $ datasetp "pitch"))
                                  (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/mu")))
                                  (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/omega")))
                                  (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/gamma")))
                                  (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/delta")))
                                  (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/etaa"))))
                       <*> pure (mkDetector'Sixs'Sbs det)
                       <*> pure (mkTimeStamp'Sbs msub)
         SixsSbsMedVFixDetector -> DataSourcePath'DataFrameQCustom
                                  <$> mkAttenuation ma dataSourcePath'Attenuation'SixsSBS
                                  <*> pure (DataSourcePath'Geometry'MedV
                                             (mkWaveLength mw dataSourcePath'WaveLength'Sixs)
                                             (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-ex-diff-med-tpp" $ groupp "TPP" $ groupp "Orientation" $ datasetp "pitch"))
                                             (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/mu")))
                                             (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/omega")))
                                             (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/gamma")))
                                             (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/delta")))
                                             (DataSourcePath'Degree(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/etaa"))))
                                  <*> pure (mkDetector'Sixs'Sbs det)
                                  <*> pure (mkTimeStamp'Sbs msub)
         SixsSbsUhv -> DataSourcePath'DataFrameQCustom
                      <$> mkAttenuation ma dataSourcePath'Attenuation'SixsSBS
                      <*> pure dataSourcePath'Geometry'Uhv'Sixs
                      <*> pure (mkDetector'Sixs'Sbs det)
                      <*> pure (mkTimeStamp'Sbs msub)

{-# INLINE spaceQCustom #-}
spaceQCustom :: Detector a DIM2 -> Array F DIM3 Double -> Resolutions DIM3 -> Maybe Mask -> SurfaceOrientation -> Maybe (RLimits DIM3) -> QCustomSubProjection -> Space DIM3 -> DataFrameQCustom -> IO (DataFrameSpace DIM3)
spaceQCustom det pixels rs mmask' surf mlimits subprojection space@(Space fSpace) (DataFrameQCustom att g img index) =
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
      {-# SCC "hkl_binoculars_space_qcustom_int32_t" #-} c'hkl_binoculars_space_qcustom_int32_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits) (CDouble . unIndex $ index) (toEnum . fromEnum $ subprojection)
    (ImageWord16 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_qcustom_uint16_t" #-} c'hkl_binoculars_space_qcustom_uint16_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits) (CDouble . unIndex $ index) (toEnum . fromEnum $ subprojection)
    (ImageWord32 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_qcustom_uint32_t" #-} c'hkl_binoculars_space_qcustom_uint32_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits) (CDouble . unIndex $ index) (toEnum . fromEnum $ subprojection)

  return (DataFrameSpace img space att)

----------
-- Pipe --
----------

class ChunkP a => FramesQCustomP a where
  framesQCustomP :: MonadSafe m
                 => a -> Pipe (FilePath, [Int]) DataFrameQCustom m ()

processQCustomP :: (MonadIO m, MonadLogger m, MonadReader (Config 'QCustomProjection) m, MonadThrow m)
                => m ()
processQCustomP = do
  (conf :: Config 'QCustomProjection) <- ask

  -- should not be Maybe
  let det = fromJust (_binocularsConfigQCustomDetector conf)
  let (Degree detrot) = fromJust ( _binocularsConfigQCustomDetrot conf)
  let surfaceOrientation = fromJust (_binocularsConfigQCustomSurfaceOrientation conf)
  let subprojection = fromJust (_binocularsConfigQCustomSubProjection conf)
  let h5d = fromJust (_binocularsConfigQCustomDataPath conf)

  -- directly from the config
  let (NCores cap) =  _binocularsConfigQCustomNCores conf
  let mlimits = _binocularsConfigQCustomProjectionLimits conf
  let destination = _binocularsConfigQCustomDestination conf
  let centralPixel' = _binocularsConfigQCustomCentralpixel conf
  let (Meter sampleDetectorDistance) = _binocularsConfigQCustomSdd conf
  let mImageSumMax = _binocularsConfigQCustomImageSumMax conf
  let res = _binocularsConfigQCustomProjectionResolution conf

  -- built from the config
  let output' = case _binocularsConfigQCustomInputRange conf of
                 Just r  -> destination' r mlimits destination
                 Nothing -> throwM MissingInputRange

  filenames <- InputFn'List
              <$> files (_binocularsConfigQCustomNexusdir conf)
                        (_binocularsConfigQCustomInputRange conf)
                        (_binocularsConfigQCustomTmpl conf)
  mask' <- getMask (_binocularsConfigQCustomMaskmatrix conf) det
  pixels <- liftIO $ getPixelsCoordinates det centralPixel' sampleDetectorDistance detrot

  -- compute the jobs

  let fns = concatMap (replicate 1) (toList filenames)
  chunks <- liftIO $ runSafeT $ toListM $ each fns >-> chunkP h5d
  let ntot = sum (Prelude.map clength chunks)
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
    >-> Pipes.Prelude.map (\(Chunk fn f t) -> (fn, [f, quot (f + t) 4, quot (f + t) 4 * 2, quot (f + t) 4 * 3, t]))
    >-> framesQCustomP h5d
    >-> project det 3 (spaceQCustom det pixels res mask' surfaceOrientation mlimits subprojection)
    >-> accumulateP c

  $(logDebug) "stop gessing final cube size"

  -- do the final projection

  $(logInfo) (pack $ printf "let's do a QCustom projection of %d %s image(s) on %d core(s)" ntot (show det) cap)

  liftIO $ withProgressBar ntot $ \pb -> do
    r' <- mapConcurrently (\job -> withCubeAccumulator guessed $ \c ->
                             runSafeT $ runEffect $
                             each job
                             >-> Pipes.Prelude.map (\(Chunk fn f t) -> (fn, [f..t]))
                             >-> framesQCustomP h5d
                             >-> Pipes.Prelude.filter (\(DataFrameQCustom _ _ img _) -> filterSumImage mImageSumMax img)
                             >-> project det 3 (spaceQCustom det pixels res mask' surfaceOrientation mlimits subprojection)
                             >-> tee (accumulateP c)
                             >-> progress pb
                         ) jobs
    saveCube output' r'


instance ChunkP (DataSourcePath DataFrameQCustom) where
    chunkP (DataSourcePath'DataFrameQCustom ma _ (DataSourcePath'Image i _) _) =
      skipMalformed $ forever $ do
      fp <- await
      withFileP (openH5 fp) $ \f ->
        withHdf5PathP f i $ \i' -> do
        (_, ss) <- liftIO $ datasetShape i'
        case head ss of
          (Just n) -> yield $ case ma of
            DataSourcePath'NoAttenuation -> Chunk fp 0 (fromIntegral n - 1)
            (DataSourcePath'Attenuation _ off _ _) -> Chunk fp 0 (fromIntegral n - 1 - off)
            (DataSourcePath'ApplyedAttenuationFactor _) -> Chunk fp 0 (fromIntegral n -1)
          Nothing  -> error "can not extract length"

instance FramesQCustomP (DataSourcePath DataFrameQCustom) where
    framesQCustomP p =
        skipMalformed $ forever $ do
          (fn, js) <- await
          withFileP (openH5 fn) $ \f ->
            withDataSourceP f p $ \ g ->
            forM_ js (tryYield . extract1DStreamValue g)

---------
-- Cmd --
---------

processQCustom :: (MonadLogger m, MonadThrow m, MonadIO m) => Maybe FilePath -> Maybe ConfigRange -> m ()
processQCustom mf mr = do
  econf :: Either String (Config 'QCustomProjection) <- getConfig' mf (Args'QCustomProjection mr)
  case econf of
    Right conf -> do
      logDebugN "config red from the config file"
      logDebugN $ serializeConfig conf
      runReaderT processQCustomP conf
    Left e      -> $(logErrorSH) e

newQCustom :: (MonadIO m, MonadLogger m, MonadThrow m)
           => Path Abs Dir -> m ()
newQCustom cwd = do
  let conf = defaultConfig' {_binocularsConfigQCustomNexusdir = Just cwd}
  liftIO $ Data.Text.IO.putStr $ serializeConfig conf

updateQCustom :: (MonadIO m, MonadLogger m, MonadThrow m)
              => Maybe FilePath -> m ()
updateQCustom mf = do
  (conf :: Either String (Config 'QCustomProjection)) <- getConfig' mf (Args'QCustomProjection Nothing)
  $(logDebug) "config red from the config file"
  $(logDebugSH) conf
  case conf of
    Left e      -> $(logErrorSH) e
    Right conf' -> liftIO $ Data.Text.IO.putStr $ serializeConfig conf'

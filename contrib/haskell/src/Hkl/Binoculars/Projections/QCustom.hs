{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -ddump-splices #-}
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
    , DataSourcePath(..)
    , FramesQCustomP(..)
    , default'DataSourcePath'DataFrameQCustom
    , guess'DataSourcePath'DataFrameQCustom
    , newQCustom
    , overload'DataSourcePath'DataFrameQCustom
    , processQCustom
    , updateQCustom
    ) where

import           Control.Applicative             ((<|>))
import           Control.Concurrent.Async        (mapConcurrently)
import           Control.Monad.Catch             (MonadThrow)
import           Control.Monad.IO.Class          (MonadIO (liftIO))
import           Control.Monad.Logger            (MonadLogger, logDebugN,
                                                  logInfoN)
import           Control.Monad.Reader            (MonadReader, ask, forM_,
                                                  forever)
import           Data.Aeson                      (FromJSON, ToJSON,
                                                  eitherDecode', encode)
import           Data.Array.Repa                 (Array)
import           Data.Array.Repa.Index           (DIM2, DIM3)
import           Data.Array.Repa.Repr.ForeignPtr (F, toForeignPtr)
import           Data.ByteString.Lazy            (fromStrict, toStrict)
import           Data.HashMap.Lazy               (fromList)
import           Data.Ini                        (Ini (..))
import           Data.Ini.Config.Bidir           (FieldValue (..))
import           Data.Maybe                      (fromJust, fromMaybe)
import           Data.Text                       (pack, unpack)
import           Data.Text.Encoding              (decodeUtf8, encodeUtf8)
import           Data.Text.IO                    (putStr)
import           Data.Vector.Storable.Mutable    (unsafeWith)
import           Foreign.C.Types                 (CDouble (..))
import           Foreign.ForeignPtr              (ForeignPtr, withForeignPtr)
import           GHC.Generics                    (Generic)
import           Generic.Random                  (genericArbitraryU)
import           Path                            (Abs, Dir, Path)
import           Pipes                           (Pipe, await, each, runEffect,
                                                  yield, (>->))
import           Pipes.Prelude                   (filter, map, tee, toListM)
import           Pipes.Safe                      (MonadSafe, runSafeT)
import           Test.QuickCheck                 (Arbitrary (..))
import           Text.Printf                     (printf)

import           Hkl.Binoculars.Common
import           Hkl.Binoculars.Config
import           Hkl.Binoculars.Config.Common
import           Hkl.Binoculars.Pipes
import           Hkl.Binoculars.Projections
import           Hkl.C.Binoculars
import           Hkl.C.Hkl
import           Hkl.DataSource
import           Hkl.Detector
import           Hkl.Geometry
import           Hkl.H5
import           Hkl.Image
import           Hkl.Orphan                      ()
import           Hkl.Pipes
import           Hkl.Types
import           Hkl.Utils

-----------------------
-- QCustom Projection --
-----------------------

data DataFrameQCustom
    = DataFrameQCustom
      Attenuation -- attenuation
      (ForeignPtr C'HklGeometry) -- geometry
      Image -- image
      Timestamp -- timestamp in double
    deriving Show

instance DataSource DataFrameQCustom where
  data DataSourcePath DataFrameQCustom
    = DataSourcePath'DataFrameQCustom
      (DataSourcePath Attenuation)
      (DataSourcePath Geometry)
      (DataSourcePath Image)
      (DataSourcePath Timestamp)
    deriving (Generic, Show, FromJSON, ToJSON)

  data DataSourceAcq DataFrameQCustom
    = DataSourceAcq'DataFrameQCustom
      (DataSourceAcq Attenuation)
      (DataSourceAcq Geometry)
      (DataSourceAcq Image)
      (DataSourceAcq Timestamp)

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

instance Arbitrary (DataSourcePath DataFrameQCustom) where
  arbitrary = DataSourcePath'DataFrameQCustom <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

default'DataSourcePath'DataFrameQCustom :: DataSourcePath DataFrameQCustom
default'DataSourcePath'DataFrameQCustom
  = DataSourcePath'DataFrameQCustom
    (DataSourcePath'Attenuation
      (DataSourcePath'Float (hdf5p $ grouppat 0 $ datasetp "scan_data/attenuation"))
      2 0 Nothing)
    (DataSourcePath'Geometry
      (Geometry'Factory Uhv)
      (DataSourcePath'Double (hdf5p $ grouppat 0 $ datasetp "SIXS/Monochromator/wavelength"))
      [ DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/UHV_MU")
      , DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/UHV_OMEGA")
      , DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/UHV_DELTA")
      , DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/UHV_GAMMA")
      ])
    (DataSourcePath'Image
      (hdf5p $ grouppat 0 $ datasetp "scan_data/xpad_image")
      defaultDetector)
    (DataSourcePath'Timestamp(hdf5p $ grouppat 0 $ datasetp "scan_data/epoch"))

instance HasFieldComment (DataSourcePath DataFrameQCustom) where
  fieldComment _ = [ "`datapath` internal value used to find the data in the data file."
                   , ""
                   , "This value is for expert only."
                   , ""
                   , "default value: <not set>"
                   ]

instance HasFieldValue (DataSourcePath DataFrameQCustom) where
  fieldvalue = FieldValue
               { fvParse = eitherDecode' . fromStrict . encodeUtf8
               , fvEmit = decodeUtf8 . toStrict . encode
               }

------------
-- Config --
------------

data instance Config 'QCustomProjection
  = BinocularsConfig'QCustom
    { binocularsConfig'QCustom'Common :: BinocularsConfig'Common
    , binocularsConfig'QCustom'HklBinocularsSurfaceOrientationEnum     :: HklBinocularsSurfaceOrientationEnum
    , binocularsConfig'QCustom'ProjectionType         :: ProjectionType
    , binocularsConfig'QCustom'ProjectionResolution   :: Resolutions DIM3
    , binocularsConfig'QCustom'ProjectionLimits       :: Maybe (RLimits DIM3)
    , binocularsConfig'QCustom'DataPath               :: DataSourcePath DataFrameQCustom
    , binocularsConfig'QCustom'SubProjection          :: Maybe HklBinocularsQCustomSubProjectionEnum
    } deriving (Show, Generic)

instance Arbitrary (Config 'QCustomProjection) where
  arbitrary = genericArbitraryU

newtype instance Args 'QCustomProjection = Args'QCustomProjection (Maybe ConfigRange)

default'BinocularsConfig'QCustom :: Config 'QCustomProjection
default'BinocularsConfig'QCustom
  = BinocularsConfig'QCustom
    { binocularsConfig'QCustom'Common = default'BinocularsConfig'Common
    , binocularsConfig'QCustom'HklBinocularsSurfaceOrientationEnum = HklBinocularsSurfaceOrientationEnum'Vertical
    , binocularsConfig'QCustom'ProjectionType = QCustomProjection
    , binocularsConfig'QCustom'ProjectionResolution = Resolutions3 0.01 0.01 0.01
    , binocularsConfig'QCustom'ProjectionLimits  = Nothing
    , binocularsConfig'QCustom'DataPath = default'DataSourcePath'DataFrameQCustom
    , binocularsConfig'QCustom'SubProjection = Just HklBinocularsQCustomSubProjectionEnum'QxQyQz
    }


instance HasIniConfig 'QCustomProjection where

  getConfig (ConfigContent cfg) (Args'QCustomProjection mr) capabilities = do
    common <- parse'BinocularsConfig'Common cfg mr capabilities
    surface_orientation <- parseFDef cfg "input" "surface_orientation" (binocularsConfig'QCustom'HklBinocularsSurfaceOrientationEnum default'BinocularsConfig'QCustom)
    let edatapath = parseMb cfg "input" "datapath"

    -- section projection
    projectiontype <- parseFDef cfg "projection" "type" (binocularsConfig'QCustom'ProjectionType default'BinocularsConfig'QCustom)
    resolution <- parseFDef cfg "projection" "resolution" (binocularsConfig'QCustom'ProjectionResolution default'BinocularsConfig'QCustom)
    limits <- parseMb cfg "projection" "limits"
    let msubprojection = parseMbDef cfg "projection" "subprojection" (binocularsConfig'QCustom'SubProjection default'BinocularsConfig'QCustom)

    -- customize a bunch of parameters

    -- fix the subprojection depending on the projection type
    let subprojection = case projectiontype of
                          QIndexProjection -> Just HklBinocularsQCustomSubProjectionEnum'QTimestamp
                          QparQperProjection -> Just HklBinocularsQCustomSubProjectionEnum'QparQper
                          QxQyQzProjection -> Just HklBinocularsQCustomSubProjectionEnum'QxQyQz
                          AnglesProjection -> msubprojection
                          Angles2Projection -> msubprojection
                          HklProjection -> msubprojection
                          QCustomProjection -> msubprojection
                          QCustom2Projection -> msubprojection
                          RealSpaceProjection -> Just HklBinocularsQCustomSubProjectionEnum'XYZ
                          PixelsProjection -> Just HklBinocularsQCustomSubProjectionEnum'YZTimestamp

        -- compute the datatype
    let datapath = case edatapath of
                 Left _ -> guess'DataSourcePath'DataFrameQCustom common subprojection
                 Right Nothing -> guess'DataSourcePath'DataFrameQCustom common subprojection
                 Right (Just d)  -> overload'DataSourcePath'DataFrameQCustom common subprojection d

    pure $ BinocularsConfig'QCustom common surface_orientation projectiontype resolution limits datapath subprojection


instance ToIni (Config 'QCustomProjection) where

  toIni c = toIni (binocularsConfig'QCustom'Common c)
            `mergeIni`
            Ini { iniSections = fromList [ ("input",    elemF' "surface_orientation" (binocularsConfig'QCustom'HklBinocularsSurfaceOrientationEnum c)
                                                     <> elemF' "datapath" (binocularsConfig'QCustom'DataPath c)
                                           )
                                         , ("projection",    elemF' "type" (binocularsConfig'QCustom'ProjectionType c)
                                                          <> elemF' "resolution" (binocularsConfig'QCustom'ProjectionResolution c)
                                                          <> elemFMb' "limits" (binocularsConfig'QCustom'ProjectionLimits c)
                                                          <> elemFMb' "subprojection" (binocularsConfig'QCustom'SubProjection c)
                                           )]

                , iniGlobals = []
                }

------------------
-- Input Path's --
------------------

mkAttenuation :: Maybe Double -> DataSourcePath Attenuation -> DataSourcePath Attenuation
mkAttenuation ma att =
    case ma of
      Nothing -> case att of
                  DataSourcePath'NoAttenuation     -> DataSourcePath'NoAttenuation
                  DataSourcePath'Attenuation{} ->  DataSourcePath'NoAttenuation
                           -- logWarnN "The current configuration extract the attenuation from the data files."
                           -- logWarnN "You forgot to provide the attenuation coefficient in the config file."
                           -- logWarnN "I continue without attenuation correction"
                           -- logWarnN "Add attenuation_coefficient=<something> under the [input] section, to fix this"
                           -- return DataSourcePath'NoAttenuation
                  applyed@DataSourcePath'ApplyedAttenuationFactor{} -> applyed
      (Just coef) -> case att of
                      DataSourcePath'NoAttenuation           -> DataSourcePath'NoAttenuation
                      (DataSourcePath'Attenuation p o _ m) -> DataSourcePath'Attenuation p o coef m
                      (DataSourcePath'ApplyedAttenuationFactor _) -> undefined

mkDetector'Sixs'Fly :: Detector Hkl DIM2 -> DataSourcePath Image
mkDetector'Sixs'Fly det@(Detector2D d _ _)
  = case d of
      HklBinocularsDetectorEnum'ImxpadS140 ->
        DataSourcePath'Image
        (hdf5p $ grouppat 0 (datasetp "scan_data/xpad_image"
                              `H5Or`
                              datasetp "scan_data/xpad_s140_image"))
        det
      HklBinocularsDetectorEnum'XpadFlatCorrected -> undefined
      HklBinocularsDetectorEnum'ImxpadS70 ->
        DataSourcePath'Image
        (hdf5p $ grouppat 0 $ datasetp "scan_data/xpad_s70_image")
        det
      HklBinocularsDetectorEnum'DectrisEiger1M ->
        DataSourcePath'Image
        (hdf5p $ grouppat 0 $ datasetp "scan_data/eiger_image")
        det
      HklBinocularsDetectorEnum'Ufxc ->
        DataSourcePath'Image
        (hdf5p $ grouppat 0 $ datasetp "scan_data/ufxc_sixs_image")
        det
      HklBinocularsDetectorEnum'Merlin -> undefined
      HklBinocularsDetectorEnum'MerlinMedipix3rxQuad -> undefined

mkDetector'Sixs'Sbs :: Detector Hkl DIM2 -> DataSourcePath Image
mkDetector'Sixs'Sbs det@(Detector2D d _ _)
  = case d of
      HklBinocularsDetectorEnum'ImxpadS140 ->
        DataSourcePath'Image
        (hdf5p (datasetpattr ("long_name", "i14-c-c00/dt/xpad.s140/image")
                `H5Or`
                datasetpattr ("long_name", "i14-c-c00/dt/xpad.1/image")))
        det
      HklBinocularsDetectorEnum'XpadFlatCorrected -> undefined
      HklBinocularsDetectorEnum'ImxpadS70 ->
        DataSourcePath'Image
        (hdf5p $ datasetpattr ("long_name", "i14-c-c00/dt/xpad.s70/image"))
        det
      HklBinocularsDetectorEnum'DectrisEiger1M ->
        DataSourcePath'Image
        (hdf5p $ datasetpattr ("long_name", "i14-c-c00/dt/eiger.1/image"))
        det
      HklBinocularsDetectorEnum'Ufxc -> undefined
      HklBinocularsDetectorEnum'Merlin -> undefined
      HklBinocularsDetectorEnum'MerlinMedipix3rxQuad -> undefined

overloadAttenuationPath :: Maybe Double -> Maybe Float -> DataSourcePath Attenuation -> DataSourcePath Attenuation
overloadAttenuationPath ma m' (DataSourcePath'Attenuation p o a m)
  = DataSourcePath'Attenuation p o (fromMaybe a ma) (m' <|> m)
overloadAttenuationPath _ _ ap@DataSourcePath'ApplyedAttenuationFactor{} = ap
overloadAttenuationPath _ _ ap@DataSourcePath'NoAttenuation = ap

overloadTimestampPath :: Maybe HklBinocularsQCustomSubProjectionEnum -> DataSourcePath Timestamp -> DataSourcePath Timestamp
overloadTimestampPath msub idx =
  case msub of
    Nothing -> DataSourcePath'Timestamp'NoTimestamp
    (Just sub) -> case sub of
                   HklBinocularsQCustomSubProjectionEnum'QxQyQz -> DataSourcePath'Timestamp'NoTimestamp
                   HklBinocularsQCustomSubProjectionEnum'QTthTimestamp -> idx
                   HklBinocularsQCustomSubProjectionEnum'QTimestamp -> idx
                   HklBinocularsQCustomSubProjectionEnum'QparQperTimestamp -> idx
                   HklBinocularsQCustomSubProjectionEnum'QparQper -> DataSourcePath'Timestamp'NoTimestamp
                   HklBinocularsQCustomSubProjectionEnum'QPhiQx -> DataSourcePath'Timestamp'NoTimestamp
                   HklBinocularsQCustomSubProjectionEnum'QPhiQy -> DataSourcePath'Timestamp'NoTimestamp
                   HklBinocularsQCustomSubProjectionEnum'QPhiQz -> DataSourcePath'Timestamp'NoTimestamp
                   HklBinocularsQCustomSubProjectionEnum'QStereo -> DataSourcePath'Timestamp'NoTimestamp
                   HklBinocularsQCustomSubProjectionEnum'AnglesZaxisOmega -> DataSourcePath'Timestamp'NoTimestamp
                   HklBinocularsQCustomSubProjectionEnum'AnglesZaxisMu -> DataSourcePath'Timestamp'NoTimestamp
                   HklBinocularsQCustomSubProjectionEnum'XYZ -> DataSourcePath'Timestamp'NoTimestamp
                   HklBinocularsQCustomSubProjectionEnum'YZTimestamp -> idx
                   HklBinocularsQCustomSubProjectionEnum'QQparQper -> DataSourcePath'Timestamp'NoTimestamp

overloadWaveLength :: Maybe Double -> DataSourcePath Double -> DataSourcePath Double
overloadWaveLength ma wp = maybe wp DataSourcePath'Double'Const ma

overloadGeometryPath ::  Maybe Double -> DataSourcePath Geometry -> DataSourcePath Geometry
overloadGeometryPath mw (DataSourcePath'Geometry g wp as) = DataSourcePath'Geometry g (overloadWaveLength mw wp) as
overloadGeometryPath mw (DataSourcePath'Geometry'Fix wp) = DataSourcePath'Geometry'Fix (overloadWaveLength mw wp)
overloadGeometryPath mw (DataSourcePath'Geometry'MedVEiger wp as x z) = DataSourcePath'Geometry'MedVEiger (overloadWaveLength mw wp) as x z


overloadImagePath :: Detector Hkl DIM2 -> DataSourcePath Image -> DataSourcePath Image
overloadImagePath det (DataSourcePath'Image p _) = DataSourcePath'Image p det

overload'DataSourcePath'DataFrameQCustom :: BinocularsConfig'Common
                                         -> Maybe HklBinocularsQCustomSubProjectionEnum
                                         -> DataSourcePath DataFrameQCustom
                                         -> DataSourcePath DataFrameQCustom
overload'DataSourcePath'DataFrameQCustom common msub (DataSourcePath'DataFrameQCustom attenuationPath' geometryPath imagePath indexP)
  = let mAttCoef = binocularsConfig'Common'AttenuationCoefficient common
        mMaxAtt = binocularsConfig'Common'AttenuationMax common
        mWavelength = binocularsConfig'Common'Wavelength common
        detector =  binocularsConfig'Common'Detector common

        newAttenuationPath = overloadAttenuationPath mAttCoef mMaxAtt attenuationPath'
        newGeometryPath = overloadGeometryPath mWavelength geometryPath
        newImagePath = overloadImagePath detector imagePath
        newTimestampPath = overloadTimestampPath msub indexP
    in
      DataSourcePath'DataFrameQCustom newAttenuationPath newGeometryPath newImagePath newTimestampPath


guess'DataSourcePath'DataFrameQCustom :: BinocularsConfig'Common
                                      -> Maybe HklBinocularsQCustomSubProjectionEnum
                                      -> DataSourcePath DataFrameQCustom
guess'DataSourcePath'DataFrameQCustom common msub =
    do
      let inputtype = binocularsConfig'Common'InputType common
      let mAttenuationCoefficient = binocularsConfig'Common'AttenuationCoefficient common
      let detector =  binocularsConfig'Common'Detector common
      let mAttenuationMax = binocularsConfig'Common'AttenuationMax common
      let mWavelength = binocularsConfig'Common'Wavelength common

      -- attenuation
      let dataSourcePath'Attenuation'Sixs :: DataSourcePath Attenuation
          dataSourcePath'Attenuation'Sixs =
            DataSourcePath'Attenuation
            (DataSourcePath'Float (hdf5p $ grouppat 0 $ groupp "scan_data" (datasetp "attenuation"
                                                                            `H5Or`
                                                                            datasetp "attenuation_old")))
            2 0 mAttenuationMax

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
            0 0 mAttenuationMax

      -- wavelength
      let dataSourcePath'WaveLength'Sixs ::  DataSourcePath Double
          dataSourcePath'WaveLength'Sixs
            = DataSourcePath'Double (hdf5p $ grouppat 0 (datasetp "SIXS/Monochromator/wavelength"
                                                         `H5Or`
                                                         datasetp "SIXS/i14-c-c02-op-mono/lambda"))

      -- geometry
      let dataSourcePaths'Sixs'Uhv'Axes :: [DataSourcePath Double]
          dataSourcePaths'Sixs'Uhv'Axes
            = [ DataSourcePath'Double(hdf5p $ grouppat 0 $ groupp "scan_data" (datasetp "mu"
                                                                               `H5Or`
                                                                               datasetpattr ("long_name", "i14-c-cx2/ex/uhv-dif-group/mu")
                                                                               `H5Or`
                                                                               datasetp "UHV_MU"
                                                                               `H5Or`
                                                                               datasetp "mu_xps"))

              , DataSourcePath'Double(hdf5p $ grouppat 0 $ groupp "scan_data" (datasetp "omega"
                                                                               `H5Or`
                                                                               datasetp "UHV_OMEGA"
                                                                               `H5Or`
                                                                               datasetpattr ("long_name", "i14-c-cx2/ex/uhv-dif-group/omega")))

              , DataSourcePath'Double(hdf5p $ grouppat 0 $ groupp "scan_data" (datasetp "delta"
                                                                               `H5Or`
                                                                               datasetp "UHV_DELTA"
                                                                               `H5Or`
                                                                               datasetpattr ("long_name", "i14-c-cx2/ex/uhv-dif-group/delta")))

              , DataSourcePath'Double(hdf5p $ grouppat 0 $ groupp "scan_data" (datasetp "gamma"
                                                                               `H5Or`
                                                                               datasetp "UHV_GAMMA"
                                                                               `H5Or`
                                                                               datasetpattr ("long_name", "i14-c-cx2/ex/uhv-dif-group/gamma")))
              ]

      let dataSourcePath'Geometry'Uhv'Sixs :: DataSourcePath Geometry
          dataSourcePath'Geometry'Uhv'Sixs
            = DataSourcePath'Geometry
              (Geometry'Factory Uhv)
              (overloadWaveLength mWavelength dataSourcePath'WaveLength'Sixs)
              dataSourcePaths'Sixs'Uhv'Axes

      let dataSourcePath'Geometry'MedH'Sixs ::  DataSourcePath Geometry
          dataSourcePath'Geometry'MedH'Sixs
            = DataSourcePath'Geometry
              (Geometry'Factory MedH)
              (overloadWaveLength mWavelength dataSourcePath'WaveLength'Sixs)
              [ DataSourcePath'Double(hdf5p $ grouppat 0 $ groupp "scan_data" (datasetp "beta"
                                                                               `H5Or`
                                                                               datasetpattr ("long_name", "i14-c-cx1/ex/diff-med-tpp/pitch")))

              , DataSourcePath'Double(hdf5p $ grouppat 0 $ groupp "scan_data" (datasetp "mu"
                                                                               `H5Or`
                                                                               datasetpattr ("long_name", "i14-c-cx1/ex/med-h-dif-group.1/mu")))

              , DataSourcePath'Double(hdf5p $ grouppat 0 $ groupp "scan_data" (datasetp "gamma"
                                                                               `H5Or`
                                                                               datasetpattr ("long_name", "i14-c-cx1/ex/med-h-dif-group.1/gamma")))

              , DataSourcePath'Double(hdf5p $ grouppat 0 $ groupp "scan_data" (datasetp "delta"
                                                                               `H5Or`
                                                                               datasetpattr ("long_name", "i14-c-cx1/ex/med-h-dif-group.1/delta")))
              ]

       -- timestamp
      let mkTimeStamp'Sbs :: Maybe HklBinocularsQCustomSubProjectionEnum -> DataSourcePath Timestamp
          mkTimeStamp'Sbs msub'
            = overloadTimestampPath msub' (DataSourcePath'Timestamp(hdf5p $ grouppat 0 $ datasetp "scan_data/sensors_timestamps"))

      let mkTimeStamp'Fly :: Maybe HklBinocularsQCustomSubProjectionEnum -> DataSourcePath Timestamp
          mkTimeStamp'Fly msub'
            = overloadTimestampPath msub' (DataSourcePath'Timestamp(hdf5p $ grouppat 0 $ datasetp "scan_data/epoch"))

      case inputtype of
         CristalK6C -> DataSourcePath'DataFrameQCustom
                      (mkAttenuation mAttenuationCoefficient  DataSourcePath'NoAttenuation)
                      (DataSourcePath'Geometry
                        (Geometry'Factory K6c)
                        (overloadWaveLength mWavelength (DataSourcePath'Double (hdf5p $ grouppat 0 $ datasetp "CRISTAL/Monochromator/lambda")))
                        [ DataSourcePath'Double (hdf5p $ grouppat 0 $ datasetp "CRISTAL/Diffractometer/i06-c-c07-ex-dif-mu/position")
                        , DataSourcePath'Double (hdf5p $ grouppat 0 $ datasetp "CRISTAL/Diffractometer/i06-c-c07-ex-dif-komega/position")
                        , DataSourcePath'Double (hdf5p $ grouppat 0 $ datasetp "CRISTAL/Diffractometer/i06-c-c07-ex-dif-kappa/position")
                        , DataSourcePath'Double (hdf5p $ grouppat 0 $ datasetp "scan_data/actuator_1_1")
                        , DataSourcePath'Double (hdf5p $ grouppat 0 $ datasetp "CRISTAL/Diffractometer/i06-c-c07-ex-dif-gamma/position")
                        , DataSourcePath'Double (hdf5p $ grouppat 0 $ datasetp "CRISTAL/Diffractometer/i06-c-c07-ex-dif-delta/position")
                        ]
                      )
                      (DataSourcePath'Image
                        (hdf5p $ grouppat 0 $ datasetp "scan_data/data_05")
                        detector)
                      (mkTimeStamp'Sbs msub)
         MarsFlyscan -> DataSourcePath'DataFrameQCustom
                       (mkAttenuation mAttenuationCoefficient (DataSourcePath'ApplyedAttenuationFactor
                                                               (DataSourcePath'Float (hdf5p $ grouppat 0 $ datasetp "scan_data/applied_att"))))
                       (DataSourcePath'Geometry
                         (Geometry'Factory Mars)
                         (overloadWaveLength mWavelength (DataSourcePath'Double'Const 1.537591))
                         [ DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/omega")
                         , DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/chi")
                         , DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/phi")
                         , DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/tth")
                         ])
                       (DataSourcePath'Image
                         (hdf5p $ grouppat 0 (datasetp "scan_data/merlin_image"
                                              `H5Or`
                                              datasetp "scan_data/merlin_quad_image"))
                         detector)
                       (mkTimeStamp'Fly msub)
         MarsSbs -> DataSourcePath'DataFrameQCustom
                   (mkAttenuation mAttenuationCoefficient DataSourcePath'NoAttenuation)
                   (DataSourcePath'Geometry
                     (Geometry'Factory Mars)
                     (overloadWaveLength mWavelength (DataSourcePath'Double'Const 1.537591))
                     [ DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/omega")
                     , DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/chi")
                     , DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/phi")
                     , DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/tth")
                     ])
                   (DataSourcePath'Image
                    (hdf5p $ datasetpattr ("long_name", "d03-1-c00/dt/merlin-quad/image"))
                    detector)
                   (mkTimeStamp'Sbs msub)
         SixsFlyMedH -> DataSourcePath'DataFrameQCustom
                       (mkAttenuation mAttenuationCoefficient dataSourcePath'Attenuation'Sixs)
                       dataSourcePath'Geometry'MedH'Sixs
                       (mkDetector'Sixs'Fly detector)
                       (mkTimeStamp'Fly msub)
         SixsFlyMedV -> DataSourcePath'DataFrameQCustom
                       (mkAttenuation mAttenuationCoefficient dataSourcePath'Attenuation'Sixs)
                       (DataSourcePath'Geometry
                         (Geometry'Factory MedV)
                         (overloadWaveLength mWavelength dataSourcePath'WaveLength'Sixs)
                         [ DataSourcePath'Double'Const 0
                           -- (DataSourcePath'Double(H5Or
                           --                         (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "beta")
                           --                         (hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-ex-diff-med-tpp" $ groupp "TPP" $ groupp "Orientation" $ datasetp "pitch")))
                         , DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/mu")
                         , DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/omega")
                         , DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/gamma")
                         , DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/delta")
                         , DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/etaa")
                         ])
                       (mkDetector'Sixs'Fly detector)
                       (mkTimeStamp'Fly msub)
         SixsFlyMedVEiger -> DataSourcePath'DataFrameQCustom
                            (mkAttenuation mAttenuationCoefficient dataSourcePath'Attenuation'Sixs)
                            (DataSourcePath'Geometry'MedVEiger
                              (overloadWaveLength mWavelength dataSourcePath'WaveLength'Sixs)
                              [ DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/beta") -- maybe nothing
                              , DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/mu")
                              , DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/omega")
                              , DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/gamma")
                              , DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/delta")
                              , DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/etaa")
                              ]
                              (DataSourcePath'Double(hdf5p (grouppat 0 $ groupp "scan_data" $ datasetp "eix")
                                                     `H5Or`
                                                     hdf5p (grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-dt-det_tx.1" $ datasetp "position_pre")))
                              (DataSourcePath'Double(hdf5p (grouppat 0 $ groupp "scan_data" $ datasetp "eiz")
                                                     `H5Or`
                                                     hdf5p (grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-dt-det_tz.1" $ datasetp "position_pre"))))
                            (mkDetector'Sixs'Fly detector)
                            (mkTimeStamp'Fly msub)
         SixsFlyMedVS70 -> DataSourcePath'DataFrameQCustom
                          (mkAttenuation mAttenuationCoefficient dataSourcePath'Attenuation'Sixs)
                          (DataSourcePath'Geometry
                            (Geometry'Factory MedV)
                            (overloadWaveLength mWavelength dataSourcePath'WaveLength'Sixs)
                            [ (DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/beta"))
                            , (DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/mu"))
                            , (DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/omega"))
                            , (DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/gamma"))
                            , (DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/delta"))
                            , (DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/etaa"))
                            ])
                          (mkDetector'Sixs'Fly detector)
                          (mkTimeStamp'Fly msub)
         SixsFlyScanUhv -> DataSourcePath'DataFrameQCustom
                          (mkAttenuation mAttenuationCoefficient dataSourcePath'Attenuation'Sixs)
                          dataSourcePath'Geometry'Uhv'Sixs
                          (mkDetector'Sixs'Fly detector)
                          (mkTimeStamp'Fly msub)
         SixsFlyScanUhv2 -> DataSourcePath'DataFrameQCustom
                           (mkAttenuation mAttenuationCoefficient dataSourcePath'Attenuation'Sixs)
                           dataSourcePath'Geometry'Uhv'Sixs
                           (mkDetector'Sixs'Fly detector)
                           (mkTimeStamp'Fly msub)
         SixsFlyScanUhvGisaxsEiger -> DataSourcePath'DataFrameQCustom
                                     (mkAttenuation mAttenuationCoefficient dataSourcePath'Attenuation'Sixs)
                                     (DataSourcePath'Geometry
                                      sixsUhvGisaxs
                                      (overloadWaveLength mWavelength dataSourcePath'WaveLength'Sixs)
                                      [ DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/mu_xps")
                                      , DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "scan_data/omega_xps")
                                      , DataSourcePath'Double(hdf5p (grouppat 0 $ groupp "scan_data" $ datasetp "eix")
                                                              `H5Or`
                                                              hdf5p (grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-dt-det_tx.1" $ datasetp "position_pre"))
                                        `DataSourcePath'Double'Or`
                                        DataSourcePath'Double'Const 0
                                      , DataSourcePath'Double(hdf5p (grouppat 0 $ groupp "scan_data" $ datasetp "eiz")
                                                              `H5Or`
                                                              hdf5p (grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-dt-det_tz.1" $ datasetp "position_pre"))
                                        `DataSourcePath'Double'Or`
                                        DataSourcePath'Double'Const 0
                                      ])
                                     (mkDetector'Sixs'Fly detector)
                                     (mkTimeStamp'Fly msub)
         SixsFlyScanUhvTest -> DataSourcePath'DataFrameQCustom
                              (mkAttenuation mAttenuationCoefficient dataSourcePath'Attenuation'Sixs)
                              (DataSourcePath'Geometry
                               (Geometry'Factory Uhv)
                               (overloadWaveLength mWavelength (DataSourcePath'Double'Const 0.672494))
                               dataSourcePaths'Sixs'Uhv'Axes)
                              (mkDetector'Sixs'Fly detector)
                              (mkTimeStamp'Fly msub)
         SixsFlyScanUhvUfxc -> DataSourcePath'DataFrameQCustom
                              (mkAttenuation mAttenuationCoefficient dataSourcePath'Attenuation'Sixs)
                              dataSourcePath'Geometry'Uhv'Sixs
                              (mkDetector'Sixs'Fly detector)
                              (mkTimeStamp'Fly msub)
         SixsSbsFixedDetector -> DataSourcePath'DataFrameQCustom
                                (mkAttenuation mAttenuationCoefficient dataSourcePath'Attenuation'SixsSBS)
                                (DataSourcePath'Geometry'Fix
                                 (overloadWaveLength mWavelength dataSourcePath'WaveLength'Sixs))
                                (mkDetector'Sixs'Sbs detector)
                                (mkTimeStamp'Sbs msub)
         SixsSbsMedH -> DataSourcePath'DataFrameQCustom
                       (mkAttenuation mAttenuationCoefficient dataSourcePath'Attenuation'SixsSBS)
                       dataSourcePath'Geometry'MedH'Sixs
                       (mkDetector'Sixs'Sbs detector)
                       (mkTimeStamp'Sbs msub)
         SixsSbsMedHFixDetector -> DataSourcePath'DataFrameQCustom
                                  (mkAttenuation mAttenuationCoefficient dataSourcePath'Attenuation'Sixs)
                                  (DataSourcePath'Geometry
                                   sixsMedHGisaxs
                                   (overloadWaveLength mWavelength dataSourcePath'WaveLength'Sixs)
                                    [ DataSourcePath'Double(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/diff-med-tpp/pitch"))
                                    , DataSourcePath'Double(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-h-dif-group.1/mu"))
                                    , DataSourcePath'Double(hdf5p (grouppat 0 $ datasetp "scan_data/eix")
                                                            `H5Or`
                                                            hdf5p (grouppat 0 $ datasetp "SIXS/i14-c-cx1-dt-det_tx.1/position_pre"))
                                      `DataSourcePath'Double'Or`
                                      DataSourcePath'Double'Const 0
                                    , DataSourcePath'Double(hdf5p (grouppat 0 $ datasetp "scan_data/eiz")
                                                            `H5Or`
                                                            hdf5p (grouppat 0 $ datasetp "SIXS/i14-c-cx1-dt-det_tz.1/position_pre"))
                                      `DataSourcePath'Double'Or`
                                      DataSourcePath'Double'Const 0
                                    ])
                                  (mkDetector'Sixs'Sbs detector)
                                  (mkTimeStamp'Sbs msub)
         SixsSbsMedV -> DataSourcePath'DataFrameQCustom
                       (mkAttenuation mAttenuationCoefficient dataSourcePath'Attenuation'SixsSBS)
                       (DataSourcePath'Geometry
                         (Geometry'Factory MedV)
                         (overloadWaveLength mWavelength dataSourcePath'WaveLength'Sixs)
                         [ DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "SIXS/i14-c-cx1-ex-diff-med-tpp/TPP/Orientation/pitch")
                         , DataSourcePath'Double(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/mu"))
                         , DataSourcePath'Double(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/omega"))
                         , DataSourcePath'Double(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/gamma"))
                         , DataSourcePath'Double(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/delta"))
                         , DataSourcePath'Double(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/etaa"))
                         ])
                       (mkDetector'Sixs'Sbs detector)
                       (mkTimeStamp'Sbs msub)
         SixsSbsMedVFixDetector -> DataSourcePath'DataFrameQCustom
                                  (mkAttenuation mAttenuationCoefficient dataSourcePath'Attenuation'SixsSBS)
                                  (DataSourcePath'Geometry
                                    (Geometry'Factory MedV)
                                    (overloadWaveLength mWavelength dataSourcePath'WaveLength'Sixs)
                                    [ DataSourcePath'Double(hdf5p $ grouppat 0 $ groupp "SIXS" $ groupp "i14-c-cx1-ex-diff-med-tpp" $ groupp "TPP" $ groupp "Orientation" $ datasetp "pitch")
                                    , DataSourcePath'Double(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/mu"))
                                    , DataSourcePath'Double(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/omega"))
                                    , DataSourcePath'Double(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/gamma"))
                                    , DataSourcePath'Double(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/delta"))
                                    , DataSourcePath'Double(hdf5p $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/etaa"))
                                    ])
                                  (mkDetector'Sixs'Sbs detector)
                                  (mkTimeStamp'Sbs msub)
         SixsSbsUhv -> DataSourcePath'DataFrameQCustom
                      (mkAttenuation mAttenuationCoefficient dataSourcePath'Attenuation'SixsSBS)
                      dataSourcePath'Geometry'Uhv'Sixs
                      (mkDetector'Sixs'Sbs detector)
                      (mkTimeStamp'Sbs msub)


{-# INLINE spaceQCustom #-}
spaceQCustom :: Detector a DIM2 -> Array F DIM3 Double -> Resolutions DIM3 -> Maybe Mask -> HklBinocularsSurfaceOrientationEnum -> Maybe (RLimits DIM3) -> HklBinocularsQCustomSubProjectionEnum -> Space DIM3 -> DataFrameQCustom -> IO (DataFrameSpace DIM3)
spaceQCustom det pixels rs mmask' surf mlimits subprojection space@(Space fSpace) (DataFrameQCustom att g img index) =
  withNPixels det $ \nPixels ->
  withForeignPtr g $ \geometry ->
  withForeignPtr (toForeignPtr pixels) $ \pix ->
  withResolutions rs $ \nr r ->
  withPixelsDims pixels $ \ndim dims ->
  withMaybeMask mmask' $ \ mask'' ->
  withMaybeLimits mlimits rs $ \nlimits limits ->
  withForeignPtr fSpace $ \pSpace -> do
  case img of
    (ImageInt32 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_qcustom_int32_t" #-} c'hkl_binoculars_space_qcustom_int32_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits) (CDouble . unTimestamp $ index) (toEnum . fromEnum $ subprojection)
    (ImageWord16 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_qcustom_uint16_t" #-} c'hkl_binoculars_space_qcustom_uint16_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits) (CDouble . unTimestamp $ index) (toEnum . fromEnum $ subprojection)
    (ImageWord32 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_qcustom_uint32_t" #-} c'hkl_binoculars_space_qcustom_uint32_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) mask'' (toEnum $ fromEnum surf) limits (toEnum nlimits) (CDouble . unTimestamp $ index) (toEnum . fromEnum $ subprojection)

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

  -- directly from the common config
  let common = binocularsConfig'QCustom'Common conf
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
  let mlimits = binocularsConfig'QCustom'ProjectionLimits conf
  let res = binocularsConfig'QCustom'ProjectionResolution conf
  let surfaceOrientation = binocularsConfig'QCustom'HklBinocularsSurfaceOrientationEnum conf
  let datapaths = binocularsConfig'QCustom'DataPath conf
  let subprojection = fromJust (binocularsConfig'QCustom'SubProjection conf) -- should not be Maybe
  let projectionType = binocularsConfig'QCustom'ProjectionType conf

  -- built from the config
  output' <- liftIO $ destination' projectionType (Just subprojection) inputRange mlimits destination overwrite
  filenames <- InputFn'List <$> files nexusDir (Just inputRange) tmpl
  mask' <- getMask maskMatrix det
  pixels <- liftIO $ getPixelsCoordinates det centralPixel' sampleDetectorDistance detrot Normalisation

  -- compute the jobs

  let fns = concatMap (replicate 1) (toList filenames)
  chunks <- liftIO $ runSafeT $ toListM $ each fns >-> chunkP datapaths
  let ntot = sum (Prelude.map clength chunks)
  let jobs = chunk (quot ntot cap) chunks

  -- log parameters

  logDebugNSH filenames
  logDebugNSH datapaths
  logDebugNSH chunks
  logDebugNSH ntot
  logDebugNSH jobs
  logDebugN "start gessing final cube size"

  -- guess the final cube dimensions (To optimize, do not create the cube, just extract the shape)

  guessed <- liftIO $ withCubeAccumulator EmptyCube $ \c ->
    runSafeT $ runEffect $
    each chunks
    >-> Pipes.Prelude.map (\(Chunk fn f t) -> (fn, [f, quot (f + t) 4, quot (f + t) 4 * 2, quot (f + t) 4 * 3, t]))
    >-> framesQCustomP datapaths
    >-> project det 3 (spaceQCustom det pixels res mask' surfaceOrientation mlimits subprojection)
    >-> accumulateP c

  logDebugN "stop gessing final cube size"

  -- do the final projection

  logInfoN $ pack $ printf "let's do a QCustom projection of %d %s image(s) on %d core(s)" ntot (show det) cap

  liftIO $ withProgressBar ntot $ \pb -> do
    r' <- mapConcurrently (\job -> withCubeAccumulator guessed $ \c ->
                             runSafeT $ runEffect $
                             each job
                             >-> Pipes.Prelude.map (\(Chunk fn f t) -> (fn, [f..t]))
                             >-> framesQCustomP datapaths
                             >-> Pipes.Prelude.filter (\(DataFrameQCustom _ _ img _) -> filterSumImage mImageSumMax img)
                             >-> project det 3 (spaceQCustom det pixels res mask' surfaceOrientation mlimits subprojection)
                             >-> tee (accumulateP c)
                             >-> progress pb
                         ) jobs
    saveCube output' (unpack . serializeConfig $ conf) r'


instance ChunkP (DataSourcePath DataFrameQCustom) where
    chunkP (DataSourcePath'DataFrameQCustom ma _ (DataSourcePath'Image i _) _) =
      skipMalformed $ forever $ do
      fp <- await
      withFileP (openFile' fp) $ \f ->
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
          withFileP (openFile' fn) $ \f ->
            withDataSourceP f p $ \ g ->
            forM_ js (tryYield . extract1DStreamValue g)

---------
-- Cmd --
---------

processQCustom :: (MonadLogger m, MonadThrow m, MonadIO m) => Maybe FilePath -> Maybe ConfigRange -> m ()
processQCustom mf mr = cmd processQCustomP mf (Args'QCustomProjection mr)

newQCustom :: (MonadIO m, MonadLogger m, MonadThrow m)
           => Path Abs Dir -> m ()
newQCustom cwd = do
  let conf = default'BinocularsConfig'QCustom
             { binocularsConfig'QCustom'Common = default'BinocularsConfig'Common
                                                 { binocularsConfig'Common'Nexusdir = Just cwd }
             }
  liftIO $ Data.Text.IO.putStr $ serializeConfig conf

updateQCustom :: (MonadIO m, MonadLogger m, MonadThrow m)
              => Maybe FilePath -> Maybe ConfigRange -> m ()
updateQCustom mf mr = cmd (pure ()) mf (Args'QCustomProjection mr)

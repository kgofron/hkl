{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -ddump-splices #-}
{-
    Copyright  : Copyright (C) 2014-2025 Synchrotron SOLEIL
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
    , DataSource(..)
    , DSDataFrameQCustom(..)
    , FramesP(..)
    , default'DataSource'DataFrameQCustom
    , guess'DataSource'DataFrameQCustom
    , newQCustom
    , overload'DataSource'DataFrameQCustom
    , processQCustom
    , updateQCustom
    ) where

import           Control.Applicative               ((<|>))
import           Control.Concurrent.Async          (mapConcurrently)
import           Control.Monad                     (forM_, forever)
import           Control.Monad.Catch               (MonadThrow)
import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Control.Monad.Logger              (MonadLogger, logDebugN,
                                                    logInfoN)
import           Control.Monad.Reader              (MonadReader, ask)
import           Data.Aeson                        (FromJSON, ToJSON)
import           Data.HashMap.Lazy                 (fromList)
import           Data.Ini                          (Ini (..))
import           Data.Maybe                        (fromJust, fromMaybe)
import           Data.Text                         (pack, unpack)
import           Data.Text.IO                      (putStr)
import           Data.Vector.Storable.Mutable      (unsafeWith)
import           Foreign.C.Types                   (CDouble (..))
import           Foreign.ForeignPtr                (withForeignPtr)
import           GHC.Generics                      (Generic)
import           Numeric.Units.Dimensional.Prelude (Angle, degree, radian, (*~),
                                                    (/~))
import           Path                              (Abs, Dir, Path)
import           Pipes                             (await, each, runEffect,
                                                    yield, (>->))
import           Pipes.Prelude                     (filter, map, tee, toListM)
import           Pipes.Safe                        (runSafeT)
import           Text.Printf                       (printf)

import           Hkl.Binoculars.Common
import           Hkl.Binoculars.Config
import           Hkl.Binoculars.Pipes
import           Hkl.Binoculars.Projections
import           Hkl.Binoculars.Projections.Config
import           Hkl.C.Binoculars
import           Hkl.DataSource
import           Hkl.Detector
import           Hkl.Geometry
import           Hkl.H5
import           Hkl.Image
import           Hkl.Orphan                        ()
import           Hkl.Repa
import           Hkl.Types
import           Hkl.Utils

-----------------------
-- QCustom Projection --
-----------------------

-- Cursor

data DataFrameQCustom
    = DataFrameQCustom
      Attenuation -- attenuation
      Geometry -- geometry
      Image -- image
      (Maybe Mask) -- mask
      Timestamp -- timestamp in double
      Timescan0 -- timescan0 in double
      Scannumber -- scannumber in int
    deriving Show

data DSDataFrameQCustom (k :: DSKind)
    = DataSource'DataFrameQCustom
      (DSWrap_ DSAttenuation k)
      (DSWrap_ DSGeometry k)
      (DSWrap_ DSImage k)
      (DSWrap_ DSMask k)
      (DSWrap_ DSTimestamp k)
      (DSWrap_ DSTimescan0 k)
      (DSWrap_ DSScannumber k)
    deriving Generic

instance DataSource DSDataFrameQCustom
deriving instance Show (DSDataFrameQCustom DSPath)
instance FromJSON (DSDataFrameQCustom DSPath)
instance ToJSON (DSDataFrameQCustom DSPath)

instance Is1DStreamable (DSDataFrameQCustom DSAcq) DataFrameQCustom where
    extract1DStreamValue (DataSource'DataFrameQCustom att geom img msk idx t0 s) i =
      DataFrameQCustom
      <$> extract1DStreamValue att i
      <*> extract1DStreamValue geom i
      <*> extract1DStreamValue img i
      <*> extract1DStreamValue msk i
      <*> extract1DStreamValue idx i
      <*> extract0DStreamValue t0
      <*> extract0DStreamValue s

default'DataSource'DataFrameQCustom :: DSWrap_ DSDataFrameQCustom DSPath
default'DataSource'DataFrameQCustom
  = [ DataSource'DataFrameQCustom
      [ DataSourcePath'Attenuation
        [ DataSourcePath'Float'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/attenuation") ] ]
        2 0 Nothing
      ]
      [ DataSourcePath'Geometry
        (Geometry'Factory Uhv Nothing)
        [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset(hdf5p $ grouppat 0 $ datasetp "SIXS/Monochromator/wavelength") ] ]
        [DataSourcePath'List
         [ [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/UHV_MU") ] ]
         , [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/UHV_OMEGA") ] ]
         , [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/UHV_DELTA") ] ]
         , [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/UHV_GAMMA") ] ]
         ]
        ]
      ]
      [ DataSourcePath'Image'Hdf5
        defaultDetector
        (hdf5p $ grouppat 0 $ datasetp "scan_data/xpad_image")
      ]
      [ DataSourcePath'Mask (MaskLocation "") defaultDetector ]
      [ DataSourcePath'Timestamp'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/epoch") ] ]
      [ DataSourcePath'Timescan0'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/epoch") ] ]
      [ DataSourcePath'Scannumber ]
    ]

instance HasFieldComment [DSDataFrameQCustom DSPath] where
  fieldComment _ = [ "`datapath` internal value used to find the data in the data file."
                   , ""
                   , "This value is for expert only."
                   , ""
                   , "default value: <not set>"
                   ]

instance HasFieldValue [DSDataFrameQCustom DSPath] where
  fieldvalue = autoJSON

------------
-- Config --
------------

instance HasIniConfig 'QCustomProjection where
  data Config 'QCustomProjection
      = BinocularsConfig'QCustom
        { binocularsConfig'QCustom'Common :: Config Common
        , binocularsConfig'QCustom'HklBinocularsSurfaceOrientationEnum     :: HklBinocularsSurfaceOrientationEnum
        , binocularsConfig'QCustom'ProjectionType         :: ProjectionType
        , binocularsConfig'QCustom'ProjectionResolution   :: Resolutions DIM3
        , binocularsConfig'QCustom'ProjectionLimits       :: Maybe (RLimits DIM3)
        , binocularsConfig'QCustom'DataPath               :: DSWrap_ DSDataFrameQCustom DSPath
        , binocularsConfig'QCustom'SubProjection          :: Maybe HklBinocularsQCustomSubProjectionEnum
        , binocularsConfig'QCustom'Uqx                    :: Degree
        , binocularsConfig'QCustom'Uqy                    :: Degree
        , binocularsConfig'QCustom'Uqz                    :: Degree
        , binocularsConfig'QCustom'SampleAxis             :: Maybe SampleAxis
        } deriving (Show, Generic)

  newtype Args 'QCustomProjection
      = Args'QCustomProjection (Maybe ConfigRange)

  defaultConfig
      = BinocularsConfig'QCustom
        { binocularsConfig'QCustom'Common = defaultConfig
        , binocularsConfig'QCustom'HklBinocularsSurfaceOrientationEnum = HklBinocularsSurfaceOrientationEnum'Vertical
        , binocularsConfig'QCustom'ProjectionType = QCustomProjection
        , binocularsConfig'QCustom'ProjectionResolution = Resolutions3 0.01 0.01 0.01
        , binocularsConfig'QCustom'ProjectionLimits  = Nothing
        , binocularsConfig'QCustom'DataPath = default'DataSource'DataFrameQCustom
        , binocularsConfig'QCustom'SubProjection = Just HklBinocularsQCustomSubProjectionEnum'QxQyQz
        , binocularsConfig'QCustom'Uqx = Degree (0.0 *~ degree)
        , binocularsConfig'QCustom'Uqy = Degree (0.0 *~ degree)
        , binocularsConfig'QCustom'Uqz = Degree (0.0 *~ degree)
        , binocularsConfig'QCustom'SampleAxis = Nothing
        }

  getConfig content@(ConfigContent cfg) (Args'QCustomProjection mr) capabilities
      = do binocularsConfig'QCustom'Common <- getConfig content (Args'Common mr) capabilities
           binocularsConfig'QCustom'HklBinocularsSurfaceOrientationEnum <-  parseFDef cfg "input" "surface_orientation" (binocularsConfig'QCustom'HklBinocularsSurfaceOrientationEnum defaultConfig)
           binocularsConfig'QCustom'ProjectionType <- parseFDef cfg "projection" "type" (binocularsConfig'QCustom'ProjectionType defaultConfig)
           let msubprojection' = parseMbDef cfg "projection" "subprojection" (binocularsConfig'QCustom'SubProjection defaultConfig)
           let binocularsConfig'QCustom'SubProjection
                   = case binocularsConfig'QCustom'ProjectionType of
                       QIndexProjection -> Just HklBinocularsQCustomSubProjectionEnum'QTimestamp
                       QparQperProjection -> Just HklBinocularsQCustomSubProjectionEnum'QparQper
                       QxQyQzProjection -> Just HklBinocularsQCustomSubProjectionEnum'QxQyQz
                       AnglesProjection -> msubprojection'
                       Angles2Projection -> msubprojection'
                       HklProjection -> msubprojection'
                       QCustomProjection -> msubprojection'
                       RealSpaceProjection -> Just HklBinocularsQCustomSubProjectionEnum'XYZ
                       PixelsProjection -> Just HklBinocularsQCustomSubProjectionEnum'YZTimestamp
                       TestProjection -> msubprojection'

           binocularsConfig'QCustom'ProjectionResolution <- parseFDef cfg "projection" "resolution" (binocularsConfig'QCustom'ProjectionResolution defaultConfig)
           binocularsConfig'QCustom'ProjectionLimits <- parseMb cfg "projection" "limits"
           binocularsConfig'QCustom'DataPath <- pure (eitherF (const $ guess'DataSource'DataFrameQCustom binocularsConfig'QCustom'Common binocularsConfig'QCustom'SubProjection content) (parse' cfg "input" "datapath")
                                                                 (\case
                                                                   Nothing -> guess'DataSource'DataFrameQCustom binocularsConfig'QCustom'Common binocularsConfig'QCustom'SubProjection content
                                                                   Just d ->  overload'DataSource'DataFrameQCustom binocularsConfig'QCustom'Common binocularsConfig'QCustom'SubProjection d))

           binocularsConfig'QCustom'Uqx <- parseFDef cfg "projection" "uqx" (binocularsConfig'QCustom'Uqx defaultConfig)
           binocularsConfig'QCustom'Uqy <- parseFDef cfg "projection" "uqy" (binocularsConfig'QCustom'Uqy defaultConfig)
           binocularsConfig'QCustom'Uqz <- parseFDef cfg "projection" "uqz" (binocularsConfig'QCustom'Uqz defaultConfig)

           let errorMissingSampleAxis
                   = case binocularsConfig'QCustom'SubProjection of
                       Nothing -> Nothing
                       Just sub -> case sub of
                                    HklBinocularsQCustomSubProjectionEnum'QxQyQz -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QTthTimestamp -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QTimestamp -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QparQperTimestamp -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QparQper -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QPhiQx -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QPhiQy -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QPhiQz -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QStereo -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'DeltalabGammalabSampleaxis -> error "expect a valid  [projection] 'sampleaxis' key"
                                    HklBinocularsQCustomSubProjectionEnum'XYZ -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'YZTimestamp -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QQparQper -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QparsQperTimestamp -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QparQperSampleaxis -> error "expect a valid [projection] 'sampleaxis' key"
                                    HklBinocularsQCustomSubProjectionEnum'QSampleaxisTth -> error "expect a valid [projection] 'sampleaxis' key"
                                    HklBinocularsQCustomSubProjectionEnum'QSampleaxisTimestamp -> error "expect a valid [projection] 'sampleaxis' key"
                                    HklBinocularsQCustomSubProjectionEnum'QxQyTimestamp -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QxQzTimestamp -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QyQzTimestamp -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'TthAzimuth -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QTimescan0 -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'QScannumber -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'TthScannumber -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'PhixQThetax -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'PhiyQThetay -> Nothing
                                    HklBinocularsQCustomSubProjectionEnum'PhizQThetaz -> Nothing
           binocularsConfig'QCustom'SampleAxis <- pure (eitherF (const errorMissingSampleAxis) (parse' cfg "projection" "sampleaxis")
                                                                   (\case
                                                                     Nothing -> errorMissingSampleAxis
                                                                     Just d  -> Just d
                                                                   ))
           pure BinocularsConfig'QCustom{..}

  toIni c = toIni (binocularsConfig'QCustom'Common c)
            `mergeIni`
            Ini { iniSections = fromList [ ("input",    elemFDef' "surface_orientation" binocularsConfig'QCustom'HklBinocularsSurfaceOrientationEnum c defaultConfig
                                                     <> elemFDef' "datapath" binocularsConfig'QCustom'DataPath c defaultConfig
                                           )
                                         , ("projection",    elemFDef' "type" binocularsConfig'QCustom'ProjectionType c defaultConfig
                                                          <> elemFDef' "resolution" binocularsConfig'QCustom'ProjectionResolution c defaultConfig
                                                          <> elemFMbDef' "limits" binocularsConfig'QCustom'ProjectionLimits c defaultConfig
                                                          <> elemFMbDef' "subprojection" binocularsConfig'QCustom'SubProjection c defaultConfig
                                                          <> elemFDef "uqx" binocularsConfig'QCustom'Uqx c defaultConfig
                                                          [ "rotation around the x-axis of the sample in the surface basis system -- degree"
                                                          , ""
                                                          , "in this basis, the x-axis is colinear to the surface of the sample along the x-rays."
                                                          , ""
                                                          , " `<not set>` - use the default value `0.0`"
                                                          , " `a value`   - use this value"
                                                          ]
                                                          <> elemFDef "uqy" binocularsConfig'QCustom'Uqy c defaultConfig
                                                          [ "rotation around the y-axis of the sample in the surface basis system -- degree"
                                                          , ""
                                                          , "in this basis, the y-axis is colinear to the surface of the sample and"
                                                          , "forme a directe basis with x-axis and z-axis."
                                                          , ""
                                                          , "examples:"
                                                          , "  - all motors set to zero and a vertical surface - y-axis along -z (labo basis)"
                                                          , "  - all motors set to zero and an horizontal surcafe - y-axis along y (labo basis)"
                                                          , ""
                                                          , " `<not set>` - use the default value `0.0`"
                                                          , " `a value`   - use this value"
                                                          ]
                                                          <> elemFDef "uqz" binocularsConfig'QCustom'Uqz c defaultConfig
                                                          [ "rotation around the z-axis of the sample in the surface basis system -- degree"
                                                          , ""
                                                          , "in this basis, the z-axis is perpendicular to the surface of the sample."
                                                          , ""
                                                          , "examples:"
                                                          , "  - all motors set to zero and a vertical surface - z-axis along y (labo basis)"
                                                          , "  - all motors set to zero and an horizontal surcafe - z-axis along z (labo basis)"
                                                          , ""
                                                          , ""
                                                          , " `<not set>` - use the default value `0.0`"
                                                          , " `a value`   - use this value"
                                                          ]
                                                          <> elemFMbDef "sampleaxis" binocularsConfig'QCustom'SampleAxis c defaultConfig
                                                          [ "the name of the sample axis expected by some subprojections."
                                                          , ""
                                                          , " `<not set>` - for all subprojections which does not expect a value."
                                                          , " `a value`   - use this value for the subprojection expecting this axis."
                                                          ]
                                           )]

                , iniGlobals = []
                }

------------------
-- Input Path's --
------------------

mkAttenuation :: Maybe Double -> DSWrap_ DSAttenuation DSPath -> DSWrap_ DSAttenuation DSPath
mkAttenuation ma atts
    = Prelude.map
      ( \att -> case ma of
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
      ) atts

mk'DataSourcePath'Mask :: Config Common -> DSMask DSPath
mk'DataSourcePath'Mask c
    = case binocularsConfig'Common'Maskmatrix c of
        Nothing -> DataSourcePath'Mask'NoMask
        (Just m) -> DataSourcePath'Mask
                   m
                   (binocularsConfig'Common'Detector c)

mkDetector'Sixs'Fly :: Detector Hkl DIM2 -> Scannumber -> DSWrap_ DSImage DSPath
mkDetector'Sixs'Fly det@(Detector2D d _ _) sn
  = [case d of
      HklBinocularsDetectorEnum'ImxpadS140 ->
        DataSourcePath'Image'Hdf5
        det
        (hdf5p $ grouppat 0 (datasetp "scan_data/xpad_image"
                              `H5Or`
                              datasetp "scan_data/xpad_s140_image"))
      HklBinocularsDetectorEnum'XpadFlatCorrected -> undefined
      HklBinocularsDetectorEnum'ImxpadS70 ->
        DataSourcePath'Image'Hdf5
        det
        (hdf5p $ grouppat 0 $ datasetp "scan_data/xpad_s70_image")
      HklBinocularsDetectorEnum'DectrisEiger1M ->
        DataSourcePath'Image'Hdf5
        det
        (hdf5p $ grouppat 0 $ datasetp "scan_data/eiger_image")
      HklBinocularsDetectorEnum'Ufxc ->
        DataSourcePath'Image'Hdf5
        det
        (hdf5p $ grouppat 0 $ datasetp "scan_data/ufxc_sixs_image")
      HklBinocularsDetectorEnum'Merlin -> undefined
      HklBinocularsDetectorEnum'MerlinMedipix3rxQuad -> undefined
      HklBinocularsDetectorEnum'MerlinMedipix3rxQuad512 ->
        DataSourcePath'Image'Hdf5
        det
        (hdf5p $ grouppat 0 $ datasetp "scan_data/merlin_image")
      HklBinocularsDetectorEnum'Cirpad -> undefined
      HklBinocularsDetectorEnum'RigakuXspa1M ->
        DataSourcePath'Image'Img det "/nfs/ruche/sixs-soleil/com-sixs/2025/Run1/Rigaku_99240224/Scan%d/Beam18keV4_scan%d_%06d.img" sn
    ]

mkDetector'Sixs'Sbs :: Detector Hkl DIM2 -> Scannumber -> DSWrap_ DSImage DSPath
mkDetector'Sixs'Sbs det@(Detector2D d _ _) sn
  = [case d of
      HklBinocularsDetectorEnum'ImxpadS140 ->
        DataSourcePath'Image'Hdf5
        det
        (hdf5p (datasetpattr ("long_name", "i14-c-c00/dt/xpad.s140/image")
                `H5Or`
                datasetpattr ("long_name", "i14-c-c00/dt/xpad.1/image")))
      HklBinocularsDetectorEnum'XpadFlatCorrected -> undefined
      HklBinocularsDetectorEnum'ImxpadS70 ->
        DataSourcePath'Image'Hdf5
        det
        (hdf5p $ datasetpattr ("long_name", "i14-c-c00/dt/xpad.s70/image"))
      HklBinocularsDetectorEnum'DectrisEiger1M ->
        DataSourcePath'Image'Hdf5
        det
        (hdf5p $ datasetpattr ("long_name", "i14-c-c00/dt/eiger.1/image"))
      HklBinocularsDetectorEnum'Ufxc -> undefined
      HklBinocularsDetectorEnum'Merlin -> undefined
      HklBinocularsDetectorEnum'MerlinMedipix3rxQuad -> undefined
      HklBinocularsDetectorEnum'MerlinMedipix3rxQuad512 -> undefined
      HklBinocularsDetectorEnum'Cirpad -> undefined
      HklBinocularsDetectorEnum'RigakuXspa1M ->
        DataSourcePath'Image'Img det "/nfs/ruche/sixs-soleil/com-sixs/2025/Run1/Rigaku_99240224/Scan%d/Beam18keV4_scan%d_%06d.img" sn
    ]

overload'DataSourcePath'Attenuation :: Maybe Double -> Maybe Float -> DSWrap_ DSAttenuation DSPath -> DSWrap_ DSAttenuation DSPath
overload'DataSourcePath'Attenuation ma m' atts
    = Prelude.map
      ( \att -> case att of
                 DataSourcePath'Attenuation p o a m -> DataSourcePath'Attenuation p o (fromMaybe a ma) (m' <|> m)
                 ap@DataSourcePath'ApplyedAttenuationFactor{} -> ap
                 ap@DataSourcePath'NoAttenuation -> ap
      )
      atts

overload'DataSourcePath'Timestamp :: Maybe HklBinocularsQCustomSubProjectionEnum -> DSWrap_ DSTimestamp DSPath -> DSWrap_ DSTimestamp DSPath
overload'DataSourcePath'Timestamp msub idxs
    = Prelude.map
      ( \idx -> case msub of
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
                                HklBinocularsQCustomSubProjectionEnum'DeltalabGammalabSampleaxis -> DataSourcePath'Timestamp'NoTimestamp
                                HklBinocularsQCustomSubProjectionEnum'XYZ -> DataSourcePath'Timestamp'NoTimestamp
                                HklBinocularsQCustomSubProjectionEnum'YZTimestamp -> idx
                                HklBinocularsQCustomSubProjectionEnum'QQparQper -> DataSourcePath'Timestamp'NoTimestamp
                                HklBinocularsQCustomSubProjectionEnum'QparsQperTimestamp -> idx
                                HklBinocularsQCustomSubProjectionEnum'QparQperSampleaxis -> DataSourcePath'Timestamp'NoTimestamp
                                HklBinocularsQCustomSubProjectionEnum'QSampleaxisTth -> DataSourcePath'Timestamp'NoTimestamp
                                HklBinocularsQCustomSubProjectionEnum'QSampleaxisTimestamp -> idx
                                HklBinocularsQCustomSubProjectionEnum'QxQyTimestamp -> idx
                                HklBinocularsQCustomSubProjectionEnum'QxQzTimestamp -> idx
                                HklBinocularsQCustomSubProjectionEnum'QyQzTimestamp -> idx
                                HklBinocularsQCustomSubProjectionEnum'TthAzimuth -> DataSourcePath'Timestamp'NoTimestamp
                                HklBinocularsQCustomSubProjectionEnum'QTimescan0 -> DataSourcePath'Timestamp'NoTimestamp
                                HklBinocularsQCustomSubProjectionEnum'QScannumber -> DataSourcePath'Timestamp'NoTimestamp
                                HklBinocularsQCustomSubProjectionEnum'TthScannumber -> DataSourcePath'Timestamp'NoTimestamp
                                HklBinocularsQCustomSubProjectionEnum'PhixQThetax -> DataSourcePath'Timestamp'NoTimestamp
                                HklBinocularsQCustomSubProjectionEnum'PhiyQThetay -> DataSourcePath'Timestamp'NoTimestamp
                                HklBinocularsQCustomSubProjectionEnum'PhizQThetaz -> DataSourcePath'Timestamp'NoTimestamp
      ) idxs

overload'DataSourcePath'Timescan0 :: Maybe HklBinocularsQCustomSubProjectionEnum -> DSWrap_ DSTimescan0 DSPath -> DSWrap_ DSTimescan0 DSPath
overload'DataSourcePath'Timescan0 msub idxs
    = Prelude.map
      (\idx -> case msub of
                Nothing -> DataSourcePath'Timescan0'NoTimescan0
                (Just sub) -> case sub of
                               HklBinocularsQCustomSubProjectionEnum'QxQyQz -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QTthTimestamp -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QTimestamp -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QparQperTimestamp -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QparQper -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QPhiQx -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QPhiQy -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QPhiQz -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QStereo -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'DeltalabGammalabSampleaxis -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'XYZ -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'YZTimestamp -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QQparQper -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QparsQperTimestamp -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QparQperSampleaxis -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QSampleaxisTth -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QSampleaxisTimestamp -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QxQyTimestamp -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QxQzTimestamp -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QyQzTimestamp -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'TthAzimuth -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'QTimescan0 -> idx
                               HklBinocularsQCustomSubProjectionEnum'QScannumber -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'TthScannumber -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'PhixQThetax -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'PhiyQThetay -> DataSourcePath'Timescan0'NoTimescan0
                               HklBinocularsQCustomSubProjectionEnum'PhizQThetaz -> DataSourcePath'Timescan0'NoTimescan0
      ) idxs

overload'DataSourcePath'Double :: Maybe Double -> DSWrap_ DSDouble DSPath -> DSWrap_ DSDouble DSPath
overload'DataSourcePath'Double ma wps
    = Prelude.map
      (\wp -> maybe wp DataSourcePath'Double'Const ma)
      wps

overload'DataSourcePath'Geometry ::  Maybe Double -> DSWrap_ DSGeometry DSPath -> DSWrap_ DSGeometry DSPath
overload'DataSourcePath'Geometry mw gs
    = Prelude.map
      (\g -> case g of
              DataSourcePath'Geometry g' wp as -> DataSourcePath'Geometry g' (overload'DataSourcePath'Double mw wp) as
              (DataSourcePath'Geometry'Fix wp) -> DataSourcePath'Geometry'Fix (overload'DataSourcePath'Double mw wp)
      ) gs


overload'DataSourcePath'Image :: Detector Hkl DIM2 -> Maybe (DSWrap_ DSImage DSPath) -> DSWrap_ DSImage DSPath -> DSWrap_ DSImage DSPath
overload'DataSourcePath'Image _ (Just i) _ = i
overload'DataSourcePath'Image det Nothing imgs
    = Prelude.map
      (\img -> case img of
                DataSourcePath'Image'Dummy _ v -> DataSourcePath'Image'Dummy det v
                DataSourcePath'Image'Hdf5 _ p -> DataSourcePath'Image'Hdf5 det p
                DataSourcePath'Image'Img _ tmpl sn ->DataSourcePath'Image'Img det tmpl sn
      ) imgs

overload'DataSourcePath'Mask :: Config Common -> DSWrap_ DSMask DSPath -> DSWrap_ DSMask DSPath
overload'DataSourcePath'Mask c masks
    = Prelude.map
      (\mask -> case mask of
                 DataSourcePath'Mask'NoMask -> mk'DataSourcePath'Mask c
                 DataSourcePath'Mask path _ -> let new_path = case binocularsConfig'Common'Maskmatrix c of
                                                               Nothing -> path
                                                               Just p  -> p
                                              in
                                                DataSourcePath'Mask new_path (binocularsConfig'Common'Detector c)
      ) masks

overload'DataSource'DataFrameQCustom :: Config Common
                                         -> Maybe HklBinocularsQCustomSubProjectionEnum
                                         -> DSWrap_ DSDataFrameQCustom DSPath
                                         -> DSWrap_ DSDataFrameQCustom DSPath
overload'DataSource'DataFrameQCustom common msub dfs
    = Prelude.map
      (\(DataSource'DataFrameQCustom attenuationPath' geometryPath imagePath maskPath indexP timescan0P scannumberPath) ->
       let mAttCoef = binocularsConfig'Common'AttenuationCoefficient common
           mMaxAtt = binocularsConfig'Common'AttenuationMax common
           mWavelength = binocularsConfig'Common'Wavelength common
           detector =  binocularsConfig'Common'Detector common
           mImage = binocularsConfig'Common'Image common

           newAttenuationPath = overload'DataSourcePath'Attenuation mAttCoef mMaxAtt attenuationPath'
           newGeometryPath = overload'DataSourcePath'Geometry mWavelength geometryPath
           newImagePath = overload'DataSourcePath'Image detector mImage imagePath
           newMaskPath = overload'DataSourcePath'Mask common maskPath
           newTimestampPath = overload'DataSourcePath'Timestamp msub indexP
           newTimescan0Path = overload'DataSourcePath'Timescan0 msub timescan0P
           newScannumberPath = scannumberPath -- this is not overloadable
      in
        DataSource'DataFrameQCustom newAttenuationPath newGeometryPath newImagePath newMaskPath newTimestampPath newTimescan0Path newScannumberPath
      ) dfs


guess'DataSource'DataFrameQCustom :: Config Common
                                      -> Maybe HklBinocularsQCustomSubProjectionEnum
                                      -> ConfigContent
                                      -> DSWrap_ DSDataFrameQCustom DSPath
guess'DataSource'DataFrameQCustom common msub cfg =
    do
      let inputtype = binocularsConfig'Common'InputType common
      let mAttenuationCoefficient = binocularsConfig'Common'AttenuationCoefficient common
      let detector =  binocularsConfig'Common'Detector common
      let mImage = binocularsConfig'Common'Image common
      let mAttenuationMax = binocularsConfig'Common'AttenuationMax common
      let mAttenuationShift = binocularsConfig'Common'AttenuationShift common
      let mWavelength = binocularsConfig'Common'Wavelength common

      -- scan number 0
      let sn0 = getInitialScannumber $ binocularsConfig'Common'InputRange common

      -- attenuation
      let dataSourcePath'Attenuation'Sixs :: DSWrap_ DSAttenuation DSPath
          dataSourcePath'Attenuation'Sixs
              = [ DataSourcePath'Attenuation
                  [ DataSourcePath'Float'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "attenuation")
                                              , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "attenuation_old")
                                              ]
                  ]
                  (fromMaybe 2 mAttenuationShift) 0 mAttenuationMax
                ]

      let dataSourcePath'Attenuation'SixsSBS :: DSWrap_ DSAttenuation DSPath
          dataSourcePath'Attenuation'SixsSBS
              = [ DataSourcePath'Attenuation
                  [ DataSourcePath'Float'Hdf5 [ DataSourcePath'Dataset (hdf5p $ datasetpattr ("long_name", "i14-c-c00/ex/roic/att"))
                                              , DataSourcePath'Dataset (hdf5p $ datasetpattr ("long_name", "i14-c-c00/ex/roic-s140/att"))
                                              , DataSourcePath'Dataset (hdf5p $ datasetpattr ("long_name", "i14-c-c00/ex/roic-s140/att_old"))
                                              , DataSourcePath'Dataset (hdf5p $ datasetpattr ("long_name", "i14-c-c00/ex/roic-s70/att"))
                                              , DataSourcePath'Dataset (hdf5p $ datasetpattr ("long_name", "i14-c-c00/ex/roic-s70/att_old"))
                                              ]
                  ]
                  (fromMaybe 0 mAttenuationShift) 0 mAttenuationMax
                ]

      -- timestamp
      let mkTimeStamp'Sbs :: Maybe HklBinocularsQCustomSubProjectionEnum -> DSWrap_ DSTimestamp DSPath
          mkTimeStamp'Sbs msub'
            = overload'DataSourcePath'Timestamp msub' [ DataSourcePath'Timestamp'Hdf5 [ DataSourcePath'Dataset(hdf5p $ grouppat 0 $ datasetp "scan_data/sensors_timestamps") ] ]

      let mkTimeStamp'Fly :: Maybe HklBinocularsQCustomSubProjectionEnum -> DSWrap_ DSTimestamp DSPath
          mkTimeStamp'Fly msub'
            = overload'DataSourcePath'Timestamp msub' [ DataSourcePath'Timestamp'Hdf5 [ DataSourcePath'Dataset(hdf5p $ grouppat 0 $ datasetp "scan_data/epoch") ] ]

      -- timescan0
      let mkTimescan0'Sbs :: Maybe HklBinocularsQCustomSubProjectionEnum -> DSWrap_ DSTimescan0 DSPath
          mkTimescan0'Sbs msub'
            = overload'DataSourcePath'Timescan0 msub' [ DataSourcePath'Timescan0'Hdf5 [ DataSourcePath'Dataset(hdf5p $ grouppat 0 $ datasetp "scan_data/sensors_timestamps") ] ]

      let mkTimescan0'Fly :: Maybe HklBinocularsQCustomSubProjectionEnum -> DSWrap_ DSTimescan0 DSPath
          mkTimescan0'Fly msub'
            = overload'DataSourcePath'Timescan0 msub' [ DataSourcePath'Timescan0'Hdf5 [ DataSourcePath'Dataset(hdf5p $ grouppat 0 $ datasetp "scan_data/epoch") ] ]

      -- wavelength
      let dataSourcePath'WaveLength'Diffabs :: DSWrap_ DSDouble DSPath
          dataSourcePath'WaveLength'Diffabs
            = [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "DIFFABS/d13-1-c03__op__mono/wavelength" ) ] ]

      let dataSourcePath'WaveLength'Mars :: DSWrap_ DSDouble DSPath
          dataSourcePath'WaveLength'Mars
              = [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "MARS/d03-1-c03__op__mono1-config_#2/lambda" ) ]
                , DataSourcePath'Double'Const 1.537591
                ]

      let dataSourcePath'WaveLength'Sixs ::  DSWrap_ DSDouble DSPath
          dataSourcePath'WaveLength'Sixs
            = [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "SIXS/Monochromator/wavelength")
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "SIXS/i14-c-c02-op-mono/lambda")
                                           ]
              ]

      -- geometry
      let sixs'eix
            = [ DataSourcePath'Double'Ini cfg "geometry.values" "eix"
              , DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/eix")
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/dt/tab-mt_tx.1/position"))
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx2/dt/tab-mt_tx.1/position"))
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "SIXS/i14-c-cx1-dt-det_tx.1/position_pre")
                                           ]
              ]
      let sixs'eiz
            = [ DataSourcePath'Double'Ini cfg "geometry.values" "eiz"
              , DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/eiz")
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/dt/tab-mt_tz.1/position"))
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx2/dt/tab-mt_tz.1/position"))
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "SIXS/i14-c-cx1-dt-det_tz.1/position_pre")
                                           ]
              ]
      let sixs'Uhv'Mu
            = [ DataSourcePath'Double'Ini cfg "geometry.values" "mu"
              , DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu")
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx2/ex/uhv-dif-group/mu"))
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_MU")
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu_xps")
                                           ]
              ]
      let sixs'Uhv'Omega
            = [ DataSourcePath'Double'Ini cfg "geometry.values" "omega"
              , DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega")
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_OMEGA")
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx2/ex/uhv-dif-group/omega"))
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega_xps")
                                           ]
              ]
      let sixs'Uhv'Delta
            = [ DataSourcePath'Double'Ini cfg "geometry.values" "delta"
              , DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta")
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_DELTA")
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx2/ex/uhv-dif-group/delta"))
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta_xps")
                                           ]
              ]
      let sixs'Uhv'Gamma
            = [ DataSourcePath'Double'Ini cfg "geometry.values" "gamma"
              , DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma")
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "UHV_GAMMA")
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx2/ex/uhv-dif-group/gamma"))
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma_xps")
                                           ]
              ]

      let dataSourcePath'Geometry'Sixs'Uhv :: DSWrap_ DSGeometry DSPath
          dataSourcePath'Geometry'Sixs'Uhv
            = [ DataSourcePath'Geometry
                (Geometry'Factory Uhv Nothing)
                (overload'DataSourcePath'Double mWavelength dataSourcePath'WaveLength'Sixs)
                [ DataSourcePath'List [sixs'Uhv'Mu, sixs'Uhv'Omega, sixs'Uhv'Delta, sixs'Uhv'Gamma] ]
              ]

      let dataSourcePath'Geometry'Sixs'UhvGisaxs :: DSWrap_ DSGeometry DSPath
          dataSourcePath'Geometry'Sixs'UhvGisaxs
            = [ DataSourcePath'Geometry
                sixsUhvGisaxs
                (overload'DataSourcePath'Double mWavelength dataSourcePath'WaveLength'Sixs)
                [ DataSourcePath'List [ sixs'Uhv'Mu, sixs'Uhv'Omega, sixs'eix, sixs'eiz ] ]
              ]

      let sixs'Med'Beta
            = [ DataSourcePath'Double'Ini cfg "geometry.values" "beta"
              , DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "beta")
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/diff-med-tpp/pitch"))
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "SIXS/i14-c-cx1-ex-diff-med-tpp/TPP/Orientation/pitch")
                                           ]
              ]
      let sixs'MedH'Mu
            = [ DataSourcePath'Double'Ini cfg "geometry.values" "mu"
              , DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu")
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-h-dif-group.1/mu"))
                                           ]
              ]
      let sixs'MedV'Mu
            = [ DataSourcePath'Double'Ini cfg "geometry.values" "mu"
              , DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "mu")
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/mu"))
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-grp_no_etaa/mu"))
                                           ]
              ]
      let sixs'MedV'Omega
            = [ DataSourcePath'Double'Ini cfg "geometry.values" "omega"
              , DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "omega")
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/omega"))
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-grp_no_etaa/omega"))
                                           ]
              ]
      let sixs'MedH'Gamma
            = [ DataSourcePath'Double'Ini cfg "geometry.values" "gamma"
              , DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma")
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-h-dif-group.1/gamma"))
                                           ]
              ]
      let sixs'MedV'Gamma
            = [ DataSourcePath'Double'Ini cfg "geometry.values" "gamma"
              , DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "gamma")
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/gamma"))
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-grp_no_etaa/gamma"))
                                           ]
              ]
      let sixs'MedH'Delta
            = [ DataSourcePath'Double'Ini cfg "geometry.values" "delta"
              , DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta")
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-h-dif-group.1/delta"))
                                           ]
              ]
      let sixs'MedV'Delta
            = [ DataSourcePath'Double'Ini cfg "geometry.values" "delta"
              , DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "delta")
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/delta"))
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-grp_no_etaa/delta"))
                                           ]
              ]
      let sixs'MedV'Etaa
            = [ DataSourcePath'Double'Ini cfg "geometry.values" "etaa"
              , DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetp "etaa")
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/med-v-dif-group.1/etaa"))
                                           , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "i14-c-cx1/ex/eta-a-med-grp/etaa"))
                                           ]
              ]


      let dataSourcePath'Geometry'Sixs'MedH ::  DSWrap_ DSGeometry DSPath
          dataSourcePath'Geometry'Sixs'MedH
            = [ DataSourcePath'Geometry
                (Geometry'Factory MedH Nothing)
                (overload'DataSourcePath'Double mWavelength dataSourcePath'WaveLength'Sixs)
                [ DataSourcePath'List [ sixs'Med'Beta, sixs'MedH'Mu, sixs'MedH'Gamma, sixs'MedH'Delta ] ]
              ]

      let dataSourcePath'Geometry'Sixs'MedHGisaxs ::  DSWrap_ DSGeometry DSPath
          dataSourcePath'Geometry'Sixs'MedHGisaxs
            = [ DataSourcePath'Geometry
                sixsMedHGisaxs
                (overload'DataSourcePath'Double mWavelength dataSourcePath'WaveLength'Sixs)
                [ DataSourcePath'List [ sixs'Med'Beta, sixs'MedH'Mu, sixs'eix, sixs'eiz ] ]
              ]

      let dataSourcePath'Geometry'Sixs'MedV :: DSWrap_ DSGeometry DSPath
          dataSourcePath'Geometry'Sixs'MedV
            = [ DataSourcePath'Geometry
                (Geometry'Factory MedV Nothing)
                (overload'DataSourcePath'Double mWavelength dataSourcePath'WaveLength'Sixs)
                [ DataSourcePath'List [ sixs'Med'Beta, sixs'MedV'Mu, sixs'MedV'Omega, sixs'MedV'Gamma, sixs'MedV'Delta, sixs'MedV'Etaa ] ]
              ]

      let dataSourcePath'Geometry'Sixs'MedVGisaxs :: DSWrap_ DSGeometry DSPath
          dataSourcePath'Geometry'Sixs'MedVGisaxs
            = [ DataSourcePath'Geometry
                sixsMedVGisaxs
                (overload'DataSourcePath'Double mWavelength dataSourcePath'WaveLength'Sixs)
                [ DataSourcePath'List [ sixs'Med'Beta, sixs'MedV'Mu, sixs'MedV'Omega, sixs'eix, sixs'eiz ] ]
              ]

      let dataSourcePath'DataFrameQCustom'Sixs'Fly :: DSWrap_ DSGeometry DSPath -> Scannumber -> DSWrap_ DSDataFrameQCustom DSPath
          dataSourcePath'DataFrameQCustom'Sixs'Fly g sn
            = [ let att = mkAttenuation mAttenuationCoefficient dataSourcePath'Attenuation'Sixs
                in DataSource'DataFrameQCustom
                   att
                   g
                   (overload'DataSourcePath'Image detector mImage (mkDetector'Sixs'Fly detector sn))
                   [ mk'DataSourcePath'Mask common ]
                   (mkTimeStamp'Fly msub)
                   (mkTimescan0'Fly msub)
                   [ DataSourcePath'Scannumber ]
              ]

      let dataSourcePath'DataFrameQCustom'Sixs'Sbs :: DSWrap_ DSGeometry DSPath -> Scannumber -> DSWrap_ DSDataFrameQCustom DSPath
          dataSourcePath'DataFrameQCustom'Sixs'Sbs g sn
            = [ let att = mkAttenuation mAttenuationCoefficient dataSourcePath'Attenuation'SixsSBS
                in DataSource'DataFrameQCustom
                   att
                   g
                   ( overload'DataSourcePath'Image detector mImage (mkDetector'Sixs'Sbs detector sn) )
                   [ mk'DataSourcePath'Mask common ]
                   ( mkTimeStamp'Sbs msub )
                   ( mkTimescan0'Sbs msub )
                   [ DataSourcePath'Scannumber ]
              ]

      case inputtype of
         CristalK6C -> [ DataSource'DataFrameQCustom
                        (mkAttenuation mAttenuationCoefficient [ DataSourcePath'NoAttenuation])
                        [ DataSourcePath'Geometry
                          (Geometry'Factory K6c Nothing)
                          (overload'DataSourcePath'Double mWavelength [ DataSourcePath'Double'Hdf5 ([ DataSourcePath'Dataset  (hdf5p $ grouppat 0 $ datasetp "CRISTAL/Monochromator/lambda")])])
                          [ DataSourcePath'List
                            [ [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "CRISTAL/Diffractometer/i06-c-c07-ex-dif-mu/position") ] ]
                            , [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "CRISTAL/Diffractometer/i06-c-c07-ex-dif-komega/position") ] ]
                            , [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "CRISTAL/Diffractometer/i06-c-c07-ex-dif-kappa/position") ] ]
                            , [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/actuator_1_1") ] ]
                            , [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "CRISTAL/Diffractometer/i06-c-c07-ex-dif-gamma/position") ] ]
                            , [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "CRISTAL/Diffractometer/i06-c-c07-ex-dif-delta/position") ] ]
                            ]
                          ]
                        ]
                        [ DataSourcePath'Image'Hdf5
                          detector
                          (hdf5p $ grouppat 0 $ datasetp "scan_data/data_05")
                        ]
                        [ mk'DataSourcePath'Mask common ]
                        (mkTimeStamp'Sbs msub)
                        (mkTimescan0'Sbs msub)
                        [ DataSourcePath'Scannumber ]
                      ]
         Custom -> undefined
         DiffabsCirpad -> [ DataSource'DataFrameQCustom
                           (mkAttenuation mAttenuationCoefficient  [ DataSourcePath'NoAttenuation])
                           [ DataSourcePath'Geometry
                             cirpad
                             (overload'DataSourcePath'Double mWavelength dataSourcePath'WaveLength'Diffabs)
                             [ DataSourcePath'List
                               [ [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "d13-1-cx1/ex/cirpad_delta/position")) ] ]
                               , [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "d13-1-cx1/ex/dif.1-cirpad-gam/position")) ] ]
                               ]
                             ]
                           ]
                           (overload'DataSourcePath'Image detector mImage
                            [ DataSourcePath'Image'Hdf5
                              detector
                              (hdf5p $ grouppat 0 $ groupp "scan_data" $ datasetpattr ("long_name", "d13-1-cx1/dt/cirpad.1/image"))
                            ]
                           )
                           [ mk'DataSourcePath'Mask common ]
                           (mkTimeStamp'Sbs msub)
                           (mkTimescan0'Sbs msub)
                           [ DataSourcePath'Scannumber ]
                         ]
         MarsFlyscan -> [ DataSource'DataFrameQCustom
                         (mkAttenuation mAttenuationCoefficient [ DataSourcePath'NoAttenuation ])
                         -- (mkAttenuation mAttenuationCoefficient (DataSourcePath'ApplyedAttenuationFactor
                         --                                         (DataSourcePath'Float'Hdf5 ([ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/applied_att"))))
                         [ DataSourcePath'Geometry
                           (Geometry'Factory Mars Nothing)
                           (overload'DataSourcePath'Double mWavelength dataSourcePath'WaveLength'Mars)
                           [ DataSourcePath'List
                             [ [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/omega") ] ]
                             , [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/chi") ] ]
                             , [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/phi") ] ]
                             , [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/tth") ] ]
                             ]
                           ]
                         ]
                         (overload'DataSourcePath'Image detector mImage
                          [ DataSourcePath'Image'Hdf5
                            detector
                            (hdf5p $ grouppat 0 (datasetp "scan_data/merlin_image"
                                                 `H5Or`
                                                 datasetp "scan_data/merlin_quad_image"))
                          ]
                         )
                         [ mk'DataSourcePath'Mask common ]
                         (mkTimeStamp'Fly msub)
                         (mkTimescan0'Sbs msub)
                         [ DataSourcePath'Scannumber ]
                       ]
         MarsSbs -> [ DataSource'DataFrameQCustom
                     (mkAttenuation mAttenuationCoefficient [ DataSourcePath'NoAttenuation ])
                     [ DataSourcePath'Geometry
                       (Geometry'Factory Mars Nothing)
                       (overload'DataSourcePath'Double mWavelength dataSourcePath'WaveLength'Mars)
                       [ DataSourcePath'List
                         [ [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/omega")
                                                        , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "MARS/d03-1-cx2__ex__dif-mt_rx.1_#2/raw_value")
                                                        ]
                           , DataSourcePath'Double'Const 0
                           ]
                         , [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/chi")
                                                        , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "MARS/d03-1-cx2__ex__gonio-mt_rs_#2/raw_value")
                                                        ]
                           , DataSourcePath'Double'Const 0
                           ]
                         , [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/phi")
                                                        , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetpattr ("long_name", "d03-1-cx2/ex/gonio-mt_rz/position"))
                                                        , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "MARS/d03-1-cx2__ex__gonio-mt_rz_#2/raw_value")
                                                        ]
                           , DataSourcePath'Double'Const 0
                           ]
                         , [ DataSourcePath'Double'Hdf5 [ DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "scan_data/tth")
                                                        , DataSourcePath'Dataset (hdf5p $ grouppat 0 $ datasetp "MARS/d03-1-cx2__ex__dif-mt_rx.2_#2/raw_value")
                                                        ]
                           , DataSourcePath'Double'Const 0
                           ]
                         ]
                       ]
                     ]
                     (overload'DataSourcePath'Image detector mImage
                      [ DataSourcePath'Image'Hdf5
                        detector
                        (hdf5p $ (datasetpattr ("long_name", "d03-1-c00/dt/merlin-quad/image")
                                  `H5Or`
                                  datasetpattr ("interpretation", "image")))
                      ]
                     )
                     [ mk'DataSourcePath'Mask common ]
                     (mkTimeStamp'Sbs msub)
                     (mkTimescan0'Sbs msub)
                     [ DataSourcePath'Scannumber ]
                   ]
         SixsFlyMedH -> dataSourcePath'DataFrameQCustom'Sixs'Fly dataSourcePath'Geometry'Sixs'MedH sn0
         SixsFlyMedHGisaxs -> dataSourcePath'DataFrameQCustom'Sixs'Fly dataSourcePath'Geometry'Sixs'MedHGisaxs sn0
         SixsFlyMedV -> dataSourcePath'DataFrameQCustom'Sixs'Fly dataSourcePath'Geometry'Sixs'MedV sn0
         SixsFlyMedVGisaxs -> dataSourcePath'DataFrameQCustom'Sixs'Fly dataSourcePath'Geometry'Sixs'MedVGisaxs sn0
         SixsFlyUhv -> dataSourcePath'DataFrameQCustom'Sixs'Fly dataSourcePath'Geometry'Sixs'Uhv sn0
         SixsFlyUhvGisaxs -> dataSourcePath'DataFrameQCustom'Sixs'Fly dataSourcePath'Geometry'Sixs'UhvGisaxs sn0
         SixsSbsMedH -> dataSourcePath'DataFrameQCustom'Sixs'Sbs dataSourcePath'Geometry'Sixs'MedH sn0
         SixsSbsMedHGisaxs -> dataSourcePath'DataFrameQCustom'Sixs'Sbs dataSourcePath'Geometry'Sixs'MedHGisaxs sn0
         SixsSbsMedV -> dataSourcePath'DataFrameQCustom'Sixs'Sbs dataSourcePath'Geometry'Sixs'MedV sn0
         SixsSbsMedVGisaxs -> dataSourcePath'DataFrameQCustom'Sixs'Sbs dataSourcePath'Geometry'Sixs'MedVGisaxs sn0
         SixsSbsUhv -> dataSourcePath'DataFrameQCustom'Sixs'Sbs dataSourcePath'Geometry'Sixs'Uhv sn0
         SixsSbsUhvGisaxs -> dataSourcePath'DataFrameQCustom'Sixs'Sbs dataSourcePath'Geometry'Sixs'UhvGisaxs sn0


{-# INLINE spaceQCustom #-}
spaceQCustom :: Detector a DIM2
             -> Array F DIM3 Double
             -> Resolutions DIM3
             -> HklBinocularsSurfaceOrientationEnum
             -> Maybe (RLimits DIM3)
             -> HklBinocularsQCustomSubProjectionEnum
             -> Angle Double -> Angle Double -> Angle Double
             -> Maybe SampleAxis
             -> Bool
             -> Space DIM3
             -> DataFrameQCustom
             -> IO (DataFrameSpace DIM3)
spaceQCustom det pixels rs surf mlimits subprojection uqx uqy uqz mSampleAxis doPolarizationCorrection space@(Space fSpace) (DataFrameQCustom att g img mmask index timescan0 scannumber) =
  withNPixels det $ \nPixels ->
  withGeometry g $ \geometry ->
  withForeignPtr (toForeignPtr pixels) $ \pix ->
  withResolutions rs $ \nr r ->
  withPixelsDims pixels $ \ndim dims ->
  withMaybeMask mmask $ \ c'mask ->
  withMaybeLimits mlimits rs $ \nlimits limits ->
  withMaybeSampleAxis mSampleAxis $ \sampleAxis ->
  withForeignPtr fSpace $ \pSpace -> do
  case img of
    (ImageDouble arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_qcustom_double" #-} c'hkl_binoculars_space_qcustom_double pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) c'mask (toEnum $ fromEnum surf) limits (toEnum nlimits) (CDouble . unTimestamp $ index) (CDouble . unTimescan0 $ timescan0) (toEnum . fromEnum . unScannumber $ scannumber) (toEnum . fromEnum $ subprojection) (CDouble (uqx /~ radian)) (CDouble (uqy /~ radian)) (CDouble (uqz /~ radian)) sampleAxis (toEnum . fromEnum $ doPolarizationCorrection)
    (ImageInt32 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_qcustom_int32_t" #-} c'hkl_binoculars_space_qcustom_int32_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) c'mask (toEnum $ fromEnum surf) limits (toEnum nlimits) (CDouble . unTimestamp $ index) (CDouble . unTimescan0 $ timescan0) (toEnum . fromEnum . unScannumber $ scannumber) (toEnum . fromEnum $ subprojection) (CDouble (uqx /~ radian)) (CDouble (uqy /~ radian)) (CDouble (uqz /~ radian)) sampleAxis (toEnum . fromEnum $ doPolarizationCorrection)
    (ImageWord16 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_qcustom_uint16_t" #-} c'hkl_binoculars_space_qcustom_uint16_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) c'mask (toEnum $ fromEnum surf) limits (toEnum nlimits) (CDouble . unTimestamp $ index) (CDouble . unTimescan0 $ timescan0) (toEnum . fromEnum . unScannumber $ scannumber) (toEnum . fromEnum $ subprojection) (CDouble (uqx /~ radian)) (CDouble (uqy /~ radian)) (CDouble (uqz /~ radian)) sampleAxis (toEnum . fromEnum $ doPolarizationCorrection)
    (ImageWord32 arr) -> unsafeWith arr $ \i -> do
      {-# SCC "hkl_binoculars_space_qcustom_uint32_t" #-} c'hkl_binoculars_space_qcustom_uint32_t pSpace geometry i nPixels (CDouble . unAttenuation $ att) pix (toEnum ndim) dims r (toEnum nr) c'mask (toEnum $ fromEnum surf) limits (toEnum nlimits) (CDouble . unTimestamp $ index) (CDouble . unTimescan0 $ timescan0)(toEnum . fromEnum . unScannumber $ scannumber)  (toEnum . fromEnum $ subprojection) (CDouble (uqx /~ radian)) (CDouble (uqy /~ radian)) (CDouble (uqz /~ radian)) sampleAxis (toEnum . fromEnum $ doPolarizationCorrection)

  return (DataFrameSpace img space att)

----------
-- Pipe --
----------

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
  let mSkipFirstPoints = binocularsConfig'Common'SkipFirstPoints common
  let mSkipLastPoints = binocularsConfig'Common'SkipLastPoints common
  let doPolarizationCorrection = binocularsConfig'Common'PolarizationCorrection common

  -- directly from the specific config
  let mlimits = binocularsConfig'QCustom'ProjectionLimits conf
  let res = binocularsConfig'QCustom'ProjectionResolution conf
  let surfaceOrientation = binocularsConfig'QCustom'HklBinocularsSurfaceOrientationEnum conf
  let datapaths = binocularsConfig'QCustom'DataPath conf
  let subprojection = fromJust (binocularsConfig'QCustom'SubProjection conf) -- should not be Maybe
  let projectionType = binocularsConfig'QCustom'ProjectionType conf
  let (Degree uqx) = binocularsConfig'QCustom'Uqx conf
  let (Degree uqy) = binocularsConfig'QCustom'Uqy conf
  let (Degree uqz) = binocularsConfig'QCustom'Uqz conf
  let mSampleAxis = binocularsConfig'QCustom'SampleAxis conf

  -- built from the config
  output' <- liftIO $ destination' projectionType (Just subprojection) inputRange mlimits destination overwrite
  filenames <- InputFn'List <$> files nexusDir inputRange tmpl
  pixels <- liftIO $ getPixelsCoordinates det centralPixel' sampleDetectorDistance detrot NoNormalisation

  logDebugNSH filenames

  -- compute the jobs

  let fns = concatMap (replicate 1) (toList filenames)
  chunks <- liftIO $ runSafeT $ toListM $ each fns >-> chunkP mSkipFirstPoints mSkipLastPoints datapaths
  let ntot = sum (Prelude.map clength chunks)
  let jobs = chunk (quot ntot cap) chunks

  -- log parameters

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
    >-> framesP datapaths
    >-> project det 3 (spaceQCustom det pixels res surfaceOrientation mlimits subprojection uqx uqy uqz mSampleAxis doPolarizationCorrection)
    >-> accumulateP c

  logDebugN "stop gessing final cube size"

  -- do the final projection

  logInfoN $ pack $ printf "let's do a QCustom projection of %d %s image(s) on %d core(s)" ntot (show det) cap

  liftIO $ withProgressBar ntot $ \pb -> do
    r' <- mapConcurrently (\job -> withCubeAccumulator guessed $ \c ->
                             runSafeT $ runEffect $
                             each job
                             >-> Pipes.Prelude.map (\(Chunk fn f t) -> (fn, [f..t]))
                             >-> framesP datapaths
                             >-> Pipes.Prelude.filter (\(DataFrameQCustom _ _ img _ _ _ _) -> filterSumImage mImageSumMax img)
                             >-> project det 3 (spaceQCustom det pixels res surfaceOrientation mlimits subprojection uqx uqy uqz mSampleAxis doPolarizationCorrection)
                             >-> tee (accumulateP c)
                             >-> progress pb
                         ) jobs
    saveCube output' (unpack . serializeConfig $ conf) r'


instance ChunkP [DSDataFrameQCustom DSPath] where
    chunkP mSkipFirst mSkipLast p =
      skipMalformed $ forever $ do
      sfp <- await
      withScanFileP sfp $ \f' ->
        withDataSourcesP f' p $ \p' -> do
          (DataSourceShape'Range (Z :. f) (Z :. t)) <- ds'Shape p'
          yield $ cclip (fromMaybe 0 mSkipFirst) (fromMaybe 0 mSkipLast) (Chunk sfp f (t - 1))

instance FramesP [DSDataFrameQCustom DSPath] DataFrameQCustom where
    framesP p =
        skipMalformed $ forever $ do
          (fp, js) <- await
          withScanFileP fp $ \f ->
            withDataSourcesP f p $ \ g ->
            forM_ js (tryYield . extract1DStreamValue g)

---------
-- Cmd --
---------

processQCustom :: (MonadLogger m, MonadThrow m, MonadIO m) => Maybe FilePath -> Maybe ConfigRange -> m ()
processQCustom mf mr = cmd processQCustomP mf (Args'QCustomProjection mr)

newQCustom :: (MonadIO m, MonadLogger m, MonadThrow m)
           => Path Abs Dir -> m ()
newQCustom cwd = do
  let conf = defaultConfig
             { binocularsConfig'QCustom'Common = defaultConfig
                                                 { binocularsConfig'Common'Nexusdir = Just cwd }
             }
  liftIO $ Data.Text.IO.putStr $ serializeConfig conf

updateQCustom :: (MonadIO m, MonadLogger m, MonadThrow m)
              => Maybe FilePath -> Maybe ConfigRange -> m ()
updateQCustom mf mr = cmd (pure ()) mf (Args'QCustomProjection mr)

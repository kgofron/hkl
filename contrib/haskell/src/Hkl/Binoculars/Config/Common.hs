{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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

module Hkl.Binoculars.Config.Common
    ( BinocularsConfig'Common(..)
    , default'BinocularsConfig'Common
    , elemF
    , elemFMb
    , parse'BinocularsConfig'Common
    , parseFDef
    , parseMb
    , parseMbDef
    ) where

import           Control.Applicative               ((<|>))
import           Data.Array.Repa.Index             (DIM2)
import           Data.HashMap.Lazy                 (fromList)
import           Data.Ini                          (Ini (..))
import           Data.Ini.Config                   (fieldMbOf, parseIniFile,
                                                    section)
import           Data.Ini.Config.Bidir             (FieldValue (..))
import           Data.List.NonEmpty                (NonEmpty (..))
import           Data.Text                         (Text)
import           GHC.Generics                      (Generic)
import           Generic.Random                    (genericArbitraryU)
import           Numeric.Interval                  (singleton)
import           Numeric.Units.Dimensional.Prelude (degree, meter, (*~))
import           Path                              (Abs, Dir, Path)
import           Test.QuickCheck                   (Arbitrary (..))

import           Hkl.Binoculars.Config
import           Hkl.C.Binoculars
import           Hkl.Detector
import           Hkl.Orphan                        ()

data BinocularsConfig'Common
  = BinocularsConfig'Common
    { binocularsConfig'Common'NCores                 :: NCores
    , binocularsConfig'Common'Destination            :: DestinationTmpl
    , binocularsConfig'Common'Overwrite              :: Bool
    , binocularsConfig'Common'InputType              :: InputType
    , binocularsConfig'Common'Nexusdir               :: Maybe (Path Abs Dir)
    , binocularsConfig'Common'Tmpl                   :: Maybe InputTmpl
    , binocularsConfig'Common'InputRange             :: ConfigRange
    , binocularsConfig'Common'Detector               :: Detector Hkl DIM2
    , binocularsConfig'Common'Centralpixel           :: (Int, Int)
    , binocularsConfig'Common'Sdd                    :: Meter
    , binocularsConfig'Common'Detrot                 :: Degree
    , binocularsConfig'Common'AttenuationCoefficient :: Maybe Double
    , binocularsConfig'Common'AttenuationMax         :: Maybe Float
    , binocularsConfig'Common'Maskmatrix             :: Maybe MaskLocation
    , binocularsConfig'Common'Wavelength             :: Maybe Double
    , binocularsConfig'Common'ImageSumMax            :: Maybe Double
    } deriving (Eq, Show, Generic)

default'BinocularsConfig'Common :: BinocularsConfig'Common
default'BinocularsConfig'Common
  = BinocularsConfig'Common
    { binocularsConfig'Common'NCores = NCores 4
    , binocularsConfig'Common'Destination = DestinationTmpl "{projection}_{first}-{last}_{limits}.h5"
    , binocularsConfig'Common'Overwrite = False
    , binocularsConfig'Common'InputType = SixsFlyScanUhv
    , binocularsConfig'Common'Nexusdir = Nothing
    , binocularsConfig'Common'Tmpl = Nothing
    , binocularsConfig'Common'InputRange  = ConfigRange (InputRange (Numeric.Interval.singleton 1) :| [])
    , binocularsConfig'Common'Detector = defaultDetector
    , binocularsConfig'Common'Centralpixel = (0, 0)
    , binocularsConfig'Common'Sdd = Meter (1 *~ meter)
    , binocularsConfig'Common'Detrot = Degree (0 *~ degree)
    , binocularsConfig'Common'AttenuationCoefficient = Nothing
    , binocularsConfig'Common'AttenuationMax = Nothing
    , binocularsConfig'Common'Maskmatrix = Nothing
    , binocularsConfig'Common'Wavelength = Nothing
    , binocularsConfig'Common'ImageSumMax = Nothing
    }

instance Arbitrary BinocularsConfig'Common where
  arbitrary = genericArbitraryU

instance ToIni  BinocularsConfig'Common where
  toIni c = Ini { iniSections = fromList [ ("dispatcher", elemF    "ncores" (binocularsConfig'Common'NCores c)
                                                          <> elemF "destination" (binocularsConfig'Common'Destination c)
                                                          <> elemF "overwrite" (binocularsConfig'Common'Overwrite c)
                                           )
                                         ,  ("input", elemF      "type" (binocularsConfig'Common'InputType c)
                                                      <> elemFMb "nexusdir" (binocularsConfig'Common'Nexusdir c)
                                                      <> elemFMb "inputtmpl" (binocularsConfig'Common'Tmpl c)
                                                      <> elemF   "inputrange" (binocularsConfig'Common'InputRange c)
                                                      <> elemF   "detector" (binocularsConfig'Common'Detector c)
                                                      <> elemF   "centralpixel" (binocularsConfig'Common'Centralpixel c)
                                                      <> elemF   "sdd" (binocularsConfig'Common'Sdd c)
                                                      <> elemF   "detrot" (binocularsConfig'Common'Detrot c)
                                                      <> elemFMb "attenuation_coefficient" (binocularsConfig'Common'AttenuationCoefficient c)
                                                      <> elemFMb "attenuation_max" (binocularsConfig'Common'AttenuationMax c)
                                                      <> elemFMb "maskmatrix" (binocularsConfig'Common'Maskmatrix c)
                                                      <> elemFMb "wavelength" (binocularsConfig'Common'Wavelength c)
                                                      <> elemFMb "image_sum_max" (binocularsConfig'Common'ImageSumMax c)
                                            )
                                         ]

                , iniGlobals = []
                }

parse'BinocularsConfig'Common :: Text -> Maybe ConfigRange -> Capabilities -> Either String BinocularsConfig'Common
parse'BinocularsConfig'Common cfg mr (Capabilities ncapmax ncoresmax)
  = do
    -- section dispatcher
    ncores <- eitherF error (parse' cfg "dispatcher" "ncores") $ \mb -> do
      let ns = case mb of
            Nothing -> [ncapmax, ncoresmax - 1]
            Just b  -> [b, ncapmax, ncoresmax -1]
      pure $ NCores (minimum ns)
    destination <- parseFDef cfg "dispatcher" "destination" (binocularsConfig'Common'Destination default'BinocularsConfig'Common)
    overwrite <- parseFDef cfg "dispatcher" "overwrite" (binocularsConfig'Common'Overwrite default'BinocularsConfig'Common)

    -- section input
    inputtype <- parseFDef cfg "input" "type" (binocularsConfig'Common'InputType default'BinocularsConfig'Common)
    nexusdir <- parseMb cfg "input" "nexusdir"
    inputtmpl <- parseMb cfg "input" "inputtmpl"
    inputrange <- eitherF error (parse' cfg "input" "inputrange") $ \mb -> do
      let mr' = mr <|> mb
      case mr' of
        Nothing -> error "please provide an input range either in the config file with the \"inputrange\" key under the \"input\" section, or on the command line"
        Just r -> pure r
    detector <- parseFDef cfg "input" "detector" (case inputtype of
                                                   CristalK6C -> mkDetector HklBinocularsDetectorEnum'XpadFlatCorrected
                                                   MarsFlyscan -> mkDetector HklBinocularsDetectorEnum'MerlinMedipix3rxQuad
                                                   MarsSbs -> mkDetector HklBinocularsDetectorEnum'MerlinMedipix3rxQuad
                                                   SixsFlyMedH -> binocularsConfig'Common'Detector default'BinocularsConfig'Common
                                                   SixsFlyMedV -> binocularsConfig'Common'Detector default'BinocularsConfig'Common
                                                   SixsFlyMedVEiger -> mkDetector HklBinocularsDetectorEnum'DectrisEiger1M
                                                   SixsFlyMedVS70 -> mkDetector HklBinocularsDetectorEnum'ImxpadS70
                                                   SixsFlyScanUhv -> binocularsConfig'Common'Detector default'BinocularsConfig'Common
                                                   SixsFlyScanUhv2 -> binocularsConfig'Common'Detector default'BinocularsConfig'Common
                                                   SixsFlyScanUhvGisaxsEiger -> mkDetector HklBinocularsDetectorEnum'DectrisEiger1M
                                                   SixsFlyScanUhvTest -> binocularsConfig'Common'Detector default'BinocularsConfig'Common
                                                   SixsFlyScanUhvUfxc -> mkDetector HklBinocularsDetectorEnum'Ufxc
                                                   SixsSbsFixedDetector -> binocularsConfig'Common'Detector default'BinocularsConfig'Common
                                                   SixsSbsMedH -> binocularsConfig'Common'Detector default'BinocularsConfig'Common
                                                   SixsSbsMedHFixDetector -> binocularsConfig'Common'Detector default'BinocularsConfig'Common
                                                   SixsSbsMedV -> binocularsConfig'Common'Detector default'BinocularsConfig'Common
                                                   SixsSbsMedVFixDetector -> binocularsConfig'Common'Detector default'BinocularsConfig'Common
                                                   SixsSbsUhv -> binocularsConfig'Common'Detector default'BinocularsConfig'Common)
    centralpixel <- eitherF error (parse' cfg "input" "centralpixel") $ \mc -> do
      case mc of
        Nothing -> pure (binocularsConfig'Common'Centralpixel default'BinocularsConfig'Common)
        Just c -> if c `inDetector` detector
                 then pure c
                 else error $ "The central pixel " <> show c <> " is not compatible with the detector"
    sdd <- parseFDef cfg "input" "sdd" (binocularsConfig'Common'Sdd default'BinocularsConfig'Common)
    detrot <- parseFDef cfg "input" "detrot" (binocularsConfig'Common'Detrot default'BinocularsConfig'Common)
    attenuation_coefficient <- parseMb cfg "input" "attenuation_coefficient"
    attenuation_max <- parseMb cfg "input" "attenuation_max"
    maskmatrix <- parseMb cfg "input" "maskmatrix"
    wavelength <- parseMb cfg "input" "wavelength"
    image_sum_max <- parseMb cfg "input" "image_sum_max"

    -- customize a bunch of parameters

    pure $ BinocularsConfig'Common ncores destination overwrite inputtype nexusdir inputtmpl inputrange detector centralpixel sdd detrot attenuation_coefficient attenuation_max maskmatrix wavelength image_sum_max

parse' :: HasFieldValue b => Text -> Text -> Text -> Either String (Maybe b)
parse' c s f = parseIniFile c $ section s (fieldMbOf f auto')

eitherF :: (t1 -> p) -> Either t1 t2 -> (t2 -> p) -> p
eitherF fa (Left a)  _ = fa a
eitherF _ (Right b) fb = fb b

parseF :: HasFieldValue b
       => Text -> Text -> Text -> (Maybe b -> Either String r) -> Either String r
parseF c s f = eitherF error (parse' c s f)

parseFDef :: HasFieldValue p
          => Text -> Text -> Text -> p -> Either String p
parseFDef c s f def = parseF c s f $ \mb -> case mb of
                                             Nothing -> Right def
                                             Just b  -> Right b

parseMb ::  HasFieldValue r
        => Text -> Text -> Text -> Either String (Maybe r)
parseMb c s f = eitherF error (parse' c s f) pure

parseMbDef :: HasFieldValue r
           => Text -> Text -> Text -> Maybe r -> Maybe r
parseMbDef c s f def = eitherF error (parse' c s f) (\mb -> mb <|> def)

elemF :: HasFieldValue a => Text -> a -> [(Text, Text)]
elemF k v = [(k,  fvEmit fieldvalue v)]

elemFMb :: HasFieldValue a => Text -> Maybe a -> [(Text, Text)]
elemFMb k = maybe [("# " <> k, "")] (elemF k)

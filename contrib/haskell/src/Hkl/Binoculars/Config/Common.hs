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
    , elemFDef
    , elemFDef'
    , elemFMbDef
    , elemFMbDef'
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
import           Data.Maybe                        (fromMaybe)
import           Data.Text                         (Text, pack)
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
    , binocularsConfig'Common'InputType = SixsFlyUhv
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

-- TODO prendre les valeurs par default definie precedemment.
instance ToIni  BinocularsConfig'Common where
  toIni c = Ini { iniSections = fromList [ ("dispatcher", elemFDef "ncores" binocularsConfig'Common'NCores c default'BinocularsConfig'Common
                                                          [ "the number of cores use for computation."
                                                          , "the effective number of core used depends on the real number of core available on your computer."
                                                          , "at maximum this value could be one less that the pysical number of core."
                                                          ]
                                                          <> elemFDef "destination" binocularsConfig'Common'Destination c default'BinocularsConfig'Common
                                                          [ "the template used to produce the destination file"
                                                          , ""
                                                          , "You can write what you want in the field but it need to end with .h5"
                                                          , "The generated file is an hdf5 file. You can add in the text some specific"
                                                          , "string which will be replace with values extracted from the configuration"
                                                          , "when saving the file."
                                                          , ""
                                                          , "These parameters are:"
                                                          , "  `{first}`      - replaced by the first scan numer of the serie"
                                                          , "  `{last}`       - replaced by the last scan numer of the serie"
                                                          , "  `{limits}`     - replaced by the limits of the final cube or nolimits"
                                                          , "  `{projection}` - the projection name"
                                                          ]
                                                          <> elemFDef "overwrite" binocularsConfig'Common'Overwrite c default'BinocularsConfig'Common
                                                          [ " `true` - the output file name is always the same, so it is overwriten when"
                                                          , "recomputed with the same parameter."
                                                          , " `false` - the output is modifier and `_<number>` is added to the filename in order"
                                                          , " to avoid overwriting them."
                                                          ]
                                           )
                                         ,  ("input", elemFDef "type" binocularsConfig'Common'InputType c default'BinocularsConfig'Common
                                                      ([ "Define the experimental setup and the type of scan used to acquire the data"
                                                       , ""
                                                       , "the list of the available values are:"
                                                       , ""
                                                       ]
                                                       <> [" - " <> fvEmit fieldvalue v | v <- [minBound..maxBound :: InputType]]
                                                       <> [ ""
                                                          , "`sbs` means step by step scan."
                                                          , ""
                                                          , "`fly` or `flyscan` means flyscan."
                                                          , ""
                                                          , "`medh` or `medv` or `uhv` refers to the diffractometer."
                                                          , ""
                                                          , "Some configurations specify the detector like eiger/s70..."
                                                          , "look at the `detector` parameter if you need to select another one."
                                                          , ""
                                                          , "This parameter defines how to find the datas in the hdf5 file."
                                                          ]
                                                      )
                                                      <> elemFMbDef "nexusdir" binocularsConfig'Common'Nexusdir c default'BinocularsConfig'Common
                                                      [ "Directory path where binoculars-ng looks for the data."
                                                      , ""
                                                      , "All sub directories are explored."
                                                      , "It can be relative to the current directory or absolute"
                                                      ]
                                                      <> elemFMbDef "inputtmpl" binocularsConfig'Common'Tmpl c default'BinocularsConfig'Common
                                                      [ "data files are matched with this template in order to decide if it must be used for computation."
                                                      , ""
                                                      , "the template is pre-processed with the printf logic and an integer value comming from the the `inputrange`"
                                                      , "for example with the default value `%05d` and this `inputrange=1-10`, all supported files which contains"
                                                      , "00001, 00002, ... 00010 are used during the computation."
                                                      ]
                                                      <> elemFDef "inputrange" binocularsConfig'Common'InputRange c default'BinocularsConfig'Common
                                                      [ "Indexes of the scans you want to process"
                                                      , ""
                                                      , "There are two ways to enter these values."
                                                      , "  `n`         - a single scan with index n"
                                                      , "  `n-m`       - a range of scans from n to m (included)"
                                                      , ""
                                                      , "  `n-m,p,...` - combination of scans indexes separated by a coma (no space allowed)."
                                                      ]
                                                      <> elemFDef "detector" binocularsConfig'Common'Detector c default'BinocularsConfig'Common
                                                      ([ "The detector name"
                                                       , ""
                                                       , "In order to process the data, we need the 3D position of each pixel"
                                                       , "in the laboratory basis. Since every detector has it's own geometry,"
                                                       , "it is necessary to provide this information unambiguously."
                                                       , ""
                                                       , "Some data files contain more than one detectors, so this parameter allows"
                                                       , "to select the right one."
                                                       , ""
                                                       , "the list of known detectors is:"]
                                                       <> [" - " <> pack n | (Detector2D _ n _) <- detectors]
                                                      )
                                                      <> elemFDef "centralpixel" binocularsConfig'Common'Centralpixel c default'BinocularsConfig'Common
                                                      [ "x,y coordinates in pixels of the direct beam on the detector."
                                                      , ""
                                                      , "with `sdd` and `detrot` this parameter allow to set the"
                                                      , "detector position on the detector's arm when all motors"
                                                      , "are set equal to `0`."
                                                      ]
                                                      <> elemFDef "sdd" binocularsConfig'Common'Sdd c default'BinocularsConfig'Common
                                                      [ "sample-detector-distance expressed in meter."
                                                      , ""
                                                      , "with `centralpixel` and `detrot` this parameter allow to set the"
                                                      , "detector position on the detector's arm when all motors"
                                                      , "are set equal to `0`."
                                                      ]
                                                      <> elemFDef "detrot" binocularsConfig'Common'Detrot c default'BinocularsConfig'Common
                                                      [ "rotation of the detector along the x axis expressed in Degrees."
                                                      , ""
                                                      , "with `centralpixel` and `sdd` this parameter allow to set the"
                                                      , "detector position on the detector's arm when all motors"
                                                      , "are set equal to `0`."
                                                      ]
                                                      <> elemFMbDef "attenuation_coefficient" binocularsConfig'Common'AttenuationCoefficient c default'BinocularsConfig'Common
                                                      ["the attenuation coefficient used to correct the detector's data."
                                                      , ""
                                                      , " `<not set>` - no correction is applyed"
                                                      , " `value`     - a correction is applyed"
                                                      , ""
                                                      , "The `value` depends on the energy of the experiment."
                                                      , ""
                                                      , "On Sixs there is two different kind of attenuation system."
                                                      , " `new attenuation` - based on piezo actuators (fast)"
                                                      , " `old attenuation` - based on pneumatic` actuators (slower)"
                                                      , ""
                                                      , "both system save the same kind of data's in the data files."
                                                      , "An attenuation value `v` at each step of the scan which allows"
                                                      , "to compute the image correction with this formula:"
                                                      , ""
                                                      , "  `attenuation_coef ** v`"
                                                      , ""
                                                      , "The only difference is a shift of the index for the the new attenuation."
                                                      , "The applyed correction for the n-th point is computed with the `value`"
                                                      , "at n + 2"
                                                      ]
                                                      <> elemFMbDef "attenuation_max" binocularsConfig'Common'AttenuationMax c default'BinocularsConfig'Common
                                                      [ "maximum attenuation allow for the attenuation correction."
                                                      , ""
                                                      , " `<not set>` - always apply the attenuation correction."
                                                      , " `max`       - apply the attenuation if `v` is lower or equal to `max`"
                                                      , ""
                                                      , "`v` is the value stored by the attenuation system in the data file."
                                                      , ""
                                                      , "This parameter has an effect only if the `attenuation_coefficient`"
                                                      , "was previously set."
                                                      ]
                                                      <> elemFMbDef "maskmatrix" binocularsConfig'Common'Maskmatrix c default'BinocularsConfig'Common
                                                      [ "name of the file which contain the detector mask or `default`"
                                                      , ""
                                                      , "The supported type of file is for now only .npy"
                                                      , ""
                                                      , "the `default` value can be set in order to use the default"
                                                      , "mask of the detector. Some of them have by default some area"
                                                      , "which required to be masked (pixels with non standard surface, "
                                                      , "area with holes)."
                                                      , ""
                                                      , "Most of the time a mask file was generated during ther experiment."
                                                      ]
                                                      <> elemFMbDef "wavelength" binocularsConfig'Common'Wavelength c default'BinocularsConfig'Common
                                                      [ "overwrite the wavelength from the data file with the one provided."
                                                      , ""
                                                      , " `<not set>`  - use the data file wavelength"
                                                      , " `wavelength` - overwrite the data file value with this one."
                                                      ]
                                                      <> elemFMbDef "image_sum_max" binocularsConfig'Common'ImageSumMax c default'BinocularsConfig'Common
                                                      [ "maximum intensity value allow for the sum of all pixels in an image in order to process it."
                                                      , ""
                                                      , " `<not set>` - process all images."
                                                      , " `max`       - process images only if the sum of all the pixels is lower than `max`"
                                                      ]
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
    minputtype <- parseMb cfg "input" "type"
    let minputtypedeprecated = eitherF (const Nothing) (parse' cfg "input" "type") id
    let inputtype = case minputtype of
          Nothing -> case minputtypedeprecated of
                      Nothing -> binocularsConfig'Common'InputType default'BinocularsConfig'Common
                      Just deprecated -> case deprecated of
                                          SixsFlyMedVEiger -> SixsFlyMedV
                                          SixsFlyMedVS70 -> SixsFlyMedV
                                          SixsFlyScanUhvGisaxsEiger -> SixsFlyUhv
                                          SixsFlyScanUhvUfxc -> SixsFlyUhv
          Just i -> i
    nexusdir <- parseMb cfg "input" "nexusdir"
    inputtmpl <- parseMb cfg "input" "inputtmpl"
    inputrange <- eitherF error (parse' cfg "input" "inputrange") $ \mb -> do
      let mr' = mr <|> mb
      case mr' of
        Nothing -> error "please provide an input range either in the config file with the \"inputrange\" key under the \"input\" section, or on the command line"
        Just r -> pure r
    detector <- parseFDef cfg "input" "detector" (case minputtypedeprecated of
                                                    Nothing -> case inputtype of
                                                                CristalK6C -> mkDetector HklBinocularsDetectorEnum'XpadFlatCorrected
                                                                MarsFlyscan -> mkDetector HklBinocularsDetectorEnum'MerlinMedipix3rxQuad
                                                                MarsSbs -> mkDetector HklBinocularsDetectorEnum'MerlinMedipix3rxQuad
                                                                SixsFlyMedH -> binocularsConfig'Common'Detector default'BinocularsConfig'Common
                                                                SixsFlyMedHGisaxs -> binocularsConfig'Common'Detector default'BinocularsConfig'Common
                                                                SixsFlyMedV -> binocularsConfig'Common'Detector default'BinocularsConfig'Common
                                                                SixsFlyMedVGisaxs -> binocularsConfig'Common'Detector default'BinocularsConfig'Common
                                                                SixsFlyUhv -> binocularsConfig'Common'Detector default'BinocularsConfig'Common
                                                                SixsFlyUhvGisaxs -> binocularsConfig'Common'Detector default'BinocularsConfig'Common
                                                                SixsSbsMedH -> binocularsConfig'Common'Detector default'BinocularsConfig'Common
                                                                SixsSbsMedHGisaxs -> binocularsConfig'Common'Detector default'BinocularsConfig'Common
                                                                SixsSbsMedV -> binocularsConfig'Common'Detector default'BinocularsConfig'Common
                                                                SixsSbsMedVGisaxs -> binocularsConfig'Common'Detector default'BinocularsConfig'Common
                                                                SixsSbsUhv -> binocularsConfig'Common'Detector default'BinocularsConfig'Common
                                                                SixsSbsUhvGisaxs -> binocularsConfig'Common'Detector default'BinocularsConfig'Common
                                                    (Just deprecated) -> case deprecated of
                                                                          SixsFlyMedVEiger -> mkDetector HklBinocularsDetectorEnum'DectrisEiger1M
                                                                          SixsFlyMedVS70 -> mkDetector HklBinocularsDetectorEnum'ImxpadS70
                                                                          SixsFlyScanUhvGisaxsEiger -> mkDetector HklBinocularsDetectorEnum'DectrisEiger1M
                                                                          SixsFlyScanUhvUfxc -> mkDetector HklBinocularsDetectorEnum'Ufxc
                                                )

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

elemFDef' :: HasFieldComment w => Text -> (v -> w) -> v -> v -> [(Text, Text)]
elemFDef' k f v d = elemFDef k f v d (fieldComment (f v))


elemContent :: Text -> Text -> [Text] -> [Text]
elemContent k def cs =
  [ ""
  , k <> ":"
  , ""
  ]
  <> cs
  <> [ ""
     , "default value: `" <> def <> "`"
     , ""
     , "uncomment and edit the next line if you want to modify the value"
     ]

elemFDef ::  HasFieldValue w => Text -> (v -> w) -> v -> v -> [Text] -> [(Text, Text)]
elemFDef k f v d cs = [ ("#", l) | l <- ls ] <> [(k,  fvEmit fieldvalue (f v))]
  where
    ls :: [Text]
    ls = elemContent k defv cs

    defv :: Text
    defv = fvEmit fieldvalue (f d)

elemFMbDef' ::  HasFieldComment w => Text -> (v -> Maybe w) -> v -> v -> [(Text, Text)]
elemFMbDef' k f v d = elemFMbDef k f v d (fieldComment (fromMaybe undefined (f v)))

elemFMbDef ::  HasFieldValue w => Text -> (v -> Maybe w) -> v -> v -> [Text] -> [(Text, Text)]
elemFMbDef k f v d cs = [ ("#", l) | l <- ls ] <> maybe [("# " <> k, "")] (\w -> [(k, fvEmit fieldvalue w)]) (f v)
  where
    ls :: [Text]
    ls = elemContent k defv cs

    defv :: Text
    defv = case f d of
             Nothing -> "<not set>"
             Just w  -> fvEmit fieldvalue w

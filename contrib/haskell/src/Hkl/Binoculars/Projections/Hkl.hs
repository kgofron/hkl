{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


{-
    Copyright  : Copyright (C) 2014-2022 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.Binoculars.Projections.Hkl
    ( BinocularsConfigHkl(..)
    ) where


import           Control.Lens                      (makeLenses)
import           Data.Array.Repa.Index             (DIM2)
import           Data.Ini.Config.Bidir             (IniSpec, field, section,
                                                    (.=), (.=?))
import           Data.Text                         (Text)
import           Numeric.Units.Dimensional.Prelude (meter, (*~))
import           Path                              (Abs, Dir, Path)

import           Hkl.Binoculars.Config
import           Hkl.Detector


data BinocularsConfigHkl = BinocularsConfigHkl
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
  , _binocularsConfigHklMaskmatrix             :: Maybe Text
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
  , _binocularsConfigHklProjectionResolution   :: [Double]
  , _binocularsConfigHklProjectionLimits       :: Maybe [Limits]
  } deriving (Eq, Show)

makeLenses ''BinocularsConfigHkl

binocularsConfigHklDefault :: BinocularsConfigHkl
binocularsConfigHklDefault = BinocularsConfigHkl
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
  , _binocularsConfigHklProjectionResolution = [0.01, 0.01, 0.01]
  , _binocularsConfigHklProjectionLimits  = Nothing
  }

binocularsConfigHklSpec :: IniSpec BinocularsConfigHkl ()
binocularsConfigHklSpec = do
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
  section "projection" $ do
    binocularsConfigHklProjectionType .= field "type" auto
    binocularsConfigHklProjectionResolution .= field "resolution" auto
    binocularsConfigHklProjectionLimits .=? field "limits" auto

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

module Hkl.Binoculars.Projections.QparQper
    ( BinocularsConfigQparQper(..)
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


data BinocularsConfigQparQper = BinocularsConfigQparQper
  { _binocularsConfigQparQperNcore                  :: Maybe Int
  , _binocularsConfigQparQperDestination            :: DestinationTmpl
  , _binocularsConfigQparQperOverwrite              :: Bool
  , _binocularsConfigQparQperInputType              :: InputType
  , _binocularsConfigQparQperNexusdir               :: Maybe (Path Abs Dir)
  , _binocularsConfigQparQperTmpl                   :: Maybe InputTmpl
  , _binocularsConfigQparQperInputRange             :: Maybe ConfigRange
  , _binocularsConfigQparQperDetector               :: Maybe (Detector Hkl DIM2)
  , _binocularsConfigQparQperCentralpixel           :: (Int, Int)
  , _binocularsConfigQparQperSdd                    :: Meter
  , _binocularsConfigQparQperDetrot                 :: Maybe Degree
  , _binocularsConfigQparQperAttenuationCoefficient :: Maybe Double
  , _binocularsConfigQparQperSurfaceOrientation     :: Maybe SurfaceOrientation
  , _binocularsConfigQparQperMaskmatrix             :: Maybe Text
  , _binocularsConfigQparQperWavelength             :: Maybe Angstrom
  , _binocularsConfigQparQperProjectionType         :: ProjectionType
  , _binocularsConfigQparQperProjectionResolution   :: [Double]
  , _binocularsConfigQparQperProjectionLimits       :: Maybe [Limits]
  } deriving (Eq, Show)

makeLenses ''BinocularsConfigQparQper

binocularsConfigQparQperDefault :: BinocularsConfigQparQper
binocularsConfigQparQperDefault = BinocularsConfigQparQper
  { _binocularsConfigQparQperNcore = Nothing
  , _binocularsConfigQparQperDestination = DestinationTmpl "."
  , _binocularsConfigQparQperOverwrite = False
  , _binocularsConfigQparQperInputType = SixsFlyScanUhv
  , _binocularsConfigQparQperNexusdir = Nothing
  , _binocularsConfigQparQperTmpl = Nothing
  , _binocularsConfigQparQperInputRange  = Nothing
  , _binocularsConfigQparQperDetector = Nothing
  , _binocularsConfigQparQperCentralpixel = (0, 0)
  , _binocularsConfigQparQperSdd = Meter (1 *~ meter)
  , _binocularsConfigQparQperDetrot = Nothing
  , _binocularsConfigQparQperAttenuationCoefficient = Nothing
  , _binocularsConfigQparQperSurfaceOrientation = Just SurfaceOrientationVertical
  , _binocularsConfigQparQperMaskmatrix = Nothing
  , _binocularsConfigQparQperWavelength = Nothing
  , _binocularsConfigQparQperProjectionType = QxQyQzProjection
  , _binocularsConfigQparQperProjectionResolution = [0.01, 0.01]
  , _binocularsConfigQparQperProjectionLimits  = Nothing
  }

binocularsConfigQparQperSpec :: IniSpec BinocularsConfigQparQper ()
binocularsConfigQparQperSpec = do
  section "dispatcher" $ do
    binocularsConfigQparQperNcore .=? field "ncores" auto
    binocularsConfigQparQperDestination .= field "destination" auto
    binocularsConfigQparQperOverwrite .= field "overwrite" auto
  section "input" $ do
    binocularsConfigQparQperInputType .= field "type" auto
    binocularsConfigQparQperNexusdir .=? field "nexusdir" auto
    binocularsConfigQparQperTmpl .=? field "inputtmpl" auto
    binocularsConfigQparQperInputRange .=? field "inputrange" auto
    binocularsConfigQparQperDetector .=? field "detector" auto
    binocularsConfigQparQperCentralpixel .= field "centralpixel" auto
    binocularsConfigQparQperSdd .= field "sdd" auto
    binocularsConfigQparQperDetrot .=? field "detrot" auto
    binocularsConfigQparQperAttenuationCoefficient .=? field "attenuation_coefficient" auto
    binocularsConfigQparQperSurfaceOrientation .=? field "surface_orientation" auto
    binocularsConfigQparQperMaskmatrix .=? field "maskmatrix" auto
    binocularsConfigQparQperWavelength .=? field "wavelength" auto
  section "projection" $ do
    binocularsConfigQparQperProjectionType .= field "type" auto
    binocularsConfigQparQperProjectionResolution .= field "resolution" auto
    binocularsConfigQparQperProjectionLimits .=? field "limits" auto

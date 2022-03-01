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

module Hkl.Binoculars.Projections.QxQyQz
    ( BinocularsConfigQxQyQz(..)
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


data BinocularsConfigQxQyQz = BinocularsConfigQxQyQz
  { _binocularsConfigQxQyQzNcore                  :: Maybe Int
  , _binocularsConfigQxQyQzDestination            :: DestinationTmpl
  , _binocularsConfigQxQyQzOverwrite              :: Bool
  , _binocularsConfigQxQyQzInputType              :: InputType
  , _binocularsConfigQxQyQzNexusdir               :: Maybe (Path Abs Dir)
  , _binocularsConfigQxQyQzTmpl                   :: Maybe InputTmpl
  , _binocularsConfigQxQyQzInputRange             :: Maybe ConfigRange
  , _binocularsConfigQxQyQzDetector               :: Maybe (Detector Hkl DIM2)
  , _binocularsConfigQxQyQzCentralpixel           :: (Int, Int)
  , _binocularsConfigQxQyQzSdd                    :: Meter
  , _binocularsConfigQxQyQzDetrot                 :: Maybe Degree
  , _binocularsConfigQxQyQzAttenuationCoefficient :: Maybe Double
  , _binocularsConfigQxQyQzSurfaceOrientation     :: Maybe SurfaceOrientation
  , _binocularsConfigQxQyQzMaskmatrix             :: Maybe Text
  , _binocularsConfigQxQyQzWavelength             :: Maybe Angstrom
  , _binocularsConfigQxQyQzProjectionType         :: ProjectionType
  , _binocularsConfigQxQyQzProjectionResolution   :: [Double]
  , _binocularsConfigQxQyQzProjectionLimits       :: Maybe [Limits]
  } deriving (Eq, Show)

makeLenses ''BinocularsConfigQxQyQz

binocularsConfigQxQyQzDefault :: BinocularsConfigQxQyQz
binocularsConfigQxQyQzDefault = BinocularsConfigQxQyQz
  { _binocularsConfigQxQyQzNcore = Nothing
  , _binocularsConfigQxQyQzDestination = DestinationTmpl "."
  , _binocularsConfigQxQyQzOverwrite = False
  , _binocularsConfigQxQyQzInputType = SixsFlyScanUhv
  , _binocularsConfigQxQyQzNexusdir = Nothing
  , _binocularsConfigQxQyQzTmpl = Nothing
  , _binocularsConfigQxQyQzInputRange  = Nothing
  , _binocularsConfigQxQyQzDetector = Nothing
  , _binocularsConfigQxQyQzCentralpixel = (0, 0)
  , _binocularsConfigQxQyQzSdd = Meter (1 *~ meter)
  , _binocularsConfigQxQyQzDetrot = Nothing
  , _binocularsConfigQxQyQzAttenuationCoefficient = Nothing
  , _binocularsConfigQxQyQzSurfaceOrientation = Just SurfaceOrientationVertical
  , _binocularsConfigQxQyQzMaskmatrix = Nothing
  , _binocularsConfigQxQyQzWavelength = Nothing
  , _binocularsConfigQxQyQzProjectionType = QxQyQzProjection
  , _binocularsConfigQxQyQzProjectionResolution = [0.01, 0.01, 0.01]
  , _binocularsConfigQxQyQzProjectionLimits  = Nothing
  }

binocularsConfigQxQyQzSpec :: IniSpec BinocularsConfigQxQyQz ()
binocularsConfigQxQyQzSpec = do
  section "dispatcher" $ do
    binocularsConfigQxQyQzNcore .=? field "ncores" auto
    binocularsConfigQxQyQzDestination .= field "destination" auto
    binocularsConfigQxQyQzOverwrite .= field "overwrite" auto
  section "input" $ do
    binocularsConfigQxQyQzInputType .= field "type" auto
    binocularsConfigQxQyQzNexusdir .=? field "nexusdir" auto
    binocularsConfigQxQyQzTmpl .=? field "inputtmpl" auto
    binocularsConfigQxQyQzInputRange .=? field "inputrange" auto
    binocularsConfigQxQyQzDetector .=? field "detector" auto
    binocularsConfigQxQyQzCentralpixel .= field "centralpixel" auto
    binocularsConfigQxQyQzSdd .= field "sdd" auto
    binocularsConfigQxQyQzDetrot .=? field "detrot" auto
    binocularsConfigQxQyQzAttenuationCoefficient .=? field "attenuation_coefficient" auto
    binocularsConfigQxQyQzSurfaceOrientation .=? field "surface_orientation" auto
    binocularsConfigQxQyQzMaskmatrix .=? field "maskmatrix" auto
    binocularsConfigQxQyQzWavelength .=? field "wavelength" auto
  section "projection" $ do
    binocularsConfigQxQyQzProjectionType .= field "type" auto
    binocularsConfigQxQyQzProjectionResolution .= field "resolution" auto
    binocularsConfigQxQyQzProjectionLimits .=? field "limits" auto

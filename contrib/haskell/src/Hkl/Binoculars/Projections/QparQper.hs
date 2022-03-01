{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}

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
import           Data.Ini.Config.Bidir             (IniSpec, bool, field,
                                                    listWithSeparator, section,
                                                    text, (.=), (.=?))
import           Data.Text                         (Text)
import           Numeric.Units.Dimensional.NonSI   (angstrom)
import           Numeric.Units.Dimensional.Prelude (Angle, Length, degree,
                                                    meter, (*~))
import           Path                              (Abs, Dir, Path)

import           Prelude                           hiding (drop, length, lines,
                                                    putStr, readFile, take,
                                                    takeWhile, unlines, unwords)

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
  , _binocularsConfigQparQperSdd                    :: Length Double
  , _binocularsConfigQparQperDetrot                 :: Maybe (Angle Double)
  , _binocularsConfigQparQperAttenuationCoefficient :: Maybe Double
  , _binocularsConfigQparQperSurfaceOrientation     :: Maybe SurfaceOrientation
  , _binocularsConfigQparQperMaskmatrix             :: Maybe Text
  , _binocularsConfigQparQperWavelength             :: Maybe (Length Double)
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
  , _binocularsConfigQparQperSdd = 1 *~ meter
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
    binocularsConfigQparQperDestination .= field "destination" destinationTmpl
    binocularsConfigQparQperOverwrite .= field "overwrite" bool
  section "input" $ do
    binocularsConfigQparQperInputType .= field "type" inputType
    binocularsConfigQparQperNexusdir .=? field "nexusdir" pathAbsDir
    binocularsConfigQparQperTmpl .=? field "inputtmpl" inputTmpl
    binocularsConfigQparQperInputRange .=? field "inputrange" parsable
    binocularsConfigQparQperDetector .=? field "detector" auto
    binocularsConfigQparQperCentralpixel .= field "centralpixel" centralPixel
    binocularsConfigQparQperSdd .= field "sdd" (numberUnit meter)
    binocularsConfigQparQperDetrot .=? field "detrot" (numberUnit degree)
    binocularsConfigQparQperAttenuationCoefficient .=? field "attenuation_coefficient" auto
    binocularsConfigQparQperSurfaceOrientation .=? field "surface_orientation" surfaceOrientation
    binocularsConfigQparQperMaskmatrix .=? field "maskmatrix" text
    binocularsConfigQparQperWavelength .=? field "wavelength" (numberUnit angstrom)
  section "projection" $ do
    binocularsConfigQparQperProjectionType .= field "type" parsable
    binocularsConfigQparQperProjectionResolution .= field "resolution" (listWithSeparator "," auto)
    binocularsConfigQparQperProjectionLimits .=? field "limits" parsable

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-
    Copyright  : Copyright (C) 2014-2025 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.Binoculars.Projections.Config.Sample
    ( Args(..)
    , Config(..)
    , default'DataSourcePath'Sample
    , guess'DataSourcePath'Sample
    , overload'DataSourcePath'Sample
    ) where

import           Data.Aeson                               (FromJSON, ToJSON)
import           Data.HashMap.Lazy                        (fromList)
import           Data.Ini                                 (Ini (..))
import           GHC.Generics                             (Generic)

import           Hkl.Binoculars.Config
import           Hkl.Binoculars.Projections.Config.Common
import           Hkl.DataSource
import           Hkl.H5
import           Hkl.Lattice
import           Hkl.Parameter
import           Hkl.Sample
import           Hkl.Types

----------------
-- DataSource --
----------------

instance DataSource Sample where
  data DataSourcePath Sample
    = DataSourcePath'Sample
      (DataSourcePath Double) -- a
      (DataSourcePath Double) -- b
      (DataSourcePath Double) -- c
      (DataSourcePath Degree) -- alpha
      (DataSourcePath Degree) -- beta
      (DataSourcePath Degree) -- gamma
      (DataSourcePath Degree) -- ux
      (DataSourcePath Degree) -- uy
      (DataSourcePath Degree) -- uz
    | DataSourcePath'Sample'Or (DataSourcePath Sample) (DataSourcePath Sample)
    deriving (FromJSON, Generic, Show, ToJSON)

  data DataSourceAcq Sample
    = DataSourceAcq'Sample
      (DataSourceAcq Double)
      (DataSourceAcq Double)
      (DataSourceAcq Double)
      (DataSourceAcq Degree)
      (DataSourceAcq Degree)
      (DataSourceAcq Degree)
      (DataSourceAcq Degree)
      (DataSourceAcq Degree)
      (DataSourceAcq Degree)

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
  withDataSourceP f (DataSourcePath'Sample'Or l r) g = withDataSourcePOr f l r g

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

default'DataSourcePath'Sample :: DataSourcePath Sample
default'DataSourcePath'Sample = DataSourcePath'Sample
  (DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "SIXS/I14-C-CX2__EX__DIFF-UHV__#1/A"))
  (DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "SIXS/I14-C-CX2__EX__DIFF-UHV__#1/B"))
  (DataSourcePath'Double(hdf5p $ grouppat 0 $ datasetp "SIXS/I14-C-CX2__EX__DIFF-UHV__#1/C"))
  (DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "SIXS/I14-C-CX2__EX__DIFF-UHV__#1/Alpha"))
  (DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "SIXS/I14-C-CX2__EX__DIFF-UHV__#1/Beta"))
  (DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "SIXS/I14-C-CX2__EX__DIFF-UHV__#1/Gamma"))
  (DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "SIXS/I14-C-CX2__EX__DIFF-UHV__#1/Ux"))
  (DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "SIXS/I14-C-CX2__EX__DIFF-UHV__#1/Uy"))
  (DataSourcePath'Degree(hdf5p $ grouppat 0 $ datasetp "SIXS/I14-C-CX2__EX__DIFF-UHV__#1/Uz"))


overload'DataSourcePath'Sample :: Config Sample
                               -> DataSourcePath Sample
                               -> DataSourcePath Sample
overload'DataSourcePath'Sample (BinocularsConfig'Sample ma mb mc malpha mbeta mgamma mux muy muz) (DataSourcePath'Sample pa pb pc palpha pbeta pgamma pux puy puz)
  = DataSourcePath'Sample
    (maybe pa DataSourcePath'Double'Const ma)
    (maybe pb DataSourcePath'Double'Const mb)
    (maybe pc DataSourcePath'Double'Const mc)
    (maybe palpha DataSourcePath'Degree'Const malpha)
    (maybe pbeta DataSourcePath'Degree'Const mbeta)
    (maybe pgamma DataSourcePath'Degree'Const mgamma)
    (maybe pux DataSourcePath'Degree'Const mux)
    (maybe puy DataSourcePath'Degree'Const muy)
    (maybe puz DataSourcePath'Degree'Const muz)
overload'DataSourcePath'Sample c (DataSourcePath'Sample'Or l r)
  = DataSourcePath'Sample'Or
    (overload'DataSourcePath'Sample c l)
    (overload'DataSourcePath'Sample c r)

guess'DataSourcePath'Sample :: Config Common
                           -> Config Sample
                           -> DataSourcePath Sample
guess'DataSourcePath'Sample common sample =
  do let inputType = binocularsConfig'Common'InputType common
     let samplePath' beamline device =
           DataSourcePath'Sample
           (DataSourcePath'Double(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "A"))
           (DataSourcePath'Double(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "B"))
           (DataSourcePath'Double(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "C"))
           (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "alpha"))
           (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "beta"))
           (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "gamma"))
           (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "Ux"))
           (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "Uy"))
           (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "Uz"))
     let sampleMarsPath beamline device =
           DataSourcePath'Sample
           (DataSourcePath'Double(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "a"))
           (DataSourcePath'Double(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "b"))
           (DataSourcePath'Double(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "c"))
           (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "alpha"))
           (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "beta"))
           (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "gamma"))
           (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "u_x"))
           (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "u_y"))
           (DataSourcePath'Degree(hdf5p $ grouppat 0 $ groupp beamline $ groupp device $ datasetp "u_z"))
     let cristalSamplePath = samplePath' "NOBEAMLINE" "NOBEAMLINE"
     let diffabsSamplePath = samplePath' "DIFFABS" undefined
     let marsSamplePath = sampleMarsPath "MARS" "d03-1-cx2__ex__dif-cm_#1"
     let medHSamplePath = samplePath' "SIXS" "i14-c-cx1-ex-cm-med.h"
     let medVSamplePath = samplePath' "SIXS" "i14-c-cx1-ex-cm-med.v"
                          `DataSourcePath'Sample'Or`
                          samplePath' "SIXS" "i14-c-cx1-ex-cm-med.v_no_etaa"
     let uhvSamplePath  = samplePath' "SIXS" "I14-C-CX2__EX__DIFF-UHV__#1"
                          `DataSourcePath'Sample'Or`
                          samplePath' "SIXS" "i14-c-cx2-ex-cm-uhv"

     let samplePath = case inputType of
                        CristalK6C        -> cristalSamplePath
                        Custom            -> undefined
                        DiffabsCirpad     -> diffabsSamplePath
                        MarsFlyscan       -> marsSamplePath
                        MarsSbs           -> marsSamplePath
                        SixsFlyMedH       -> medHSamplePath
                        SixsFlyMedHGisaxs -> medHSamplePath
                        SixsFlyMedV       -> medVSamplePath
                        SixsFlyMedVGisaxs -> medVSamplePath
                        SixsFlyUhv        -> uhvSamplePath
                        SixsFlyUhvGisaxs  -> uhvSamplePath
                        SixsSbsMedH       -> medHSamplePath
                        SixsSbsMedHGisaxs -> medHSamplePath
                        SixsSbsMedV       -> medVSamplePath
                        SixsSbsMedVGisaxs -> medVSamplePath
                        SixsSbsUhv        -> uhvSamplePath
                        SixsSbsUhvGisaxs  -> uhvSamplePath

     overload'DataSourcePath'Sample sample samplePath

------------
-- Config --
------------

instance HasIniConfig Sample where

    data Config Sample
        = BinocularsConfig'Sample
          { binocularsConfig'Sample'A     :: Maybe Double
          , binocularsConfig'Sample'B     :: Maybe Double
          , binocularsConfig'Sample'C     :: Maybe Double
          , binocularsConfig'Sample'Alpha :: Maybe Degree
          , binocularsConfig'Sample'Beta  :: Maybe Degree
          , binocularsConfig'Sample'Gamma :: Maybe Degree
          , binocularsConfig'Sample'Ux    :: Maybe Degree
          , binocularsConfig'Sample'Uy    :: Maybe Degree
          , binocularsConfig'Sample'Uz    :: Maybe Degree
          } deriving (Eq, Show, Generic)

    data Args Sample
        = Args'Sample

    defaultConfig
        = BinocularsConfig'Sample
          { binocularsConfig'Sample'A = Nothing
          , binocularsConfig'Sample'B = Nothing
          , binocularsConfig'Sample'C = Nothing
          , binocularsConfig'Sample'Alpha = Nothing
          , binocularsConfig'Sample'Beta = Nothing
          , binocularsConfig'Sample'Gamma = Nothing
          , binocularsConfig'Sample'Ux = Nothing
          , binocularsConfig'Sample'Uy = Nothing
          , binocularsConfig'Sample'Uz = Nothing
          }

    toIni c = Ini { iniSections = fromList [ ("input", elemFMbDef "a" binocularsConfig'Sample'A c defaultConfig
                                                     [ "`a` parameter of the sample lattice (same unit than the wavelength)."
                                                     , ""
                                                     , "This parameter with the 5 others, `b`, `c`, `alpha`, `beta` and `gamma`"
                                                     , "can be set in order to overwrite the values from the data file."
                                                     , ""
                                                     , " `<not set>` - read `a` from the data file."
                                                     , " `a`         - override `a` with this value."
                                                     ]
                                                     <> elemFMbDef "b" binocularsConfig'Sample'B c defaultConfig
                                                     [ "`b` parameter of the sample lattice (same unit than the wavelength)."
                                                     , ""
                                                     , "This parameter with the 5 others, `a`, `c`, `alpha`, `beta` and `gamma`"
                                                     , "can be set in order to overwrite the values from the data file."
                                                     , ""
                                                     , " `<not set>` - read `b` from the data file."
                                                     , " `b`         - override `b` with this value."
                                                     ]
                                                     <> elemFMbDef "c" binocularsConfig'Sample'C c defaultConfig
                                                     [ "`c` parameter of the sample lattice (same unit than the wavelength)."
                                                     , ""
                                                     , "This parameter with the 5 others, `a`, `b`, `alpha`, `beta` and `gamma`"
                                                     , "can be set in order to overwrite the values from the data file."
                                                     , ""
                                                     , " `<not set>` - read `c` from the data file."
                                                     , " `c`         - override `c` with this value."
                                                     ]
                                                     <> elemFMbDef "alpha" binocularsConfig'Sample'Alpha c defaultConfig
                                                     [ "`alpha` parameter of the sample lattice (Degree)."
                                                     , ""
                                                     , "This parameter with the 5 others, `a`, `b`, `c`, `beta` and `gamma`"
                                                     , "can be set in order to overwrite the values from the data file."
                                                     , ""
                                                     , " `<not set>` - read `alpha` from the data file."
                                                     , " `alpha`     - override `alpha` with this value."
                                                     ]
                                                     <> elemFMbDef "beta" binocularsConfig'Sample'Beta c defaultConfig
                                                     [ "`beta` parameter of the sample lattice (Degree)."
                                                     , ""
                                                     , "This parameter with the 5 others, `a`, `b`, `c`, `alpha`, and `gamma`"
                                                     , "can be set in order to overwrite the values from the data file."
                                                     , ""
                                                     , " `<not set>` - read `beta` from the data file."
                                                     , " `beta`      - override `beta` with this value."
                                                     ]
                                                     <> elemFMbDef "gamma" binocularsConfig'Sample'Gamma c defaultConfig
                                                     [ "`gamma` parameter of the sample lattice (Degree)."
                                                     , ""
                                                     , "This parameter with the 5 others, `a`, `b`, `c`, `alpha` and `beta`"
                                                     , "can be set in order to overwrite the values from the data file."
                                                     , ""
                                                     , " `<not set>` - read `gamma` from the data file."
                                                     , " `gamma`     - override `gamma` with this value."
                                                     ]
                                                     <> elemFMbDef "ux" binocularsConfig'Sample'Ux c defaultConfig
                                                     [ "`ux` rotation of the sample around the x axis"
                                                     , ""
                                                     , "`ux`, `uy`, `uz` are the eulerian angles, which define"
                                                     , "the orientation of the sample lattice, relatively to"
                                                     , "the sample holder."
                                                     , ""
                                                     , "the rotation is computed like this:"
                                                     , ""
                                                     , "Ux * Uy * Uz"
                                                     , ""
                                                     , " `<not set>` - read `ux` from the data file."
                                                     , " `ux`        - override `ux` with this value."
                                                     ]
                                                     <> elemFMbDef "uy" binocularsConfig'Sample'Uy c defaultConfig
                                                     [ "`uy` rotation of the sample around the y axis"
                                                     , ""
                                                     , "`ux`, `uy`, `uz` are the eulerian angles, which define"
                                                     , "the orientation of the sample lattice, relatively to"
                                                     , "the sample holder."
                                                     , ""
                                                     , "the rotation is computed like this:"
                                                     , ""
                                                     , "Ux * Uy * Uz"
                                                     , ""
                                                     , " `<not set>` - read `uy` from the data file."
                                                     , " `uy`        - override `uy` with this value."
                                                     ]
                                                     <> elemFMbDef "uz" binocularsConfig'Sample'Uz c defaultConfig
                                                     [ "`uz` rotation of the sample around the z axis"
                                                     , ""
                                                     , "`ux`, `uy`, `uz` are the eulerian angles, which define"
                                                     , "the orientation of the sample lattice, relatively to"
                                                     , "the sample holder."
                                                     , ""
                                                     , "the rotation is computed like this:"
                                                     , ""
                                                     , "Ux * Uy * Uz"
                                                     , ""
                                                     , " `<not set>` - read `uz` from the data file."
                                                     , " `uz`        - override `uz` with this value."
                                                     ]
                                           )
                                         ]
                , iniGlobals = []
                }

    getConfig (ConfigContent cfg) _ _
        = do binocularsConfig'Sample'A <- parseMb cfg "input" "a"
             binocularsConfig'Sample'B <- parseMb cfg "input" "b"
             binocularsConfig'Sample'C <- parseMb cfg "input" "c"
             binocularsConfig'Sample'Alpha <- parseMb cfg "input" "alpha"
             binocularsConfig'Sample'Beta <- parseMb cfg "input" "beta"
             binocularsConfig'Sample'Gamma <- parseMb cfg "input" "gamma"
             binocularsConfig'Sample'Ux <- parseMb cfg "input" "ux"
             binocularsConfig'Sample'Uy <- parseMb cfg "input" "uy"
             binocularsConfig'Sample'Uz <- parseMb cfg "input" "uz"
             pure BinocularsConfig'Sample{..}

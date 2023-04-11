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

{-
    Copyright  : Copyright (C) 2014-2023 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.Binoculars.Config.Sample
    ( BinocularsConfig'Sample(..)
    , default'BinocularsConfig'Sample
    , default'DataSourcePath'Sample
    , guess'DataSourcePath'Sample
    , overload'DataSourcePath'Sample
    , parse'BinocularsConfig'Sample
    ) where

import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.HashMap.Lazy            (fromList)
import           Data.Ini                     (Ini (..))
import           Data.Text                    (Text)
import           GHC.Generics                 (Generic)
import           Generic.Random               (genericArbitraryU)
import           Test.QuickCheck              (Arbitrary (..))

import           Hkl.Binoculars.Config
import           Hkl.Binoculars.Config.Common
import           Hkl.DataSource
import           Hkl.H5
import           Hkl.Lattice
import           Hkl.Parameter
import           Hkl.Sample

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

instance Arbitrary (DataSourcePath Sample) where
  arbitrary = DataSourcePath'Sample <$> arbitrary  <*> arbitrary <*> arbitrary <*> arbitrary  <*> arbitrary <*> arbitrary <*> arbitrary  <*> arbitrary <*> arbitrary

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


overload'DataSourcePath'Sample :: BinocularsConfig'Sample
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

guess'DataSourcePath'Sample :: BinocularsConfig'Common
                           -> BinocularsConfig'Sample
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
     let marsSamplePath = sampleMarsPath "MARS" "d03-1-cx2__ex__dif-cm_#1"
     let medHSamplePath = samplePath' "SIXS" "i14-c-cx1-ex-cm-med.h"
     let medVSamplePath = samplePath' "SIXS" "i14-c-cx1-ex-cm-med.v"
     let uhvSamplePath  = samplePath' "SIXS" "I14-C-CX2__EX__DIFF-UHV__#1"
     -- let uhvSamplePath2 = samplePath' "SIXS" "i14-c-cx2-ex-diff-uhv"
     let uhvSamplePath3 = samplePath' "SIXS" "i14-c-cx2-ex-cm-uhv"

     let samplePath = case inputType of
                        CristalK6C             -> cristalSamplePath
                        MarsFlyscan            -> marsSamplePath
                        MarsSbs                -> marsSamplePath
                        SixsFlyMedH            -> medHSamplePath
                        SixsFlyMedV            -> medVSamplePath
                        SixsFlyMedVEiger       -> medVSamplePath
                        SixsFlyMedVS70         -> medVSamplePath
                        SixsFlyScanUhv         -> uhvSamplePath
                        SixsFlyScanUhvGisaxs   -> uhvSamplePath3
                        SixsFlyScanUhv2        -> uhvSamplePath3
                        SixsFlyScanUhvTest     -> uhvSamplePath3
                        SixsFlyScanUhvUfxc     -> uhvSamplePath
                        SixsSbsFixedDetector   -> undefined -- TODO this must not be possible.
                        SixsSbsMedH            -> medHSamplePath
                        SixsSbsMedV            -> medVSamplePath
                        SixsSbsMedVFixDetector -> medVSamplePath
                        SixsSbsUhv             -> uhvSamplePath3

     overload'DataSourcePath'Sample sample samplePath
------------
-- Config --
------------

data BinocularsConfig'Sample
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

default'BinocularsConfig'Sample :: BinocularsConfig'Sample
default'BinocularsConfig'Sample
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

instance Arbitrary BinocularsConfig'Sample where
  arbitrary = genericArbitraryU

instance ToIni BinocularsConfig'Sample where
  toIni c = Ini { iniSections = fromList [ ("input", elemFMb "a" (binocularsConfig'Sample'A c)
                                                     <> elemFMb "b" (binocularsConfig'Sample'B c)
                                                     <> elemFMb "c" (binocularsConfig'Sample'C c)
                                                     <> elemFMb "alpha" (binocularsConfig'Sample'Alpha c)
                                                     <> elemFMb "beta" (binocularsConfig'Sample'Beta c)
                                                     <> elemFMb "gamma" (binocularsConfig'Sample'Gamma c)
                                                     <> elemFMb "ux" (binocularsConfig'Sample'Ux c)
                                                     <> elemFMb "uy" (binocularsConfig'Sample'Uy c)
                                                     <> elemFMb "uz" (binocularsConfig'Sample'Uz c)
                                           )
                                         ]
                , iniGlobals = []
                }

parse'BinocularsConfig'Sample :: Text -> Either String BinocularsConfig'Sample
parse'BinocularsConfig'Sample cfg
  = BinocularsConfig'Sample
    <$> parseMb cfg "input" "a"
    <*> parseMb cfg "input" "b"
    <*> parseMb cfg "input" "c"
    <*> parseMb cfg "input" "alpha"
    <*> parseMb cfg "input" "beta"
    <*> parseMb cfg "input" "gamma"
    <*> parseMb cfg "input" "ux"
    <*> parseMb cfg "input" "uy"
    <*> parseMb cfg "input" "uz"

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module BinocularsSpec
  (spec)
  where


import           Control.Monad                            (forM_)
import           Data.Attoparsec.Text                     (parseOnly)
import           Data.Either                              (isRight)
import           Data.Ini.Config                          (parseIniFile)
import           Data.List.NonEmpty                       (NonEmpty (..))
import           Data.Text                                (unpack)
import           Data.Tree                                (Tree(..))
import           Numeric.Interval                         (singleton, (...))
import           Test.Hspec
import           Test.Hspec.Attoparsec                    (shouldParse)
import           Test.Hspec.Attoparsec.Source
import           Test.Hspec.QuickCheck                    (prop)

import           Hkl.Binoculars
import           Hkl.Binoculars.Projections.QCustom
import           Hkl.Geometry
import           Hkl.Lattice
import           Hkl.Repa

import           Paths_hkl

import           Prelude                                  hiding (putStrLn,
                                                           readFile)


spec :: Spec
spec = do
  describe "Limits" $ do
    prop "quickcheck Limits2" $
      \x -> (parseOnly fieldParser . fieldEmitter $ x) `shouldBe` (Right (x :: RLimits DIM2))

    prop "quickcheck Limits3" $
      \x -> (parseOnly fieldParser . fieldEmitter $ x) `shouldBe` (Right (x :: RLimits DIM3))

  describe "ConfigRange" $ do

    let it'range t e = it ("parse a range: " <> unpack t) $ do
          t ~> fieldParser
          `shouldParse` e

    it'range "120 123-453" (ConfigRange (InputRange (singleton 120) :| [InputRange (123...453)]))
    it'range "120,123-453" (ConfigRange (InputRange (singleton 120) :| [InputRange (123...453)]))
    it'range "120,,,123-453" (ConfigRange (InputRange (singleton 120) :| [InputRange (123...453)]))
    it'range "120-135 137-453" (ConfigRange (InputRange (120...135) :| [InputRange (137...453)]))
    it'range "120-135, 137-453" (ConfigRange (InputRange (120...135) :| [InputRange (137...453)]))
    it'range "0 1--1 0-0" (ConfigRange (InputRange (singleton 0) :| [InputRange (1...(-1)), InputRange (singleton 0)]))
    it'range "210-286" (ConfigRange (InputRange (210...286) :| []))

    prop "quickcheck" $
      \x -> (parseOnly fieldParser . fieldEmitter $ x) `shouldBe` (Right (x :: ConfigRange))

    let it'maskLocation t e = it ("parse a MaskLocation: " <> unpack t) $ do
          t ~> fieldParser
          `shouldParse` e

    it'maskLocation "mask.npy" (MaskLocation "mask.npy")
    it'maskLocation "mask_{scannumber:03d}.npy" (MaskLocation'Tmpl "mask_{scannumber:03d}.npy")
    it'maskLocation "mask_{scannumber:03d}.npy | mask.npy" (MaskLocation'Or (MaskLocation'Tmpl "mask_{scannumber:03d}.npy") (MaskLocation "mask.npy"))
    it'maskLocation "mask_{scannumber:03d}.npy | mask.npy|default  " (MaskLocation'Or (MaskLocation'Tmpl "mask_{scannumber:03d}.npy") (MaskLocation'Or (MaskLocation "mask.npy") (MaskLocation "default")))

  describe "Geometry" $ do
    let it'geometry t e = it ("Parse a geometry from ini string") $ do
          parseIniFile t iniParser'Geometry `shouldBe` e

    it'geometry "" (Right Nothing)
    it'geometry "[geometry]\ngeometry_sample=omega chi phi\ngeometry_detector=tth\naxis_omega=rotation 0 -1 0 degree\naxis_chi=rotation 1 0 0 degree\naxis_phi=rotation 0 -1 0 degree\naxis_tth=rotation 0 -1 0 degree\n"
      (Right (Just
               (Geometry'Custom
                ( Node (Axis "" NoTransformation Unit'NoUnit)
                  [ Node (Axis "omega" (Rotation 0 (-1) 0) Unit'Angle'Degree) [Node (Axis "chi" (Rotation 1  0 0) Unit'Angle'Degree) [Node (Axis "phi" (Rotation 0 (-1) 0) Unit'Angle'Degree) [] ] ]
                  , Node (Axis "tth" (Rotation 0 (-1) 0) Unit'Angle'Degree) []
                  ]
                )
                Nothing
               )
             )
      )

  describe "Transformation" $ do
    let it'transformation t e = it ("parse a transformation: " <> unpack t) $ do
          t ~> fieldParser `shouldParse` e

    it'transformation "no-transformation" NoTransformation
    it'transformation "rotation 1 0 0" (Rotation 1 0 0)
    it'transformation "rotation 1.0 0.0 0.0" (Rotation 1 0 0)
    it'transformation "translation -1.0 1.0 1.0" (Translation (-1) 1 1)

  describe "Unit" $ do
    let it'unit t e = it ("parse an unit: " <> unpack t) $ do
          t ~> fieldParser
          `shouldParse` e

    it'unit "ua" Unit'NoUnit
    it'unit "degree" Unit'Angle'Degree
    it'unit "millimeter" Unit'Length'MilliMeter

  describe "read and parse binoculars configuration" $ do
    it "deprecated inputype" $ do
      forM_ [ "data/test/config_ech6eiger.txt"
            , "data/test/config_sixs_ruche_parsing.ini"
            , "data/test/config_geometry_custom.ini"
            ] $ \f -> do
        content <- readConfig =<< Just <$> getDataFileName f
        capabilities <- getCapabilities
        let args = Args'QCustomProjection (Just $ ConfigRange (InputRange (120...135) :| [InputRange (137...453)]))
        let cfg = getConfig content args capabilities
        isRight cfg `shouldBe` True

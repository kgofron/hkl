{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module BinocularsSpec
  (spec)
  where


import           Control.Monad                      (forM_)
import           Data.Attoparsec.Text               (Parser, parseOnly)
import           Data.Either                        (isRight)
import           Data.Ini.Config                    (parseIniFile)
import           Data.List.NonEmpty                 (NonEmpty (..))
import           Data.Text                          (unpack)
import           Data.Tree                          (Tree (..))
import           Numeric.Interval                   (singleton, (...))
import           Test.Hspec
import           Test.Hspec.Attoparsec              (shouldFailOn, shouldParse)
import           Test.Hspec.Attoparsec.Source
import           Test.Hspec.QuickCheck              (prop)

import           Hkl.Binoculars
import           Hkl.Binoculars.Projections.QCustom
import           Hkl.Geometry
import           Hkl.Lattice
import           Hkl.Repa

import           Paths_hkl

import           Prelude                            hiding (putStrLn, readFile)



spec :: Spec
spec = do

  -------------
  -- RLimits --
  -------------

  describe "Limits" $ do
    prop "quickcheck Limits2" $
      \x -> (parseOnly fieldParser . fieldEmitter $ x) `shouldBe` (Right (x :: RLimits DIM2))

    prop "quickcheck Limits3" $
      \x -> (parseOnly fieldParser . fieldEmitter $ x) `shouldBe` (Right (x :: RLimits DIM3))

  -----------------
  -- ConfigRange --
  -----------------

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

    ------------------
    -- MaskLocation --
    ------------------

    let it'maskLocation t e = it ("parse a MaskLocation: " <> unpack t) $ do
          t ~> fieldParser
          `shouldParse` e

    it'maskLocation "mask.npy" (MaskLocation "mask.npy")
    it'maskLocation "mask_{scannumber:03d}.npy" (MaskLocation'Tmpl "mask_{scannumber:03d}.npy")
    it'maskLocation "mask_{scannumber:03d}.npy | mask.npy" (MaskLocation'Or (MaskLocation'Tmpl "mask_{scannumber:03d}.npy") (MaskLocation "mask.npy"))
    it'maskLocation "mask_{scannumber:03d}.npy | mask.npy|default  " (MaskLocation'Or (MaskLocation'Tmpl "mask_{scannumber:03d}.npy") (MaskLocation'Or (MaskLocation "mask.npy") (MaskLocation "default")))

  --------------
  -- Geometry --
  --------------

  describe "Geometry" $ do
    let it'geometry t e = it ("Parse a geometry from ini string: " <> unpack t) $ do
          parseIniFile t iniParser'Geometry `shouldBe` e

    let it'geometry'right t e = it'geometry t (Right e)
    let it'geometry'left t e = it'geometry t (Left e)

    it'geometry'right "" Nothing

    it'geometry'right "[geometry]\ngeometry_sample=omega chi phi\ngeometry_detector=tth\naxis_omega=rotation 0 -1 0 degree\naxis_chi=rotation 1 0 0 degree\naxis_phi=rotation 0 -1 0 degree\naxis_tth=rotation 0 -1 0 degree\n"
       (Just
        (Geometry'Custom
         ( Node (Axis "" NoTransformation Unit'NoUnit)
           [ Node (Axis "omega" (Rotation 0 (-1) 0) Unit'Angle'Degree) [Node (Axis "chi" (Rotation 1  0 0) Unit'Angle'Degree) [Node (Axis "phi" (Rotation 0 (-1) 0) Unit'Angle'Degree) [] ] ]
           , Node (Axis "tth" (Rotation 0 (-1) 0) Unit'Angle'Degree) []
           ]
         )
         Nothing
        )
       )

    -- error handling

    it'geometry'left "[geometry]\ngeometry_sample=omega chi phi\ngeometry_detector=tth\n\naxis_chi=rotation 1 0 0 degree\naxis_phi=rotation 0 -1 0 degree\naxis_tth=rotation 0 -1 0 degree\n"
       "Missing field \"axis_omega\" in section \"geometry\""

    it'geometry'left "[geometry]\ngeometry_sample=omega chi phi\ngeometry_detector=tth\naxis_omega=rotation 0 -1 0 degree\naxis_chi=rotation 1 0 degree\naxis_phi=rotation 0 -1 0 degree\naxis_tth=rotation 0 -1 0 degree\n"
       "Line 5, in section \"geometry\": rotation maformed expect: 'rotation <x> <y> <z>': Failed reading: takeWhile1"

    it'geometry'left "[geometry]\ngeometry_sample=omega chi phi\ngeometry_detector=tth\naxis_omega=rotation 0 -1 0 degree\naxis_chi=rotation 1 0 0 toto\naxis_phi=rotation 0 -1 0 degree\naxis_tth=rotation 0 -1 0 degree\n"
       "Line 5, in section \"geometry\": Failed reading: unknown unit 'toto' only supported are 'ua' 'degree' 'millimeter'"

  --------------------
  -- Transformation --
  --------------------

  describe "Transformation" $ do
         let it'transformation'good t e
                 = it ("parse a transformation: " <> unpack t)
                   (t ~> fieldParser `shouldParse` e)

         let it'transformation'error t
                 = it ("parse a malformed transformation: " <> unpack t)
                   ((fieldParser :: Parser Transformation) `shouldFailOn` t)

         it'transformation'good "no-transformation" NoTransformation
         it'transformation'good "rotation 1 0 0" (Rotation 1 0 0)
         it'transformation'good "rotation 1.0 0.0 0.0" (Rotation 1 0 0)
         it'transformation'good "translation -1.0 1.0 1.0" (Translation (-1) 1 1)

         it'transformation'error "rotation 1 0"

                                   -- (Left "Line 5, in section \"geometry\": rotation maformed expect: rotation <x> <y> <z>: Failed reading: takeWhile1")

  ----------
  -- Unit --
  ----------

  describe "Unit" $ do
    let it'unit t e = it ("parse an unit: " <> unpack t) $ do
          t ~> fieldParser
          `shouldParse` e

    it'unit "ua" Unit'NoUnit
    it'unit "degree" Unit'Angle'Degree
    it'unit "millimeter" Unit'Length'MilliMeter

  ------------------------
  -- Parse full configs --
  ------------------------

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

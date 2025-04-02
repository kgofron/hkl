{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BinocularsSpec
  (spec)
  where


import           Control.Monad                      (forM_)
import           Control.Monad.IO.Class             (liftIO)
import           Data.Attoparsec.Text               (Parser, parseOnly)
import           Data.Either                        (isRight)
import           Data.Ini.Config                    (IniParser, parseIniFile)
import           Data.Ini.Config.Bidir              (FieldValue (..))
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
import           Hkl.DataSource                     ()
import           Hkl.Detector
import           Hkl.Geometry
import           Hkl.H5
import           Hkl.Image
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
      \x -> (parseOnly fieldParser . fieldEmitter $ x) `shouldBe` Right (x :: RLimits DIM2)

    prop "quickcheck Limits3" $
      \x -> (parseOnly fieldParser . fieldEmitter $ x) `shouldBe` Right (x :: RLimits DIM3)

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
      \x -> (parseOnly fieldParser . fieldEmitter $ x) `shouldBe` Right (x :: ConfigRange)

  --------------------
  -- DataSourcePath --
  --------------------

  describe "DataSourcePath Image" $ do
         let it'datasource'path'image (t, v)
                 = do it ("emit a DataSourcePath Image: " <> unpack t) $ do
                                      (fvEmit fieldvalue $ v) `shouldBe` t
                      it ("parse a DataSourcePath Image: " <> show v) $ do
                                      (fvParse fieldvalue $ t) `shouldBe` Right (v :: DataSourcePath Image)

         let tests = [ ( "{\"contents\":[{\"detector\":\"ImXpadS140\"},{\"tag\":\"DataSourcePath'NoAttenuation\"},1],\"tag\":\"DataSourcePath'Image'Dummy\"}"
                       , (DataSourcePath'Image'Dummy defaultDetector DataSourcePath'NoAttenuation 1.0))
                     , ( "{\"contents\":[{\"detector\":\"ImXpadS140\"},{\"attenuationPath\":{\"contents\":{\"contents\":[0,{\"contents\":[\"scan_data\",{\"contents\":[{\"contents\":\"attenuation\",\"tag\":\"H5DatasetPath\"},{\"contents\":\"attenuation_old\",\"tag\":\"H5DatasetPath\"}],\"tag\":\"H5Or\"}],\"tag\":\"H5GroupPath\"}],\"tag\":\"H5GroupAtPath\"},\"tag\":\"H5RootPath\"},\"attenuationPathCoefficient\":0,\"attenuationPathMax\":5,\"attenuationPathOffset\":2,\"tag\":\"DataSourcePath'Attenuation\"},10],\"tag\":\"DataSourcePath'Image'Dummy\"}"
                       , (DataSourcePath'Image'Dummy defaultDetector
                          (DataSourcePath'Attenuation
                           (DataSourcePath'Float (hdf5p $ grouppat 0 $ groupp "scan_data" (datasetp "attenuation"
                                                                                           `H5Or`
                                                                                           datasetp "attenuation_old")))
                           2 0 (Just 5))
                          10.0)
                         )
                     ]

         mapM_ it'datasource'path'image tests

-- data instance DataSourcePath Image
--     = DataSourcePath'Image'Dummy (Detector Hkl DIM2) (DataSourcePath Attenuation) Double
--     | DataSourcePath'Image'Hdf5 (Detector Hkl DIM2) (Hdf5Path DIM3 Int32) -- TODO Int32 is wrong
--     | DataSourcePath'Image'Img (Detector Hkl DIM2) (DataSourcePath Attenuation) Text Scannumber
--     deriving (Eq, Generic, Show, FromJSON, ToJSON)


  ------------------
  -- MaskLocation --
  ------------------

  describe "MaskLocation" $ do

    let it'maskLocation t e =
            it ("parse a MaskLocation: " <> unpack t) $ do
              t ~> fieldParser `shouldParse` e

    it'maskLocation "mask.npy" (MaskLocation "mask.npy")
    it'maskLocation "mask_{scannumber:03d}.npy" (MaskLocation'Tmpl "mask_{scannumber:03d}.npy")
    it'maskLocation "mask_{scannumber:03d}.npy | mask.npy" (MaskLocation'Or (MaskLocation'Tmpl "mask_{scannumber:03d}.npy") (MaskLocation "mask.npy"))
    it'maskLocation "mask_{scannumber:03d}.npy | mask.npy|default  " (MaskLocation'Or (MaskLocation'Tmpl "mask_{scannumber:03d}.npy") (MaskLocation'Or (MaskLocation "mask.npy") (MaskLocation "default")))

  --------------
  -- Geometry --
  --------------

  describe "Geometry" $ do
    let title t = "Parse a geometry from ini string: " <> unpack t
    let checkParse t r = parseIniFile t (iniParser :: IniParser (Maybe Geometry)) `shouldBe` r

    let it'geometry'right t me
            = it (title t)
              (case me of
                Nothing -> checkParse t (Right me)
                Just e  -> liftIO $ withGeometry e (const $ checkParse t (Right me))
              )

    let it'geometry'left t err
            = it (title t)
              (const $ checkParse t (Left err))

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

    it'geometry'right "[geometry]\ngeometry_sample=omega chi phi\naxis_omega=rotation 0 -1 0 degree\naxis_chi=rotation 1 0 0 degree\naxis_phi=rotation 0 -1 0 degree\n"
       (Just
        (Geometry'Custom
         ( Node (Axis "omega" (Rotation 0 (-1) 0) Unit'Angle'Degree) [Node (Axis "chi" (Rotation 1  0 0) Unit'Angle'Degree) [Node (Axis "phi" (Rotation 0 (-1) 0) Unit'Angle'Degree) [] ] ]
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
        content <- readConfig . Just =<< getDataFileName f
        capabilities <- getCapabilities
        let args = Args'QCustomProjection (Just $ ConfigRange (InputRange (120...135) :| [InputRange (137...453)]))
        let cfg = getConfig content args capabilities
        isRight cfg `shouldBe` True

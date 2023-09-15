{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module BinocularsSpec
  (spec)
  where


import           Control.Monad                      (forM_)
import           Data.Aeson                         (Result (..), fromJSON,
                                                     toJSON)
import           Data.Array.Repa.Index              (DIM2, DIM3)
import           Data.Attoparsec.Text               (parseOnly)
import           Data.Either                        (isRight)
import           Data.HashMap.Lazy                  (fromList)
import           Data.List.NonEmpty                 (NonEmpty (..))
import           Numeric.Interval                   (singleton, (...))
import           Numeric.Units.Dimensional.Prelude  (meter, radian, (*~))
import           Path                               (mkAbsDir)
import           Test.Hspec
import           Test.Hspec.QuickCheck              (prop)

import           Hkl.Binoculars
import           Hkl.Binoculars.Config.Common
import           Hkl.Binoculars.Config.Sample
import           Hkl.Binoculars.Projections.Hkl
import           Hkl.Binoculars.Projections.QCustom
import           Hkl.DataSource
import           Paths_hkl

import           Prelude                            hiding (putStrLn, readFile)


spec :: Spec
spec = do
  describe "Limits" $ do
    prop "quickcheck Limits2" $
      \x -> (parseOnly fieldParser . fieldEmitter $ x) `shouldBe` (Right (x :: RLimits DIM2))

    prop "quickcheck Limits3" $
      \x -> (parseOnly fieldParser . fieldEmitter $ x) `shouldBe` (Right (x :: RLimits DIM3))

  describe "ConfigRange" $ do
    it "parse a range" $ do
      let p = parseOnly fieldParser "120 123-453"
      p `shouldBe` (Right (ConfigRange (InputRange (singleton 120) :| [InputRange (123...453)])))
    it "parse a range" $ do
      let p = parseOnly fieldParser "120,123-453"
      p `shouldBe` (Right (ConfigRange (InputRange (singleton 120) :| [InputRange (123...453)])))
    it "parse a range" $ do
      let p = parseOnly fieldParser "120,,,123-453"
      p `shouldBe` (Right (ConfigRange (InputRange (singleton 120) :| [InputRange (123...453)])))
    it "parse a range" $ do
      let p = parseOnly fieldParser "120-135 137-453"
      p `shouldBe` (Right (ConfigRange (InputRange (120...135) :| [InputRange (137...453)])))
    it "parse a range" $ do
      let p = parseOnly fieldParser "120-135, 137-453"
      p `shouldBe` (Right (ConfigRange (InputRange (120...135) :| [InputRange (137...453)])))
    it "parse a range" $ do
      let p = parseOnly fieldParser "0 1--1 0-0"
      p  `shouldBe` (Right (ConfigRange (InputRange (singleton 0) :| [InputRange (1...(-1)), InputRange (singleton 0)])))
    it "parse a range" $ do
      let p = parseOnly fieldParser "210-286"
      p  `shouldBe` (Right (ConfigRange (InputRange (210...286) :| [])))

    prop "quickcheck" $
      \x -> (parseOnly fieldParser . fieldEmitter $ x) `shouldBe` (Right (x :: ConfigRange))

  describe "read and parse binoculars configuration" $ do
    it "deprecated inputype" $ do
      forM_ [ "data/test/config_ech6eiger.txt"
            , "data/test/config_sixs_ruche_parsing.ini"
            ] $ \f -> do
        content <- readConfig =<< Just <$> getDataFileName f
        capabilities <- getCapabilities
        let args = Args'QCustomProjection (Just $ ConfigRange (InputRange (120...135) :| [InputRange (137...453)]))
        let cfg = getConfig content args capabilities
        isRight cfg `shouldBe` True

  -- describe "read and parse binoculars Configuration" $ do
  --   it "hkl projection" $ do
  --     cfg <- getConfig =<< Just <$> getDataFileName "data/test/config_sixs_biggest.ini"
  --     cfg `shouldBe` (Right
  --                      BinocularsConfig'Hkl { binocularsConfig'Hkl'Common = BinocularsConfig'Common { binocularsConfig'Common'NCores = Nothing
  --                                                                                                   , binocularsConfig'Common'Destination = DestinationTmpl {unDestinationTmpl = "Al13Co4_facets_res_0.01_0.01_0.01_HK_4.7_5_{first}-{last}.hdf5"}
  --                                                                                                   , binocularsConfig'Common'Overwrite = False
  --                                                                                                   , binocularsConfig'Common'InputType = SixsFlyScanUhv2
  --                                                                                                   , binocularsConfig'Common'Nexusdir = Just $(mkAbsDir "/nfs/ruche-sixs/sixs-soleil/com-sixs/2018/Run3/Corentin/Al13Co4/")
  --                                                                                                   , binocularsConfig'Common'Tmpl = Nothing
  --                                                                                                   , binocularsConfig'Common'InputRange = Just (ConfigRange (InputRange (4...5) :| []))
  --                                                                                                   , binocularsConfig'Common'Detector = Nothing
  --                                                                                                   , binocularsConfig'Common'Centralpixel = (293,141)
  --                                                                                                   , binocularsConfig'Common'Sdd = Meter (1.1083 *~ meter)
  --                                                                                                   , binocularsConfig'Common'Detrot = Just (Degree (1.5707963267948966 *~ radian))
  --                                                                                                   , binocularsConfig'Common'AttenuationCoefficient = Just 1.825
  --                                                                                                   , binocularsConfig'Common'AttenuationMax = Nothing
  --                                                                                                   , binocularsConfig'Common'Maskmatrix = Just "/nfs/ruche-sixs/sixs-soleil/com-sixs/2018/Run3/Corentin/Al13Co4/binoculars/mask8.npy"
  --                                                                                                   , binocularsConfig'Common'ImageSumMax = Nothing
  --                                                                                                   , binocularsConfig'Common'Wavelength = Nothing
  --                                                                                                   }
  --                                           , binocularsConfig'Hkl'Sample = BinocularsConfig'Sample { binocularsConfig'Sample'A = Just (Angstrom (1.4452000000000001e-9 *~ meter))
  --                                                                                                   , binocularsConfig'Sample'B = Just (Angstrom (8.158e-10 *~ meter))
  --                                                                                                   , binocularsConfig'Sample'C = Just (Angstrom (1.2342000000000001e-9 *~ meter))
  --                                                                                                   , binocularsConfig'Sample'Alpha = Just (Degree (1.5707963267948966 *~ radian))
  --                                                                                                   , binocularsConfig'Sample'Beta = Just (Degree (1.5707963267948966 *~ radian))
  --                                                                                                   , binocularsConfig'Sample'Gamma = Just (Degree (1.5707963267948966 *~ radian))
  --                                                                                                   , binocularsConfig'Sample'Ux = Just (Degree ((-1.4169480479319254) *~ radian))
  --                                                                                                   , binocularsConfig'Sample'Uy = Just (Degree (0.9401030022569337 *~ radian))
  --                                                                                                   , binocularsConfig'Sample'Uz = Just (Degree ((-0.19054917649924238) *~ radian))
  --                                                                                                   }
  --                                           , binocularsConfig'Hkl'ProjectionType = HklProjection
  --                                           , binocularsConfig'Hkl'ProjectionResolution = Resolutions3 1.0e-2 1.0e-2 1.0e-2
  --                                           , binocularsConfig'Hkl'ProjectionLimits = Just (Limits3
  --                                                                                          (Limits (Just 4.45) (Just 4.95))
  --                                                                                          (Limits (Just 4.75) (Just 5.25))
  --                                                                                          (Limits Nothing Nothing))
  --                                           , binocularsConfig'Hkl'DataPath = Nothing
  --                                           }
  --                    )

  -- describe "quickcheck DataSourcePath json parsing" $ do
  --   prop "DataFrameHkl" $
  --     \x -> (fromJSON . toJSON) x `shouldBe` (Success x :: Result (DataFrameHkl' DataSourcePath))
  --   prop "DataFrameQCustom" $
  --     \x -> (fromJSON . toJSON) x `shouldBe` (Success x :: Result (DataSourcePath DataFrameQCustom))

  -- describe "quickcheck config parsing" $ do
  --   prop "Config 'QCustomProjection" $
  --     \x -> (parseConfig . serializeConfig) x `shouldBe` (Right (x :: Config 'QCustomProjection))

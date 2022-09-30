{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module BinocularsSpec
  (spec)
  where


import           Data.Attoparsec.Text              (parseOnly)
import           Numeric.Units.Dimensional.Prelude (meter, radian, (*~))
import           Path                              (mkAbsDir)


import           Test.Hspec

import           Hkl.Binoculars
import           Hkl.Binoculars.Projections.Hkl
import           Paths_hkl

import           Prelude                           hiding (readFile)


spec :: Spec
spec = do
  describe "parseProjectionLimits" $ do
    it "parse a limits" $ do
      let p = parseOnly limitsP "[-1.0:1.0]"
      p `shouldBe` (Right [Limits (Just (-1)) (Just 1)])
    it "parse a limits with no limits" $ do
      let p = parseOnly limitsP "[-1.0:1.0,:]"
      p `shouldBe` (Right [Limits (Just (-1)) (Just 1), Limits Nothing Nothing])
    it "parse a limits with no limits" $ do
      let p = parseOnly limitsP "[:1,-1:]"
      p `shouldBe` (Right [Limits Nothing (Just 1), Limits (Just (-1)) Nothing])
    it "parse a limits with no limits" $ do
      let p = parseOnly limitsP "[-1.0:,:]"
      p `shouldBe` (Right [Limits (Just (-1)) Nothing, Limits Nothing Nothing])
    it "parse a limits with no limits" $ do
      let p = parseOnly limitsP "[:,:]"
      p `shouldBe` (Right [Limits Nothing Nothing, Limits Nothing Nothing])

  describe "parseConfigRange" $ do
    it "parse a range" $ do
      let p = parseOnly configRangeP "120 123-453"
      p `shouldBe` (Right (ConfigRange [InputRangeSingle 120, InputRangeFromTo 123 453]))
    it "parse a range" $ do
      let p = parseOnly configRangeP "120,123-453"
      p `shouldBe` (Right (ConfigRange [InputRangeSingle 120, InputRangeFromTo 123 453]))
    it "parse a range" $ do
      let p = parseOnly configRangeP "120,,,123-453"
      p `shouldBe` (Right (ConfigRange [InputRangeSingle 120, InputRangeFromTo 123 453]))
    it "parse a range" $ do
      let p = parseOnly configRangeP "120-135 137-453"
      p `shouldBe` (Right (ConfigRange [InputRangeFromTo 120 135, InputRangeFromTo 137 453]))
    it "parse a range" $ do
      let p = parseOnly configRangeP "120-135, 137-453"
      p `shouldBe` (Right (ConfigRange [InputRangeFromTo 120 135, InputRangeFromTo 137 453]))

  describe "read and parse binoculars Configuration" $ do
    it "hkl projection" $ do
      cfg <- getConfig =<< Just <$> getDataFileName "data/test/config_sixs_biggest.ini"
      cfg `shouldBe` (Right
                      BinocularsConfigHkl { _binocularsConfigHklNcore = Nothing
                                          , _binocularsConfigHklDestination = DestinationTmpl {unDestinationTmpl = "Al13Co4_facets_res_0.01_0.01_0.01_HK_4.7_5_{first}-{last}.hdf5"}
                                          , _binocularsConfigHklOverwrite = False
                                          , _binocularsConfigHklInputType = SixsFlyScanUhv2
                                          , _binocularsConfigHklNexusdir = Just $(mkAbsDir "/nfs/ruche-sixs/sixs-soleil/com-sixs/2018/Run3/Corentin/Al13Co4/")
                                          , _binocularsConfigHklTmpl = Nothing
                                          , _binocularsConfigHklInputRange = Just (ConfigRange [InputRangeFromTo 4 5])
                                          , _binocularsConfigHklDetector = Nothing
                                          , _binocularsConfigHklCentralpixel = (293,141)
                                          , _binocularsConfigHklSdd = Meter (1.1083 *~ meter)
                                          , _binocularsConfigHklDetrot = Just (Degree (1.5707963267948966 *~ radian))
                                          , _binocularsConfigHklAttenuationCoefficient = Just 1.825
                                          , _binocularsConfigHklAttenuationMax = Nothing
                                          , _binocularsConfigHklMaskmatrix = Just "/nfs/ruche-sixs/sixs-soleil/com-sixs/2018/Run3/Corentin/Al13Co4/binoculars/mask8.npy"
                                          , _binocularsConfigHklA = Just (Angstrom (1.4452000000000001e-9 *~ meter))
                                          , _binocularsConfigHklB = Just (Angstrom (8.158e-10 *~ meter))
                                          , _binocularsConfigHklC = Just (Angstrom (1.2342000000000001e-9 *~ meter))
                                          , _binocularsConfigHklAlpha = Just (Degree (1.5707963267948966 *~ radian))
                                          , _binocularsConfigHklBeta = Just (Degree (1.5707963267948966 *~ radian))
                                          , _binocularsConfigHklGamma = Just (Degree (1.5707963267948966 *~ radian))
                                          , _binocularsConfigHklUx = Just (Degree ((-1.4169480479319254) *~ radian))
                                          , _binocularsConfigHklUy = Just (Degree (0.9401030022569337 *~ radian))
                                          , _binocularsConfigHklUz = Just (Degree ((-0.19054917649924238) *~ radian))
                                          , _binocularsConfigHklWavelength = Nothing
                                          , _binocularsConfigHklProjectionType = HklProjection
                                          , _binocularsConfigHklProjectionResolution = [1.0e-2,1.0e-2,1.0e-2]
                                          , _binocularsConfigHklProjectionLimits = Just [Limits (Just 4.45) (Just 4.95),Limits (Just 4.75) (Just 5.25),Limits Nothing Nothing]
                                          , _binocularsConfigHklDataPath = Nothing
                                          , _binocularsConfigHklImageSumMax = Nothing
                                          }
                   )

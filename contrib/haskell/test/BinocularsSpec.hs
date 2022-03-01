{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module BinocularsSpec
  (spec)
  where


import           Control.Monad                     (forM_)
import           Data.Attoparsec.Text              (parseOnly)
import           Numeric.Units.Dimensional.Prelude (meter, radian, (*~))
import           Path                              (mkAbsDir)


import           Test.Hspec

import           Hkl.Binoculars
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

  describe "parseBinocularsConfig" $ do
         forM_ [ "data/test/config_sixs_biggest.ini"
               -- , "data/test/config_map.txt"
               ] $ \f -> it f $ do
           cfg <- getConfig =<< Just <$> getDataFileName f
           cfg `shouldBe` (Right
                            BinocularsConfig {_binocularsDispatcherNcore = Nothing,
                                              _binocularsDispatcherDestination = DestinationTmpl {unDestinationTmpl = "Al13Co4_facets_res_0.01_0.01_0.01_HK_4.7_5_{first}-{last}.hdf5"},
                                              _binocularsDispatcherOverwrite = False,
                                              _binocularsInputItype = SixsFlyScanUhv2,
                                              _binocularsInputNexusdir = Just $(mkAbsDir "/nfs/ruche-sixs/sixs-soleil/com-sixs/2018/Run3/Corentin/Al13Co4/"),
                                              _binocularsInputTmpl = Nothing,
                                              _binocularsInputInputRange = Just (ConfigRange [InputRangeFromTo 4 5]),
                                              _binocularsInputDetector = Nothing,
                                              _binocularsInputCentralpixel = (293,141),
                                              _binocularsInputSdd = Meter (1.1083 *~ meter),
                                              _binocularsInputDetrot = Just (Degree (1.5707963267948966 *~ radian)),
                                              _binocularsInputAttenuationCoefficient = Just 1.825,
                                              _binocularsInputSurfaceOrientation = Nothing,
                                              _binocularsInputMaskmatrix = Just "/nfs/ruche-sixs/sixs-soleil/com-sixs/2018/Run3/Corentin/Al13Co4/binoculars/mask8.npy",
                                              _binocularsInputA = Just (Angstrom (1.4452000000000001e-9 *~ meter)),
                                              _binocularsInputB = Just (Angstrom (8.158e-10 *~ meter)),
                                              _binocularsInputC = Just (Angstrom (1.2342000000000001e-9 *~ meter)),
                                              _binocularsInputAlpha = Just (1.5707963267948966 *~ radian),
                                              _binocularsInputBeta = Just (1.5707963267948966 *~ radian),
                                              _binocularsInputGamma = Just (1.5707963267948966 *~ radian),
                                              _binocularsInputUx = Just ((-1.4169480479319254) *~ radian),
                                              _binocularsInputUy = Just (0.9401030022569337 *~ radian),
                                              _binocularsInputUz = Just ((-0.19054917649924238) *~ radian),
                                              _binocularsInputWavelength = Nothing,
                                              _binocularsProjectionPtype = HklProjection,
                                              _binocularsProjectionResolution = [1.0e-2,1.0e-2,1.0e-2],
                                              _binocularsProjectionLimits = Just [Limits (Just 4.45) (Just 4.95),Limits (Just 4.75) (Just 5.25),Limits Nothing Nothing]
                                             }
                          )

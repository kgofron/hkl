{-# LANGUAGE OverloadedStrings #-}

module BinocularsSpec
  (spec)
  where


import           Control.Monad        (forM_)
import           Data.Attoparsec.Text (parseOnly)
import           Data.Either          (isRight)
import           Data.Text.IO         (readFile)
import           Test.Hspec

import           Hkl.Binoculars
import           Paths_hkl

import           Prelude              hiding (readFile)


spec :: Spec
spec = do
  describe "parseBinocularsConfig" $ do
         forM_ [ "data/test/config_sixs_biggest.ini"
               -- , "data/test/config_map.txt"
               ] $ \f -> it f $ do
           cfg <- getConfig =<< Just <$> getDataFileName f
           case cfg of
             (Right c) -> True `shouldBe` True
             (Left e) -> do
                      print e
                      False `shouldBe` True

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
      let p = parseOnly configRangeP "120-135 123-453"
      p `shouldBe` (Right (ConfigRange [InputRangeFromTo 120 135, InputRangeFromTo 123 453]))
    it "parse a range" $ do
      let p = parseOnly configRangeP "120-135, 123-453"
      p `shouldBe` (Right (ConfigRange [InputRangeFromTo 120 135, InputRangeFromTo 123 453]))

{-# LANGUAGE OverloadedStrings #-}

module BinocularsSpec
  (spec)
  where


import           Data.Either     (isRight)
import           Data.Ini.Config (parseIniFile)
import           Data.Text.IO    (readFile)
import           Test.Hspec

import           Hkl.Binoculars
import           Paths_hkl

import           Prelude         hiding (readFile)

spec :: Spec
spec = do
  describe "parseBinocularsConfig" $ do
    it "parse data/test/config_hkl_facet.cfg" $ do
      cfg <- readFile =<< getDataFileName "data/test/config_hkl_facet.cfg"
      let r = parseIniFile cfg parseBinocularsConfig
      isRight r `shouldBe` True

    it "parse data/test/config_map.txt" $ do
      cfg <- readFile =<< getDataFileName "data/test/config_map.txt"
      let r = parseIniFile cfg parseBinocularsConfig
      isRight r `shouldBe` True

{-# LANGUAGE OverloadedStrings #-}

module BinocularsSpec
  (spec)
  where


import           Control.Monad  (forM_)
import           Data.Either    (isRight)
import           Data.Text.IO   (readFile)
import           Test.Hspec

import           Hkl.Binoculars
import           Paths_hkl

import           Prelude        hiding (readFile)


spec :: Spec
spec = do
  describe "parseBinocularsConfig" $ do
         forM_ [ "data/test/config_hkl_facet.cfg"
               -- , "data/test/config_map.txt"
               ] $ \f -> it f $ do
           cfg <- getConfig =<< Just <$> getDataFileName f
           case cfg of
             (Right c) -> True `shouldBe` True
             (Left e) -> do
                      print e
                      False `shouldBe` True

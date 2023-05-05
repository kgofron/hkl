{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hkl.Orphan where

import           Data.Aeson                      (FromJSON (..), ToJSON (..),
                                                  Value (..))
import           Data.Array.Repa                 (Array, Shape, extent,
                                                  showShape)
import           Data.Array.Repa.Repr.ForeignPtr (F)
import           Data.ByteString.Char8           (ByteString)
import           Data.Ini.Config.Bidir           (FieldValue (..))
import           Data.Text                       (Text, strip, unpack, unwords)
import           Data.Text.Encoding              (decodeUtf8, encodeUtf8)
import           Data.Typeable                   (Proxy (..), typeRep)
import           Foreign.Storable                (Storable)
import           GHC.Base                        (Alternative (..))
import           Path                            (Abs, Dir, Path, mkAbsDir)
import           Test.QuickCheck                 (Arbitrary (..), elements)

import           Prelude                         hiding (unwords)

import           Hkl.Binoculars.Config
import           Hkl.C.Binoculars

instance (Shape sh, Storable e) => Show (Array F sh e) where
    show = showShape . extent

instance ToJSON ByteString where
    toJSON = String . decodeUtf8
    {-# INLINE toJSON #-}

instance FromJSON ByteString where
    parseJSON (String t) = pure . encodeUtf8 $ t
    parseJSON _          = GHC.Base.empty
    {-# INLINE parseJSON #-}

instance Arbitrary (Path Abs Dir) where
  arbitrary = pure $(mkAbsDir "/toto")


-- HklBinocularsSurfaceOrientationEnum

instance Arbitrary HklBinocularsSurfaceOrientationEnum where
  arbitrary = elements ([minBound .. maxBound] :: [HklBinocularsSurfaceOrientationEnum])

instance HasFieldValue HklBinocularsSurfaceOrientationEnum where
  fieldvalue = FieldValue { fvParse = parse . strip . uncomment, fvEmit = emit }
    where
      err t = "Unsupported "
              ++ show (typeRep (Proxy :: Proxy HklBinocularsSurfaceOrientationEnum))
              ++ " :" ++ unpack t
              ++ " Supported ones are: "
              ++ unpack (unwords $ Prelude.map emit [minBound..maxBound])

      parse :: Text -> Either String HklBinocularsSurfaceOrientationEnum
      parse t = parseEnum (err t) t

      emit :: HklBinocularsSurfaceOrientationEnum -> Text
      emit HklBinocularsSurfaceOrientationEnum'Vertical   = "vertical"
      emit HklBinocularsSurfaceOrientationEnum'Horizontal = "horizontal"

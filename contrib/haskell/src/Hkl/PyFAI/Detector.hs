{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Hkl.PyFAI.Detector
       ( ToPyFAI(..)
       ) where

import           Data.Text    (Text)

import           Hkl.Detector

class ToPyFAI a where
  toPyFAI ∷ a → Text

instance ToPyFAI (Detector a sh) where
  toPyFAI Xpad32            = "Xpad_flat"
  toPyFAI ImXpadS140        = "imxpad_s140"
  toPyFAI XpadFlatCorrected = error "Unsupported Detector"
  toPyFAI ZeroD             = error "Unsupported Detector"

instance ToPyFAI SomeDetector where
  toPyFAI (SomeDetector v) = toPyFAI v

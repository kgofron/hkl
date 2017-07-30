{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hkl.PyFAI.Detector
       ( ToPyFAI(..)
       ) where

import Data.Text (Text)

import Hkl.Detector ( Detector ( Xpad32, ImXpadS140, ZeroD ) )

class ToPyFAI a where
  toPyFAI ∷ a → Text

instance ToPyFAI (Detector a) where
  toPyFAI Xpad32 = "Xpad_flat"
  toPyFAI ImXpadS140 = "imxpad_s140"
  toPyFAI ZeroD = error "Unsupported Detector"

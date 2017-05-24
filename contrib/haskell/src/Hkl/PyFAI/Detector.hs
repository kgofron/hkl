{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hkl.PyFAI.Detector
       ( ToPyFAI(..)
       ) where

import Data.Text (Text)

import Hkl.Detector

class ToPyFAI a where
  toPyFAI ∷ a → Text

instance ToPyFAI (Detector a) where
  toPyFAI Xpad32 = "Xpad_flat"
  toPyFAI ImXpadS140 = "imxpad_s140"

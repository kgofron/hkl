{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hkl.PyFAI.Detector
       ( ToPyFAICalibArg(..) ) where

import Hkl.Detector

class ToPyFAICalibArg a where
  toPyFAICalibArg ∷ a → String

instance ToPyFAICalibArg (Detector a) where
  toPyFAICalibArg Xpad32 = "-D xpad_flat"
  toPyFAICalibArg ImXpadS140 = "-D imxpad_s140"

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hkl.PyFAI.Calib
       ( ToPyFAICalibArg(..) ) where

import Numeric.Units.Dimensional.Prelude ((/~), nano, meter)

import Hkl.Types
import Hkl.Detector
import Hkl.PyFAI.Calibrant

class ToPyFAICalibArg a where
  toPyFAICalibArg ∷ a → String

instance ToPyFAICalibArg FilePath where
  toPyFAICalibArg f = f

instance ToPyFAICalibArg (Detector a) where
  toPyFAICalibArg Xpad32 = "-D xpad_flat"
  toPyFAICalibArg ImXpadS140 = "-D imxpad_s140"

instance ToPyFAICalibArg Calibrant where
  toPyFAICalibArg c = "-c " ++ show c

instance ToPyFAICalibArg WaveLength where
    toPyFAICalibArg w = "-w " ++ show ((w /~ nano meter) * 10)

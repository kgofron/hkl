{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hkl.PyFAI.Calib
       ( ToPyFAICalibArg(..) ) where

import Data.Text (unpack)
import Numeric.Units.Dimensional.Prelude ((/~), nano, meter)

import Hkl.Types ( WaveLength )
import Hkl.Detector ( Detector )
import Hkl.PyFAI.Calibrant ( Calibrant )
import Hkl.PyFAI.Detector ( toPyFAI )

class ToPyFAICalibArg a where
  toPyFAICalibArg ∷ a → String

instance ToPyFAICalibArg FilePath where
  toPyFAICalibArg f = f

instance ToPyFAICalibArg (Detector a) where
  toPyFAICalibArg d = "-D" ++ unpack (toPyFAI d)

instance ToPyFAICalibArg Calibrant where
  toPyFAICalibArg c = "-c " ++ show c

instance ToPyFAICalibArg WaveLength where
    toPyFAICalibArg w = "-w " ++ show ((w /~ nano meter) * 10)

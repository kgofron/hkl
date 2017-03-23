module Hkl.PyFAI.PoniExt
       ( PoniExt(..)
       , setPose
       , flip
       ) where

import Hkl.PyFAI.Poni

import Prelude hiding (flip)

data PoniExt = PoniExt Poni Pose deriving (Show)

setPose :: PoniExt -> Pose -> PoniExt
setPose (PoniExt p1 mym1) mym2 = PoniExt p mym2
  where
    p = map (poniEntrySetPose mym1 mym2) p1

flip :: PoniExt -> PoniExt
flip (PoniExt ps mym1) = PoniExt p mym1
  where
    p = map poniEntryFlip ps

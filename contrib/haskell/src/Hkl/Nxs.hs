{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hkl.Nxs
    ( DataFrameH5(..)
    , DataFrameH5Path(..)
    , NxEntry
    , Nxs(..)
    , PoniGenerator
    , XrdFlat
    , XrdOneD
    , mkNxs
    ) where

import Hkl.DataSource
import Hkl.H5
import Hkl.PyFAI

type NxEntry = String

-- to remove an put directly into OneD
type PoniGenerator = Pose -> Int -> IO PoniExt

data XrdFlat
data XrdOneD

data DataFrameH5Path a where
  XrdFlatH5Path ∷ (DataItem H5) -- ^ image
                → DataFrameH5Path XrdFlat
  XrdOneDH5Path ∷ (DataItem H5) -- ^ image
                → (DataItem H5) -- ^ gamma
                → (DataItem H5) -- ^ delta
                → (DataItem H5) -- ^ wavelength
                → DataFrameH5Path XrdOneD

deriving instance Show (DataFrameH5Path a)

data Nxs a where
  Nxs ∷ FilePath → DataFrameH5Path a → Nxs a

deriving instance Show (Nxs a)

data DataFrameH5 a where
  XrdFlatH5 ∷ (Nxs XrdFlat) -- Nexus Source file
            → File -- h5file handler
            → (DataSource H5) --images
            → DataFrameH5 XrdFlat
  DataFrameH5 ∷ (Nxs XrdOneD) -- Nexus file
              → File -- h5file handler
              → (DataSource H5) -- gamma
              → (DataSource H5) -- delta
              → (DataSource H5) -- wavelength
              → PoniGenerator -- ponie generator
              → DataFrameH5 XrdOneD


mkNxs ∷ FilePath → NxEntry → (NxEntry → DataFrameH5Path a) → Nxs a
mkNxs f e h = Nxs f (h e)

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE GADTs          #-}

module Hkl.Sample
       ( Sample(..)
       , newSample
       , withSample
       ) where

import           Control.Monad (void)
import           Data.Aeson    (FromJSON, ToJSON)
import           Foreign       (ForeignPtr, Ptr, newForeignPtr, nullPtr,
                                withForeignPtr)
import           Foreign.C     (withCString)
import           GHC.Generics  (Generic)

import           Hkl.C.Hkl
import           Hkl.Lattice
import           Hkl.Parameter
import           Hkl.Types



--  Sample

data Sample
    = Sample { sampleName    :: SampleName
             , sampleLattice :: Lattice
             , sampleUx      :: Parameter
             , sampleUy      :: Parameter
             , sampleUz      :: Parameter
             }
  deriving (Eq, FromJSON, Generic, Show, ToJSON)


withSample :: Sample -> (Ptr C'HklSample -> IO r) -> IO r
withSample s fun = do
  fptr <- newSample s
  withForeignPtr fptr fun

newSample :: Sample -> IO (ForeignPtr C'HklSample)
newSample (Sample name l ux uy uz) =
    withCString name $ \cname -> do
      sample <- c'hkl_sample_new cname
      withLattice l $ \lattice -> do
          c'hkl_sample_lattice_set sample lattice
          go sample ux c'hkl_sample_ux_get c'hkl_sample_ux_set
          go sample uy c'hkl_sample_uy_get c'hkl_sample_uy_set
          go sample uz c'hkl_sample_uz_get c'hkl_sample_uz_set
          newForeignPtr p'hkl_sample_free sample
            where
              go s p getter setter = do
                fptr <- copyParameter =<< getter s
                withForeignPtr fptr $ \ptr -> do
                  pokeParameter ptr p
                  void $ setter s ptr nullPtr

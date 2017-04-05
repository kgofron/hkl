{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

module Hkl.C.Sample
       ( HklSample
       , newSample
       , withSample
       ) where

import Prelude hiding (min, max)

import Control.Monad (void)
import Foreign ( ForeignPtr
               , FunPtr
               , Ptr
               , nullPtr
               , newForeignPtr
               , withForeignPtr)
import Foreign.C (CInt(..), CString, withCString)
import Foreign.Storable

import Hkl.C.Lattice
import Hkl.Types

#include "hkl.h"

-- private types

data HklSample

-- Sample

withSample :: Sample a -> (Ptr HklSample -> IO r) -> IO r
withSample s fun = do
  fptr <- newSample s
  withForeignPtr fptr fun

newSample :: Sample a -> IO (ForeignPtr HklSample)
newSample (Sample name l ux uy uz) =
    withCString name $ \cname -> do
      sample <- c_hkl_sample_new cname
      withLattice l $ \lattice -> do
          c_hkl_sample_lattice_set sample lattice
          go sample ux c_hkl_sample_ux_get c_hkl_sample_ux_set
          go sample uy c_hkl_sample_uy_get c_hkl_sample_uy_set
          go sample uz c_hkl_sample_uz_get c_hkl_sample_uz_set
          newForeignPtr c_hkl_sample_free sample
            where
              go s p getter setter = do
                fptr <- copyParameter =<< (getter s)
                withForeignPtr fptr $ \ptr -> do
                  poke ptr p
                  void $ setter s ptr nullPtr

foreign import ccall unsafe "hkl.h hkl_sample_new"
  c_hkl_sample_new:: CString -> IO (Ptr HklSample)

foreign import ccall unsafe "hkl.h hkl_sample_lattice_set"
  c_hkl_sample_lattice_set :: Ptr HklSample -> Ptr HklLattice -> IO ()

foreign import ccall unsafe "hkl.h &hkl_sample_free"
  c_hkl_sample_free :: FunPtr (Ptr HklSample -> IO ())

foreign import ccall unsafe "hkl.h hkl_sample_ux_get"
  c_hkl_sample_ux_get :: Ptr HklSample
                      -> IO (Ptr Parameter)

foreign import ccall unsafe "hkl.h hkl_sample_uy_get"
  c_hkl_sample_uy_get :: Ptr HklSample
                      -> IO (Ptr Parameter)

foreign import ccall unsafe "hkl.h hkl_sample_uz_get"
  c_hkl_sample_uz_get :: Ptr HklSample
                      -> IO (Ptr Parameter)

foreign import ccall unsafe "hkl.h hkl_sample_ux_set"
  c_hkl_sample_ux_set :: Ptr HklSample
                      -> Ptr Parameter
                      -> Ptr ()
                      -> IO CInt

foreign import ccall unsafe "hkl.h hkl_sample_uy_set"
  c_hkl_sample_uy_set :: Ptr HklSample
                      -> Ptr Parameter
                      -> Ptr ()
                      -> IO CInt

foreign import ccall unsafe "hkl.h hkl_sample_uz_set"
  c_hkl_sample_uz_set :: Ptr HklSample
                      -> Ptr Parameter
                      -> Ptr ()
                      -> IO CInt

{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}

module Hkl.C.EngineList
       ( HklEngineList
       , engineListEnginesGet
       , newEngineList
       , withEngineList
       ) where

import Prelude hiding (min, max)

import Foreign ( ForeignPtr
               , FunPtr
               , Ptr
               , newForeignPtr
               , withForeignPtr
               , peekArray)
import Foreign.C ( CSize(..) )
import Foreign.Storable

import Hkl.C.Engine
import Hkl.C.Geometry
import Hkl.Types

#include "hkl.h"

-- private types

data HklEngineList

-- EngineList

withEngineList :: Factory -> (Ptr HklEngineList -> IO b) -> IO b
withEngineList f func = do
  fptr <- newEngineList f
  withForeignPtr fptr func

newEngineList :: Factory -> IO (ForeignPtr HklEngineList)
newEngineList f = newFactory f
                  >>= c_hkl_factory_create_new_engine_list
                  >>= newForeignPtr c_hkl_engine_list_free

foreign import ccall unsafe "hkl.h hkl_factory_create_new_engine_list"
  c_hkl_factory_create_new_engine_list:: Ptr HklFactory -> IO (Ptr HklEngineList)

foreign import ccall unsafe "hkl.h &hkl_engine_list_free"
  c_hkl_engine_list_free :: FunPtr (Ptr HklEngineList -> IO ())

engineListEnginesGet :: Ptr HklEngineList -> IO [Engine]
engineListEnginesGet e = do
  pdarray <- c_hkl_engine_list_engines_get e
  n <- (#{peek darray_engine, size} pdarray) :: IO CSize
  engines <- #{peek darray_engine ,item} pdarray :: IO (Ptr (Ptr HklEngine))
  enginess <- peekArray (fromEnum n) engines
  mapM peekEngine enginess

foreign import ccall unsafe "hkl.h hkl_engine_list_engines_get"
  c_hkl_engine_list_engines_get:: Ptr HklEngineList -> IO (Ptr ())

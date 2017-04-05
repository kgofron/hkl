{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}

module Hkl.C.Engine
       ( HklEngine
       , engineName
       , peekEngine
       ) where

import Prelude hiding (min, max)

import Foreign (Ptr, nullPtr)
import Foreign.C (CString, peekCString)
import Foreign.Storable

import Hkl.C.DArray
import Hkl.Types

#include "hkl.h"

-- private types

data HklEngine

-- Engine

engineName :: Engine -> String
engineName (Engine name _ _) = name

-- Engine

peekMode :: Ptr HklEngine -> IO Mode
peekMode e = do
  name <- c_hkl_engine_current_mode_get e >>= peekCString
  (DArray _ ns) <- peek =<< c_hkl_engine_parameters_names_get e
  parameters <- mapM f ns
  return (Mode name parameters)
  where
    f n = (c_hkl_engine_parameter_get e n nullPtr >>= peek)

foreign import ccall unsafe "hkl.h hkl_engine_current_mode_get"
  c_hkl_engine_current_mode_get :: Ptr HklEngine -> IO CString

foreign import ccall unsafe "hkl.h hkl_engine_parameters_names_get"
  c_hkl_engine_parameters_names_get:: Ptr HklEngine -> IO (Ptr (DArray CString))

foreign import ccall unsafe "hkl.h hkl_engine_parameter_get"
  c_hkl_engine_parameter_get:: Ptr HklEngine -> CString -> Ptr () -> IO (Ptr Parameter) -- darray_string


peekEngine :: Ptr HklEngine -> IO Engine
peekEngine e = do
  name <- peekCString =<< c_hkl_engine_name_get e
  ps <- enginePseudoAxesGet e
  mode <- peekMode e
  return (Engine name ps mode)

-- engineNameGet :: Ptr HklEngine -> IO String
-- engineNameGet engine = c_hkl_engine_name_get engine >>= peekCString

foreign import ccall unsafe "hkl.h hkl_engine_name_get"
  c_hkl_engine_name_get :: Ptr HklEngine -> IO CString

foreign import ccall unsafe "hkl.h hkl_engine_pseudo_axis_names_get"
  c_hkl_engine_pseudo_axis_names_get:: Ptr HklEngine -> IO (Ptr (DArray CString))

-- enginePseudoAxisNamesGet :: Ptr HklEngine -> IO [String]
-- enginePseudoAxisNamesGet e = enginePseudoAxisNamesGet' e >>= mapM peekCString

enginePseudoAxisGet :: Ptr HklEngine -> CString -> IO Parameter
enginePseudoAxisGet e n = c_hkl_engine_pseudo_axis_get e n nullPtr >>= peek

foreign import ccall unsafe "hkl.h hkl_engine_pseudo_axis_get"
  c_hkl_engine_pseudo_axis_get:: Ptr HklEngine -> CString -> Ptr () -> IO (Ptr Parameter)

enginePseudoAxesGet :: Ptr HklEngine -> IO [Parameter]
enginePseudoAxesGet ptr = do
  (DArray _ ns) <- peek =<< c_hkl_engine_pseudo_axis_names_get ptr
  mapM (enginePseudoAxisGet ptr) ns


{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

module Hkl.C.Geometry
       ( Geometry(..)
       , Factory(..)
       , HklDetector
       , HklFactory
       , HklMatrix
       , HklQuaternion
       , factoryFromString
       , newFactory
       , newGeometry
       , peekGeometry
       , withGeometry
       ) where

import Prelude hiding (min, max)

import Numeric.LinearAlgebra
import Foreign ( ForeignPtr
               , FunPtr
               , Ptr
               , nullPtr
               , newForeignPtr
               , withForeignPtr
               , peekArray)
import Foreign.C (CInt(..), CDouble(..), CSize(..), CString,
                 peekCString, withCString)
import Foreign.Storable

import Numeric.Units.Dimensional.Prelude ( meter, nano
                                         , (*~), (/~))

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV

import Hkl.Types

#include "hkl.h"

-- | Factory

data Factory = K6c | Uhv | MedH | MedV | SoleilSiriusKappa

instance Show Factory where
  show K6c = "K6C"
  show Uhv = "ZAXIS"
  show MedH = "todo"
  show MedV = "todo"
  show SoleilSiriusKappa = "SOLEIL SIRIUS KAPPA"

factoryFromString :: String -> Factory
factoryFromString s
  | s == "K6C"  = K6c
  | s == "ZAXIS" = Uhv
  | s == "todo" = MedH
  | s == "todo" = MedV
  | s == "SOLEIL SIRIUS KAPPA" = SoleilSiriusKappa
  | otherwise   = error $ "unknown diffractometer type:" ++ s

-- | Geometry

data Geometry = Geometry
                Factory -- ^ the type of diffractometer
                Source -- ^ source
                (Vector Double) -- ^ axes position
                (Maybe [Parameter]) -- ^ axes configuration
              deriving (Show)


-- private types

data HklDetector
data HklFactory
data HklMatrix
data HklQuaternion

-- helpers

peekDArrayString :: Ptr () -> IO [CString]
peekDArrayString p = do
  n <- (#{peek darray_string, size} p) :: IO CSize
  items <- #{peek darray_string ,item} p :: IO (Ptr CString)
  peekArray (fromEnum n) items

darrayStringLen :: Ptr () -> IO (CSize)
darrayStringLen p = do
    n <- (#{peek darray_string, size} p) :: IO CSize
    return n

-- Factory

newFactory :: Factory -> IO (Ptr HklFactory)
newFactory f = withCString (show f) $ \cname -> c_hkl_factory_get_by_name cname nullPtr

foreign import ccall unsafe "hkl.h hkl_factory_get_by_name"
  c_hkl_factory_get_by_name :: CString  -- ^ name
                            -> Ptr () -- ^ GError (null for now)
                            -> IO (Ptr HklFactory)
-- Geometry

peekGeometry :: Ptr Geometry -> IO (Geometry)
peekGeometry gp = do
  f_name <- c_hkl_geometry_name_get gp >>= peekCString
  let factory = factoryFromString f_name
  (CDouble w) <- c_hkl_geometry_wavelength_get gp unit
  darray <- c_hkl_geometry_axis_names_get gp
  n <- darrayStringLen darray
  v <- MV.new (fromEnum n)
  MV.unsafeWith v $ \values ->
      c_hkl_geometry_axis_values_get gp values n unit
  vs <- V.freeze v

  axis_names <- peekDArrayString darray
  ps <- mapM (getAxis gp) axis_names
  return $ Geometry factory (Source (w *~ nano meter)) vs (Just ps)
      where
        getAxis :: Ptr Geometry -> CString -> IO Parameter
        getAxis _g n = c_hkl_geometry_axis_get _g n nullPtr >>= peek

foreign import ccall unsafe "hkl.h hkl_geometry_wavelength_get"
  c_hkl_geometry_wavelength_get :: Ptr Geometry -- geometry
                                -> CInt -- unit
                                -> IO CDouble -- wavelength


foreign import ccall unsafe "hkl.h hkl_geometry_axis_values_get"
  c_hkl_geometry_axis_values_get :: Ptr Geometry -- geometry
                                 -> Ptr Double -- axis values
                                 -> CSize -- size of axis values
                                 -> CInt -- unit
                                 -> IO () -- IO CInt but for now do not deal with the errors

foreign import ccall unsafe "hkl.h hkl_geometry_axis_names_get"
  c_hkl_geometry_axis_names_get :: Ptr Geometry -- goemetry
                                -> IO (Ptr ()) -- darray_string

foreign import ccall unsafe "hkl.h hkl_geometry_axis_get"
  c_hkl_geometry_axis_get :: Ptr Geometry -- geometry
                          -> CString -- axis name
                          -> Ptr () -- gerror
                          -> IO (Ptr Parameter) -- parameter or nullPtr

foreign import ccall unsafe "hkl.h hkl_geometry_name_get"
  c_hkl_geometry_name_get :: Ptr Geometry -> IO CString


withGeometry ::  Geometry -> (Ptr Geometry -> IO b) -> IO b
withGeometry g fun = do
  fptr <- newGeometry g
  withForeignPtr fptr fun

newGeometry :: Geometry -> IO (ForeignPtr Geometry)
newGeometry (Geometry f (Source lw) vs _ps) = do
  let wavelength = CDouble (lw /~ nano meter)
  factory <- newFactory f
  geometry <- c_hkl_factory_create_new_geometry factory
  c_hkl_geometry_wavelength_set geometry wavelength unit nullPtr
  darray <- c_hkl_geometry_axis_names_get geometry
  n <- darrayStringLen darray
  V.unsafeWith vs $ \values ->
      c_hkl_geometry_axis_values_set geometry values n unit nullPtr

  newForeignPtr c_hkl_geometry_free geometry

foreign import ccall unsafe "hkl.h hkl_factory_create_new_geometry"
  c_hkl_factory_create_new_geometry :: Ptr HklFactory -> IO (Ptr Geometry)

foreign import ccall unsafe "hkl.h &hkl_geometry_free"
  c_hkl_geometry_free :: FunPtr (Ptr Geometry -> IO ())

foreign import ccall unsafe "hkl.h hkl_geometry_wavelength_set"
  c_hkl_geometry_wavelength_set :: Ptr Geometry -- geometry
                                -> CDouble -- wavelength
                                -> CInt -- unit
                                -> Ptr () -- *gerror
                                -> IO () -- IO CInt but for now do not deal with the errors

foreign import ccall unsafe "hkl.h hkl_geometry_axis_values_set"
  c_hkl_geometry_axis_values_set :: Ptr Geometry -- geometry
                                 -> Ptr Double -- axis values
                                 -> CSize -- size of axis values
                                 -> CInt -- unit
                                 -> Ptr () -- gerror
                                 -> IO () -- IO CInt but for now do not deal with the errors

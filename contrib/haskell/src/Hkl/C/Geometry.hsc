{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}

module Hkl.C.Geometry
       ( Geometry(..)
       , Factory(..)
       , HklFactory
       , HklMatrix
       , HklQuaternion
       , factoryFromString
       , newFactory
       , newGeometry
       , withGeometry
       ) where

import Prelude hiding (min, max)

import Numeric.LinearAlgebra
import Foreign ( ForeignPtr
               , FunPtr
               , Ptr
               , nullPtr
               , newForeignPtr
               , withForeignPtr)
import Foreign.C (CInt(..), CDouble(..), CSize(..), CString,
                 peekCString, withCString)
import Foreign.Storable

import Numeric.Units.Dimensional.Prelude ( meter, nano
                                         , (*~), (/~))

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV

import Hkl.Types
import Hkl.C.DArray

#include "hkl.h"

--  Factory

data Factory = K6c | Fixe | Uhv | Mars | MedH | MedV | SoleilSiriusKappa

instance Show Factory where
  show K6c = "K6C"
  show Fixe = undefined
  show Uhv = "ZAXIS"
  show Mars = "SOLEIL MARS"
  show MedH = "SOLEIL SIXS MED1+2"
  show MedV = "SOLEIL SIXS MED2+3"
  show SoleilSiriusKappa = "SOLEIL SIRIUS KAPPA"

factoryFromString :: String -> Factory
factoryFromString s
  | s == "K6C"  = K6c
  | s == undefined = Fixe
  | s == "ZAXIS" = Uhv
  | s == "SOLEIL MARS" = Mars
  | s == undefined = MedH
  | s == "SOLEIL SIXS MED2+3" = MedV
  | s == "SOLEIL SIRIUS KAPPA" = SoleilSiriusKappa
  | otherwise   = error $ "unknown diffractometer type:" ++ s

--  Geometry

data Geometry = Geometry
                Factory --  the type of diffractometer
                Source --  source
                (Vector Double) --  axes position
                (Maybe [Parameter]) --  axes configuration
              deriving (Show)


-- private types

data HklFactory
data HklMatrix
data HklQuaternion

#if __GLASGOW_HASKELL__ <= 710
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

-- Factory

newFactory :: Factory -> IO (Ptr HklFactory)
newFactory f = withCString (show f) $ \cname -> c_hkl_factory_get_by_name cname nullPtr

foreign import ccall unsafe "hkl.h hkl_factory_get_by_name"
  c_hkl_factory_get_by_name :: CString  --  name
                            -> Ptr () --  GError (null for now)
                            -> IO (Ptr HklFactory)
-- Geometry

peekSource :: Ptr Geometry -> IO (Source)
peekSource ptr = do
    (CDouble w) <- c_hkl_geometry_wavelength_get ptr unit
    return (Source (w *~ nano meter))

foreign import ccall unsafe "hkl.h hkl_geometry_wavelength_set"
  c_hkl_geometry_wavelength_set :: Ptr Geometry -- geometry
                                -> CDouble -- wavelength
                                -> CInt -- unit
                                -> Ptr () -- gerror
                                -> IO () -- IO CInt but for now do not deal with the errors

pokeSource :: Ptr Geometry -> Source -> IO ()
pokeSource ptr (Source lw) = do
  let wavelength = CDouble (lw /~ nano meter)
  c_hkl_geometry_wavelength_set ptr wavelength unit nullPtr

foreign import ccall unsafe "hkl.h hkl_geometry_wavelength_get"
  c_hkl_geometry_wavelength_get :: Ptr Geometry -- geometry
                                -> CInt -- unit
                                -> IO CDouble -- wavelength

peekAxis :: Ptr Geometry -> CString -> IO Parameter
peekAxis ptr s = c_hkl_geometry_axis_get ptr s nullPtr >>= peek

instance Storable Geometry where
  alignment _ = #{alignment int}

  sizeOf _ = #{size int}

  peek ptr = do
    f_name <- c_hkl_geometry_name_get ptr >>= peekCString
    let factory = factoryFromString f_name

    source <- peekSource ptr

    (DArray n axis_names) <- peek =<< c_hkl_geometry_axis_names_get ptr
    v <- MV.new (fromEnum n)
    MV.unsafeWith v $ \values ->
      c_hkl_geometry_axis_values_get ptr values n unit
    vs <- V.freeze v

    ps <- mapM (peekAxis ptr) axis_names

    return $ Geometry factory source vs (Just ps)

  poke ptr (Geometry _ s vs _) = do
    pokeSource ptr s
    (DArray n _) <- peek =<< c_hkl_geometry_axis_names_get ptr
    V.unsafeWith vs $ \values ->
      c_hkl_geometry_axis_values_set ptr values n unit nullPtr

foreign import ccall unsafe "hkl.h hkl_geometry_axis_values_get"
  c_hkl_geometry_axis_values_get :: Ptr Geometry -- geometry
                                 -> Ptr Double -- axis values
                                 -> CSize -- size of axis values
                                 -> CInt -- unit
                                 -> IO () -- IO CInt but for now do not deal with the errors

foreign import ccall unsafe "hkl.h hkl_geometry_axis_names_get"
  c_hkl_geometry_axis_names_get :: Ptr Geometry -- goemetry
                                -> IO (Ptr (DArray CString)) -- darray_string

foreign import ccall unsafe "hkl.h hkl_geometry_axis_get"
  c_hkl_geometry_axis_get :: Ptr Geometry -- geometry
                          -> CString -- axis name
                          -> Ptr () -- gerror
                          -> IO (Ptr Parameter) -- parameter or nullPtr

foreign import ccall unsafe "hkl.h hkl_geometry_name_get"
  c_hkl_geometry_name_get :: Ptr Geometry -> IO CString

foreign import ccall unsafe "hkl.h hkl_geometry_axis_values_set"
  c_hkl_geometry_axis_values_set :: Ptr Geometry -- geometry
                                 -> Ptr Double -- axis values
                                 -> CSize -- size of axis values
                                 -> CInt -- unit
                                 -> Ptr () -- gerror
                                 -> IO () -- IO CInt but for now do not deal with the errors

withGeometry ::  Geometry -> (Ptr Geometry -> IO b) -> IO b
withGeometry g fun = do
  fptr <- newGeometry g
  withForeignPtr fptr fun

newGeometry :: Geometry -> IO (ForeignPtr Geometry)
newGeometry g@(Geometry f _ _ _) = do
  ptr <- c_hkl_factory_create_new_geometry =<< newFactory f
  poke ptr g
  newForeignPtr c_hkl_geometry_free ptr

foreign import ccall unsafe "hkl.h hkl_factory_create_new_geometry"
  c_hkl_factory_create_new_geometry :: Ptr HklFactory -> IO (Ptr Geometry)

foreign import ccall unsafe "hkl.h &hkl_geometry_free"
  c_hkl_geometry_free :: FunPtr (Ptr Geometry -> IO ())

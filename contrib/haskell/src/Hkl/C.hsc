{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

module Hkl.C
       ( compute
       , computePipe
       , solve
       , solveTraj
       , solveTrajPipe
       , module X
       ) where

import Prelude hiding (min, max)

import Control.Monad (forever)
import Control.Monad.Trans.State.Strict
import Foreign ( ForeignPtr
               , FunPtr
               , Ptr
               , nullPtr
               , newForeignPtr
               , withForeignPtr
               , peekArray
               , withArray)
import Foreign.C (CInt(..), CSize(..), CString,
                 peekCString, withCString)
import Foreign.Storable

import Pipes (Pipe, await, lift, yield)

import Hkl.C.DArray as X
import Hkl.C.Detector as X
import Hkl.C.Geometry as X
import Hkl.C.GeometryList as X
import Hkl.C.Lattice as X
import Hkl.C.Sample as X
import Hkl.Detector
import Hkl.Types

#include "hkl.h"

-- private types

data HklEngine
data HklEngineList

-- Engine

solve' :: Ptr HklEngine -> Engine -> IO (ForeignPtr HklGeometryList)
solve' engine (Engine _ ps _) = do
  let positions = [v | (Parameter _ v _) <- ps]
  let n = toEnum (length positions)
  withArray positions $ \values ->
      c_hkl_engine_pseudo_axis_values_set engine values n unit nullPtr
      >>= newForeignPtr c_hkl_geometry_list_free

solve :: Geometry -> Detector a -> Sample b -> Engine -> IO [Geometry]
solve g@(Geometry f _ _ _) d s e@(Engine name _ _) = do
  withSample s $ \sample ->
      withDetector d $ \detector ->
          withGeometry g $ \geometry ->
              withEngineList f $ \engines ->
                  withCString name $ \cname -> do
                  c_hkl_engine_list_init engines geometry detector sample
                  engine <- c_hkl_engine_list_engine_get_by_name engines cname nullPtr
                  solve' engine e >>= peekHklGeometryList

engineName :: Engine -> String
engineName (Engine name _ _) = name

solveTraj :: Geometry -> Detector a -> Sample b -> [Engine] -> IO [Geometry]
solveTraj g@(Geometry f _ _ _) d s es = do
  let name = engineName (head es)
  withSample s $ \sample ->
      withDetector d $ \detector ->
          withGeometry g $ \geometry ->
              withEngineList f $ \engines ->
                withCString name $ \cname -> do
                  c_hkl_engine_list_init engines geometry detector sample
                  engine <- c_hkl_engine_list_engine_get_by_name engines cname nullPtr
                  mapM (\e -> solve' engine e >>= getSolution0) es

-- Pipe

data Diffractometer = Diffractometer { difEngineList :: (ForeignPtr HklEngineList)
                                     , difGeometry :: (ForeignPtr Geometry)
                                     , difDetector :: (ForeignPtr HklDetector)
                                     , difSample :: (ForeignPtr HklSample)
                                     }
                    deriving (Show)

withDiffractometer :: Diffractometer -> (Ptr HklEngineList -> IO b) -> IO b
withDiffractometer d fun = do
  let f_engines = difEngineList d
  withForeignPtr f_engines fun

newDiffractometer :: Geometry -> Detector a -> Sample b -> IO Diffractometer
newDiffractometer g@(Geometry f _ _ _) d s = do
  f_engines <- newEngineList f
  f_geometry <- newGeometry g
  f_detector <- newDetector d
  f_sample <- newSample s
  withForeignPtr f_sample $ \sample ->
    withForeignPtr f_detector $ \detector ->
    withForeignPtr f_geometry $ \geometry ->
    withForeignPtr f_engines $ \engines -> do
      c_hkl_engine_list_init engines geometry detector sample
      return $ Diffractometer { difEngineList = f_engines
                              , difGeometry = f_geometry
                              , difDetector = f_detector
                              , difSample = f_sample
                              }

computePipe :: Detector a -> Sample b -> Pipe Geometry [Engine] IO ()
computePipe d s = forever $ do
  g <- await
  e <- lift $ compute g d s
  yield e

solveTrajPipe :: Geometry -> Detector a -> Sample b -> Pipe Engine Geometry IO ()
solveTrajPipe g d s = do
  dif <- lift $ newDiffractometer g d s
  solveTrajPipe' dif

solveTrajPipe' :: Diffractometer -> Pipe Engine Geometry IO ()
solveTrajPipe' dif = flip evalStateT dif $ forever $ do
    -- Inside here we are using `StateT Diffractometer (Pipe Engine Geometry IO ()) r`
    e <- lift await
    dif_ <- get
    let name = engineName e
    solutions <- lift . lift $ withDiffractometer dif_ $ \engines ->
     withCString name $ \cname -> do
       engine <- c_hkl_engine_list_engine_get_by_name engines cname nullPtr
       solve' engine e >>= getSolution0
    put dif_
    lift $ yield solutions

foreign import ccall unsafe "hkl.h hkl_engine_list_engine_get_by_name"
  c_hkl_engine_list_engine_get_by_name :: Ptr HklEngineList --engine list
                                       -> CString -- engine name
                                       -> Ptr () --  gerror need to deal about this
                                       -> IO (Ptr HklEngine) -- the returned HklEngine

foreign import ccall unsafe "hkl.h hkl_engine_pseudo_axis_values_set"
  c_hkl_engine_pseudo_axis_values_set :: Ptr HklEngine
                                      -> Ptr Double --values
                                      -> CSize -- n_values
                                      -> CInt -- unit_type
                                      -> Ptr () --  GError **error
                                      -> IO (Ptr HklGeometryList)

foreign import ccall unsafe "hkl.h &hkl_geometry_list_free"
  c_hkl_geometry_list_free :: FunPtr (Ptr HklGeometryList -> IO ())

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

compute :: Geometry -> Detector a -> Sample b -> IO [Engine]
compute g@(Geometry f _ _ _) d s = do
  withSample s $ \sample ->
      withDetector d $ \detector ->
          withGeometry g $ \geometry ->
              withEngineList f $ \engines -> do
                    c_hkl_engine_list_init engines geometry detector sample
                    c_hkl_engine_list_get engines
                    engineListEnginesGet engines

foreign import ccall unsafe "hkl.h hkl_engine_list_init"
  c_hkl_engine_list_init:: Ptr HklEngineList -> Ptr Geometry -> Ptr HklDetector -> Ptr HklSample -> IO ()

foreign import ccall unsafe "hkl.h hkl_engine_list_get"
  c_hkl_engine_list_get:: Ptr HklEngineList -> IO ()

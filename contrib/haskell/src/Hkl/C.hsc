{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs                    #-}
{-
    Copyright  : Copyright (C) 2014-2019 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}
module Hkl.C
       ( compute
       , computePipe
       , solve
       , solveTraj
       , solveTrajPipe
       , module X
       ) where

import           Prelude                          hiding (max, min)

import           Control.Monad                    (forever, (>=>))
import           Control.Monad.Trans.State.Strict
import           Foreign                          (ForeignPtr, FunPtr, Ptr,
                                                   newForeignPtr, nullPtr,
                                                   withArray, withForeignPtr)
import           Foreign.C                        (CInt (..), CSize (..),
                                                   CString, withCString)

import           Pipes                            (Pipe, await, lift, yield)

import           Hkl.C.Binoculars                 as X
import           Hkl.C.Detector
import           Hkl.C.Engine
import           Hkl.C.EngineList
import           Hkl.C.Geometry                   as X
import           Hkl.C.GeometryList               as X
import           Hkl.C.Sample
import           Hkl.Detector
import           Hkl.Types

#include "hkl.h"

-- Engine

solve' :: Ptr HklEngine -> Engine -> IO (ForeignPtr HklGeometryList)
solve' engine (Engine _ ps _) = do
  let positions = [v | (Parameter _ v _) <- ps]
  let n = toEnum (length positions)
  withArray positions $ \values ->
      c_hkl_engine_pseudo_axis_values_set engine values n unit nullPtr
      >>= newForeignPtr c_hkl_geometry_list_free

solve :: Geometry -> Detector a sh -> Sample b -> Engine -> IO [Geometry]
solve g@(Geometry f _ _ _) d s e@(Engine name _ _) =
  withSample s $ \sample ->
      withDetector d $ \detector ->
          withGeometry g $ \geometry ->
              withEngineList f $ \engines ->
                  withCString name $ \cname -> do
                  c_hkl_engine_list_init engines geometry detector sample
                  engine <- c_hkl_engine_list_engine_get_by_name engines cname nullPtr
                  solve' engine e >>= peekHklGeometryList

solveTraj :: Geometry -> Detector a sh -> Sample b -> [Engine] -> IO [Geometry]
solveTraj g@(Geometry f _ _ _) d s es = do
  let name = engineName (head es)
  withSample s $ \sample ->
      withDetector d $ \detector ->
          withGeometry g $ \geometry ->
              withEngineList f $ \engines ->
                withCString name $ \cname -> do
                  c_hkl_engine_list_init engines geometry detector sample
                  engine <- c_hkl_engine_list_engine_get_by_name engines cname nullPtr
                  mapM (solve' engine >=> getSolution0) es

-- Pipe

data Diffractometer = Diffractometer { difEngineList :: ForeignPtr HklEngineList
                                     , difGeometry   :: ForeignPtr Geometry
                                     , difDetector   :: ForeignPtr HklDetector
                                     , difSample     :: ForeignPtr HklSample
                                     }
                    deriving (Show)

withDiffractometer :: Diffractometer -> (Ptr HklEngineList -> IO b) -> IO b
withDiffractometer d fun = do
  let f_engines = difEngineList d
  withForeignPtr f_engines fun

newDiffractometer :: Geometry -> Detector a sh -> Sample b -> IO Diffractometer
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

computePipe :: Detector a sh -> Sample b -> Pipe Geometry [Engine] IO ()
computePipe d s = forever $ do
  g <- await
  e <- lift $ compute g d s
  yield e

solveTrajPipe :: Geometry -> Detector a sh -> Sample b -> Pipe Engine Geometry IO ()
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

compute :: Geometry -> Detector a sh -> Sample b -> IO [Engine]
compute g@(Geometry f _ _ _) d s =
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

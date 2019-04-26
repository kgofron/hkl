{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

module Hkl.C.GeometryList
       ( HklGeometryList
       , geometryDetectorRotationGet
       , getSolution0
       , peekHklGeometryList
       ) where

import Prelude hiding (min, max)

import Control.Monad.Loops (unfoldrM)
import Numeric.LinearAlgebra
import Foreign ( ForeignPtr
               , FunPtr
               , Ptr
               , nullPtr
               , newForeignPtr
               , withForeignPtr)
import Foreign.C (CInt(..), CDouble(..))
import Foreign.Storable

import Hkl.C.Detector
import Hkl.C.Geometry
import Hkl.Detector

#include "hkl.h"

-- private types

data HklGeometryList
data HklGeometryListItem

-- | HklGeometryList

getSolution0 :: ForeignPtr HklGeometryList -> IO Geometry
getSolution0 gl = withForeignPtr gl $ \solutions ->
                  c_hkl_geometry_list_items_first_get solutions
                  >>= c_hkl_geometry_list_item_geometry_get
                  >>= peek

buildMatrix' :: Element a => CInt -> CInt -> ((CInt, CInt) -> IO a) -> IO (Matrix a)
buildMatrix' rc cc f = do
   let coordinates' = map (\ ri -> map (\ ci -> (ri, ci)) [0 .. (cc - 1)]) [0 .. (rc - 1)]
   l <- mapM (mapM f) coordinates'
   return $ fromLists l


    -- fromLists $ map (map f)
    --     $ map (\ ri -> map (\ ci -> (ri, ci)) [0 .. (cc - 1)]) [0 .. (rc - 1)]

geometryDetectorRotationGet :: Geometry -> Detector a -> IO (Matrix Double)
geometryDetectorRotationGet g d  = do
  f_geometry <- newGeometry g
  f_detector <- newDetector d
  withForeignPtr f_detector $ \detector ->
    withForeignPtr f_geometry $ \geometry -> do
      f_q <- newForeignPtr c_hkl_quaternion_free =<< c_hkl_geometry_detector_rotation_get_binding geometry detector
      withForeignPtr f_q $ \quaternion -> do
        f_m <- newForeignPtr c_hkl_matrix_free =<< c_hkl_quaternion_to_matrix_binding quaternion
        withForeignPtr f_m $ \matrix' ->
          buildMatrix' 3 3 (getV matrix')
          where
            getV :: Ptr HklMatrix -> (CInt, CInt) -> IO Double
            getV m (i', j') = do
              (CDouble v) <- c_hkl_matrix_get m i' j'
              return v

foreign import ccall unsafe "hkl.h hkl_geometry_detector_rotation_get_binding"
  c_hkl_geometry_detector_rotation_get_binding :: Ptr Geometry
                                               -> Ptr HklDetector
                                               -> IO (Ptr HklQuaternion)

foreign import ccall unsafe "hkl.h hkl_quaternion_to_matrix_binding"
  c_hkl_quaternion_to_matrix_binding :: Ptr HklQuaternion
                                     -> IO (Ptr HklMatrix)

foreign import ccall unsafe "hkl.h &free"
  c_hkl_quaternion_free :: FunPtr (Ptr HklQuaternion -> IO ())

foreign import ccall unsafe "hkl.h &hkl_matrix_free"
  c_hkl_matrix_free :: FunPtr (Ptr HklMatrix -> IO ())

foreign import ccall unsafe "hkl.h hkl_matrix_get"
  c_hkl_matrix_get :: Ptr HklMatrix
                   -> CInt
                   -> CInt
                   -> IO CDouble


peekItems :: Ptr HklGeometryList -> IO [Ptr HklGeometryListItem]
peekItems l = c_hkl_geometry_list_items_first_get l >>= unfoldrM go
   where
      go e
         | e == nullPtr = return Nothing
         | otherwise    = do
               next <- c_hkl_geometry_list_items_next_get l e
               return (Just (e, next))

peekHklGeometryList :: ForeignPtr HklGeometryList -> IO [Geometry]
peekHklGeometryList l = withForeignPtr l $ \ls -> do
  items <- peekItems ls
  mapM extract items
    where
      extract it = c_hkl_geometry_list_item_geometry_get it >>= peek

foreign import ccall unsafe "hkl.h hkl_geometry_list_items_first_get"
  c_hkl_geometry_list_items_first_get :: Ptr HklGeometryList
                                      -> IO (Ptr HklGeometryListItem)

foreign import ccall unsafe "hkl.h hkl_geometry_list_items_next_get"
  c_hkl_geometry_list_items_next_get :: Ptr HklGeometryList
                                     -> Ptr HklGeometryListItem
                                     -> IO (Ptr HklGeometryListItem)

foreign import ccall unsafe "hkl.h hkl_geometry_list_item_geometry_get"
  c_hkl_geometry_list_item_geometry_get :: Ptr HklGeometryListItem
                                        -> IO (Ptr Geometry)

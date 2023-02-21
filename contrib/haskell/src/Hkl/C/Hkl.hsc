#include "hkl.h"
#include <bindings.dsl.h>

{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Hkl.C.Hkl where

import Foreign ( FunPtr
               , Ptr
               , Storable(..)
               , plusPtr
               , )

import Foreign.C (CInt(..), CUInt(..), CDouble(..), CSize(..), CString)


-- darray_engine

#starttype darray_engine
#field item , Ptr (Ptr <HklEngine>)
#field size , CSize
#field alloc , CSize
#stoptype

-- darray_string

#starttype darray_string
#field item , Ptr CString
#field size , CSize
#field alloc , CSize
#stoptype

-- HklDetector

#opaque_t HklDetector

#integral_t HklDetectorType

#num HKL_DETECTOR_TYPE_0D


#ccall hkl_detector_factory_new, <HklDetectorType> -> IO (Ptr <HklDetector>)
#ccall hkl_detector_free, Ptr <HklDetector> -> IO ()

-- HklEngine

#opaque_t HklEngine

#ccall hkl_engine_current_mode_get, Ptr <HklEngine> -> IO CString
#ccall hkl_engine_name_get, Ptr <HklEngine> -> IO CString
#ccall hkl_engine_parameter_get, Ptr <HklEngine> -> CString -> Ptr () -> IO (Ptr <HklParameter>)
#ccall hkl_engine_parameters_names_get, Ptr <HklEngine> -> IO (Ptr <darray_string>)
#ccall hkl_engine_pseudo_axis_get, Ptr <HklEngine> -> CString -> Ptr () -> IO (Ptr <HklParameter>)
#ccall hkl_engine_pseudo_axis_names_get, Ptr <HklEngine> -> IO (Ptr <darray_string>)
#ccall hkl_engine_pseudo_axis_values_set, Ptr <HklEngine> -> Ptr CDouble -> CSize -> CInt -> Ptr ()-> IO (Ptr <HklGeometryList>)


-- HklEngineList

#opaque_t HklEngineList

#ccall hkl_engine_list_engine_get_by_name, Ptr <HklEngineList> -> CString -> Ptr () -> IO (Ptr <HklEngine>)
#ccall hkl_engine_list_engines_get, Ptr <HklEngineList> -> IO (Ptr <darray_engine>)
#ccall hkl_engine_list_free, Ptr <HklEngineList> -> IO ()
#ccall hkl_engine_list_get, Ptr <HklEngineList> -> IO ()
#ccall hkl_engine_list_init, Ptr <HklEngineList> -> Ptr <HklGeometry> -> Ptr <HklDetector> -> Ptr <HklSample> -> IO ()

-- HklFactory

#opaque_t HklFactory

#ccall hkl_factory_create_new_engine_list, Ptr <HklFactory> -> IO (Ptr <HklEngineList>)
#ccall hkl_factory_create_new_geometry, Ptr <HklFactory> -> IO (Ptr <HklGeometry>)
#ccall hkl_factory_get_by_name, CString -> Ptr () -> IO (Ptr <HklFactory>)

-- HklGeometry

#opaque_t HklGeometry

#globalvar hkl_geometry_operations_defaults, Ptr ()

#ccall hkl_geometry_add_holder, Ptr <HklGeometry> -> IO (Ptr <HklHolder>)
#ccall hkl_geometry_axis_values_get, Ptr <HklGeometry> -> Ptr CDouble -> CSize -> CInt -> IO ()
#ccall hkl_geometry_axis_values_set, Ptr <HklGeometry> -> Ptr CDouble -> CSize -> CInt -> Ptr () -> IO ()
#ccall hkl_geometry_axis_names_get, Ptr <HklGeometry> -> IO (Ptr <darray_string>)
#ccall hkl_geometry_axis_get, Ptr <HklGeometry> -> CString -> Ptr () -> IO (Ptr <HklParameter>)
#ccall hkl_geometry_detector_rotation_get_binding, Ptr <HklGeometry> -> Ptr <HklDetector> -> IO (Ptr <HklQuaternion>)
#ccall hkl_geometry_free, Ptr <HklGeometry> -> IO ()
#ccall hkl_geometry_name_get, Ptr <HklGeometry> -> IO CString
#ccall hkl_geometry_new, Ptr <HklFactory> -> Ptr (Ptr ()) -> IO (Ptr <HklGeometry>)
#ccall hkl_geometry_wavelength_get, Ptr <HklGeometry> -> CInt -> IO CDouble
#ccall hkl_geometry_wavelength_set, Ptr <HklGeometry> -> CDouble -> CInt -> Ptr () -> IO ()

-- HklGeometryList

#opaque_t HklGeometryList
#opaque_t HklGeometryListItem

#ccall hkl_geometry_list_free, Ptr <HklGeometryList> -> IO ()
#ccall hkl_geometry_list_items_first_get, Ptr <HklGeometryList> -> IO (Ptr <HklGeometryListItem>)
#ccall hkl_geometry_list_item_geometry_get, Ptr <HklGeometryListItem> -> IO (Ptr <HklGeometry>)
#ccall hkl_geometry_list_items_next_get, Ptr <HklGeometryList> -> Ptr <HklGeometryListItem> -> IO (Ptr <HklGeometryListItem>)

-- HklHolder

#opaque_t HklHolder
#ccall hkl_holder_add_rotation, Ptr <HklHolder> -> CString -> CDouble -> CDouble -> CDouble -> Ptr <HklUnit> -> IO ()
#ccall hkl_holder_add_translation, Ptr <HklHolder> -> CString -> CDouble -> CDouble -> CDouble -> Ptr <HklUnit> -> IO ()
-- HklLattice

#opaque_t HklLattice

#ccall hkl_lattice_free, Ptr <HklLattice> -> IO ()
#ccall hkl_lattice_new, CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> Ptr () -> IO (Ptr <HklLattice>)

-- HklMatrix

#opaque_t HklMatrix

#ccall hkl_matrix_free, Ptr <HklMatrix> -> IO ()
#ccall hkl_matrix_get, Ptr <HklMatrix> -> CInt -> CInt -> IO CDouble

-- HklParameter

#opaque_t HklParameter

#ccall hkl_parameter_free, Ptr <HklParameter> -> IO ()
#ccall hkl_parameter_min_max_get , Ptr <HklParameter> -> Ptr CDouble -> Ptr CDouble -> CInt -> IO ()
#ccall hkl_parameter_min_max_set , Ptr <HklParameter> -> CDouble -> CDouble -> CInt -> Ptr () -> IO CInt
#ccall hkl_parameter_name_get, Ptr <HklParameter> -> IO CString
#ccall hkl_parameter_new_copy, Ptr <HklParameter> -> IO (Ptr <HklParameter>)
#ccall hkl_parameter_value_get, Ptr <HklParameter> -> CInt -> IO Double
#ccall hkl_parameter_value_set, Ptr <HklParameter> -> CDouble -> CInt -> Ptr () -> IO (CInt)

-- HklQuaternion

#opaque_t HklQuaternion

#ccall hkl_quaternion_free, Ptr <HklQuaternion> -> IO ()
#ccall hkl_quaternion_to_matrix_binding, Ptr <HklQuaternion> -> IO (Ptr <HklMatrix>)


-- HklSample

#opaque_t HklSample

#ccall hkl_sample_new, CString -> IO (Ptr <HklSample>)
#ccall hkl_sample_lattice_set, Ptr <HklSample> -> Ptr <HklLattice> -> IO ()
#ccall hkl_sample_free, Ptr <HklSample> -> IO ()
#ccall hkl_sample_ux_get, Ptr <HklSample> -> IO (Ptr <HklParameter>)
#ccall hkl_sample_uy_get, Ptr <HklSample> -> IO (Ptr <HklParameter>)
#ccall hkl_sample_uz_get, Ptr <HklSample> -> IO (Ptr <HklParameter>)
#ccall hkl_sample_ux_set, Ptr <HklSample> -> Ptr <HklParameter> -> Ptr () -> IO CInt
#ccall hkl_sample_uy_set, Ptr <HklSample> -> Ptr <HklParameter> -> Ptr () -> IO CInt
#ccall hkl_sample_uz_set, Ptr <HklSample> -> Ptr <HklParameter> -> Ptr () -> IO CInt

-- HklUnit

#opaque_t HklUnit

#globalvar hkl_unit_angle_deg, <HklUnit>
#globalvar hkl_unit_angle_rad, <HklUnit>
#globalvar hkl_unit_length_nm, <HklUnit>
#globalvar hkl_unit_angle_mrad, <HklUnit>
#globalvar hkl_unit_length_mm, <HklUnit>
#globalvar hkl_unit_length_meter, <HklUnit>

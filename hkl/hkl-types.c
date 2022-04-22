/* This file is part of the hkl library.
 *
 * The hkl library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * The hkl library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with the hkl library.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) 2011-2019, 2022 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <glib-object.h>
#include "hkl-types.h"
#include "glib/gthread.h"               // for g_once_init_enter, etc
#include "glibconfig.h"                 // for gsize
#include "hkl-detector-private.h"       // for hkl_detector_new_copy
#include "hkl-geometry-private.h"       // for hkl_geometry_list_free, etc
#include "hkl-matrix-private.h"         // for hkl_matrix_dup
#include "hkl-pseudoaxis-private.h"     // for hkl_engine_list_new_copy
#include "hkl-sample-private.h"         // for hkl_sample_reflection_free, etc
#include "hkl-unit-private.h"           // for hkl_unit_dup, hkl_unit_free
#include "hkl-vector-private.h"         // for hkl_vector_dup, etc

static void *hkl_fake_ref(void *src) { return src; }
static void hkl_fake_unref(void *src) { return; }

G_DEFINE_BOXED_TYPE (HklDetector, hkl_detector, hkl_detector_new_copy, hkl_detector_free);
G_DEFINE_BOXED_TYPE (HklEngine, hkl_engine, hkl_fake_ref, hkl_fake_unref);
G_DEFINE_BOXED_TYPE (HklEngineList, hkl_engine_list, hkl_engine_list_new_copy, hkl_engine_list_free);
G_DEFINE_BOXED_TYPE (HklFactory, hkl_factory, hkl_fake_ref, hkl_fake_unref);
G_DEFINE_BOXED_TYPE (HklGeometry, hkl_geometry, hkl_geometry_new_copy, hkl_geometry_free);
G_DEFINE_BOXED_TYPE (HklGeometryList, hkl_geometry_list, hkl_geometry_list_new_copy, hkl_geometry_list_free);
G_DEFINE_BOXED_TYPE (HklGeometryListItem, hkl_geometry_list_item, hkl_geometry_list_item_new_copy, hkl_geometry_list_item_free);
G_DEFINE_BOXED_TYPE (HklLattice, hkl_lattice, hkl_lattice_new_copy, hkl_lattice_free);
G_DEFINE_BOXED_TYPE (HklMatrix, hkl_matrix, hkl_matrix_dup, hkl_matrix_free);
G_DEFINE_BOXED_TYPE (HklParameter, hkl_parameter, hkl_parameter_new_copy, hkl_parameter_free);
G_DEFINE_BOXED_TYPE (HklQuaternion, hkl_quaternion, hkl_quaternion_dup, hkl_quaternion_free);
G_DEFINE_BOXED_TYPE (HklSample, hkl_sample, hkl_sample_new_copy, hkl_sample_free);
G_DEFINE_BOXED_TYPE (HklSampleReflection, hkl_sample_reflection, hkl_fake_ref, hkl_fake_unref);
G_DEFINE_BOXED_TYPE (HklUnit, hkl_unit, hkl_unit_dup, hkl_unit_free);
G_DEFINE_BOXED_TYPE (HklVector, hkl_vector, hkl_vector_dup, hkl_vector_free);

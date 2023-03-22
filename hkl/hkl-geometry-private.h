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
 * Copyright (C) 2003-2020, 2023 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef __HKL_GEOMETRY_PRIVATE_H__
#define __HKL_GEOMETRY_PRIVATE_H__

#include <stddef.h>                     // for size_t
#include <stdio.h>                      // for FILE
#include "hkl-parameter-private.h"      // for darray_parameter
#include "hkl-quaternion-private.h"     // for _HklQuaternion
#include "hkl-source-private.h"         // for HklSource
#include "hkl-vector-private.h"         // for HklQuaternion
#include "hkl.h"                        // for HklGeometry, etc
#include "hkl/ccan/darray/darray.h"     // for darray
#include "hkl/ccan/list/list.h"

G_BEGIN_DECLS

#define HKL_HOLDER_SAMPLE_IDX 0
#define HKL_HOLDER_DETECTOR_IDX 1

typedef struct _HklHolder HklHolder;

typedef void (* HklGeometryListMultiplyFunction) (HklGeometryList *self,
						  HklGeometryListItem *item);

typedef darray(HklHolder *) darray_holder;

struct HklHolderConfig {
	int gc;
	size_t *idx;
	size_t len;
};

struct _HklHolder {
	struct HklHolderConfig *config;
	HklGeometry *geometry;
	HklQuaternion q;
};

typedef struct _HklGeometryOperations HklGeometryOperations;

struct _HklGeometryOperations
{
	HklHolder* (*sample_holder_get) (const HklGeometry *self, const HklSample *sample);

	HklHolder* (*detector_holder_get) (const HklGeometry *self, const HklDetector *detector);

	HklVector (*ki_get) (const HklGeometry *geometry);

	HklVector (*kf_get) (const HklGeometry *self, const HklDetector *detector);
};

struct _HklGeometry
{
	const HklFactory *factory;
	HklSource source;
	darray_parameter axes;
	darray_holder holders;
	const HklGeometryOperations *ops;
};

static inline HklHolder *hkl_geometry_sample_holder_get_real(const HklGeometry *self,
							     UNUSED const HklSample *sample)
{
	return darray_item(self->holders, HKL_HOLDER_SAMPLE_IDX);
}

static inline HklHolder *hkl_geometry_detector_holder_get_real(const HklGeometry *self,
							       UNUSED const HklDetector *detector)
{
	return darray_item(self->holders, HKL_HOLDER_DETECTOR_IDX);
}

static inline HklVector hkl_geometry_ki_get_real(const HklGeometry *self)
{
	HklVector ki;

	hkl_source_compute_ki(&self->source, &ki);

	return ki;
}

static inline HklVector hkl_geometry_kf_get_real(const HklGeometry *self,
                                                 UNUSED const HklDetector *detector)
{
	HklVector kf = {{HKL_TAU / self->source.wave_length, 0, 0}};
	HklHolder *detector_holder = darray_item(self->holders, HKL_HOLDER_DETECTOR_IDX);

	hkl_vector_rotated_quaternion(&kf, &detector_holder->q);

	return kf;
}

#define HKL_GEOMETRY_OPERATIONS_DEFAULTS				\
	.sample_holder_get = hkl_geometry_sample_holder_get_real,	\
		.detector_holder_get = hkl_geometry_detector_holder_get_real, \
		.ki_get = hkl_geometry_ki_get_real,			\
		.kf_get = hkl_geometry_kf_get_real

extern const HklGeometryOperations hkl_geometry_operations_defaults;

#define HKL_GEOMETRY_ERROR hkl_geometry_error_quark ()

static inline GQuark hkl_geometry_error_quark (void)
{
	return g_quark_from_static_string ("hkl-geometry-error-quark");
}

typedef enum {
	HKL_GEOMETRY_ERROR_AXIS_GET, /* can not get the axis */
	HKL_GEOMETRY_ERROR_AXIS_SET, /* can not set the axis */
} HklGeometryError;

struct _HklGeometryList
{
	HklGeometryListMultiplyFunction multiply;
	struct list_head items;
	size_t n_items;
};

struct _HklGeometryListItem
{
	struct list_node list;
	HklGeometry *geometry;
};

/*************/
/* HklHolder */
/*************/

extern HklParameter *hkl_holder_add_parameter(HklHolder *self,
                                              char const *name,
                                              const HklUnit *punit);

extern HklParameter *hkl_holder_add_rotation(HklHolder *self,
					     char const *name,
					     double x, double y, double z,
					     const HklUnit *punit);

extern HklParameter *hkl_holder_add_rotation_with_origin(HklHolder *self,
							 const char *name,
							 double x, double y, double z,
							 double ox, double oy, double oz,
							 const HklUnit *punit);

extern HklParameter *hkl_holder_add_translation(HklHolder *self,
						char const *name,
						double x, double y, double z,
						const HklUnit *punit);

extern HklVector hkl_holder_transformation_apply(const HklHolder *self,
						 const HklVector *v);

/***************/
/* HklGeometry */
/***************/

extern HklGeometry *hkl_geometry_new(const HklFactory *factory,
				     const HklGeometryOperations *ops);

extern int hkl_geometry_init_geometry(HklGeometry *self,
				      const HklGeometry *src);

extern HklHolder *hkl_geometry_add_holder(HklGeometry *self);

extern void hkl_geometry_update(HklGeometry *self);

extern int hkl_geometry_get_axis_idx_by_name(const HklGeometry *self,
					     const char *name);

/* internally require do not use the hkl_geometry_axis_get */
extern HklParameter *hkl_geometry_get_axis_by_name(HklGeometry *self,
						   const char *name);

extern double hkl_geometry_distance(const HklGeometry *self,
				    const HklGeometry *ref);

extern double hkl_geometry_distance_orthodromic(const HklGeometry *self,
						const HklGeometry *ref);

extern int hkl_geometry_closest_from_geometry_with_range(HklGeometry *self,
							 const HklGeometry *ref);

extern int hkl_geometry_is_valid(const HklGeometry *self);

extern int hkl_geometry_is_valid_range(const HklGeometry *self);

extern HklHolder *hkl_geometry_sample_holder_get(const HklGeometry *self, const HklSample *sample);

extern HklHolder *hkl_geometry_detector_holder_get(const HklGeometry *self, const HklDetector *detector);

/*******************/
/* HklGeometryList */
/*******************/

extern HklGeometryList *hkl_geometry_list_new(void);

extern HklGeometryList *hkl_geometry_list_new_copy(const HklGeometryList *self);

extern void hkl_geometry_list_add(HklGeometryList *self, const HklGeometry *geometry);

extern void hkl_geometry_list_reset(HklGeometryList *self);

extern void hkl_geometry_list_sort(HklGeometryList *self, HklGeometry *ref);

extern void hkl_geometry_list_fprintf(FILE *f, const HklGeometryList *self);

extern void hkl_geometry_list_multiply(HklGeometryList *self);

extern void hkl_geometry_list_multiply_from_range(HklGeometryList *self);

extern void hkl_geometry_list_remove_invalid(HklGeometryList *self);

/***********************/
/* HklGeometryListItem */
/***********************/

extern HklGeometryListItem *hkl_geometry_list_item_new(const HklGeometry *geometry);

extern HklGeometryListItem *hkl_geometry_list_item_new_copy(const HklGeometryListItem *self);

extern void hkl_geometry_list_item_free(HklGeometryListItem *self);

G_END_DECLS

#endif /* __HKL_GEOMETRY_PRIVATE_H__ */

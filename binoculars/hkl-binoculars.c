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
 * Copyright (C) 2003-2023 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */

/* #define DEBUG */

#ifdef DEBUG
# define CGLM_DEFINE_PRINTS
# define CGLM_PRINT_PRECISION 7
#endif

#include <stdio.h>
#include <time.h>

#include "ccan/array_size/array_size.h"
#include "hkl-binoculars-private.h"
#include "hkl-matrix-private.h"
#include "hkl-quaternion-private.h"
#include "hkl-sample-private.h"
#include "hkl-vector-private.h"

/* mark the masked pixels with this value */
#define MASKED PTRDIFF_MAX
#define REMOVED 0

/* Math */

static inline ptrdiff_t min(ptrdiff_t x, ptrdiff_t y)
{
	return y ^ ((x ^ y) & -(x < y));
}

static inline ptrdiff_t max(ptrdiff_t x, ptrdiff_t y)
{
	return x ^ ((x ^ y) & -(x < y));
}

/* Axis Limits */

datatype(
        AxisLimitType,
        (NoLimit),
        (Limit, ptrdiff_t)
        );

struct _HklBinocularsAxisLimits
{
        AxisLimitType imin;
        AxisLimitType imax;
};

void hkl_binoculars_axis_limits_free(HklBinocularsAxisLimits *self)
{
	free(self);
}

HklBinocularsAxisLimits *hkl_binoculars_axis_limits_new(const ptrdiff_t *imin,
                                                        const ptrdiff_t *imax)
{
	HklBinocularsAxisLimits *self = g_new(HklBinocularsAxisLimits, 1);

        if (NULL == imin){
                self->imin = NoLimit();
        } else {
                self->imin = Limit(*imin);
        }

        if (NULL == imax){
                self->imax = NoLimit();
        } else {
                self->imax = Limit(*imax);
        }

        return self;
}

/* Axis */

static inline double axis_min(const HklBinocularsAxis *self)
{
	return self->imin * self->resolution;
}

static inline double axis_max(const HklBinocularsAxis *self)
{
	return self->imax * self->resolution;
}

static inline void hkl_binoculars_axis_init(HklBinocularsAxis *self,
                                            const char *name,
                                            size_t index,
                                            ptrdiff_t imin,
                                            ptrdiff_t imax,
                                            double resolution)
{
	self->name = name;
	self->index = index;
	self->resolution = resolution;
	self->imin = imin;
	self->imax = imax;
}

double *hkl_binoculars_axis_array(const HklBinocularsAxis *self)
{
	double *arr = g_new0(double, 6);

        arr[0] = self->index;
        arr[1] = axis_min(self);
        arr[2] = axis_max(self);
        arr[3] = self->resolution;
        arr[4] = self->imin;
        arr[5] = self->imax;

	return arr;
}

/* check if *self contains *other */
static inline int hkl_binoculars_axis_contains_axis(const HklBinocularsAxis *self,
                                                    const HklBinocularsAxis *other)
{
        return self->imin <= other->imin && self->imax >= other->imax;
}

static inline void hkl_binoculars_axis_merge(HklBinocularsAxis *self, const HklBinocularsAxis *other)
{
	self->imin = min(self->imin, other->imin);
	self->imax = max(self->imax, other->imax);
}

void hkl_binoculars_axis_fprintf(FILE *f, const HklBinocularsAxis *self)
{
	fprintf(f, "%s : %ld min: %f max: %f res: %f size: %ld",
		self->name, self->index,
		axis_min(self), axis_max(self),
		self->resolution, axis_size(self));
}


/* darray_axis */

static inline void merge_axes(darray_axis *axes,
                              const darray_axis *others)
{
        size_t i;

        for(i=0; i<darray_size(*axes); ++i)
                hkl_binoculars_axis_merge(&darray_item(*axes, i),
                                          &darray_item(*others, i));
}

static inline int does_not_include(const darray_axis *axes,
                                   const darray_axis *others)
{
        size_t i;
        int res = 0;

        if (darray_size(*axes) == darray_size(*others)){
                for(i=0; i<darray_size(*axes); ++i){
                        if (0 == hkl_binoculars_axis_contains_axis(&darray_item(*axes, i),
                                                                   &darray_item(*others, i))){
                                res = 1;
                                break;
                        }
                }
        } else {
                res = 1;
        }

        return res;
}


/* Space */

static inline void hkl_binoculars_space_item_fprintf(FILE *f, const HklBinocularsSpaceItem *self)
{
        fprintf(f, "item->indexes(%p) v: %ld %ld %ld, intensity: %d", &self->indexes_0[0],
                self->indexes_0[0], self->indexes_0[1], self->indexes_0[2], self->intensity);
}

static inline int space_is_empty(const HklBinocularsSpace *space)
{
        return 0 == darray_size(space->items);
}

static inline void space_update_axes(HklBinocularsSpace *space,
                                     const char *names[],
                                     size_t n_pixels,
                                     const double resolutions[])
{
        size_t i;
        HklBinocularsSpaceItem *item;
        HklBinocularsSpaceItem minimum;
        HklBinocularsSpaceItem maximum;

        if (space_is_empty(space))
                return;

        minimum = maximum = darray_item(space->items, 0);

        for(i=0; i<darray_size(space->axes); ++i){
                darray_foreach(item, space->items){
                        minimum.indexes_0[i] = min(minimum.indexes_0[i], item->indexes_0[i]);
                        maximum.indexes_0[i] = max(maximum.indexes_0[i], item->indexes_0[i]);
                }
        }

        for(i=0; i<darray_size(space->axes); ++i){
                HklBinocularsAxis *axis = &darray_item(space->axes, i);
                hkl_binoculars_axis_init(axis, names[i], i,
                                         minimum.indexes_0[i], maximum.indexes_0[i],
                                         resolutions[i]);
        }
}

static inline int item_in_the_limits(const HklBinocularsSpaceItem *item,
                                     const HklBinocularsAxisLimits **limits,
                                     size_t n_limits)
{
        int res = TRUE;

        if (NULL != limits){
                for(size_t i=0; i<n_limits; ++i){
                        ptrdiff_t v = item->indexes_0[i];

                        match(limits[i]->imin){
                                of(NoLimit){
                                }
                                of(Limit, imin){
                                        if (v < *imin) res = FALSE;
                                        break;
                                }
                        }

                        match(limits[i]->imax){
                                of(NoLimit){
                                }
                                of(Limit, imax){
                                        if(v > *imax) res = FALSE;
                                        break;
                                }
                        }
                }
        }

        return res;
}

HklBinocularsSpace *hkl_binoculars_space_new(size_t max_items, size_t n_axes)
{
	HklBinocularsSpace *self = g_new(HklBinocularsSpace, 1);

        self->max_items = max_items;
        darray_init(self->items);
        darray_resize(self->items, max_items);
        darray_init(self->axes);
        darray_resize(self->axes, n_axes);

	return self;
}

void hkl_binoculars_space_free(HklBinocularsSpace *self)
{
	darray_free(self->axes);
        darray_free(self->items);
	free(self);
}

void hkl_binoculars_space_fprintf(FILE *f, const HklBinocularsSpace *self)
{
        size_t masked;
        HklBinocularsAxis *axis;

	fprintf(f, "\nHklBinocularsSpace: %p", self);
	fprintf(f, "\nn_indexes_0: %ld", darray_size(self->items));
        if(space_is_empty(self)){
                fprintf(f, "\nempty");
        } else {
                fprintf(f, "\nn_axes: %ld", darray_size(self->axes));
                darray_foreach(axis, self->axes){
                        fprintf(f, "\n");
                        hkl_binoculars_axis_fprintf(f, axis);
                }

                masked = self->max_items - darray_size(self->items);
                fprintf(f, "\nmasked pixels: %ld (%f%%)", masked, (double)masked / self->max_items * 100);
        }
}

/* angles */

#define HKL_BINOCULARS_SPACE_ANGLES_IMPL(image_t)                       \
        HKL_BINOCULARS_SPACE_ANGLES_DECL(image_t)                       \
        {                                                               \
                size_t i, j;                                            \
                const char * names[] = {"delta_lab", "gamma_lab", "tth"}; \
                double delta0, gamma0, tth;                             \
                                                                        \
                assert(ARRAY_SIZE(names) == darray_size(space->axes));  \
                assert(ARRAY_SIZE(names) == n_resolutions);             \
                assert(n_pixels == space->max_items);                   \
                                                                        \
                darray_size(space->items) = 0;                          \
                                                                        \
                const double *p_x = &pixels_coordinates[0 * n_pixels];  \
                const double *p_y = &pixels_coordinates[1 * n_pixels];  \
                const double *p_z = &pixels_coordinates[2 * n_pixels];  \
                                                                        \
                HklDetector *detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D); \
                const HklQuaternion q = hkl_geometry_detector_rotation_get(geometry, detector); \
                                                                        \
                for(i=0;i<n_pixels;++i){                                \
                        if(NULL == masked || 0 == masked[i]){           \
                                HklBinocularsSpaceItem item;            \
                                HklVector v = {{p_x[i], p_y[i], p_z[i]}}; \
                                                                        \
                                hkl_vector_rotated_quaternion(&v, &q);  \
                                delta0 = atan2(v.data[2], v.data[0]);   \
                                gamma0 = M_PI_2 - atan2(sqrt(v.data[2] * v.data[2] + v.data[0] * v.data[0]), v.data[1]); \
                                tth = acos(v.data[0]);                  \
                                                                        \
                                v.data[0] = delta0 / M_PI * 180.0;      \
                                v.data[1] = gamma0 / M_PI * 180.0;      \
                                v.data[2] = tth / M_PI * 180.0;         \
                                                                        \
                                for(j=0; j<ARRAY_SIZE(names); ++j){     \
                                        item.indexes_0[j] = rint(v.data[j] / resolutions[j]); \
                                }                                       \
                                item.intensity = rint((double)image[i] * weight); \
                                                                        \
                                                                        \
                                if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                        darray_append(space->items, item); \
                        }                                               \
                }                                                       \
                                                                        \
                space_update_axes(space, names, n_pixels, resolutions); \
                                                                        \
                hkl_detector_free(detector);                            \
        }

HKL_BINOCULARS_SPACE_ANGLES_IMPL(int32_t);
HKL_BINOCULARS_SPACE_ANGLES_IMPL(uint16_t);
HKL_BINOCULARS_SPACE_ANGLES_IMPL(uint32_t);

/* qcustom */

static const char **axis_name_from_subprojection(HklBinocularsQCustomSubProjectionEnum subprojection,
                                                 HklBinocularsSpace *space,
                                                 int n_resolutions)
{
        const char **names;

        switch(subprojection){
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QX_QY_QZ:
        case HKL_BINOCULARS_QCUSTOM_NUM_SUBPROJECTIONS:
        {
                static const char *names_qx_qy_qz[] = {"qx", "qy", "qz"};
                assert(ARRAY_SIZE(names_qx_qy_qz) == darray_size(space->axes));
                assert(ARRAY_SIZE(names_qx_qy_qz) == n_resolutions);
                names = names_qx_qy_qz;
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_TTH_TIMESTAMP:
        {
                static const char *names_q_tth_timestamp[] = {"q", "tth", "timestamp"};
                assert(ARRAY_SIZE(names_q_tth_timestamp) == darray_size(space->axes));
                assert(ARRAY_SIZE(names_q_tth_timestamp) == n_resolutions);
                names = names_q_tth_timestamp;
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_INDEX:
        {
                static const char *names_q_index[] = {"q", "index"};
                assert(ARRAY_SIZE(names_q_index) == darray_size(space->axes));
                assert(ARRAY_SIZE(names_q_index) == n_resolutions);
                names = names_q_index;
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPAR_QPER_TIMESTAMP:
        {
                static const char *names_qpar_qper_timestamp[] = {"qpar", "qper", "timestamp"};
                assert(ARRAY_SIZE(names_qpar_qper_timestamp) == darray_size(space->axes));
                assert(ARRAY_SIZE(names_qpar_qper_timestamp) == n_resolutions);
                names = names_qpar_qper_timestamp;
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPAR_QPER:
        {
                static const char *names_qpar_qper[] = {"qpar", "qper"};
                assert(ARRAY_SIZE(names_qpar_qper) == darray_size(space->axes));
                assert(ARRAY_SIZE(names_qpar_qper) == n_resolutions);
                names = names_qpar_qper;
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHI_QX:
        {
                static const char *names_q_phi_qx[] = {"q", "phi", "qx"};
                assert(ARRAY_SIZE(names_q_phi_qx) == darray_size(space->axes));
                assert(ARRAY_SIZE(names_q_phi_qx) == n_resolutions);
                names = names_q_phi_qx;
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHI_QY:
        {
                static const char *names_q_phi_qy[] = {"q", "phi", "qy"};
                assert(ARRAY_SIZE(names_q_phi_qy) == darray_size(space->axes));
                assert(ARRAY_SIZE(names_q_phi_qy) == n_resolutions);
                names = names_q_phi_qy;
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHI_QZ:
        {
                static const char *names_q_phi_qz[] = {"q", "phi", "qz"};
                assert(ARRAY_SIZE(names_q_phi_qz) == darray_size(space->axes));
                assert(ARRAY_SIZE(names_q_phi_qz) == n_resolutions);
                names = names_q_phi_qz;
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_STEREO:
        {
                static const char *names_q_stereo[] = {"q", "xp", "yp"};
                assert(ARRAY_SIZE(names_q_stereo) == darray_size(space->axes));
                assert(ARRAY_SIZE(names_q_stereo) == n_resolutions);
                names = names_q_stereo;
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_ANGLES_ZAXIS_OMEGA:
        {
                static const char *names_angles_zaxis_omega[] = {"delta", "gamma", "omega"};
                assert(ARRAY_SIZE(names_angles_zaxis_omega) == darray_size(space->axes));
                assert(ARRAY_SIZE(names_angles_zaxis_omega) == n_resolutions);
                names = names_angles_zaxis_omega;
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_ANGLES_ZAXIS_MU:
        {
                static const char *names_angles_zaxis_mu[] = {"delta", "gamma", "mu"};
                assert(ARRAY_SIZE(names_angles_zaxis_mu) == darray_size(space->axes));
                assert(ARRAY_SIZE(names_angles_zaxis_mu) == n_resolutions);
                names = names_angles_zaxis_mu;
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Y_Z_TIMESTAMP:
        {
                static const char *names_y_z_timestamp[] = {"y", "z", "timestamp"};
                assert(ARRAY_SIZE(names_y_z_timestamp) == darray_size(space->axes));
                assert(ARRAY_SIZE(names_y_z_timestamp) == n_resolutions);
                names = names_y_z_timestamp;
                break;
        }
        case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Y_Z:
        {
                static const char *names_y_z[] = {"y", "z"};
                assert(ARRAY_SIZE(names_y_z) == darray_size(space->axes));
                assert(ARRAY_SIZE(names_y_z) == n_resolutions);
                names = names_y_z;
                break;
        }
        default:
        {
                static const char *names_qx_qy_qz[] = {"qx", "qy", "qz"};
                assert(ARRAY_SIZE(names_qx_qy_qz) == darray_size(space->axes));
                assert(ARRAY_SIZE(names_qx_qy_qz) == n_resolutions);
                names = names_qx_qy_qz;
                break;
        }
        }
        return names;
}

static inline int not_masked(const uint8_t *masked, size_t idx)
{
        return NULL == masked || 0 == masked[idx];
}

#define HKL_BINOCULARS_SPACE_QCUSTOM_IMPL(image_t)			\
        HKL_BINOCULARS_SPACE_QCUSTOM_DECL(image_t)			\
        {                                                               \
		size_t i;						\
                HklBinocularsSpaceItem item;                            \
		const char **names = axis_name_from_subprojection(subprojection, space, n_resolutions); \
                assert(n_pixels == space->max_items);                   \
									\
		const double *q_x = &pixels_coordinates[0 * n_pixels];	\
		const double *q_y = &pixels_coordinates[1 * n_pixels];	\
		const double *q_z = &pixels_coordinates[2 * n_pixels];	\
                                                                        \
		HklSample *sample = hkl_sample_new("test");		\
		HklDetector *detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D); \
		const HklQuaternion q = hkl_geometry_detector_rotation_get(geometry, detector); \
		const HklVector ki = hkl_geometry_ki_get(geometry);	\
		double k = hkl_vector_norm2(&ki);			\
		HklQuaternion qs_1 = hkl_geometry_sample_rotation_get(geometry, sample); \
		switch(surf){						\
		case HKL_BINOCULARS_SURFACE_ORIENTATION_VERTICAL:	\
		{							\
			HklQuaternion q_ub;				\
			HklVector v_s = {{1, 0, 0}};			\
			hkl_quaternion_init_from_angle_and_axe(&q_ub, -M_PI_2, &v_s); \
			hkl_quaternion_times_quaternion(&qs_1, &q_ub);	\
			break;						\
		}							\
		case HKL_BINOCULARS_SURFACE_ORIENTATION_HORIZONTAL:	\
		case HKL_BINOCULARS_SURFACE_ORIENTATION_NUM_ORIENTATION: \
			break;						\
		}							\
		hkl_quaternion_conjugate(&qs_1);			\
                                                                        \
		darray_size(space->items) = 0;				\
                                                                        \
                switch(subprojection){                                  \
                case HKL_BINOCULARS_QCUSTOM_NUM_SUBPROJECTIONS:         \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QX_QY_QZ:    \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i)){              \
                                        HklVector v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        hkl_vector_rotated_quaternion(&v, &q); \
                                        hkl_vector_times_double(&v, k); \
                                        hkl_vector_minus_vector(&v, &ki); \
                                        hkl_vector_rotated_quaternion(&v, &qs_1); \
                                                                        \
					item.indexes_0[0] = rint(v.data[0] / resolutions[0]); \
					item.indexes_0[1] = rint(v.data[1] / resolutions[1]); \
					item.indexes_0[2] = rint(v.data[2] / resolutions[2]); \
                                        item.intensity = rint((double)image[i] * weight); \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_TTH_TIMESTAMP: \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i)){              \
                                        HklVector v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        hkl_vector_rotated_quaternion(&v, &q); \
                                        hkl_vector_times_double(&v, k); \
                                        hkl_vector_minus_vector(&v, &ki); \
                                        hkl_vector_rotated_quaternion(&v, &qs_1); \
                                                                        \
					item.indexes_0[0] = rint(hkl_vector_norm2(&v) / resolutions[0]); \
					item.indexes_0[1] = rint(asin(hkl_vector_norm2(&v) / 2 / k) * 2 / M_PI * 180 / resolutions[1]); \
					item.indexes_0[2] = rint(timestamp / resolutions[2]); \
                                        item.intensity = rint((double)image[i] * weight); \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_INDEX:     \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i)){              \
                                        HklVector v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        hkl_vector_rotated_quaternion(&v, &q); \
                                        hkl_vector_times_double(&v, k); \
                                        hkl_vector_minus_vector(&v, &ki); \
                                        hkl_vector_rotated_quaternion(&v, &qs_1); \
                                                                        \
					item.indexes_0[0] = rint(hkl_vector_norm2(&v) / resolutions[0]); \
					item.indexes_0[1] = rint(timestamp / resolutions[1]); \
					item.indexes_0[2] = REMOVED;	\
                                        item.intensity = rint((double)image[i] * weight); \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPAR_QPER_TIMESTAMP: \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i)){              \
                                        HklVector v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        hkl_vector_rotated_quaternion(&v, &q); \
                                        hkl_vector_times_double(&v, k); \
                                        hkl_vector_minus_vector(&v, &ki); \
                                        hkl_vector_rotated_quaternion(&v, &qs_1); \
                                                                        \
					item.indexes_0[0] = sqrt(v.data[0] * v.data[0] + v.data[1] * v.data[1]) / resolutions[0]; \
					item.indexes_0[1] = v.data[2] / resolutions[1]; \
					item.indexes_0[2] = rint(timestamp / resolutions[2]); \
                                        item.intensity = rint((double)image[i] * weight); \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPAR_QPER:   \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i)){              \
                                        HklVector v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        hkl_vector_rotated_quaternion(&v, &q); \
                                        hkl_vector_times_double(&v, k); \
                                        hkl_vector_minus_vector(&v, &ki); \
                                        hkl_vector_rotated_quaternion(&v, &qs_1); \
                                                                        \
					item.indexes_0[0] = sqrt(v.data[0] * v.data[0] + v.data[1] * v.data[1]) / resolutions[0]; \
					item.indexes_0[1] = v.data[2] / resolutions[1]; \
					item.indexes_0[2] = REMOVED;	\
                                        item.intensity = rint((double)image[i] * weight); \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHI_QX:    \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i)){              \
                                        HklVector v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        hkl_vector_rotated_quaternion(&v, &q); \
                                        hkl_vector_times_double(&v, k); \
                                        hkl_vector_minus_vector(&v, &ki); \
                                        hkl_vector_rotated_quaternion(&v, &qs_1); \
                                                                        \
					item.indexes_0[0] = rint(hkl_vector_norm2(&v) / resolutions[0]); \
					item.indexes_0[1] = rint((atan2(v.data[2], -v.data[1])) / M_PI * 180 / resolutions[1]); \
					item.indexes_0[2] = rint(v.data[0] / resolutions[2]); \
                                        item.intensity = rint((double)image[i] * weight); \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHI_QY:    \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i)){              \
                                        HklVector v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        hkl_vector_rotated_quaternion(&v, &q); \
                                        hkl_vector_times_double(&v, k); \
                                        hkl_vector_minus_vector(&v, &ki); \
                                        hkl_vector_rotated_quaternion(&v, &qs_1); \
                                                                        \
					item.indexes_0[0] = rint(hkl_vector_norm2(&v) / resolutions[0]); \
					item.indexes_0[1] = rint((atan2(v.data[2], v.data[0])) / M_PI * 180 / resolutions[1]); \
					item.indexes_0[2] = rint(v.data[1] / resolutions[2]); \
                                        item.intensity = rint((double)image[i] * weight); \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHI_QZ:    \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i)){              \
                                        HklVector v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        hkl_vector_rotated_quaternion(&v, &q); \
                                        hkl_vector_times_double(&v, k); \
                                        hkl_vector_minus_vector(&v, &ki); \
                                        hkl_vector_rotated_quaternion(&v, &qs_1); \
                                                                        \
					item.indexes_0[0] = rint(hkl_vector_norm2(&v) / resolutions[0]); \
					item.indexes_0[1] = rint((atan2(v.data[0], v.data[1])) / M_PI * 180 / resolutions[1]); \
					item.indexes_0[2] = rint(v.data[2] / resolutions[2]); \
                                        item.intensity = rint((double)image[i] * weight); \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_STEREO:    \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i)){              \
                                        HklVector v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        hkl_vector_rotated_quaternion(&v, &q); \
                                        hkl_vector_times_double(&v, k); \
                                        hkl_vector_minus_vector(&v, &ki); \
                                        hkl_vector_rotated_quaternion(&v, &qs_1); \
                                                                        \
					item.indexes_0[0] = rint(hkl_vector_norm2(&v) / resolutions[0]); \
					double ratio = v.data[2] + item.indexes_0[0]; \
					item.indexes_0[1] = rint(v.data[0] / ratio / resolutions[1]); \
					item.indexes_0[2] = rint(v.data[1] / ratio / resolutions[2]); \
                                        item.intensity = rint((double)image[i] * weight); \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_ANGLES_ZAXIS_OMEGA: \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i)){              \
                                        HklVector v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        hkl_vector_rotated_quaternion(&v, &q); \
                                        hkl_vector_times_double(&v, k); \
                                        hkl_vector_minus_vector(&v, &ki); \
                                        hkl_vector_rotated_quaternion(&v, &qs_1); \
                                                                        \
					item.indexes_0[0] = rint(atan2(v.data[1], v.data[0]) / M_PI * 180 / resolutions[0]); \
					item.indexes_0[1] = rint(atan2(sqrt(v.data[0] * v.data[0] + v.data[1] * v.data[1]), v.data[2]) / M_PI * 180 / resolutions[1]); \
					item.indexes_0[2] = rint(hkl_vector_norm2(&v) / resolutions[2]); \
                                        item.intensity = rint((double)image[i] * weight); \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_ANGLES_ZAXIS_MU: \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i)){              \
                                        HklVector v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        hkl_vector_rotated_quaternion(&v, &q); \
                                        hkl_vector_times_double(&v, k); \
                                        hkl_vector_minus_vector(&v, &ki); \
                                        hkl_vector_rotated_quaternion(&v, &qs_1); \
                                                                        \
					item.indexes_0[0] = rint(atan2(v.data[1], v.data[0]) / M_PI * 180 / resolutions[0]); \
					item.indexes_0[1] = rint(atan2(sqrt(v.data[0] * v.data[0] + v.data[1] * v.data[1]), v.data[2]) / M_PI * 180 / resolutions[1]); \
					item.indexes_0[2] = REMOVED;	\
                                        item.intensity = rint((double)image[i] * weight); \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                        }                               \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Y_Z_TIMESTAMP: \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i)){              \
                                        HklVector v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        hkl_vector_rotated_quaternion(&v, &q); \
                                                                        \
					item.indexes_0[0] = rint(v.data[1] / resolutions[0]); \
					item.indexes_0[1] = rint(v.data[2] / resolutions[1]); \
					item.indexes_0[2] = rint(timestamp / resolutions[2]); \
                                        item.intensity = rint((double)image[i] * weight); \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Y_Z:         \
                {                                                       \
                        for(i=0;i<n_pixels;++i){                        \
                                if(not_masked(masked, i)){              \
                                        HklVector v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                        hkl_vector_rotated_quaternion(&v, &q); \
                                                                        \
					item.indexes_0[0] = rint(v.data[1] / resolutions[0]); \
					item.indexes_0[1] = rint(v.data[2] / resolutions[1]); \
					item.indexes_0[2] = REMOVED;    \
                                        item.intensity = rint((double)image[i] * weight); \
                                                                        \
                                        if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                                darray_append(space->items, item); \
                                }                                       \
                        }                                               \
                        break;                                          \
                }                                                       \
                }                                                       \
		space_update_axes(space, names, n_pixels, resolutions);	\
		hkl_detector_free(detector);				\
		hkl_sample_free(sample);				\
                }

HKL_BINOCULARS_SPACE_QCUSTOM_IMPL(int32_t);
HKL_BINOCULARS_SPACE_QCUSTOM_IMPL(uint16_t);
HKL_BINOCULARS_SPACE_QCUSTOM_IMPL(uint32_t);

/* qcustom2 */

#define HKL_BINOCULARS_SPACE_QCUSTOM2_IMPL(image_t)			\
        HKL_BINOCULARS_SPACE_QCUSTOM2_DECL(image_t)			\
        {                                                               \
		size_t i;						\
		const char **names = axis_name_from_subprojection(subprojection, space, n_resolutions); \
		assert(n_pixels == space->max_items);			\
                                                                        \
		const double *q_x = &pixels_coordinates[0 * n_pixels];	\
		const double *q_y = &pixels_coordinates[1 * n_pixels];	\
		const double *q_z = &pixels_coordinates[2 * n_pixels];	\
                                                                        \
		HklSample *sample = hkl_sample_new("test");		\
		HklDetector *detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D); \
		HklHolder *holder_d = hkl_geometry_detector_holder_get(geometry, detector); \
                CGLM_ALIGN_MAT mat4s m_holder_d = hkl_binoculars_holder_transformation_get(holder_d); \
		const HklVector ki_v = hkl_geometry_ki_get(geometry);	\
                CGLM_ALIGN_MAT vec3s ki = {{ki_v.data[0], ki_v.data[1], ki_v.data[2]}}; \
	        float k = glms_vec3_norm(ki);                          \
		HklHolder *holder_s = hkl_geometry_sample_holder_get(geometry,sample); \
                CGLM_ALIGN_MAT mat4s m_holder_s = hkl_binoculars_holder_transformation_get(holder_s); \
                                                                        \
		switch(surf){						\
		case HKL_BINOCULARS_SURFACE_ORIENTATION_VERTICAL:	\
		{							\
                        CGLM_ALIGN_MAT vec3s axis = GLMS_XUP;           \
			CGLM_ALIGN_MAT mat4s m_q_ub = glms_rotate_make(-M_PI_2, axis); \
                                                                        \
                        m_holder_s = glms_mat4_mul(m_holder_s, m_q_ub);              \
			break;						\
		}							\
		case HKL_BINOCULARS_SURFACE_ORIENTATION_HORIZONTAL:	\
		case HKL_BINOCULARS_SURFACE_ORIENTATION_NUM_ORIENTATION: \
			break;						\
		}							\
		m_holder_s = glms_mat4_inv(m_holder_s);                 \
                                                                        \
		darray_size(space->items) = 0;				\
                                                                        \
		for(i=0;i<n_pixels;++i){				\
			if(NULL == masked || 0 == masked[i]){		\
				HklBinocularsSpaceItem item;		\
				CGLM_ALIGN_MAT vec3s v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
				v = glms_mat4_mulv3(m_holder_d, v, 0);  \
                                v = glms_vec3_scale_as(v, k);           \
                                v = glms_vec3_sub(v , ki);              \
                                v = glms_mat4_mulv3(m_holder_s, v, 0);  \
                                                                        \
				switch(subprojection){			\
				case HKL_BINOCULARS_QCUSTOM_NUM_SUBPROJECTIONS: \
				case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QX_QY_QZ: \
				{					\
					item.indexes_0[0] = rint(v.raw[0] / resolutions[0]); \
					item.indexes_0[1] = rint(v.raw[1] / resolutions[1]); \
					item.indexes_0[2] = rint(v.raw[2] / resolutions[2]); \
					break;				\
				}					\
				case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_TTH_TIMESTAMP: \
				{					\
					item.indexes_0[0] = rint(glms_vec3_norm(v) / resolutions[0]); \
					item.indexes_0[1] = rint(asin(glms_vec3_norm(v) / 2 / k) * 2 / M_PI * 180 / resolutions[1]); \
					item.indexes_0[2] = rint(timestamp / resolutions[2]); \
					break;				\
				}					\
				case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_INDEX: \
				{					\
					item.indexes_0[0] = rint(glms_vec3_norm(v) / resolutions[0]); \
					item.indexes_0[1] = rint(timestamp / resolutions[1]); \
					item.indexes_0[2] = REMOVED;	\
					break;				\
				}					\
				case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPAR_QPER_TIMESTAMP: \
				{					\
					item.indexes_0[0] = sqrt(v.raw[0] * v.raw[0] + v.raw[1] * v.raw[1]) / resolutions[0]; \
					item.indexes_0[1] = v.raw[2] / resolutions[1]; \
					item.indexes_0[2] = rint(timestamp / resolutions[2]); \
					break;				\
				}					\
				case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPAR_QPER: \
				{					\
					item.indexes_0[0] = sqrt(v.raw[0] * v.raw[0] + v.raw[1] * v.raw[1]) / resolutions[0]; \
					item.indexes_0[1] = v.raw[2] / resolutions[1]; \
					item.indexes_0[2] = REMOVED;	\
					break;				\
				}					\
				case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHI_QX: \
				{					\
					item.indexes_0[0] = rint(glms_vec3_norm(v) / resolutions[0]); \
					item.indexes_0[1] = rint((atan2(v.raw[2], -v.raw[1])) / M_PI * 180 / resolutions[1]); \
					item.indexes_0[2] = rint(v.raw[0] / resolutions[2]); \
					break;				\
				}					\
				case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHI_QY: \
				{					\
					item.indexes_0[0] = rint(glms_vec3_norm(v) / resolutions[0]); \
					item.indexes_0[1] = rint((atan2(v.raw[2], v.raw[0])) / M_PI * 180 / resolutions[1]); \
					item.indexes_0[2] = rint(v.raw[1] / resolutions[2]); \
					break;				\
				}					\
				case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHI_QZ: \
				{					\
					item.indexes_0[0] = rint(glms_vec3_norm(v) / resolutions[0]); \
					item.indexes_0[1] = rint((atan2(v.raw[0], v.raw[1])) / M_PI * 180 / resolutions[1]); \
					item.indexes_0[2] = rint(v.raw[2] / resolutions[2]); \
					break;				\
				}					\
				case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_STEREO: \
				{					\
					item.indexes_0[0] = rint(glms_vec3_norm(v) / resolutions[0]); \
					double ratio = v.raw[2] + item.indexes_0[0]; \
					item.indexes_0[1] = rint(v.raw[0] / ratio / resolutions[1]); \
					item.indexes_0[2] = rint(v.raw[1] / ratio / resolutions[2]); \
					break;				\
				}					\
				case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_ANGLES_ZAXIS_OMEGA: \
				{					\
					item.indexes_0[0] = rint(atan2(v.raw[1], v.raw[0]) / M_PI * 180 / resolutions[0]); \
					item.indexes_0[1] = rint(atan2(sqrt(v.raw[0] * v.raw[0] + v.raw[1] * v.raw[1]), v.raw[2]) / M_PI * 180 / resolutions[1]); \
					item.indexes_0[2] = rint(glms_vec3_norm(v) / resolutions[2]); \
					break;				\
				}					\
				case HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_ANGLES_ZAXIS_MU: \
				{					\
					item.indexes_0[0] = rint(atan2(v.raw[1], v.raw[0]) / M_PI * 180 / resolutions[0]); \
					item.indexes_0[1] = rint(atan2(sqrt(v.raw[0] * v.raw[0] + v.raw[1] * v.raw[1]), v.raw[2]) / M_PI * 180 / resolutions[1]); \
					item.indexes_0[2] = REMOVED;	\
					break;				\
				}					\
				}					\
				item.intensity = rint((double)image[i] * weight); \
                                                                        \
				if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
					darray_append(space->items, item); \
			}						\
		}							\
		space_update_axes(space, names, n_pixels, resolutions);	\
		hkl_detector_free(detector);				\
		hkl_sample_free(sample);				\
        }

HKL_BINOCULARS_SPACE_QCUSTOM2_IMPL(int32_t);
HKL_BINOCULARS_SPACE_QCUSTOM2_IMPL(uint16_t);
HKL_BINOCULARS_SPACE_QCUSTOM2_IMPL(uint32_t);

/* hkl */

#define HKL_BINOCULARS_SPACE_HKL_IMPL(image_t)                          \
        HKL_BINOCULARS_SPACE_HKL_DECL(image_t)                          \
        {                                                               \
                size_t i, j;                                            \
                const char * names[] = {"H", "K", "L"};                 \
                                                                        \
                assert(ARRAY_SIZE(names) == darray_size(space->axes));  \
                assert(ARRAY_SIZE(names) == n_resolutions);             \
                assert(n_pixels == space->max_items);                   \
                                                                        \
                const double *h = &pixels_coordinates[0 * n_pixels];    \
                const double *k = &pixels_coordinates[1 * n_pixels];    \
                const double *l = &pixels_coordinates[2 * n_pixels];    \
                                                                        \
                HklDetector *detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D); \
                const HklQuaternion q_d = hkl_geometry_detector_rotation_get(geometry, detector); \
                HklQuaternion qs = hkl_geometry_sample_rotation_get(geometry, sample); \
                const HklVector ki = hkl_geometry_ki_get(geometry);     \
                double K = hkl_vector_norm2(&ki);                       \
                HklMatrix RUB;                                          \
                HklMatrix RUB_1;                                        \
                hkl_quaternion_to_matrix(&qs, &RUB);                    \
                hkl_matrix_times_matrix(&RUB, hkl_sample_UB_get(sample)); \
                hkl_matrix_inv(&RUB, &RUB_1);                           \
                                                                        \
                darray_size(space->items) = 0;                          \
                for(i=0;i<n_pixels;++i){                                \
                        if(NULL == masked || 0 == masked[i]){           \
                                HklBinocularsSpaceItem item;            \
                                HklVector v = {{h[i], k[i], l[i]}};     \
                                                                        \
                                hkl_vector_rotated_quaternion(&v, &q_d); \
                                hkl_vector_times_double(&v, K);         \
                                hkl_vector_minus_vector(&v, &ki);       \
                                hkl_matrix_times_vector(&RUB_1, &v);    \
                                                                        \
                                for(j=0; j<ARRAY_SIZE(names); ++j){     \
                                        item.indexes_0[j] = rint(v.data[j] / resolutions[j]); \
                                }                                       \
                                item.intensity = rint((double)image[i] * weight); \
                                                                        \
                                if(TRUE == item_in_the_limits(&item, limits, n_limits)) \
                                        darray_append(space->items, item); \
                        }                                               \
                }                                                       \
                                                                        \
                space_update_axes(space, names, n_pixels, resolutions); \
                                                                        \
                hkl_detector_free(detector);                            \
        }

HKL_BINOCULARS_SPACE_HKL_IMPL(int32_t);
HKL_BINOCULARS_SPACE_HKL_IMPL(uint16_t);
HKL_BINOCULARS_SPACE_HKL_IMPL(uint32_t);

/* Cube */

/* this method compute the linear coordinates of the first element in
 * the absolute coordinates of the bin */
static inline ptrdiff_t compute_offset0(const darray_axis *axes)
{
	size_t i;
	ptrdiff_t len = 1;
	ptrdiff_t offset = 0;
        size_t n_axes = darray_size(*axes);

	for(i=0; i<n_axes; ++i){
		const HklBinocularsAxis *axis = &darray_item(*axes, n_axes-1-i);
		offset += len * axis->imin;
		len *= axis_size(axis);
	}
	return offset;
}

static inline HklBinocularsCube *empty_cube_from_axes(const darray_axis *axes)
{
        HklBinocularsCube *self = NULL;

        if (0 != darray_size(*axes)){
                HklBinocularsAxis *axis;

                self = g_new0(HklBinocularsCube, 1);
                darray_foreach(axis, *axes){
                        darray_append(self->axes, *axis);
                }
                self->offset0 = compute_offset0(&self->axes);
                self->photons = NULL;
                self->contributions = NULL;
        }

        return self;
}

static inline size_t cube_size(const HklBinocularsCube *self)
{
        size_t n = 1;
        HklBinocularsAxis *axis;

        darray_foreach(axis, self->axes){
                n *= axis_size(axis);
        }

	return n;
}

static inline size_t malloc_cube(HklBinocularsCube *self)
{
        size_t n = cube_size(self);

        self->photons = malloc(n * sizeof(*self->photons));
        self->contributions = malloc(n * sizeof(*self->contributions));

        return n;
}

static inline size_t calloc_cube(HklBinocularsCube *self)
{
        size_t n = cube_size(self);

        self->photons = calloc(n, sizeof(*self->photons));
        self->contributions = calloc(n, sizeof(*self->contributions));

        return n;
}

/* Using this method the Cube has already the right dimensions, we
 * just add the Space data into it. */
static inline void add_non_empty_space(HklBinocularsCube *cube,
                                       const HklBinocularsSpace *space)
{
        size_t i;
        size_t n_axes = darray_size(cube->axes);
        ptrdiff_t lens[n_axes];
        HklBinocularsSpaceItem *item;

        assert(n_axes == darray_size(space->axes));

        /* compute the lens */
        lens[0] = 1;

        for(i=1; i<n_axes; ++i){
                lens[i] = lens[i - 1] * axis_size(&darray_item(cube->axes, n_axes - i));
        }

        darray_foreach(item, space->items){
                ptrdiff_t w = -cube->offset0;

                for(i=0; i<n_axes; ++i){
                        w += lens[i] * item->indexes_0[n_axes - 1 - i];
                        /* fprintf(stdout, " %ld %ld", lens[i], item->indexes_0[n_axes - 1 - i]); */
                }

                /* fprintf(stdout, " w: %ld %ld\n", w, cube_size(cube)); */
                cube->photons[w] += item->intensity;
                cube->contributions[w] += 1;
        }
}

static inline int cube_is_empty(const HklBinocularsCube *self)
{
        return 0 == darray_size(self->axes);
}

HklBinocularsCube *hkl_binoculars_cube_new_empty(void)
{
        HklBinocularsCube *self = g_new(HklBinocularsCube, 1);

        darray_init(self->axes);
        self->offset0 = 0;
        self->photons = NULL;
        self->contributions = NULL;

        return self;
}

void hkl_binoculars_cube_free(HklBinocularsCube *self)
{
        free(self->contributions);
        free(self->photons);
        darray_free(self->axes);
        free(self);
}

void hkl_binoculars_cube_fprintf(FILE *f, const HklBinocularsCube *self)
{
        HklBinocularsAxis *axis;

	fprintf(f, "HklBinocularsCube: %p", self);

        fprintf(f, "\nn_axes: %ld", darray_size(self->axes));
        darray_foreach(axis, self->axes){
                fprintf(f, "\n");
                hkl_binoculars_axis_fprintf(f, axis);
        }
        fprintf(f, "\nphotons: %p", self->photons);
        fprintf(f, "\ncontributions: %p", self->contributions);
}

void hkl_binoculars_cube_dims(const HklBinocularsCube *self, size_t ndims, size_t *dims)
{
        HklBinocularsAxis *axis;

        assert(ndims == darray_size(self->axes));

        darray_foreach(axis, self->axes){
                *(dims++) = axis_size(axis);
        }
}

static inline size_t find_first_non_empty_space_index(size_t n_spaces,
                                                      const HklBinocularsSpace *const *spaces)
{
        size_t i;

        for(i=0; i<n_spaces; ++i)
                if(!space_is_empty(spaces[i]))
                        return i;

        return n_spaces;
}

HklBinocularsCube *hkl_binoculars_cube_new(size_t n_spaces,
                                           const HklBinocularsSpace *const *spaces)
{
        size_t i, i0;
        HklBinocularsCube *self = NULL;

        i0 = find_first_non_empty_space_index(n_spaces, spaces);

        if(i0 < n_spaces){
                self = empty_cube_from_axes(&spaces[i0]->axes);

                if(NULL != self){
                        /* compute the final cube dimensions and the index offset */
                        for(i=i0; i<n_spaces; ++i){
                                const HklBinocularsSpace *space = spaces[i];

                                if(space_is_empty(space))
                                        continue;

                                merge_axes(&self->axes, &space->axes);
                        }
                        self->offset0 = compute_offset0(&self->axes);

                        /* allocated the final cube photons and contributions */
                        calloc_cube(self);

                        /* add all the spaces */
                        for(i=i0; i<n_spaces; ++i){
                                const HklBinocularsSpace *space = spaces[i];

                                if(space_is_empty(space))
                                        continue;

                                add_non_empty_space(self, space);
                        }
                }
        }

	return self;
}

HklBinocularsCube *hkl_binoculars_cube_new_empty_from_cube(const HklBinocularsCube *cube)
{
	HklBinocularsCube *self = empty_cube_from_axes(&cube->axes);
        if(NULL != self)
                calloc_cube(self);

	return self;
}

HklBinocularsCube *hkl_binoculars_cube_new_from_space(const HklBinocularsSpace *space)
{
	HklBinocularsCube *self = NULL;

        if(space_is_empty(space)){
                self =  hkl_binoculars_cube_new_empty();
        } else {
                self = empty_cube_from_axes(&space->axes);
                if(NULL != self){
                        calloc_cube(self);

                        add_non_empty_space(self, space);
                }
        }

        return self;
}

HklBinocularsCube *hkl_binoculars_cube_new_copy(const HklBinocularsCube *src)
{
        size_t n;
	HklBinocularsCube *self = empty_cube_from_axes(&src->axes);

        if(NULL != self){
                /* allocate the final cube */
                n = malloc_cube(self);

                /* copy the data */
                if(self->photons)
                        memcpy(self->photons, src->photons, n * sizeof(*self->photons));
                if(self->contributions)
                        memcpy(self->contributions, src->contributions, n * sizeof(*self->contributions));
        }

        return self;
}

static inline void cube_add_cube_2(HklBinocularsCube *self,
                                   const HklBinocularsCube *other)
{
        size_t i, j;
        size_t i_offset, j_offset;

        size_t stride_i = 1;
        size_t stride_j = stride_i * axis_size(&darray_item(self->axes, 1));

        /* fill the values of other */
        size_t stride_i_other = 1;
        size_t stride_j_other = stride_i_other * axis_size(&darray_item(other->axes, 1));

        i_offset = darray_item(other->axes, 1).imin - darray_item(self->axes, 1).imin;
        j_offset = darray_item(other->axes, 0).imin - darray_item(self->axes, 0).imin;

        for(j=0; j<axis_size(&darray_item(other->axes, 0)); ++j){
                for(i=0; i<axis_size(&darray_item(other->axes, 1)); ++i){
                        size_t w = (i + i_offset) * stride_i + (j + j_offset) * stride_j;
                        size_t w1 = i * stride_i_other + j * stride_j_other;

                        self->photons[w] += other->photons[w1];
                        self->contributions[w] += other->contributions[w1];
                }
        }
}

static inline void cube_add_cube_3(HklBinocularsCube *self,
                                   const HklBinocularsCube *other)
{
        size_t i, j, k;
        size_t i_offset, j_offset, k_offset;

        size_t stride_i = 1;
        size_t stride_j = stride_i * axis_size(&darray_item(self->axes, 2));
        size_t stride_k = stride_j * axis_size(&darray_item(self->axes, 1));

        /* fill the values of other */
        size_t stride_i_other = 1;
        size_t stride_j_other = stride_i_other * axis_size(&darray_item(other->axes, 2));
        size_t stride_k_other = stride_j_other * axis_size(&darray_item(other->axes, 1));

        i_offset = darray_item(other->axes, 2).imin - darray_item(self->axes, 2).imin;
        j_offset = darray_item(other->axes, 1).imin - darray_item(self->axes, 1).imin;
        k_offset = darray_item(other->axes, 0).imin - darray_item(self->axes, 0).imin;

        for(k=0; k<axis_size(&darray_item(other->axes, 0)); ++k){
                for(j=0; j<axis_size(&darray_item(other->axes, 1)); ++j){
                        for(i=0; i<axis_size(&darray_item(other->axes, 2)); ++i){
                                size_t w = (i + i_offset) * stride_i + (j + j_offset) * stride_j + (k + k_offset) * stride_k;
                                size_t w1 = i * stride_i_other + j * stride_j_other + k * stride_k_other;

                                self->photons[w] += other->photons[w1];
                                self->contributions[w] += other->contributions[w1];
                        }
                }
        }
}

static inline void cube_add_cube(HklBinocularsCube *self,
                                 const HklBinocularsCube *other)
{
        assert(darray_size(self->axes) == darray_size(other->axes));

        switch(darray_size(self->axes)){
        case 2: cube_add_cube_2(self, other);
                break;
        case 3: cube_add_cube_3(self, other);
                break;
        default: assert(0);
        }
}

static inline void compute_strides(const darray_axis *axes, size_t strides[], size_t n_strides)
{
        size_t i;

        strides[0] = 1;
        for(i=1; i<n_strides; ++i)
                strides[i] = strides[i-1] * axis_size(&darray_item(*axes, n_strides - i + 1));
}

HklBinocularsCube *hkl_binoculars_cube_new_merge(const HklBinocularsCube *cube1,
                                                 const HklBinocularsCube *cube2)
{
        HklBinocularsCube *self = empty_cube_from_axes(&cube1->axes);
        if(NULL != self){
                merge_axes(&self->axes, &cube2->axes);
                calloc_cube(self);

                cube_add_cube(self, cube1);
                cube_add_cube(self, cube2);
        }
        return self;
}

static inline void switch_content(HklBinocularsCube *self,
                                  HklBinocularsCube *other)
{
        unsigned int *ptr;
        darray_axis tmp;
        ptrdiff_t offset0;

        tmp = self->axes;
        self->axes = other->axes;
        other->axes = tmp;

        offset0 = self->offset0;
        self->offset0 = other->offset0;
        other->offset0 = offset0;

        ptr = self->photons;
        self->photons = other->photons;
        other->photons = ptr;

        ptr = self->contributions;
        self->contributions = other->contributions;
        other->contributions = ptr;
}

void hkl_binoculars_cube_add_space(HklBinocularsCube *self,
                                   const HklBinocularsSpace *space)
{
#ifdef DEBUG
        fprintf(stdout, "\nENTERING hkl_binoculars_cube_add_space:\n");
        hkl_binoculars_cube_fprintf(stdout, self);
        hkl_binoculars_space_fprintf(stdout, space);
#endif
        /* check the compatibility of the cube and the space. */
        if (1 != space_is_empty(space)){
                if (does_not_include(&self->axes, &space->axes)){
#ifdef DEBUG
                        fprintf(stdout, "\nthe Cube does not contain the space, so create a new cube.");
#endif
                        if(0 != darray_size(self->axes)){ /* self cube is not empty */
                                HklBinocularsCube *cube = empty_cube_from_axes(&self->axes);
                                if(NULL != cube){
                                        merge_axes(&cube->axes, &space->axes); /* circonscript */
                                        cube->offset0 = compute_offset0(&cube->axes);
                                        calloc_cube(cube);
                                        cube_add_cube(cube, self);
                                        switch_content(self, cube);
                                        hkl_binoculars_cube_free(cube);
                                }
                        } else { /* self cube is empty */
                                HklBinocularsCube *cube =  empty_cube_from_axes(&space->axes);
                                if(NULL != cube){
                                        cube->offset0 = compute_offset0(&cube->axes);
                                        calloc_cube(cube);
                                        switch_content(self, cube);
                                        hkl_binoculars_cube_free(cube);
                                }
                        }
                }
                add_non_empty_space(self, space);
        }

#ifdef DEBUG
        fprintf(stdout, "\n");
        hkl_binoculars_cube_fprintf(stdout, self);
        fprintf(stdout, "\nLEAVING hkl_binoculars_cube_add_space:\n");
#endif
}

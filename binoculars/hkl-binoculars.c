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
 * Copyright (C) 2003-2022 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <stdio.h>
#include <time.h>

#include "ccan/array_size/array_size.h"
#include "datatype99.h"
#include "hkl-binoculars-private.h"
#include "hkl-geometry-private.h"
#include "hkl-matrix-private.h"
#include "hkl-quaternion-private.h"
#include "hkl-sample-private.h"
#include "hkl-vector-private.h"

/* #define DEBUG */

/* mark the masked pixels with this value */
#define MASKED PTRDIFF_MAX

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
	HklBinocularsAxisLimits *self = malloc(sizeof(*self));

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
	double *arr = malloc(6 * sizeof(*arr));

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

HklBinocularsSpace *hkl_binoculars_space_new(size_t max_items, size_t n_axes)
{
	HklBinocularsSpace *self = malloc(sizeof(*self));

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
	fprintf(f, "\nn_axes: %ld", darray_size(self->axes));
        darray_foreach(axis, self->axes){
		fprintf(f, "\n");
		hkl_binoculars_axis_fprintf(f, axis);
	}

        masked = self->max_items - darray_size(self->items);
        fprintf(f, "\nmasked pixels: %ld (%f%%)", masked, (double)masked / self->max_items * 100);
}

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

/* the array is pre filled with the pixel coordinates */

/* qparqper */

#define HKL_BINOCULARS_SPACE_QPARQPER_IMPL(image_t)                     \
        HKL_BINOCULARS_SPACE_QPARQPER_DECL(image_t)                     \
        {                                                               \
                size_t i;                                               \
                const char * names[] = {"qpar", "qper"};                \
                                                                        \
                assert(ARRAY_SIZE(names) == darray_size(space->axes));  \
                assert(ARRAY_SIZE(names) == n_resolutions);             \
                assert(n_pixels == space->max_items);                   \
                                                                        \
                const double *q_x = &pixels_coordinates[0 * n_pixels];  \
                const double *q_y = &pixels_coordinates[1 * n_pixels];  \
                const double *q_z = &pixels_coordinates[2 * n_pixels];  \
                                                                        \
                HklSample *sample = hkl_sample_new("test");             \
                HklDetector *detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D); \
                const HklQuaternion q = hkl_geometry_detector_rotation_get(geometry, detector); \
                const HklVector ki = hkl_geometry_ki_get(geometry);     \
                double k = hkl_vector_norm2(&ki);                       \
                HklQuaternion qs_1 = hkl_geometry_sample_rotation_get(geometry, sample); \
                switch(surf){                                           \
                case HKL_BINOCULARS_SURFACE_ORIENTATION_VERTICAL:       \
                {                                                       \
                        HklQuaternion q_ub;                             \
                        HklVector v_s = {{1, 0, 0}};                    \
                        hkl_quaternion_init_from_angle_and_axe(&q_ub, -M_PI_2, &v_s); \
                        hkl_quaternion_times_quaternion(&qs_1, &q_ub);  \
                        break;                                          \
                };                                                      \
                case HKL_BINOCULARS_SURFACE_ORIENTATION_HORIZONTAL:     \
                case HKL_BINOCULARS_SURFACE_ORIENTATION_NUM_ORIENTATION: \
                        break;                                          \
                };                                                      \
                hkl_quaternion_conjugate(&qs_1);                        \
                                                                        \
                darray_size(space->items) = 0;                          \
                                                                        \
                for(i=0;i<n_pixels;++i){                                \
                        if(NULL == masked || 0 == masked[i]){           \
                                HklBinocularsSpaceItem item;            \
                                HklVector v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                hkl_vector_rotated_quaternion(&v, &q);  \
                                hkl_vector_times_double(&v, k);         \
                                hkl_vector_minus_vector(&v, &ki);       \
                                hkl_vector_rotated_quaternion(&v, &qs_1); \
                                                                        \
                                item.indexes_0[0] = sqrt(v.data[0] * v.data[0] + v.data[1] * v.data[1]) / resolutions[0] / 10; \
                                item.indexes_0[1] = v.data[2] / resolutions[1] / 10; \
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
                hkl_sample_free(sample);                                \
        }

HKL_BINOCULARS_SPACE_QPARQPER_IMPL(int32_t);
HKL_BINOCULARS_SPACE_QPARQPER_IMPL(uint16_t);
HKL_BINOCULARS_SPACE_QPARQPER_IMPL(uint32_t);

/* qxqyqz */

#define HKL_BINOCULARS_SPACE_QXQYQZ_IMPL(image_t)                            \
        HKL_BINOCULARS_SPACE_QXQYQZ_DECL(image_t)                            \
        {                                                               \
                size_t i, j;                                            \
                const char * names[] = {"qx", "qy", "qz"};              \
                                                                        \
                assert(ARRAY_SIZE(names) == darray_size(space->axes));  \
                assert(ARRAY_SIZE(names) == n_resolutions);             \
                assert(n_pixels == space->max_items);                   \
                                                                        \
                const double *q_x = &pixels_coordinates[0 * n_pixels];  \
                const double *q_y = &pixels_coordinates[1 * n_pixels];  \
                const double *q_z = &pixels_coordinates[2 * n_pixels];  \
                                                                        \
                HklSample *sample = hkl_sample_new("test");             \
                HklDetector *detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D); \
                const HklQuaternion q = hkl_geometry_detector_rotation_get(geometry, detector); \
                const HklVector ki = hkl_geometry_ki_get(geometry);     \
                double k = hkl_vector_norm2(&ki);                       \
                HklQuaternion qs_1 = hkl_geometry_sample_rotation_get(geometry, sample); \
                switch(surf){                                           \
                case HKL_BINOCULARS_SURFACE_ORIENTATION_VERTICAL:       \
                {                                                       \
                        HklQuaternion q_ub;                             \
                        HklVector v_s = {{1, 0, 0}};                    \
                        hkl_quaternion_init_from_angle_and_axe(&q_ub, -M_PI_2, &v_s); \
                        hkl_quaternion_times_quaternion(&qs_1, &q_ub);  \
                        break;                                          \
                };                                                      \
                case HKL_BINOCULARS_SURFACE_ORIENTATION_HORIZONTAL:     \
                case HKL_BINOCULARS_SURFACE_ORIENTATION_NUM_ORIENTATION: \
                        break;                                          \
                };                                                      \
                hkl_quaternion_conjugate(&qs_1);                        \
                                                                        \
                darray_size(space->items) = 0;                          \
                                                                        \
                for(i=0;i<n_pixels;++i){                                \
                        if(NULL == masked || 0 == masked[i]){           \
                                HklBinocularsSpaceItem item;            \
                                HklVector v = {{q_x[i], q_y[i], q_z[i]}}; \
                                                                        \
                                hkl_vector_rotated_quaternion(&v, &q);  \
                                hkl_vector_times_double(&v, k);         \
                                hkl_vector_minus_vector(&v, &ki);       \
                                hkl_vector_rotated_quaternion(&v, &qs_1); \
                                                                        \
                                for(j=0; j<ARRAY_SIZE(names); ++j){     \
                                        item.indexes_0[j] = rint(v.data[j] / resolutions[j] / 10); \
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
                hkl_sample_free(sample);                                \
        }

HKL_BINOCULARS_SPACE_QXQYQZ_IMPL(int32_t);
HKL_BINOCULARS_SPACE_QXQYQZ_IMPL(uint16_t);
HKL_BINOCULARS_SPACE_QXQYQZ_IMPL(uint32_t);

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

static inline HklBinocularsCube *empty_cube(const darray_axis *axes)
{
        HklBinocularsAxis *axis;
        HklBinocularsCube *self = malloc(sizeof(HklBinocularsCube));

	/* compute the final cube dimensions and the index offset */
        darray_init(self->axes);
        if (NULL != axes){
                darray_foreach(axis, *axes){
                        darray_append(self->axes, *axis);
                }
        }
        self->offset0 = compute_offset0(&self->axes);
        self->photons = NULL;
        self->contributions = NULL;

        return self;
}

static inline int cube_is_empty(const HklBinocularsCube *self)
{
        return 0 == darray_size(self->axes);
}

static inline size_t cube_size(const HklBinocularsCube *self)
{
	HklBinocularsAxis *axis;
	size_t n = 1;

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

void hkl_binoculars_cube_free(HklBinocularsCube *self)
{
        /* fprintf(stdout, "\nHklBinocularsCube: deallocated %p\n", self); */

	free(self->contributions);
	free(self->photons);
	darray_free(self->axes);
	free(self);
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

void hkl_binoculars_cube_fprintf(FILE *f, const HklBinocularsCube *self)
{
        HklBinocularsAxis *axis;

	fprintf(f, "HklBinocularsCube: %p\n", self);
	fprintf(f, "n_axes: %ld", darray_size(self->axes));
        darray_foreach(axis, self->axes){
		fprintf(f, "\n");
		hkl_binoculars_axis_fprintf(f, axis);
	}
	fprintf(f, "\nphotons: %p", self->photons);
	fprintf(f, "\ncontributions: %p", self->contributions);
}

void hkl_binoculars_cube_dims(const HklBinocularsCube *self, size_t ndim, size_t *dims)
{
        HklBinocularsAxis *axis;

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
        HklBinocularsCube *self;

        i0 = find_first_non_empty_space_index(n_spaces, spaces);

        if (i0 == n_spaces){ /* all spaces are empty */
                self = empty_cube(NULL);
        } else {
                self = empty_cube(&spaces[i0]->axes);

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

	return self;
}

HklBinocularsCube *hkl_binoculars_cube_new_empty(void)
{
        return empty_cube(NULL);
}

HklBinocularsCube *hkl_binoculars_cube_new_empty_from_cube(const HklBinocularsCube *cube)
{
	HklBinocularsCube *self = empty_cube(&cube->axes);

	/* allocated the final cube */
        calloc_cube(self);

	return self;
}

HklBinocularsCube *hkl_binoculars_cube_new_from_space(const HklBinocularsSpace *space)
{
	HklBinocularsCube *self;

        if(space_is_empty(space)){
                self = empty_cube(NULL);
        } else {
                self = empty_cube(&space->axes);

                /* allocated the final cube */
                calloc_cube(self);

                add_non_empty_space(self, space);
        }

        return self;
}

HklBinocularsCube *hkl_binoculars_cube_new_copy(const HklBinocularsCube *src)
{
        size_t n;
	HklBinocularsCube *self = empty_cube(&src->axes);

	/* allocate the final cube */
        n = malloc_cube(self);

        /* copy the data */
        memcpy(self->photons, src->photons, n * sizeof(*self->photons));
        memcpy(self->contributions, src->contributions, n * sizeof(*self->contributions));

        hkl_binoculars_cube_fprintf(stdout, src);
        hkl_binoculars_cube_fprintf(stdout, self);
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
        if ((NULL == other->photons) || (NULL == other->contributions))
                return;

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
        HklBinocularsCube *self = empty_cube(&cube1->axes);

        merge_axes(&self->axes, &cube2->axes);
        self->offset0 = compute_offset0(&self->axes);
        calloc_cube(self);

        cube_add_cube(self, cube1);
        cube_add_cube(self, cube2);

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
        HklBinocularsCube *cube;

#ifdef DEBUG
        fprintf(stdout, "\nENTERING hkl_binoculars_cube_add_space:\n");
        hkl_binoculars_cube_fprintf(stdout, self);
        hkl_binoculars_space_fprintf(stdout, space);
#endif
        if(cube_is_empty(self)){
#ifdef DEBUG
                fprintf(stdout, "\nCreate a new empty cube");
#endif

                cube = hkl_binoculars_cube_new_from_space(space);
                switch_content(self, cube);
                hkl_binoculars_cube_free(cube);
        }else{
                /* check the compatibility of the cube and the space. */
                if (!space_is_empty(space)){
                        if (does_not_include(&self->axes, &space->axes)){
#ifdef DEBUG
                                fprintf(stdout, "\nthe Cube does not contain the space, so create a new cube.");
#endif
                                HklBinocularsCube *cube = empty_cube(&self->axes);

                                merge_axes(&cube->axes, &space->axes); /* circonscript */
                                cube->offset0 = compute_offset0(&cube->axes);
                                calloc_cube(cube);

                                cube_add_cube(cube, self);

                                switch_content(self, cube);
                                hkl_binoculars_cube_free(cube);
                        }
                        add_non_empty_space(self, space);
                }
        }
#ifdef DEBUG
        fprintf(stdout, "\n");
        hkl_binoculars_cube_fprintf(stdout, self);
        fprintf(stdout, "\nLEAVING hkl_binoculars_cube_add_space:\n");
#endif
}

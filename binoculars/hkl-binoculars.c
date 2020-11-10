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
 * Copyright (C) 2003-2020 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <stdio.h>
#include <time.h>
#include "hkl-binoculars.h"
#include "ccan/array_size/array_size.h"
#include "hkl-geometry-private.h"
#include "hkl-matrix-private.h"
#include "hkl-quaternion-private.h"
#include "hkl-sample-private.h"
#include "hkl-vector-private.h"

/* mark the masked pixels with this value */
#define MASKED PTRDIFF_MAX

static inline ptrdiff_t min(ptrdiff_t x, ptrdiff_t y)
{
	return y ^ ((x ^ y) & -(x < y));
}

static inline ptrdiff_t max(ptrdiff_t x, ptrdiff_t y)
{
	return x ^ ((x ^ y) & -(x < y));
}

static inline size_t axis_size(const HklBinocularsAxis *self)
{
	return self->imax - self->imin + 1;
}

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
        return self->imin >= other->imin && self->imax <= other->imax;
}

static inline void hkl_binoculars_axis_merge(HklBinocularsAxis *self, const HklBinocularsAxis *other)
{
	self->imin = min(self->imin, other->imin);
	self->imax = max(self->imax, other->imax);
}

static inline void axes_merge(darray_axis *axes,
                              const darray_axis *others)
{
        size_t i;

        for(i=0; i<darray_size(*axes); ++i)
                hkl_binoculars_axis_merge(&darray_item(*axes, i),
                                          &darray_item(*others, i));
}

void hkl_binoculars_axis_fprintf(FILE *f, const HklBinocularsAxis *self)
{
	fprintf(f, "%s : %ld min: %f max: %f res: %f size: %ld",
		self->name, self->index,
		axis_min(self), axis_max(self),
		self->resolution, axis_size(self));
}

HklBinocularsSpace *hkl_binoculars_space_new(size_t n_indexes_0, size_t n_axes)
{
	HklBinocularsSpace *self = malloc(sizeof(*self));

	self->n_indexes_0 = n_indexes_0;
	self->indexes_0 = malloc(n_indexes_0 * n_axes * sizeof(*self->indexes_0));
        darray_init(self->axes);
        darray_resize(self->axes, n_axes);

	return self;
}

void hkl_binoculars_space_free(HklBinocularsSpace *self)
{
	darray_free(self->axes);
	free(self->indexes_0);
	free(self);
}

static inline ptrdiff_t *space_indexes(const HklBinocularsSpace *space,size_t index)
{
	return &space->indexes_0[index * space->n_indexes_0];
}

void hkl_binoculars_space_fprintf(FILE *f, const HklBinocularsSpace *self)
{
	uint32_t i;
        size_t masked=0;
        HklBinocularsAxis *axis;

	fprintf(f, "'self: %p\n", self);
	fprintf(f, "n_indexes_0: %ld\n", self->n_indexes_0);
	fprintf(f, "n_axes: %ld", darray_size(self->axes));
        darray_foreach(axis, self->axes){
		fprintf(f, "\n");
		hkl_binoculars_axis_fprintf(f, axis);
	}
        for(i=0; i<self->n_indexes_0; ++i){
                if(space_indexes(self, 0)[i] == MASKED)
                        masked +=1;
        }
        fprintf(f, "\nmasked pixels: %ld (%f%%)", masked, (double)masked / self->n_indexes_0 * 100);
}

static inline void space_update_axes(HklBinocularsSpace *space,
                                     const char *names[],
                                     size_t n_pixels,
                                     const double resolutions[])
{
        size_t i;
        size_t j;
        size_t ji=0;

        /* find the first index of non masked array. All masked pixels
         * are marked with the value MASKED so we just need to
         * find the index using the first axis (0) */
        for(j=0; j<n_pixels; j++){
                if (space_indexes(space, 0)[j] != MASKED){
                        ji = j;
                        break;
                }
        }

	/* compress the coordinates into the space */
	for(i=0; i<darray_size(space->axes); ++i){
		const ptrdiff_t *idx = space_indexes(space, i);
		const double resolution = resolutions[i];
		HklBinocularsAxis *axis = &darray_item(space->axes, i);
                ptrdiff_t origin = idx[ji];
                ptrdiff_t last = idx[ji];

                /* continue */
		for(j=ji+1; j<n_pixels; ++j){
			ptrdiff_t index = idx[j];

                        if (index != MASKED){
                                origin = min(origin, index);
                                last = max(last, index);
                        }
		}
                hkl_binoculars_axis_init(axis, names[i], i, origin, last, resolution);
	}
}

/* the array is pre filled with the pixel coordinates */
void hkl_binoculars_space_q(HklBinocularsSpace *space,
                            const HklGeometry *geometry,
                            const uint16_t *image,
                            size_t n_pixels,
                            const double *pixels_coordinates,
                            size_t pixels_coordinates_ndim,
                            const size_t *pixels_coordinates_dims,
                            const double *resolutions,
                            size_t n_resolutions,
                            const uint8_t *masked)
{
	size_t i, j;
	const char * names[] = {"qx", "qy", "qz"};

        assert(ARRAY_SIZE(names) == darray_size(space->axes) && n_pixels == space->n_indexes_0);

	const double *q_x = &pixels_coordinates[0 * n_pixels];
	const double *q_y = &pixels_coordinates[1 * n_pixels];
	const double *q_z = &pixels_coordinates[2 * n_pixels];

	HklSample *sample = hkl_sample_new("test");
	HklDetector *detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	const HklQuaternion q = hkl_geometry_detector_rotation_get(geometry, detector);
	HklQuaternion qs_1 = hkl_geometry_sample_rotation_get(geometry, sample);
	const HklVector ki = hkl_geometry_ki_get(geometry);
        double k = hkl_vector_norm2(&ki);
	hkl_quaternion_conjugate(&qs_1);

	/* compute the coordinates in the last axis basis and the
	 * indexes */
	for(i=0;i<n_pixels;++i){
                if(0 == masked[i]){
                        HklVector v = {{ q_x[i],
                                         q_y[i],
                                         q_z[i]}};

                        hkl_vector_rotated_quaternion(&v, &q);
                        hkl_vector_normalize(&v);
                        hkl_vector_times_double(&v, k);
                        hkl_vector_minus_vector(&v, &ki);
                        hkl_vector_rotated_quaternion(&v, &qs_1);

                        for(j=0; j<ARRAY_SIZE(names); ++j)
                                space_indexes(space, j)[i] = rint(v.data[j] / resolutions[j]);
                } else {
                        for(j=0; j<ARRAY_SIZE(names); ++j)
                                space_indexes(space, j)[i] = MASKED;
                }
	}

        space_update_axes(space, names, n_pixels, resolutions);

	hkl_detector_free(detector);
        hkl_sample_free(sample);
}

/* the array is pre filled with the pixel coordinates */
void hkl_binoculars_space_hkl(HklBinocularsSpace *space,
                              const HklGeometry *geometry,
                              const HklSample *sample,
                              const uint16_t *image,
                              size_t n_pixels,
                              const double *pixels_coordinates,
                              size_t pixels_coordinates_ndim,
                              const size_t *pixels_coordinates_dims,
                              const double *resolutions,
                              size_t n_resolutions,
                              const uint8_t *masked)
{
	size_t i, j;
	const char * names[] = {"h", "k", "l"};

        assert(ARRAY_SIZE(names) == darray_size(space->axes) && n_pixels == space->n_indexes_0);

        const double *h = &pixels_coordinates[0 * n_pixels];
	const double *k = &pixels_coordinates[1 * n_pixels];
	const double *l = &pixels_coordinates[2 * n_pixels];

	HklDetector *detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	const HklQuaternion q_d = hkl_geometry_detector_rotation_get(geometry, detector);
	HklQuaternion qs = hkl_geometry_sample_rotation_get(geometry, sample);
	const HklVector ki = hkl_geometry_ki_get(geometry);
        double K = hkl_vector_norm2(&ki);
        HklMatrix RUB;
        HklMatrix RUB_1;
        hkl_quaternion_to_matrix(&qs, &RUB);
	hkl_matrix_times_matrix(&RUB, hkl_sample_UB_get(sample));
        hkl_matrix_inv(&RUB, &RUB_1);

	/* compute the coordinates in the last axis basis and the
	 * indexes */
	for(i=0;i<n_pixels;++i){
                if(0 == masked[i]){
                        HklVector v = {{h[i], k[i], l[i]}};

                        hkl_vector_rotated_quaternion(&v, &q_d);
                        hkl_vector_normalize(&v);
                        hkl_vector_times_double(&v, K);
                        hkl_vector_minus_vector(&v, &ki);
                        hkl_matrix_times_vector(&RUB_1, &v);

                        for(j=0; j<ARRAY_SIZE(names); ++j)
                                space_indexes(space, j)[i] = rint(v.data[j] / resolutions[j]);
                } else {
                        for(j=0; j<ARRAY_SIZE(names); ++j)
                                space_indexes(space, j)[i] = MASKED;
                }
	}

        space_update_axes(space, names, n_pixels, resolutions);

        /* hkl_binoculars_space_fprintf(stdout, space); */
	hkl_detector_free(detector);
}

static inline HklBinocularsCube *empty_cube(const darray_axis *axes)
{
        HklBinocularsAxis *axis;
        HklBinocularsCube *self = malloc(sizeof(HklBinocularsCube));

	/* compute the final cube dimensions and the index offset */
	darray_init(self->axes);
        darray_foreach(axis, *axes){
                darray_append(self->axes, *axis);
        }

        self->photons = NULL;
        self->contributions = NULL;

        return self;
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
	free(self->contributions);
	free(self->photons);
	darray_free(self->axes);
	free(self);
}

static inline ptrdiff_t compute_offset(const darray_axis *axes)
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

/* Using this method the Cube has already the right dimensions, we
 * just add the Space data into it. */
void add_space(HklBinocularsCube *cube,
	       const HklBinocularsSpace *space,
	       size_t n_pixels,
	       const uint16_t *img,
	       ptrdiff_t offset0)
{
	size_t i;
	size_t j;
	ptrdiff_t len = 1;
	ptrdiff_t indexes[n_pixels];
        size_t n_axes = darray_size(cube->axes);

	for(i=0; i<n_pixels; ++i){
		indexes[i] = -offset0;
	}

	for(i=0; i<n_axes; ++i){
                size_t _i = n_axes - 1 - i;
		ptrdiff_t *idx = space_indexes(space, _i);
		for(j=0; j<n_pixels; ++j){
                        if (idx[j] != MASKED)
                                indexes[j] += len * idx[j];
                        else
                                indexes[j] = MASKED;
		}
		len *= axis_size(&darray_item(cube->axes, _i));
	}

	for(i=0; i<n_pixels; ++i){
                if (indexes[i] != MASKED){
                        size_t w = indexes[i];
                        cube->photons[w] += img[i];
                        cube->contributions[w] += 1;
                }
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

void hkl_binoculars_cube_add_space(HklBinocularsCube *self,  const HklBinocularsSpace *space,
                                   size_t n_pixels, const uint16_t *img)
{
        /* check the compatibility of the cube and the space. */

        /* if not compatible create a new empty cube, add the cube then the space */

        /* if compatible, just add the space. */
}

HklBinocularsCube *hkl_binoculars_cube_new(size_t n_spaces,
                                           const HklBinocularsSpace *const *spaces,
					   size_t n_pixels,
                                           const uint16_t **imgs)
{
	size_t i;
	const HklBinocularsSpace *space0 = spaces[0];
	ptrdiff_t offset0;
	HklBinocularsCube *self = empty_cube(&space0->axes);

	/* compute the final cube dimensions and the index offset */
	for(i=1; i<n_spaces; ++i){
                axes_merge(&self->axes, &spaces[i]->axes);
        }
	offset0 = compute_offset(&self->axes);

	/* allocated the final cube photons and contributions */
        calloc_cube(self);

	/* add all the spaces */
	for(i=0; i<n_spaces; ++i){
		add_space(self, spaces[i], n_pixels, imgs[i], offset0);
	}

	return self;
}

HklBinocularsCube *hkl_binoculars_cube_new_from_space(const HklBinocularsSpace *space,
                                                      size_t n_pixels, const uint16_t *img)
{
	ptrdiff_t offset0 = compute_offset(&space->axes);
	HklBinocularsCube *self = empty_cube(&space->axes);

	/* allocated the final cube */
        calloc_cube(self);

        add_space(self, space, n_pixels, img, offset0);

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

        return self;
}


static inline void cube_add_cube(HklBinocularsCube *self,
                                 const HklBinocularsCube *other)
{
        if ((NULL == other->photons) || (NULL == other->contributions))
                return;

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

HklBinocularsCube *hkl_binoculars_cube_new_merge(const HklBinocularsCube *cube1,
                                                 const HklBinocularsCube *cube2)
{
        HklBinocularsCube *self = empty_cube(&cube1->axes);

        axes_merge(&self->axes, &cube2->axes);

        calloc_cube(self);

        cube_add_cube(self, cube1);
        cube_add_cube(self, cube2);

        return self;
}

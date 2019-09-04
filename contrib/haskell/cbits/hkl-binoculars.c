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
 * Copyright (C) 2003-2019 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <time.h>
#include "hkl-binoculars.h"

/* #define DEBUG 1 */

/**
 * hkl_vector_norm2: (skip)
 * @self: the #hklvector use to compute the norm2
 *
 * compute the norm2 of an #HklVector
 *
 * Returns: the sqrt(|v|)
 **/
double hkl_vector_norm2(const HklVector *self)
{
	return sqrt(self->data[0] * self->data[0]
		    + self->data[1] * self->data[1]
		    + self->data[2] * self->data[2]);
}

/**
 * hkl_vector_rotated_quaternion: (skip)
 * @self: the #HklVector to rotate
 * @qr: the #HklQuaternion use to rotate the vector
 *
 * rotate an #HklVector using an #HklQuaternion.
 **/
void hkl_vector_rotated_quaternion(HklVector *self, const HklQuaternion *qr)
{
	double v1 = self->data[0];
	double v2 = self->data[1];
	double v3 = self->data[2];
	double a = qr->data[0];
	double b = qr->data[1];
	double c = qr->data[2];
	double d = qr->data[3];

	double t2 =   a*b;
	double t3 =   a*c;
	double t4 =   a*d;
	double t5 =  -b*b;
	double t6 =   b*c;
	double t7 =   b*d;
	double t8 =  -c*c;
	double t9 =   c*d;
	double t10 = -d*d;

	self->data[0] = 2*( (t8 + t10)*v1 + (t6 -  t4)*v2 + (t3 + t7)*v3 ) + v1;
	self->data[1] = 2*( (t4 +  t6)*v1 + (t5 + t10)*v2 + (t9 - t2)*v3 ) + v2;
	self->data[2] = 2*( (t7 -  t3)*v1 + (t2 +  t9)*v2 + (t5 + t8)*v3 ) + v3;
}

/**
 * hkl_vector_minus_vector: (skip)
 * @self: the modified #HklVector
 * @vector: the #hklvector to substract
 *
 * substract an #HklVector to another one.
 **/
void hkl_vector_minus_vector(HklVector *self, const HklVector *vector)
{
	unsigned int i;
	for (i=0;i<3;i++)
		self->data[i] -= vector->data[i];
}

/**
 * hkl_vector_times_double: (skip)
 * @self: the #HklVector to modify
 * @d: the multiply factor
 *
 * multiply an #HklVector by a constant value.
 **/
void hkl_vector_times_double(HklVector *self, const double d)
{
	unsigned int i;
	for (i=0;i<3;i++)
		self->data[i] *= d;
}

/**
 * hkl_vector_div_double: (skip)
 * @self: the #HklVector to divide.
 * @d: constant use to divide the #HklVector
 *
 * divide an #HklVector by constant.
 **/
void hkl_vector_div_double(HklVector *self, const double d)
{
	unsigned int i;
	for (i=0;i<3;i++)
		self->data[i] /= d;
}

/**
 * hkl_vector_normalize: (skip)
 * @self: the #HklVector to normalize
 *
 * normalize a hkl_vector
 *
 * Returns: TRUE if the #HklVector can be normalized, FALSE otherwise
 **/
int hkl_vector_normalize(HklVector *self)
{
	double norm = hkl_vector_norm2(self);
	if ( norm <= HKL_EPSILON )
		return FALSE;

	hkl_vector_div_double(self, norm);

	return TRUE;
}

/**
 * hkl_vector_fprintf: (skip)
 * @file: the stream to print into
 * @self: the #HklVector to print.
 *
 * print an #HklVector into a stream
 **/
void hkl_vector_fprintf(FILE *file, const HklVector *self)
{
	fprintf(file, "|%f, %f, %f|", self->data[0], self->data[1], self->data[2]);
}

inline int32_t min(int32_t x, int32_t y)
{
	return y ^ ((x ^ y) & -(x < y));
}

inline int32_t max(int32_t x, int32_t y)
{
	return x ^ ((x ^ y) & -(x < y));
}

static int axis_size(const HklBinocularsAxis *self)
{
	return self->imax - self->imin + 1;
}

static double axis_min(const HklBinocularsAxis *self)
{
	return self->imin * self->resolution;
}

static double axis_max(const HklBinocularsAxis *self)
{
	return self->imax * self->resolution;
}

static void hkl_binoculars_axis_init(HklBinocularsAxis *self,
				     const char *name,
				     int index,
				     int imin,
				     int imax,
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
static void hkl_binoculars_axis_merge(HklBinocularsAxis *self, const HklBinocularsAxis *other)
{
	self->imin = min(self->imin, other->imin);
	self->imax = max(self->imax, other->imax);
}

void hkl_binoculars_axis_fprintf(FILE *f, const HklBinocularsAxis *self)
{
	fprintf(f, "%s : %d min: %f max: %f res: %f size: %d",
		self->name, self->index,
		axis_min(self), axis_max(self),
		self->resolution, axis_size(self));
}

HklBinocularsSpace *space_new(int32_t n_indexes_0, int32_t n_axes)
{
	HklBinocularsSpace *self = malloc(sizeof(*self));

	self->n_indexes_0 = n_indexes_0;
	self->indexes_0 = malloc(n_indexes_0 * n_axes * sizeof(*self->indexes_0));
	self->n_axes = n_axes;
	self->axes = malloc(sizeof(*self->axes) * n_axes);

	return self;
}

void hkl_binoculars_space_free(HklBinocularsSpace *self)
{
	free(self->axes);
	free(self->indexes_0);
	free(self);
}

void hkl_binoculars_space_fprintf(FILE *f, const HklBinocularsSpace *self)
{
	uint32_t i;

	fprintf(f, "'self: %p\n", self);
	fprintf(f, "n_indexes_0: %d\n", self->n_indexes_0);
	fprintf(f, "n_axes: %d", self->n_axes);
	for(i=0; i<self->n_axes; ++i){
		fprintf(f, "\n");
		hkl_binoculars_axis_fprintf(f, &self->axes[i]);
	}
}

int32_t *space_indexes(const HklBinocularsSpace *space, int index)
{
	return &space->indexes_0[index * space->n_indexes_0];
}

/* the array is pre filled with the pixel coordinates */
HklBinocularsSpace *hkl_binoculars_space_q(const HklGeometry *geometry,
					   double k,
					   const uint16_t *image,
					   int32_t n_pixels,
					   const double *pixels_coordinates,
					   int32_t pixels_coordinates_ndim,
					   const int32_t *pixels_coordinates_dims,
					   const double *resolutions,
					   int32_t n_resolutions)
{
	int32_t i;
	int32_t j;
	int32_t n_coordinates = 3;
	const char * names[] = {"Qx", "Qy", "Qz"};

	const double *q_x = &pixels_coordinates[0 * n_pixels];
	const double *q_y = &pixels_coordinates[1 * n_pixels];
	const double *q_z = &pixels_coordinates[2 * n_pixels];

	HklDetector *detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	const HklQuaternion q = hkl_geometry_detector_rotation_get(geometry, detector);
	const HklVector ki = {{1, 0, 0}};
	HklBinocularsSpace *space = space_new(n_pixels, n_coordinates);
	int32_t *indexes_0 = space_indexes(space, 0);
	int32_t *indexes_1 = space_indexes(space, 1);
	int32_t *indexes_2 = space_indexes(space, 2);

	/* compute the coordinates and the indexes */
	for(i=0;i<n_pixels;++i){
		HklVector v = {{ q_x[i],
				 q_y[i],
				 q_z[i]}};

		hkl_vector_rotated_quaternion(&v, &q);
		hkl_vector_normalize(&v);
		hkl_vector_minus_vector(&v, &ki);
		hkl_vector_times_double(&v, k);

		indexes_0[i] = rint(v.data[0] / resolutions[0]);
		indexes_1[i] = rint(v.data[1] / resolutions[1]);
		indexes_2[i] = rint(v.data[2] / resolutions[2]);
	}

	/* compress the coordinates into the space */
	for(i=0; i<n_coordinates; ++i){
		const int32_t *idx = space_indexes(space, i);
		const double resolution = resolutions[i];
		HklBinocularsAxis *axis = &space->axes[i];
		int32_t origin = idx[0];
		int32_t last = idx[0];

		for(j=1; j<n_pixels; ++j){
			int32_t index = idx[j];

			origin = min(origin, index);
			last = max(last, index);
		}
		hkl_binoculars_axis_init(axis, names[i], i, origin, last, resolution);
	}

	hkl_detector_free(detector);

	return space;
}

void hkl_binoculars_cube_free(HklBinocularsCube *self)
{
	free(self->contributions);
	free(self->photons);
	free(self->axes);
	free(self);
}

static int32_t compute_offset(int32_t n_axes, const HklBinocularsAxis *axes)
{
	int i;
	int len = 1;
	int32_t offset = 0;

	for(i=0; i<n_axes; ++i){
		const HklBinocularsAxis *axis = &axes[n_axes-1-i];
		offset += axis->imin * len;
		len *= axis_size(axis);
	}
	return offset;
}

void add_space(HklBinocularsCube *cube,
	       const HklBinocularsSpace *space,
	       int32_t n_pixels,
	       const uint16_t *img,
	       int32_t offset0)
{
	int i;
	int j;
	int len = 1;
	long indexes[n_pixels];

	for(i=0; i<n_pixels; ++i){
		indexes[i] = -offset0;
	}

	for(i=0; i<cube->n_axes; ++i){
		int32_t *idx = space_indexes(space, cube->n_axes - 1 - i);
		for(j=0; j<n_pixels; ++j){
			indexes[j] += idx[j] * len;
		}
		len *= axis_size(&cube->axes[cube->n_axes - 1 - i]);
	}

	for(i=0; i<n_pixels; ++i){
		size_t w = indexes[i];
		cube->photons[w] += img[i];
		cube->contributions[w] += 1;
	}
}

void hkl_binoculars_cube_fprintf(FILE *f, const HklBinocularsCube *self)
{
	int i;

	fprintf(f, "HklBinocularsCube: %p\n", self);
	fprintf(f, "n_axes: %d", self->n_axes);
	for(i=0; i<self->n_axes;  ++i){
		fprintf(f, "\n");
		hkl_binoculars_axis_fprintf(f, &self->axes[i]);
	}
	fprintf(f, "photons: %p\n", self->photons);
	fprintf(f, "contributions: %p\n", self->contributions);
}

void hkl_binoculars_cube_dims(const HklBinocularsCube *self, int ndim, int *dims)
{
	int i;

	for(i=0; i<ndim; ++i)
		dims[i] = axis_size(&self->axes[i]);
}

static inline size_t cube_size(int n_axes, HklBinocularsAxis *axes)
{
	int i;
	size_t n = 1;

	for(i=0; i<n_axes; ++i)
		n *= axis_size(&axes[i]);
	return n;
}

HklBinocularsCube *hkl_binoculars_cube_new(int n_spaces, const HklBinocularsSpace *const *spaces,
					   int32_t n_pixels, const uint16_t **imgs)
{
	int i;
	int j;
	int n = 1;
	const HklBinocularsSpace *space0 = spaces[0];
	int n_axes = space0->n_axes;
	int32_t offset0;
	HklBinocularsCube *self = malloc(sizeof(HklBinocularsCube));
	self->n_axes = n_axes;
	self->axes = calloc(n_axes, sizeof(HklBinocularsAxis));

	/* compute the final cube dimensions and the index offset */
	for(i=0; i<n_axes; ++i)
		self->axes[i] = space0->axes[i];

	for(i=1; i<n_spaces; ++i){
		for(j=0; j<n_axes; ++j){
			hkl_binoculars_axis_merge(&self->axes[j],
						  &spaces[i]->axes[j]);
		}
	}
	offset0 = compute_offset(n_axes, self->axes);

	/* allocated the final cube */
	n = cube_size(n_axes, self->axes);
	self->photons = calloc(n, sizeof(*self->photons));
	self->contributions = calloc(n, sizeof(*self->contributions));

	/* add all the spaces */
	for(i=0; i<n_spaces; ++i){
		add_space(self, spaces[i], n_pixels, imgs[i], offset0);
	}

	hkl_binoculars_cube_fprintf(stdout, self);

	return self;
}

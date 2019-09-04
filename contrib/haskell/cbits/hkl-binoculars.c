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

HklBinocularsSpace *space_new(int32_t n_indexes, int32_t ndim)
{
	HklBinocularsSpace *self = malloc(sizeof(*self));

	self->indexes_0 = malloc(n_indexes * ndim * sizeof(*self->indexes_0));
	self->n_indexes = n_indexes;
	self->offset_indexes = 0;

	self->ndim = ndim;
	self->axes = malloc(sizeof(*self->axes) * ndim);

	return self;
}

void hkl_binoculars_space_free(HklBinocularsSpace *self)
{
	free(self->axes);
	free(self->indexes_0);
	free(self);
}

static void hkl_binoculars_space_fprintf(FILE *f, const HklBinocularsSpace *self)
{
	uint32_t i;

	fprintf(f, "'\nself: %p", self);
	fprintf(f, "\nn_indexes: %d", self->n_indexes);
	fprintf(f, "\noffset_indexes: %d", self->offset_indexes);
	fprintf(f, "\nndim: %d", self->ndim);
	for(i=0; i<self->ndim; ++i){
		fprintf(f, "\n");
		hkl_binoculars_axis_fprintf(f, &self->axes[i]);
	}
}

int32_t *space_indexes(const HklBinocularsSpace *space, int index)
{
	return &space->indexes_0[index * space->n_indexes];
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
	int32_t len = 1;
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

		for(j=0; j<n_pixels; ++j){
			uint32_t index = idx[j];

			origin = min(origin, index);
			last = max(last, index);
		}
		hkl_binoculars_axis_init(axis, names[i], i, origin, last, resolution);
		space->offset_indexes += origin * len;
		len *=  axis_size(axis);
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

static size_t compute_offset(int32_t ndim, const HklBinocularsAxis *axes)
{
	int i;
	int len = 1;
	size_t res = 0;

	for(i=0; i<ndim; ++i){
		const HklBinocularsAxis *axis = &axes[i];
		res += axis->imin * len;
		len *= axis_size(axis);
	}
	return res;
}

void add_space(HklBinocularsCube *cube,
	       const HklBinocularsSpace *space,
	       int32_t n_pixels,
	       const uint16_t *img)
{
	int i;
	int j;
	int len = 1;
	long indexes[n_pixels];

	for(i=0; i<n_pixels; ++i){
		indexes[i] = 0;
	}

	for(i=0; i<cube->ndim; ++i){
		int32_t *idx = space_indexes(space, i);
		for(j=0; j<n_pixels; ++j){
			indexes[j] += idx[j] * len;
		}
		len *= axis_size(&space->axes[i]);
	}

	for(i=0; i<n_pixels; ++i){
		size_t w = indexes[i] - cube->_offset;
		cube->photons[w] += img[i];
		cube->contributions[w] += 1;
	}
}

static void hkl_binoculars_cube_fprintf(FILE *f, const HklBinocularsCube *self)
{
	int i;

	fprintf(f, "HklBinocularsCube: %p\n", self);
	fprintf(f, "ndim: %d", self->ndim);
	for(i=0; i<self->ndim;  ++i){
		fprintf(f, "\n");
		hkl_binoculars_axis_fprintf(f, &self->axes[i]);
	}
	fprintf(f, "photons: %p\n", self->photons);
	fprintf(f, "contributions: %p\n", self->contributions);
	fprintf(f, "_offset: %d\n", self->_offset);
}

void hkl_binoculars_cube_dims(const HklBinocularsCube *self, int ndim, int *dims)
{
	int i;

	/* we need the c-order, so revert the dims */
	for(i=0; i<ndim; ++i)
		dims[ndim-i-1] = axis_size(&self->axes[i]);
}

HklBinocularsCube *hkl_binoculars_cube_new(int n_spaces, const HklBinocularsSpace *const *spaces,
					   int32_t n_pixels, const uint16_t **imgs)
{
	int i;
	int j;
	int n = 1;
	const HklBinocularsSpace *space0 = spaces[0];
	int ndim = space0->ndim;
	size_t offset0;
	HklBinocularsCube *self = malloc(sizeof(HklBinocularsCube));
	self->ndim = ndim;
	self->axes = calloc(ndim, sizeof(HklBinocularsAxis));

	/* compute the final cube dimensions and the index offset */
	for(i=0; i<ndim; ++i)
		self->axes[i] = space0->axes[i];

	for(i=1; i<n_spaces; ++i){
		for(j=0; j<ndim; ++j){
			hkl_binoculars_axis_merge(&self->axes[j],
						  &spaces[i]->axes[j]);
		}
	}
	for(i=0; i<ndim; ++i){
		n *= axis_size(&self->axes[i]);
	}
	self->_offset = compute_offset(ndim, self->axes);

	/* allocated the final cube */
	self->photons = calloc(n, sizeof(*self->photons));
	self->contributions = calloc(n, sizeof(*self->contributions));

	/* add all the spaces */
	for(i=0; i<n_spaces; ++i){
		add_space(self, spaces[i], n_pixels, imgs[i]);
	}

	hkl_binoculars_cube_fprintf(stdout, self);

	return self;
}


/* /\* Find the minimum and maximum of an integer array *\/ */
/* static void */
/* minmax(const npy_intp *data, npy_intp data_len, npy_intp *mn, npy_intp *mx) */
/* { */
/*     npy_intp min = *data; */
/*     npy_intp max = *data; */

/*     while (--data_len) { */
/*         const npy_intp val = *(++data); */
/*         if (val < min) { */
/*             min = val; */
/*         } */
/*         else if (val > max) { */
/*             max = val; */
/*         } */
/*     } */

/*     *mn = min; */
/*     *mx = max; */
/* } */

/* /\* */
/*  * arr_bincount is registered as bincount. */
/*  * */
/*  * bincount accepts one, two or three arguments. The first is an array of */
/*  * non-negative integers The second, if present, is an array of weights, */
/*  * which must be promotable to double. Call these arguments list and */
/*  * weight. Both must be one-dimensional with len(weight) == len(list). If */
/*  * weight is not present then bincount(list)[i] is the number of occurrences */
/*  * of i in list.  If weight is present then bincount(self,list, weight)[i] */
/*  * is the sum of all weight[j] where list [j] == i.  Self is not used. */
/*  * The third argument, if present, is a minimum length desired for the */
/*  * output array. */
/*  *\/ */
/* NPY_NO_EXPORT PyObject * */
/* arr_bincount(PyObject *NPY_UNUSED(self), PyObject *args, PyObject *kwds) */
/* { */
/*     PyObject *list = NULL, *weight = Py_None, *mlength = NULL; */
/*     PyArrayObject *lst = NULL, *ans = NULL, *wts = NULL; */
/*     npy_intp *numbers, *ians, len, mx, mn, ans_size; */
/*     npy_intp minlength = 0; */
/*     npy_intp i; */
/*     double *weights , *dans; */
/*     static char *kwlist[] = {"list", "weights", "minlength", NULL}; */

/*     if (!PyArg_ParseTupleAndKeywords(args, kwds, "O|OO:bincount", */
/*                 kwlist, &list, &weight, &mlength)) { */
/*             goto fail; */
/*     } */

/*     lst = (PyArrayObject *)PyArray_ContiguousFromAny(list, NPY_INTP, 1, 1); */
/*     if (lst == NULL) { */
/*         goto fail; */
/*     } */
/*     len = PyArray_SIZE(lst); */

/*     /\* */
/*      * This if/else if can be removed by changing the argspec to O|On above, */
/*      * once we retire the deprecation */
/*      *\/ */
/*     if (mlength == Py_None) { */
/*         /\* NumPy 1.14, 2017-06-01 *\/ */
/*         if (DEPRECATE("0 should be passed as minlength instead of None; " */
/*                       "this will error in future.") < 0) { */
/*             goto fail; */
/*         } */
/*     } */
/*     else if (mlength != NULL) { */
/*         minlength = PyArray_PyIntAsIntp(mlength); */
/*         if (error_converting(minlength)) { */
/*             goto fail; */
/*         } */
/*     } */

/*     if (minlength < 0) { */
/*         PyErr_SetString(PyExc_ValueError, */
/*                         "'minlength' must not be negative"); */
/*         goto fail; */
/*     } */

/*     /\* handle empty list *\/ */
/*     if (len == 0) { */
/*         ans = (PyArrayObject *)PyArray_ZEROS(1, &minlength, NPY_INTP, 0); */
/*         if (ans == NULL){ */
/*             goto fail; */
/*         } */
/*         Py_DECREF(lst); */
/*         return (PyObject *)ans; */
/*     } */

/*     numbers = (npy_intp *)PyArray_DATA(lst); */
/*     minmax(numbers, len, &mn, &mx); */
/*     if (mn < 0) { */
/*         PyErr_SetString(PyExc_ValueError, */
/*                 "'list' argument must have no negative elements"); */
/*         goto fail; */
/*     } */
/*     ans_size = mx + 1; */
/*     if (mlength != Py_None) { */
/*         if (ans_size < minlength) { */
/*             ans_size = minlength; */
/*         } */
/*     } */
/*     if (weight == Py_None) { */
/*         ans = (PyArrayObject *)PyArray_ZEROS(1, &ans_size, NPY_INTP, 0); */
/*         if (ans == NULL) { */
/*             goto fail; */
/*         } */
/*         ians = (npy_intp *)PyArray_DATA(ans); */
/*         NPY_BEGIN_ALLOW_THREADS; */
/*         for (i = 0; i < len; i++) */
/*             ians[numbers[i]] += 1; */
/*         NPY_END_ALLOW_THREADS; */
/*         Py_DECREF(lst); */
/*     } */
/*     else { */
/*         wts = (PyArrayObject *)PyArray_ContiguousFromAny( */
/*                                                 weight, NPY_DOUBLE, 1, 1); */
/*         if (wts == NULL) { */
/*             goto fail; */
/*         } */
/*         weights = (double *)PyArray_DATA(wts); */
/*         if (PyArray_SIZE(wts) != len) { */
/*             PyErr_SetString(PyExc_ValueError, */
/*                     "The weights and list don't have the same length."); */
/*             goto fail; */
/*         } */
/*         ans = (PyArrayObject *)PyArray_ZEROS(1, &ans_size, NPY_DOUBLE, 0); */
/*         if (ans == NULL) { */
/*             goto fail; */
/*         } */
/*         dans = (double *)PyArray_DATA(ans); */
/*         NPY_BEGIN_ALLOW_THREADS; */
/*         for (i = 0; i < len; i++) { */
/*             dans[numbers[i]] += weights[i]; */
/*         } */
/*         NPY_END_ALLOW_THREADS; */
/*         Py_DECREF(lst); */
/*         Py_DECREF(wts); */
/*     } */
/*     return (PyObject *)ans; */

/* fail: */
/*     Py_XDECREF(lst); */
/*     Py_XDECREF(wts); */
/*     Py_XDECREF(ans); */
/*     return NULL; */
/* } */


typedef struct _HklBinocularAxis HklBinoclarAxis;
struct _HklBinocularAxis
{
	int imin;
	int imax;
	double resolution;
	const char *label;
};

/* data Axis = Axis */
/*   Int    -- imin     lower bound */
/*   Int    -- imax     upper bound */
/*   Double -- res     step size / resolution */
/*   Text   -- label   human-readable identifier */

/* -- class Axis(object): */
/* --     """Represents a single dimension finite discrete grid centered at 0. */

/* --     Important attributes: */
/* --     min     lower bound */
/* --     max     upper bound */
/* --     res     step size / resolution */
/* --     label   human-readable identifier */

/* --     min, max and res are floats, but internally only integer operations are used. In particular */
/* --     min = imin * res, max = imax * res */
/* --     """ */

/* --     def __init__(self, min, max, res, label=None): */
/* --         self.res = float(res) */
/* --         if isinstance(min, int): */
/* --             self.imin = min */
/* --         else: */
/* --             self.imin = int(numpy.floor(min / self.res)) */
/* --         if isinstance(max, int): */
/* --             self.imax = max */
/* --         else: */
/* --             self.imax = int(numpy.ceil(max / self.res)) */
/* --         self.label = label */



/* getIndex :: Axis -> Array r1 DIM2 Double -> Array r2 DIM2 Int */
/* getIndex (Axis imin imax res _label) arr = res */
/*   where */
/*     res = map (v -> round (v / res) - imin) arr */

/*     -- def get_index(self, value): */
/*     --     if isinstance(value, numbers.Number): */
/*     --         intvalue = int(round(value / self.res)) */
/*     --         if self.imin <= intvalue <= self.imax: */
/*     --             return intvalue - self.imin */
/*     --         raise ValueError('cannot get index: value {0} not in range [{1}, {2}]'.format(value, self.min, self.max)) */
/*     --     elif isinstance(value, slice): */
/*     --         if value.step is not None: */
/*     --             raise IndexError('stride not supported') */
/*     --         if value.start is None: */
/*     --             start = None */
/*     --         else: */
/*     --             start = self.get_index(value.start) */
/*     --         if value.stop is None: */
/*     --             stop = None */
/*     --         else: */
/*     --             stop = self.get_index(value.stop) */
/*     --         if start is not None and stop is not None and start > stop: */
/*     --             start, stop = stop, start */
/*     --         return slice(start, stop) */
/*     --     else: */
/*     --         intvalue = numpy.around(value / self.res).astype(int) */
/*     --         if ((self.imin <= intvalue) & (intvalue <= self.imax)).all(): */
/*     --             return intvalue - self.imin */
/*     --         raise ValueError('cannot get indices, values from [{0}, {1}], axes range [{2}, {3}]'.format(value.min(), value.max(), self.min, self.max)) */


/* axisLength :: Axis -> Int */
/* axisLength (Axis imin imax _ _ ) = imax - imin + 1 */

/*     -- def __len__(self): */
/*     --     return self.imax - self.imin + 1 */



/* data Axes = Axes [Axis] */

/* -- class Axes(object): */
/* --     """Luxurious tuple of Axis objects.""" */

/* --     def __init__(self, axes): */
/* --         self.axes = tuple(axes) */
/* --         if len(self.axes) > 1 and any(axis.label is None for axis in self.axes): */
/* --             raise ValueError('axis label is required for multidimensional space') */


/* data Space = Space */
/*   Axes -- ^  axes - Axes instances describing range and stepsizes of each of the dimensions */
/*   (Array F DIM3 Double) -- ^ photons - n-dimension float array, total intensity per grid point */
/*   (Array F DIM3 Int) -- ^ contributions - n-dimensional numpy integer array, number of original datapoints (pixels) per grid point */
/*   Int -- ^ dimension n */

/*     -- def __init__(self, axes, config=None, metadata=None): */
/*     --     if not isinstance(axes, Axes): */
/*     --         self.axes = Axes(axes) */
/*     --     else: */
/*     --         self.axes = axes */

/*     --     self.config = config */
/*     --     self.metadata = metadata */

/*     --     self.photons = numpy.zeros([len(ax) for ax in self.axes], order='C') */
/*     --     self.contributions = numpy.zeros(self.photons.shape, order='C') */

/* processImage :: Space -> Array F DIM3 Double -> Array F DIM2 Word16 -> Array F DIM2 Bool -> Space */
/* processImage s _coordinates _intensity _weights = s */
/*         -- if len(coordinates) != len(self.axes): */
/*         --     raise ValueError('dimension mismatch between coordinates and axes') */

/*         -- intensity = numpy.nan_to_num(intensity).flatten()  # invalids can be handeled by setting weight to 0, this ensures the weights can do that */
/*         -- weights = weights.flatten() */

/*         -- indices = numpy.array(tuple(ax.get_index(coord) for (ax, coord) in zip(self.axes, coordinates))) */
/*         -- for i in range(0, len(self.axes)): */
/*         --     for j in range(i+1, len(self.axes)): */
/*         --         indices[i, :] *= len(self.axes[j]) */
/*         -- indices = indices.sum(axis=0).astype(int).flatten() */

/*         -- photons = numpy.bincount(indices, weights=intensity * weights) */
/*         -- contributions = numpy.bincount(indices, weights=weights) */

/*         -- self.photons.ravel()[:photons.size] += photons */
/*         -- self.contributions.ravel()[:contributions.size] += contributions */

/* fromImage :: [Double] -> [Text] -> [Array F DIM2 Double]  -> Array F DIM2 Word16 -> Array F DIM2 Double -> g -> Space */
/* fromImage rs ls cs i w = Space axes photons contributions n */
/*   where */
/*     axes = [mkAxis r l c | r l c <- zip3 rs ls cs] */

/*     indices' :: [Array r DIM2 Int] */
/*     indices' = [getIndex a c | a c <- zip axes cs] */

/*     indices :: Array r DIM2 Int */
/*     indices = toIndex rsh sh */

/*     -- from the indices' compute the new indices in the final array */
/*     indices = undefined */

/*     photons' = bincount indices (i * w) */

/*     contributions' = bincount indices w */

/*     n = length rs */


/*     -- @classmethod */
/*     -- def from_image(cls, resolutions, labels, coordinates, intensity, weights, limits=None): */
/*     --     """Create Space from image data. */

/*     --     resolutions   n-tuple of axis resolutions */
/*     --     labels          n-tuple of axis labels */
/*     --     coordinates   n-tuple of data coordinate arrays */
/*     --     intensity     data intensity array""" */

/*     --     if limits is not None: */
/*     --         invalid = numpy.zeros(intensity.shape).astype(numpy.bool) */
/*     --         for coord, sl in zip(coordinates, limits): */
/*     --             if sl.start is None and sl.stop is not None: */
/*     --                 invalid += coord > sl.stop */
/*     --             elif sl.start is not None and sl.stop is None: */
/*     --                 invalid += coord < sl.start */
/*     --             elif sl.start is not None and sl.stop is not None: */
/*     --                 invalid += numpy.bitwise_or(coord < sl.start, coord > sl.stop) */

/*     --         if numpy.all(invalid == True): */
/*     --             return EmptySpace() */
/*     --         coordinates = tuple(coord[~invalid] for coord in coordinates) */
/*     --         intensity = intensity[~invalid] */
/*     --         weights = weights[~invalid] */

/*     --     axes = tuple(Axis(coord.min(), coord.max(), res, label) for res, label, coord in zip(resolutions, labels, coordinates)) */
/*     --     newspace = cls(axes) */
/*     --     newspace.process_image(coordinates, intensity, weights) */
/*     --     return newspace */

#include <stdio.h>
#include <Python.h>
#include <string.h>

#include "hkl.h"

#define NPY_NO_DEPRECATED_API NPY_1_7_API_VERSION
#include <numpy/arrayobject.h>

void print_object(PyObject* object)
{
  PyObject_Print(object, stdout, 0);
}

PyObject* getObject(const char* string_name)
{
  PyObject *evalModule;
  PyObject *evalDict;
  PyObject *evalVal;
  evalModule = PyImport_AddModule( (char*)"__main__" );
  evalDict = PyModule_GetDict( evalModule );
  evalVal = PyDict_GetItemString( evalDict, string_name);
  if ( PyErr_Occurred() ) {
    PyErr_Print();PyErr_Clear(); return NULL;
  }
  else {
    return evalVal;
  }
}

PyObject* getObjectInModule(const char* objectName, const char* moduleName)
{
  PyObject *evalModule;
  PyObject *evalDict;
  PyObject *evalVal;
  evalModule = PyImport_AddModule( moduleName );
  evalDict = PyModule_GetDict( evalModule );
  evalVal = PyDict_GetItemString( evalDict, objectName );
  if ( PyErr_Occurred() ) {
    PyErr_Print();PyErr_Clear(); return evalVal;
  }
  else {
    return evalVal;
  }
}

char* checkError() {
  if ( PyErr_Occurred() ) {
    PyObject *exc_typ = NULL, *exc_val = NULL, *exc_tb = NULL;
    PyErr_Fetch( &exc_typ, &exc_val, &exc_tb);
    PyObject *exc_typ_string = PyObject_Str(exc_typ);
    PyObject *exc_val_string = PyObject_Str(exc_val);
    const char *exc_typ_tmp = PyUnicode_AsUTF8(exc_typ_string);
    const char *exc_val_tmp = PyUnicode_AsUTF8(exc_val_string);
    char *exc = malloc(strlen(exc_val_tmp) + strlen(exc_typ_tmp) + 1);
    strcpy(exc, exc_typ_tmp);
    strcat(exc, ",");
    strcat(exc, exc_val_tmp);
    Py_DECREF(exc_val_string);
    Py_DECREF(exc_typ_string);
    Py_XDECREF(exc_val);
    Py_XDECREF(exc_typ);
    Py_XDECREF(exc_tb);
    return exc;
  }
  else {
    return NULL;
  }
}

void execInModule(const char* payload, const char* moduleName) {
    PyObject *evalModule;
    PyObject *evalDict;
    PyObject *evalVal;
    evalModule = PyImport_AddModule(moduleName);
    PyObject *globals = PyModule_GetDict(evalModule);
    PyDict_SetItemString(globals, "__builtins__", PyEval_GetBuiltins());
    PyObject *locals = Py_BuildValue("{}");
    PyObject *result = PyRun_StringFlags(payload,
                                         Py_file_input,
                                         globals,
                                         globals,
                                         NULL);

    if ( PyErr_Occurred() ) {PyErr_Print();PyErr_Clear();}
    return;
}

void finalizer(PyObject* p) {
    Py_DecRef(p);
}

typedef void funcType(PyObject*);
typedef funcType * pFuncType;

pFuncType gimmeFunc(int dummy) {
    return &finalizer;
}


npy_intp get_PyArray_NBYTES(PyArrayObject *object)
{
	npy_intp res = 0;

	_import_array();
	if(PyArray_Check(object))
		res = PyArray_NBYTES(object);

	return res;
}

char* get_PyArray_BYTES(PyArrayObject *object)
{
	char *res = NULL;

	_import_array();
	if(PyArray_Check(object))
		res = PyArray_BYTES(object);

	return res;
}

void release_PyArray_BYTES(PyArrayObject *object)
{
	((PyArrayObject_fields *)object)->data = NULL;
}

int get_PyArray_NDIM(PyArrayObject *object)
{
	int res = 0;

	_import_array();
	if(PyArray_Check(object))
		res = PyArray_NDIM(object);

	return res;
}

npy_intp* get_PyArray_DIMS(PyArrayObject *object)
{
	npy_intp* res = NULL;

	_import_array();
	if(PyArray_Check(object))
		res = PyArray_DIMS(object);

	return res;
}

PyObject* matrix3x3_to_pyobject(double *data)
{
	int nd = 2;
	npy_intp dims[] = {3, 3};
	int typenum = NPY_DOUBLE;
	npy_intp nbytes = 0;

	_import_array();
	PyObject *obj = PyArray_SimpleNew(nd, dims, typenum);
	PyArrayObject *arr = (PyArrayObject *)obj;
	memcpy(PyArray_BYTES(arr), data, PyArray_NBYTES(arr));

	return obj;
}

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

/* the array is pre filled with the pixel coordinates */
void computeQ(const HklGeometry *geometry,
	      double *inout, int n_inout,
	      double k)
{
	double *q_x = &inout[0 * n_inout];
	double *q_y = &inout[1 * n_inout];
	double *q_z = &inout[2 * n_inout];
	HklDetector *detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	const HklQuaternion q = hkl_geometry_detector_rotation_get(geometry, detector);
	const HklVector ki = {{1, 0, 0}};

	for(int i=0;i<n_inout;++i){
		HklVector v = {{ q_x[i],
				 q_y[i],
				 q_z[i]}};

		hkl_vector_rotated_quaternion(&v, &q);
		hkl_vector_normalize(&v);
		hkl_vector_minus_vector(&v, &ki);
		hkl_vector_times_double(&v, k);
		q_x[i] = v.data[0];
		q_y[i] = v.data[1];
		q_z[i] = v.data[2];
	}
	hkl_detector_free(detector);
}

struct space_t
{
	int *indexes;
	int n_indexes;

	int *origin;
	int *dims;
	int ndim;
} space_t;


static inline struct space_t *space_new(int n_indexes, int ndim)
{
	struct space_t *self = malloc(sizeof(*self));

	self->indexes = calloc(n_indexes, sizeof(self->indexes));
	self->n_indexes = n_indexes;

	self->origin = malloc(sizeof(self->origin) * ndim);
	self->dims = malloc(sizeof(self->dims) * ndim);
	self->ndim = ndim;

	return self;
}

void space_free(struct space_t *self)
{
	free(self->dims);
	free(self->origin);
	free(self->indexes);
	free(self);
}

inline void min_max(double val, double *min, double *max, int *origin, int index)
{
	if(val < *min){
		*min = val;
		*origin = index;
	}else{
		if (val > *max){
			*max = val;
		}
	}
}

struct space_t *fromImage(const double *resolutions , int resolutions_n,
			  const double *coordinates, int *coordinates_dims, int coordinates_ndim,
			  const uint16_t *image, int *image_dims, int image_ndim)
{
	int i;
	int j;
	int len = 1;
	int n_pixels = image_dims[0] * image_dims[1];
	struct space_t *space = space_new(n_pixels, coordinates_ndim);

	for(i=0; i<coordinates_ndim; ++i){
		const double *coordinate = &coordinates[i * n_pixels];
		const double resolution = resolutions[i];
		double vmin = *coordinates;
		double vmax = *coordinates;
		int origin = *coordinates / resolution;

		for(j=0; j<n_pixels; ++j){
			double val = coordinate[j];
			int index = val / resolution;

			min_max(val, &vmin, &vmax, &origin, index);
			space->indexes[j] += index * len;
		}
		space->dims[i] = (vmax - vmin) / resolution + 1;
		space->origin[i] = origin;

		len *=  space->dims[i];
	}
	return space;
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
struct _BinocularAxis
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

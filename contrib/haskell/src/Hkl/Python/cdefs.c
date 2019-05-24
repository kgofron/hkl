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

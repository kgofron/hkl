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
    evalModule = PyImport_AddModule(moduleName);
    PyObject *globals = PyModule_GetDict(evalModule);
    PyDict_SetItemString(globals, "__builtins__", PyEval_GetBuiltins());
    Py_BuildValue("{}");
    PyRun_StringFlags(payload,
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

	_import_array();
	PyObject *obj = PyArray_SimpleNew(nd, dims, typenum);
	PyArrayObject *arr = (PyArrayObject *)obj;
	memcpy(PyArray_BYTES(arr), data, PyArray_NBYTES(arr));

	return obj;
}

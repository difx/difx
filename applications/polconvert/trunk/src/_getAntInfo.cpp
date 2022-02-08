/* getAntInfo -
             Copyright (C) 2017-2020  Ivan Marti-Vidal
             Nordic Node of EU ALMA Regional Center (Onsala, Sweden)
             Max-Planck-Institut fuer Radioastronomie (Bonn, Germany)
             Observatori Astronomic, Universitat de Valencia
  
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.
  
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
  
You should have received a copy of the GNU General Public License   
along with this program.  If not, see <http://www.gnu.org/licenses/>
  
*/


#include <Python.h>

// compiler warning that we use a deprecated NumPy API
// #define NPY_NO_DEPRECATED_API NPY_1_7_API_VERSION
// #define NO_IMPORT_ARRAY
#if PY_MAJOR_VERSION >= 3
#define NPY_NO_DEPRECATED_API 0x0
#endif
#include <numpy/npy_common.h>
#include <numpy/arrayobject.h>

#include <sys/types.h>
#include <iostream> 
#include <fstream>
#include <stdlib.h>  
#include <string.h>
#include "fitsio.h"
#include <math.h>


// cribbed from SWIG machinery
#if PY_MAJOR_VERSION >= 3
#define PyClass_Check(obj) PyObject_IsInstance(obj, (PyObject *)&PyType_Type)
#define PyInt_Check(x) PyLong_Check(x)
#define PyInt_AsLong(x) PyLong_AsLong(x)
#define PyInt_FromLong(x) PyLong_FromLong(x)
#define PyInt_FromSize_t(x) PyLong_FromSize_t(x)
#define PyString_Check(name) PyBytes_Check(name)
#define PyString_FromString(x) PyUnicode_FromString(x)
#define PyString_Format(fmt, args)  PyUnicode_Format(fmt, args)
//#define PyString_AsString(str) PyBytes_AsString(str)
#define PyString_Size(str) PyBytes_Size(str)
#define PyString_InternFromString(key) PyUnicode_InternFromString(key)
#define Py_TPFLAGS_HAVE_CLASS Py_TPFLAGS_BASETYPE
#define PyString_AS_STRING(x) PyUnicode_AS_STRING(x)
#define _PyLong_FromSsize_t(x) PyLong_FromSsize_t(x)
#endif

// and after some hacking
#if PY_MAJOR_VERSION >= 3
#define PyString_AsString(obj) PyUnicode_AsUTF8(obj)
#endif

/* Docstrings */
static char module_docstring[] =
    "Get antenna information (coordinates and mount types).";
static char getAntInfo_docstring[] =
    "Returns antenna information";
static char getCoords_docstring[] =
    "Returns antenna coordinates";
static char getMounts_docstring[] =
    "Returns the mount types";
//z
//static char getNames_docstring[] = 
//    "Returns the antenna names (codes)";

/* Available functions */
static PyObject *getAntInfo(PyObject *self, PyObject *args);
static PyObject *getCoords(PyObject *self, PyObject *args);
static PyObject *getMounts(PyObject *self, PyObject *args);
//z
//static PyObject *getNames(PyObject *self, PyObject *args);


/* Module specification */
static PyMethodDef module_methods[] = {
    {"getAntInfo", getAntInfo, METH_VARARGS, getAntInfo_docstring},
    {"getCoords", getCoords, METH_VARARGS, getCoords_docstring},
//z {"getMounts", getMounts, METH_VARARGS, getMounts_docstring},
    {"getMounts", getMounts, METH_VARARGS, getMounts_docstring}, //,
//  {"getNames", getNames, METH_VARARGS, getNames_docstring},
    {NULL, NULL, 0, NULL}   /* terminated by list of NULLs, apparently */
};

/* Initialize the module */
#if PY_MAJOR_VERSION >= 3
static struct PyModuleDef pc_module_def = {
    PyModuleDef_HEAD_INIT,
    "_getAntInfo",          /* m_name */
    module_docstring,       /* m_doc */
    -1,                     /* m_size */
    module_methods,         /* m_methods */
    NULL,NULL,NULL,NULL     /* m_reload, m_traverse, m_clear, m_free */
};
PyMODINIT_FUNC PyInit__getAntInfo(void)
{
    PyObject *m = PyModule_Create(&pc_module_def);
    import_array();
    return(m);
}
#else
PyMODINIT_FUNC init_getAntInfo(void)
{
    PyObject *m = Py_InitModule3("_getAntInfo", module_methods, module_docstring);import_array();
    if (m == NULL)
        return;
}
#endif

///////////////////////


   char message[512];


int Nants = 1;
double *Coords = new double[3];
int *Mounts = new int[1];



static PyObject *getAntInfo(PyObject *self, PyObject *args){

  PyObject *IDI;
  int status, ist, imt, iAux;
  long lAux, jj;

  status = 0;

  if (!PyArg_ParseTuple(args, "O",&IDI)){
     printf("Failed initialization of getAntInfo! Check inputs!\n"); 
     PyObject *ret = Py_BuildValue("i",-1);
     return ret;
  };

  std::string fname;

  fname = PyString_AsString(IDI);


// TYPE == 1 -> IDI

  fitsfile *ifile;
  char ARRAY[] = "ARRAY_GEOMETRY";
  char STABXYZ[] = "STABXYZ";
  char MNTSTA[] = "MNTSTA";


  fits_open_file(&ifile, fname.c_str(), READONLY, &status);

  if (status){
     printf("\n\nPROBLEM OPENING FILE!  ERR: %i\n\n",status);
     return Py_BuildValue("i",1);
  };
  fits_movnam_hdu(ifile, BINARY_TBL, ARRAY,1, &status);
  if (status){
     printf("\n\nPROBLEM ACCESSING ARRAY INFO!  ERR: %i\n\n",status); 
     return Py_BuildValue("i",1);
  };

  fits_get_num_rows(ifile, &lAux, &status);

  if(status){
    printf("\n\nPROBLEM ACCESSING ARRAY INFO!  ERR: %i\n\n",status);
    return Py_BuildValue("i",1);
  };

  Nants = (int) lAux;

// Allocate memory:
  delete Coords;
  delete Mounts;

  Coords = new double[3*Nants];
  Mounts = new int[Nants];
 
// Read coordinates:
  fits_get_colnum(ifile, CASEINSEN, STABXYZ, &ist, &status);
  if(status){
    printf("\n\nPROBLEM ACCESSING ARRAY INFO!  ERR: %i\n\n",status);
    return Py_BuildValue("i",1);
  };
  fits_get_colnum(ifile, CASEINSEN, MNTSTA, &imt, &status);
  if(status){
    printf("\n\nPROBLEM ACCESSING ARRAY INFO!  ERR: %i\n\n",status);
    return Py_BuildValue("i",1);
  };

  for (jj=0;jj<Nants;jj++){

  fits_read_col(ifile, TDOUBLE, ist, jj+1, 1, 3, NULL, &Coords[3*jj], &iAux, &status);
  if(status){
    printf("\n\nPROBLEM ACCESSING ARRAY COORDINATES DATA!  ERR: %i\n\n",status);
    return Py_BuildValue("i",2);
  };


  fits_read_col(ifile, TINT, imt, jj+1, 1, 1, NULL, &Mounts[jj], &iAux, &status);
  if(status){
    printf("\n\nPROBLEM ACCESSING ARRAY MOUNTS DATA!  ERR: %i\n\n",status);
    return Py_BuildValue("i",2);
  };


  printf("ANTENNA %2li: X=%-9.1f Y=%-9.1f Z=%-9.1f | MOUNT: %i\n",jj,Coords[3*jj],Coords[3*jj+1],Coords[3*jj+2],Mounts[jj]);


  };



  fits_close_file(ifile, &status);

  if(status){
//z printf("\n\nPROBLEM LOSING FITS-IDI!  ERR: %i\n\n",status);
    printf("\n\nPROBLEM CLOSING FITS-IDI!  ERR: %i\n\n",status);
    return Py_BuildValue("i",3);
  };


    return Py_BuildValue("i",0);

};







static PyObject *getCoords(PyObject *self, PyObject *args){

// Build numpy array:

PyObject *CoordArr;
//z long CD[2] = {Nants,3};
//z
//z GBC had already removed Py_INCREF.
//z Py_INCREF(CoordArr); -- original code
//z CoordArr = PyArray_SimpleNewFromData(2, &CD[0], NPY_DOUBLE, (void*) Coords);
//z Py_INCREF(CoordArr); -- needed only if we retain it and use it
int nd = 2;
npy_intp* CD = new npy_intp[nd];

CD[0] = Nants; CD[1] = 3; // = {Nants,3};

//Py_INCREF(CoordArr);
CoordArr = PyArray_SimpleNewFromData(nd, CD, NPY_DOUBLE, (void*) Coords);

return CoordArr;

};







static PyObject *getMounts(PyObject *self, PyObject *args){


// Build numpy array:
PyObject *MountArr;

//z long MD[1] = {Nants};
int nd = 1;
npy_intp* MD = new npy_intp[nd];
MD[0] = Nants;

//z GBC had already removed Py_INCREF
//z Py_INCREF(MountArr); -- original code
//z MountArr = PyArray_SimpleNewFromData(1, &MD[0], NPY_INT, (void*) Mounts);
//z Py_INCREF(MountArr); -- needed only if we retain it and use it

//z
//Py_INCREF(MountArr);
MountArr = PyArray_SimpleNewFromData(nd, MD, NPY_INT, (void*) Mounts);

return MountArr;

};




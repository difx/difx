/* getAntInfo -
             Copyright (C) 2017  Ivan Marti-Vidal
             Nordic Node of EU ALMA Regional Center (Onsala, Sweden)
             Max-Planck-Institut fuer Radioastronomie (Bonn, Germany)
  
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
#include <numpy/arrayobject.h>
#include <sys/types.h>
#include <iostream> 
#include <fstream>
#include <stdlib.h>  
#include <string.h>
#include "fitsio.h"
#include <math.h>




/* Docstrings */
static char module_docstring[] =
    "Get antenna information (coordinates and mount types).";
static char getAntInfo_docstring[] =
    "Returns antenna information";
static char getCoords_docstring[] =
    "Returns antenna coordinates";
static char getMounts_docstring[] =
    "Returns the mount types";

/* Available functions */
static PyObject *getAntInfo(PyObject *self, PyObject *args);
static PyObject *getCoords(PyObject *self, PyObject *args);
static PyObject *getMounts(PyObject *self, PyObject *args);

/* Module specification */
static PyMethodDef module_methods[] = {
    {"getAntInfo", getAntInfo, METH_VARARGS, getAntInfo_docstring},
    {"getCoords", getCoords, METH_VARARGS, getCoords_docstring},
    {"getMounts", getMounts, METH_VARARGS, getMounts_docstring}
};


/* Initialize the module */
PyMODINIT_FUNC init_getAntInfo(void)
{
    PyObject *m = Py_InitModule3("_getAntInfo", module_methods, module_docstring);import_array();
    if (m == NULL)
        return;

}

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
    printf("\n\nPROBLEM LOSING FITS-IDI!  ERR: %i\n\n",status);
    return Py_BuildValue("i",3);
  };


    return Py_BuildValue("i",0);

};


static PyObject *getCoords(PyObject *self, PyObject *args){

// Build numpy array:

PyObject *CoordArr;
long CD[2] = {Nants,3};

Py_INCREF(CoordArr);
CoordArr = PyArray_SimpleNewFromData(2, &CD[0], NPY_DOUBLE, (void*) Coords);

return CoordArr;

};


static PyObject *getMounts(PyObject *self, PyObject *args){

// Build numpy array:
PyObject *MountArr;

long MD[1] = {Nants};

Py_INCREF(MountArr);
MountArr = PyArray_SimpleNewFromData(1, &MD[0], NPY_INT, (void*) Mounts);

return MountArr;

};




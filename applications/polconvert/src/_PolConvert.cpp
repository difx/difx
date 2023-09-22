/*
# Copyright (c) Ivan Marti-Vidal 2015-2022.
#               EU ALMA Regional Center. Nordic node (Sweden).
#               University of Valencia (Spain)
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>,
# or write to the Free Software Foundation, Inc., 
# 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# a. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# b. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the
#    distribution.
# c. Neither the name of the author nor the names of contributors may 
#    be used to endorse or promote products derived from this software 
#    without specific prior written permission.
#
#
#THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
#LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
#THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
#
#

*/
 



//z old code 1, new code 0
//#define IVAN_OLD    1
 
//z straight up merge
//#define IVAN_MERGE  0




#include <Python.h>
// compiler warning that we use a deprecated NumPy API
// #define NPY_NO_DEPRECATED_API NPY_1_7_API_VERSION
#define NO_IMPORT_ARRAY
#if PY_MAJOR_VERSION >= 3
#define NPY_NO_DEPRECATED_API 0x0
#endif
#include <numpy/npy_common.h>
#include <numpy/arrayobject.h>

#include <stdio.h>  
#include <sys/types.h>
#include <new>
#include <ctime>
#include <sys/stat.h>
#include <string.h>
#include <dirent.h>
#include <iostream>
#include <fstream>
#include <complex.h>
#include "./DataIO.h"
#include "./DataIOFITS.h"
#include "./DataIOSWIN.h"
#include "./CalTable.h"
#include "./Weighter.h"
#include <sstream> 




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
    "This module provides an interface for PolConvert.";
static char PolConvert_docstring[] =
    "Converts mixed-polarization visibilities into pure circular-polarization basis";
static char setPCMode_docstring[] =
    "Sets the running mode to either ALMA (true; the default) or non-ALMA (false).";



/* Available functions */
static PyObject *PolConvert(PyObject *self, PyObject *args);
static PyObject *setPCMode(PyObject *self, PyObject *args);


/* Module specification */
static PyMethodDef module_methods[] = {
    {"PolConvert", PolConvert, METH_VARARGS, PolConvert_docstring},
    {"setPCMode", setPCMode, METH_VARARGS, setPCMode_docstring},
    {NULL, NULL, 0, NULL}   /* terminated by list of NULLs, apparently */
};



bool PCMode;

/* Initialize the module */
#if PY_MAJOR_VERSION >= 3
static struct PyModuleDef pc_module_def = {
    PyModuleDef_HEAD_INIT,
    "_PolConvert",          /* m_name */
    module_docstring,       /* m_doc */
    -1,                     /* m_size */
    module_methods,         /* m_methods */
    NULL,NULL,NULL,NULL     /* m_reload, m_traverse, m_clear, m_free */
};

PyMODINIT_FUNC PyInit__PolConvert(void)
{
    PCMode = true;          /* ALMA mode by default */
    PyObject *m = PyModule_Create(&pc_module_def);
    return(m);
}


#else


PyMODINIT_FUNC init_PolConvert(void)
{
    PCMode = true;          /* ALMA mode by default */
    PyObject *m = Py_InitModule3("_PolConvert", module_methods, module_docstring);
    if (m == NULL)
        return;

}
#endif



// Set the mode to non-ALMA, if the argument is 0 (false).
static PyObject *setPCMode(PyObject *self, PyObject *args){
  PyObject *ret;
  int whichMode;
  if (!PyArg_ParseTuple(args, "i",&whichMode)){      
      printf("FAILED PolConvert! Unable to parse arguments!\n");
      fflush(stdout);
      ret = Py_BuildValue("i",-1);
      return ret;
  };

  if(whichMode==0){
    PCMode=false;
    printf("PolConvert interface will change to non-ALMA.\n");
    fflush(stdout);
  } else {
    /* PCMode=true */
    printf("PolConvert interface remains in ALMA mode.\n");
    fflush(stdout);
  };
  ret = Py_BuildValue("i",0);
  return ret;


};






//////////////////////////////////
// MAIN FUNCTION: 
static PyObject *PolConvert(PyObject *self, PyObject *args)
{


  static const std::complex<float> oneOverSqrt2 = 0.7071067811;
  static const cplx32f Im = cplx32f(0.,1.);

  long i,j,k,auxI;
  int ii, ij, ik, il, im;
  int IFoffset;

  // initialization warnings...
  PyObject *ngain = nullptr, *nsum = nullptr, *gains = nullptr;
  PyObject *ikind = nullptr, *dterms = nullptr, *plotRange;
  PyObject *IDI, *antnum, *tempPy, *ret; 
  PyObject *allphants = nullptr, *nphtimes = nullptr;
  PyObject *phanttimes, *Range, *SWAP;
  PyObject *doIF, *metadata, *refAnts = nullptr, *ACorrPy, *logNameObj;
  PyObject *asdmTimes, *plIF, *isLinearObj, *XYaddObj, *ALMAstuff; 
  PyObject *antcoordObj, *soucoordObj, *antmountObj, *timeranges; 
  int nALMA, plAnt, nPhase = 0, doTest, doConj, doNorm;
  int calField, verbose, doParI;
  int currFile;
  double doSolve;
  bool isSWIN, doParang; 
  int AutoCorrMedianWindow;   

  printf("Parsing arguments\n");
 



  if (!PyArg_ParseTuple(args, "iOiOiiOOOOOidiiOOOOOOiOiiOO",
    &nALMA, &plIF, &plAnt, &doIF, &IFoffset, &AutoCorrMedianWindow,  // 0-5
    &SWAP, &IDI, &antnum, &plotRange,                                // 6-9
    &Range, &doTest, &doSolve, &doConj,                              // 10-13
    &doNorm, &XYaddObj, &metadata, &soucoordObj,                     // 14-17
    &antcoordObj, &antmountObj, &isLinearObj, &calField,             //18-21
    &ACorrPy, &doParI, &verbose, &logNameObj, &ALMAstuff)) {         //22-26
      printf("FAILED PolConvert! Unable to parse arguments!\n");
      fflush(stdout);
      ret = Py_BuildValue("i",-1);
      return ret;
  };



// Load ALMA-specific stuff
  if(PCMode){
    nPhase = PyList_Size(PyList_GetItem(ALMAstuff,5));
    ngain = PyList_GetItem(ALMAstuff,0);
    nsum = PyList_GetItem(ALMAstuff,1);
    ikind = PyList_GetItem(ALMAstuff,2);
    gains = PyList_GetItem(ALMAstuff,3);
    dterms = PyList_GetItem(ALMAstuff,4);
    allphants = PyList_GetItem(ALMAstuff,5);
    nphtimes = PyList_GetItem(ALMAstuff,6);
    phanttimes = PyList_GetItem(ALMAstuff,7);
    refAnts = PyList_GetItem(ALMAstuff,8);
    asdmTimes = PyList_GetItem(ALMAstuff,9);
    timeranges = PyList_GetItem(ALMAstuff,10);
    isLinearObj = PyList_GetItem(ALMAstuff,11);
  };

// cleanup: isLinearObj from arguments is overridden by ALMAstuff..
// XYaddObj is PrioriGains

  doParang = (doParI!=0);
  printf("Parsed arguments\n");

 
// default return value set now in case there is an error:
  ret = Py_BuildValue("i",1);





  if (verbose){
    std::cout<<"\n\n VERBOSE MODE ON\n\n";
    fflush(stdout);
  };

// OPEN LOG FILE:
  char message[2048];
  std::string logName = PyString_AsString(logNameObj);
  FILE *logFile = fopen(logName.c_str(),"a");

// Echo some calibration information:
  if(calField>=0){
    sprintf(message,"\nWill use field %i as calibrator/plot\n",calField);
    fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
  } else {
    sprintf(message,"\nWill use all fields in the timerange\n");
    fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
  };

// Sort out if SWIN files or FITS-IDI files are gonig to be converted:
// (if the length of the metadata list is zero, this is a FITS-IDI file)
  int SWINnIF = (int) PyList_Size(metadata)-1 ;
  isSWIN = SWINnIF > 0;
  sprintf(message,"\nisSwin is %s\n", isSWIN ? "True" : "False");
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

  int nSWINFiles = 1; // compiler warning
  std::string* SWINFiles, outputfits;

  if (isSWIN) {
    nSWINFiles = (int) PyList_Size(IDI) ;
    SWINFiles = new std::string[nSWINFiles];
    for (ii=0; ii<nSWINFiles; ii++){
      SWINFiles[ii] = PyString_AsString(PyList_GetItem(IDI,ii));
      sprintf(message,"file[%i] %s\n\n",ii,SWINFiles[ii].c_str());
      fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
    }; 
    sprintf(message,"\nCONVERTING %i SWIN (DiFX) FILES\n\n",nSWINFiles);
    fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
  } else {
    SWINFiles = new std::string[nSWINFiles];  // compiler warning
    outputfits = PyString_AsString(IDI);
    sprintf(message,"\nOUTPUT FITS-IDI FILE:  %s \n\n",outputfits.c_str());
    fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
  };



// Specific for ALMA:
  double *BadTimes = nullptr;
  int NBadTimes = 0;
  if(PCMode){
// Time ranges with unphased signal:
    BadTimes = (double *)PyArray_DATA(timeranges);
    NBadTimes = (int) PyArray_DIM(timeranges,0);
  };


// If SWIN, read the frequency channels from the metadata: 
  int *SWINnchan = nullptr; 
  double **SWINFreqs = nullptr; 
  double jd0 = 0.0; 

  if (isSWIN) {
    SWINFreqs = new double*[SWINnIF];
    SWINnchan = new int[SWINnIF];
    for (ii=0; ii<SWINnIF; ii++) {
      SWINFreqs[ii] =  (double *)PyArray_DATA(PyList_GetItem(metadata,ii));
      SWINnchan[ii] = ((int *) PyArray_DIMS(PyList_GetItem(metadata,ii)))[0] ;
    };
    jd0 = PyFloat_AsDouble(PyList_GetItem(metadata,SWINnIF));
  };

  int *ACorrs = (int *)PyArray_DATA(ACorrPy);

// Do we just test?
  if (doTest) {
    sprintf(message,"\nWill compute, but not update the output file(s)\n");
    fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
  };


// return value if there is an error:
  ret = Py_BuildValue("i",1);



// Times to analyse:
  double *plRange = (double *)PyArray_DATA(plotRange);
  double *doRange = (double *)PyArray_DATA(Range);




// READ PRIORI GAINS:

  int NPGain, NPIF;

  NPGain = PyList_Size(PyList_GetItem(XYaddObj,0));
  NPIF = PyList_Size(PyList_GetItem(PyList_GetItem(XYaddObj,0),0));
  sprintf(message,"Array sizes: NPGain = %i NPIF = %i\n", NPGain, NPIF);
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
  cplx32f ****PrioriGains = new cplx32f***[nSWINFiles]; 
  for(k=0;k<nSWINFiles;k++){
    PrioriGains[k] = new cplx32f **[NPGain];
    for (i=0;i<NPGain;i++){
      PrioriGains[k][i] = new cplx32f*[NPIF];
      for (j=0; j<NPIF; j++){
        PrioriGains[k][i][j] = (cplx32f *) PyArray_DATA(PyList_GetItem(PyList_GetItem(PyList_GetItem(XYaddObj,k),i),j));
      };
    };
  };



// Array and Observation Geometry:
  ArrayGeometry *Geometry = new ArrayGeometry;
  double *AntCoordArr = (double *)PyArray_DATA(antcoordObj);

  int *AntMountArr = (int *)PyArray_DATA(antmountObj);

  double *SouCoordArr = (double *)PyArray_DATA(PyList_GetItem(soucoordObj,1));
  double *SouCoordRA = (double *)PyArray_DATA(PyList_GetItem(soucoordObj,0));

    
  Geometry->NtotSou = (int) PyArray_DIM(PyList_GetItem(soucoordObj,1),0);
  Geometry->NtotAnt = (int) PyArray_DIM(antcoordObj,0);

  int Nbas = Geometry->NtotAnt*(Geometry->NtotAnt-1)/2;
  sprintf(message,"Array sizes: Nbas = %i NtotAnt = %i NtotSou = %i\n",
    Nbas, Geometry->NtotAnt, Geometry->NtotSou);
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

  Geometry->BaseLine[0] = new double[Nbas+Geometry->NtotAnt+1];
  Geometry->BaseLine[1] = new double[Nbas+Geometry->NtotAnt+1];
  Geometry->BaseLine[2] = new double[Nbas+Geometry->NtotAnt+1];
  Geometry->SinDec = new double[Geometry->NtotSou];
  Geometry->CosDec = new double[Geometry->NtotSou];
  Geometry->RA = new double[Geometry->NtotSou];
  Geometry->AntLon = new double[Geometry->NtotAnt];
  Geometry->Mount = new int[Geometry->NtotAnt];
  Geometry->Lat = new double[Geometry->NtotAnt];
  Geometry->BasNum = new int*[Geometry->NtotAnt];

  for (i=0; i<Geometry->NtotAnt;i++){
    Geometry->BasNum[i] = new int[Geometry->NtotAnt];
  };


  int Inum = 1, I3, J3;
  double RR;
  for (i=0; i<Geometry->NtotAnt;i++){
    I3 = 3*i;
    Geometry->AntLon[i] = atan2(AntCoordArr[I3+1],AntCoordArr[I3]);
    RR = sqrt(AntCoordArr[I3]*AntCoordArr[I3] + AntCoordArr[I3+1]*AntCoordArr[I3+1]) ;
    Geometry->Lat[i] = atan2(AntCoordArr[I3+2],RR);
    Geometry->Mount[i] = AntMountArr[i];
    for (j=i; j<Geometry->NtotAnt;j++){
      J3 = 3*j;
      Geometry->BasNum[i][j] = Inum;
      Geometry->BasNum[j][i] = -Inum;
      Geometry->BaseLine[0][Inum] = (AntCoordArr[I3] - AntCoordArr[J3]);
      Geometry->BaseLine[1][Inum] = (AntCoordArr[I3+1] - AntCoordArr[J3+1]);
      Geometry->BaseLine[2][Inum] = (AntCoordArr[I3+2] - AntCoordArr[J3+2]);
      Inum += 1;   
    };
  };

  for (i=0; i<Geometry->NtotSou;i++){
    Geometry->SinDec[i] = sin(SouCoordArr[i]);
    Geometry->CosDec[i] = cos(SouCoordArr[i]);
    Geometry->RA[i] = SouCoordRA[i];
  };




// Read info for linear-polarization antennas:
  double *time0, *time1;
  long nASDMEntries;
  int *ASDMant, *ALMARef;
  double **ASDMtimes;
  long *nASDMtimes;
  Weighter *ALMAWeight;





////////////////////////////////////////////////
/////////////////////////////////
/// SPECIFIC FOR ALMA:
  if(PCMode){
    time0 = (double *)PyArray_DATA(PyList_GetItem(asdmTimes,0));
    time1 = (double *)PyArray_DATA(PyList_GetItem(asdmTimes,1));
    nASDMEntries = ((long *) PyArray_DIMS(PyList_GetItem(asdmTimes,0)))[0];  

///////////
// Useful to determine which ALMA antennas are phased up at each time:

  sprintf(message,"Array sizes: nPhase = %i nALMA = %i\n", nPhase, nALMA);
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

  ASDMant = new int[nPhase];
  ASDMtimes = new double*[nPhase];
  nASDMtimes = new long[nPhase];
  // refAnts may be used uninitialized
  ALMARef = (int *)PyArray_DATA(refAnts);
  for (ii=0; ii<nPhase; ii++){
    nASDMtimes[ii] = (long)PyInt_AsLong(PyList_GetItem(nphtimes,ii));
    ASDMant[ii] = (int)PyInt_AsLong(PyList_GetItem(allphants,ii));
    ASDMtimes[ii] = (double *)PyArray_DATA(PyList_GetItem(phanttimes,ii));
  };
  // BadTimes not initialized
  ALMAWeight = new Weighter(nPhase,nASDMtimes,nASDMEntries,ASDMant,ASDMtimes,ALMARef,time0,time1,BadTimes, NBadTimes, logFile);

  } else {  // If ALMA is not used, set weights to dummy:
  ALMAWeight = new Weighter(logFile);
  };

///////////
//////////////////////////////////////////////




// ALLOCATE MEMORY
  int **kind = new int*[nALMA];
  bool **isLinear = new bool*[nALMA];
  int *ngainTabs = new int[nALMA];
  int *almanums = new int[nALMA];
  int *nsumArr = new int[nALMA];
  long ***ntimeArr = new long**[nALMA];
  long **nchanArr = new long*[nALMA];

  double ***freqsArr = new double**[nALMA];
  double ****timesArr = new double***[nALMA];
  double ****gainsArrR1 = new double***[nALMA];
  double ****gainsArrI1 = new double***[nALMA];
  double ****gainsArrR2 = new double***[nALMA];
  double ****gainsArrI2 = new double***[nALMA];
  bool ****gainflag = new bool***[nALMA];

  long *nchanDt = new long[nALMA];
  long **ndttimeArr = new long*[nALMA];
  double **dtfreqsArr = new double*[nALMA];
  double ***dttimesArr = new double**[nALMA];
  double ***dtermsArrR1 = new double**[nALMA];
  double ***dtermsArrI1 = new double**[nALMA];
  double ***dtermsArrR2 = new double**[nALMA];
  double ***dtermsArrI2 = new double**[nALMA];
  bool ***dtflag = new bool**[nALMA];
  bool *XYSWAP = new bool[nALMA];

  for (i=0;i<nALMA;i++){
    isLinear[i] = (bool *)PyArray_DATA(PyList_GetItem(isLinearObj,i));
    almanums[i] = (int)PyInt_AsLong(PyList_GetItem(antnum,i));

    XYSWAP[i] = (bool)PyInt_AsLong(PyList_GetItem(SWAP,i));
    if(XYSWAP[i]){
       sprintf(message,"\nWill swap X/Y channels for antenna #%i.\n",
           almanums[i]);
       fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
    };


// Arrays with gain information. If ALMA is not used, we set them to
// summy values:
    if(PCMode){
      ngainTabs[i] = (int)PyInt_AsLong(PyList_GetItem(ngain,i));
      kind[i] = new int[ngainTabs[i]];
      nsumArr[i] = (int)PyInt_AsLong(PyList_GetItem(nsum,i));
      ntimeArr[i] = new long*[ngainTabs[i]];
      nchanArr[i] = new long[ngainTabs[i]];
      timesArr[i] = new double**[ngainTabs[i]];
      gainsArrR1[i] = new double**[ngainTabs[i]];
      gainsArrI1[i] = new double**[ngainTabs[i]];
      gainsArrR2[i] = new double**[ngainTabs[i]];
      gainsArrI2[i] = new double**[ngainTabs[i]];
      gainflag[i] = new bool**[ngainTabs[i]];
      freqsArr[i] = new double*[ngainTabs[i]];
    } else {
      ngainTabs[i] = 1;
      nsumArr[i] = 1;
    };
  };
//////////////////////////////////////////////


//////////////////////////////////////////////
// READ CALIBRATION TABLES (ALMA CASE):

if(PCMode){

  for (i=0;i<nALMA;i++){
    nchanDt[i] = PyArray_DIM(PyList_GetItem(PyList_GetItem(dterms,i),0),0);
    dtfreqsArr[i] = (double *)PyArray_DATA(PyList_GetItem(PyList_GetItem(dterms,i),0));
    dttimesArr[i] = new double*[nsumArr[i]];
    ndttimeArr[i] = new long[nsumArr[i]];
    dtflag[i] = new bool*[nsumArr[i]];
    dtermsArrR1[i] = new double*[nsumArr[i]];
    dtermsArrI1[i] = new double*[nsumArr[i]];
    dtermsArrR2[i] = new double*[nsumArr[i]];
    dtermsArrI2[i] = new double*[nsumArr[i]];
    for (j=0;j<nsumArr[i];j++){
      dttimesArr[i][j] = new double[1];
      dttimesArr[i][j][0] = 0.0;
      ndttimeArr[i][j] = 1;
      dtermsArrR1[i][j] = (double *)PyArray_DATA(
              PyList_GetItem(PyList_GetItem(PyList_GetItem(dterms,i),j+1),0));
      dtermsArrI1[i][j] = (double *)PyArray_DATA(
              PyList_GetItem(PyList_GetItem(PyList_GetItem(dterms,i),j+1),1));
      dtermsArrR2[i][j] = (double *)PyArray_DATA(
              PyList_GetItem(PyList_GetItem(PyList_GetItem(dterms,i),j+1),2));
      dtermsArrI2[i][j] = (double *)PyArray_DATA(
              PyList_GetItem(PyList_GetItem(PyList_GetItem(dterms,i),j+1),3));
      dtflag[i][j] = (bool *)PyArray_DATA(
              PyList_GetItem(PyList_GetItem(PyList_GetItem(dterms,i),j+1),4));
    };

    for (j=0; j<ngainTabs[i]; j++){
      tempPy = PyList_GetItem(PyList_GetItem(gains,i),j);
      kind[i][j] = (int)PyInt_AsLong(PyList_GetItem(PyList_GetItem(ikind,i),j));
      nchanArr[i][j] = PyArray_DIM(PyList_GetItem(tempPy,0),0);
      freqsArr[i][j] = (double *)PyArray_DATA(PyList_GetItem(tempPy,0));
      ntimeArr[i][j] = new long[nsumArr[i]];
      timesArr[i][j] = new double*[nsumArr[i]];
      gainsArrR1[i][j] = new double*[nsumArr[i]];
      gainsArrI1[i][j] = new double*[nsumArr[i]];
      gainsArrR2[i][j] = new double*[nsumArr[i]];
      gainsArrI2[i][j] = new double*[nsumArr[i]];
      gainflag[i][j] = new bool*[nsumArr[i]];

      for (k=0; k<nsumArr[i]; k++){
        ntimeArr[i][j][k] = PyArray_DIM(PyList_GetItem(PyList_GetItem(tempPy,k+1),0),0);
        timesArr[i][j][k] = (double *)PyArray_DATA(
              PyList_GetItem(PyList_GetItem(tempPy,k+1),0));
        gainsArrR1[i][j][k] = (double *)PyArray_DATA(
              PyList_GetItem(PyList_GetItem(tempPy,k+1),1));
        gainsArrI1[i][j][k] = (double *)PyArray_DATA(
              PyList_GetItem(PyList_GetItem(tempPy,k+1),2));
        gainsArrR2[i][j][k] = (double *)PyArray_DATA(
              PyList_GetItem(PyList_GetItem(tempPy,k+1),3));
        gainsArrI2[i][j][k] = (double *)PyArray_DATA(
              PyList_GetItem(PyList_GetItem(tempPy,k+1),4));
        gainflag[i][j][k] = (bool *)PyArray_DATA(
              PyList_GetItem(PyList_GetItem(tempPy,k+1),5));
      };
    };
  };

};




// ALLOCATE MEMORY FOR CALIBRATION INSTANCES:
  CalTable ***allgains = new CalTable**[nALMA];
  CalTable **alldterms = new CalTable*[nALMA];

// CREATE CALIBRATION INSTANCES:  

 
  for (i=0; i<nALMA; i++){
    allgains[i] = new CalTable*[ngainTabs[i]];

 // ALMA CASE: ACTUAL CALIBRATION INFORMATION OF THE PHASED ARRAY:        
    if(PCMode){
       alldterms[i] = new CalTable(2,dtermsArrR1[i],dtermsArrI1[i],
           dtermsArrR2[i],dtermsArrI2[i],dtfreqsArr[i],dttimesArr[i],
           nsumArr[i],ndttimeArr[i], nchanDt[i],dtflag[i],true,logFile,
           false); // verbose);

       for (j=0; j<ngainTabs[i];j++){
/*
         printf("CALLING GAIN %i WITH %i\n",j,kind[i][j]);fflush(stdout); 
         printf("GAINS R1: %.3e\n",gainsArrR1[i][j][0][0]);
         printf("GAINS I1: %.3e\n",gainsArrI1[i][j][0][0]);
         printf("GAINS R2: %.3e\n",gainsArrR2[i][j][0][0]);
         printf("GAINS I2: %.3e\n",gainsArrI2[i][j][0][0]);
         printf("FREQS: %.3e\n",freqsArr[i][j][0]);
         printf("TIMES: %.3e\n",timesArr[i][j][0][0]);
         printf("NA: %i\n",nsumArr[i]);
         printf("NTI: %i\n",ntimeArr[i][j][0]);
         printf("FG: %i\n",gainflag[i][j][0][0]);
         printf("IL: %i\n",isLinear[i][j]);
*/
         allgains[i][j] = new CalTable(kind[i][j],gainsArrR1[i][j],
           gainsArrI1[i][j],gainsArrR2[i][j],gainsArrI2[i][j],freqsArr[i][j],
           timesArr[i][j],nsumArr[i],ntimeArr[i][j], nchanArr[i][j],
           gainflag[i][j],isLinear[i][j],logFile,
           false); // verbose);
       };

     // NON-ALMA CASE: DUMMY GAINS.
    } else {
      alldterms[i] = new CalTable(2,logFile);
      allgains[i][0] = new CalTable(0,logFile);
    };

  };


  fflush(logFile);




/////////////////////////////////
// READ VLBI DATA:


// How many IFs do we convert?
  int nIFconv = (int) PyList_Size(doIF) ;
  bool doAll = false;

// How many IFs do we plot?
  int nIFplot = (int) PyList_Size(plIF) ;
  int IFs2Plot[nIFplot];
  for (ii=0; ii<nIFplot; ii++) {
    IFs2Plot[ii] = (int)PyInt_AsLong(PyList_GetItem(plIF,ii)) - 1;
  };


// If no IF list was given, convert all of them:
//  if (nIFconv==0){nIFconv = DifXData->getNfreqs(); doAll=true;};

// Which IFs do we convert?
  int IFs2Conv[nIFconv];
  for (ii=0; ii<nIFconv; ii++) {
    if (doAll){IFs2Conv[ii]=ii;} else {
      IFs2Conv[ii] = (int)PyInt_AsLong(PyList_GetItem(doIF,ii)) - 1;
    };
  }; 


  DataIO *DifXData ;  // Polymorphism to SWIN or FITS-IDI.
  bool OverWrite= true; // Always force overwrite (for now)
  bool iDoSolve = doSolve >= 0.0;




  if (isSWIN) {
    sprintf(message,"\n\n Opening and preparing SWIN files.\n");
    fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
    DifXData = new DataIOSWIN(nSWINFiles, SWINFiles, nALMA, 
           almanums, doRange, SWINnIF, SWINnchan, nIFconv, IFs2Conv, IFoffset, AutoCorrMedianWindow, ACorrs, SWINFreqs, 
           OverWrite, doTest, iDoSolve, calField, jd0, Geometry, doParang, logFile);
  } else {
    sprintf(message,"\n\n Opening FITS-IDI file and reading header.\n");
    fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
    DifXData = new DataIOFITS(outputfits, nALMA, almanums, 
          doRange, OverWrite, doConj, iDoSolve, calField, Geometry, doParang, logFile);
  };

  if(!DifXData->succeed()){
     sprintf(message,"\nERROR WITH DATA FILE(S)!\n");
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
     ret = Py_BuildValue("i",-1);
     return ret;
  };


  sprintf(message,"\n\nFirst observing Julian day: %11.2f\n",DifXData->getDay0());
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);



  int nnu = DifXData->getNfreqs();
  int ALMARefAnt = -1; // If no calAPP is used, do not look for any extra X-Y phase offset.

  int nchans[nnu]; 
  int maxnchan=0;
  for (ii=0; ii<nnu; ii++) {
    nchans[ii] = DifXData->getNchan(ii); 
    if (nchans[ii]>maxnchan) {maxnchan=nchans[ii];};
  };
  sprintf(message,"\n The VLBI IFs have a maximum of %i channels\n",maxnchan);
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);


/////////////////////////////////




/////////////////////////////////
// ALLOCATE MEMORY FOR CALIBRATION MATRICES:

  double DifXFreqs[maxnchan];



// Gains:
  std::complex<float> ***AnG[nALMA], ***AnDt[nALMA];
  bool *Weight[nALMA];

// K matrix (beware: 3rd dimension of the matrix elements is now real/imag; not X/Y!):
  std::complex<float> **K[nALMA][2][2];

// part of K matrix that is unlikely to change much with time (i.e., BP, D-terms, X/Y delay):
  std::complex<float> **Kfrozen[nALMA][2][2]; 

// Ktotal will be the calibration+conversion matrix (i.e., just multiply V by it, to get final V):
  std::complex<float> *Ktotal[nALMA][2][2];



  for (ij=0; ij<nALMA; ij++) {
    auxI = nsumArr[ij];

    AnG[ij] =  new std::complex<float> **[auxI];
    AnDt[ij] =  new std::complex<float> **[auxI];
    Weight[ij] =  new bool [auxI];

    for (ii=0; ii<auxI; ii++) {
      AnG[ij][ii] =  new std::complex<float> *[2];
      AnG[ij][ii][0] = new std::complex<float>[maxnchan];
      AnG[ij][ii][1] = new std::complex<float>[maxnchan];
      AnDt[ij][ii] =  new std::complex<float> *[2];
      AnDt[ij][ii][0] = new std::complex<float>[maxnchan];
      AnDt[ij][ii][1] = new std::complex<float>[maxnchan];
    };

// K matrix:
    for (ii=0; ii<2; ii++) {
      for (ik=0; ik<2; ik++) {
        K[ij][ii][ik] =  new std::complex<float> *[auxI];
        Kfrozen[ij][ii][ik] =  new std::complex<float> *[auxI];
        Ktotal[ij][ii][ik] = new std::complex<float>[maxnchan];
  
        for (il=0; il<auxI; il++) {
          K[ij][ii][ik][il] =  new std::complex<float>[maxnchan];
          Kfrozen[ij][ii][ik][il] =  new std::complex<float>[maxnchan];
       };
      };
    };

  };




// Some extra auxiliary variables:

  double currT, lastTFailed;
  int currAnt, currAntIdx, currNant, otherAnt,currF; 
  bool notinlist, gchanged=true, dtchanged=true, toconj;

  lastTFailed = 0.0;

  long countNvis;

  std::complex<float> AD, BC, auxD;
  auxD = 0.0;
  float NormFac[2];
  float AntTab;
  std::complex<float> DetInv;
  std::complex<float> Kinv[2][2];
  std::complex<float> H[2][2]; 
  std::complex<float> HSw[2][2]; 

  H[0][0] = 1.; H[0][1] = Im;
  H[1][0] = 1.; H[1][1] = -Im;

  HSw[0][0] = Im; HSw[0][1] = 1.;
  HSw[1][0] = -Im; HSw[1][1] = 1.;


  std::complex<float> gainXY[2]; 
  std::complex<float> gainRatio[maxnchan];
  std::complex<float> absGainRatio;

  bool allflagged, auxB1, auxB2, Phased ;

  sprintf(message,"\n Will modify %li visibilities (lin-lin counted twice).\n\n",
       DifXData->getMixedNvis());
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);



// TIME RANGE FOR PLOTTING:
  plRange[0] = ((plRange[0]+DifXData->getDay0())-2400000.5)*86400.;
  plRange[1] = ((plRange[1]+DifXData->getDay0())-2400000.5)*86400.;

// TIME RANGE FOR CORRECTING:
  doRange[0] = ((doRange[0]+DifXData->getDay0())-2400000.5)*86400.;
  doRange[1] = ((doRange[1]+DifXData->getDay0())-2400000.5)*86400.;

/////////////////////////////////




  FILE *plotFile[nIFplot];
  FILE *gainsFile = (FILE*)0;

// Prepare plotting or solving files:
//  In the ALMA case, IFs2Plot holds the subset of IFs to plot;
//  in the non-ALMA case, we need to create all of them for solving.
  int noI = -1;
  if (PCMode) {
   for (ii=0; ii<nIFplot; ii++) {    // ALMA plot case
    sprintf(message,"POLCONVERT.FRINGE/POLCONVERT.FRINGE_IF%i",IFs2Plot[ii]+1);
    printf("Writing %s\n", message);
    plotFile[ii] = fopen(message,"wb");
    if (IFs2Plot[ii]>=0 && IFs2Plot[ii]<nnu){
       fwrite(&nchans[IFs2Plot[ii]],sizeof(int),1,plotFile[ii]);
    } else {
       fwrite(&noI,sizeof(int),1,plotFile[ii]);
    };
   };
  } else {
   for (ii=0; ii<nIFconv; ii++) {           // non-ALMA solve case
    sprintf(message,"POLCONVERT.FRINGE/POLCONVERT.FRINGE_IF%i",IFs2Conv[ii]+1);
    printf("Writing %s\n", message);
    plotFile[ii] = fopen(message,"wb");
    if (IFs2Conv[ii]>=0 && IFs2Conv[ii]<nnu){
       fwrite(&nchans[IFs2Conv[ii]],sizeof(int),1,plotFile[ii]);
    } else {
       fwrite(&noI,sizeof(int),1,plotFile[ii]);
    };
   };
  };

  if(doNorm){
    printf("CREATING GAIN FILE.\n"); 
    sprintf(message,"POLCONVERT.GAINS opened for writing");
    fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
    gainsFile = fopen("POLCONVERT.GAINS","wb");
  };

  sprintf(message,"\n POLCONVERTING THE DATA.\n\n");
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);


/////////////////////////////////////
// APPLY AUTO-CORRELATIONS CORRECTION:

  for (currAntIdx=0; currAntIdx<nALMA; currAntIdx++) {

    for (k=0;k<nSWINFiles;k++){
      for (im=0; im<nIFconv; im++) {
        ii = IFs2Conv[im];
        for (ij=0; ij<nchans[ii]; ij++){
          PrioriGains[k][currAntIdx][im][ij] *=
            DifXData->getAmpRatio(currAntIdx, ii, ij);
//          DifXData->getAmpRatio(currAntIdx, im, ij);
        };
      };
    };

  };







//////////////////////////////////////////////////////////
///////////////////////////////////
// MAIN LOOP FOR CORRECTION (LOOP OVER IFs):

////////////////////////////////////
// Start of iteration over IFs
////////////////////////////////////

  for (im=0; im<nIFconv; im++) {

    ii = IFs2Conv[im];
    int IFplot = -1;    // flags the no-plot case of ALMA mode
    char pltmsg[20];

    for (ij=0; ij<nIFplot; ij++){
      if (IFs2Plot[ij]==ii){IFplot=ij; break;};
    };
    if (PCMode && IFplot < 0) { sprintf(pltmsg, "not plotted"); }
    else if (PCMode)          { sprintf(pltmsg, "fringe plot"); }
    else                      { sprintf(pltmsg, "for solving"); };

    sprintf(message,"\nDoing subband %i of %i (%s)\n",ii+1,nnu,pltmsg);
    fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
    fflush(logFile);
    //useful in development, not in production:
    //printf("\rDoing subband %i of %i   ",ii+1,nnu);
    //fflush(stdout);

// Only proceed if IF is OK:
    if(!DifXData->setCurrentIF(ii)){
      sprintf(message,
          "WARNING! DATA DO NOT HAVE SUCH AN IF!! WILL SKIP CONVERSION\n");  
      fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);} 
    else {    // IF is OK: the check of IF.


// Get the frequencies of the current IF:
      DifXData->getFrequencies(DifXFreqs);

// Set the VLBI <-> ALMA frequency mapping for the interpolation:
      for (ij=0; ij<nALMA; ij++) {
        alldterms[ij]->setMapping(nchans[ii],DifXFreqs);
        for (ik=0; ik<ngainTabs[ij]; ik++){
          allgains[ij][ik]->setMapping(nchans[ii],DifXFreqs);
        };
      };


// Get the next visibility to correct:
      countNvis = 0;

     // next mixed-vis indent level
      while(DifXData->getNextMixedVis(
         currT,currAnt, otherAnt, toconj, currF)){

         countNvis += 1;

         currFile = DifXData->getFileNumber();



// Check if there was an error in reading:
         if (!DifXData->succeed()){
           fclose(gainsFile);
           DifXData->finish();
           return ret;
         };

// Do we have to correct this visibility?

         //indent level for time range
         if(currT>=doRange[0] && currT<=doRange[1]) {  // vis in time range?
           //indent level within time range

// Sanity check (if antenna is in the list of linear-pol antennas):
           notinlist = true;
           for (ij=0; ij<nALMA; ij++) {
             if (currAnt == almanums[ij]){
               currAntIdx = ij; notinlist=false; break;
             };
           };

           if (notinlist){
             sprintf(message,
               "ERROR: Found linear-pol data for antenna number %i.\n",currAnt);
             fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
             sprintf(message,
               "This antenna is not in the list of linear-pol antennas!\n");
             fprintf(logFile,"%s",message);  std::cout<<message; fflush(logFile);
             DifXData->finish(); 
             return ret;
           };





//////////////////////////////////////////////////////
// Set the interpolation time and compute gains:
           currNant = nsumArr[currAntIdx] ;

// Find the ALMA antennas involved in the phasing:

           if(verbose){printf(" Doing vis %li  -  %.3f  -  %i\n",
             countNvis, currT, currNant);fflush(stdout);
           };
           allflagged = true;

           if (currNant>1 && PCMode){
             Phased = ALMAWeight->isPhased(currT);
             if (Phased){
               for (ij=0; ij<currNant; ij++) {
                 Weight[currAntIdx][ij] = ALMAWeight->getWeight(ij,currT);
                 if (Weight[currAntIdx][ij]){allflagged = false;};
               };
             };
           } else {
             Phased=true; Weight[currAntIdx][0] = true; allflagged = false;
           };

// get ALMA refant used in the Phasing (to correct for X-Y phase offset):
           ALMARefAnt = ALMAWeight->getRefAnt(currT);
    
 
           for (ij=0; ij<nchans[ii]; ij++){
             gainRatio[ij] = PrioriGains[currFile][currAntIdx][im][ij]; 
           };

           if(PCMode && allflagged && currT != lastTFailed){
             double dayFrac = (currT/86400. - DifXData->getDay0()+2400000.5);
             int day = (int) dayFrac ;
             int hour = (int) (dayFrac*24.);
             int min = (int) ((dayFrac*24. - ((double) hour))*60.);
             int sec = (int) ((dayFrac*24. - ((double) hour) - ((double) min)/60.)*3600.);
             if (Phased){
               sprintf(message,
                  "WARNING: NO VALID ALMA ANTENNAS ON %i-%i:%i:%i ?!?!\n WILL CONVERT ON THIS TIME *WITHOUT* CALIBRATION\n",
                  day,hour,min,sec);
             } else {
               sprintf(message,
                  "WARNING: ARRAY WAS UNPHASED AT TIME %i-%i:%i:%i ?!?!\n WILL SET THE WEIGHTS TO ZERO\n",
                  day,hour,min,sec);
             };
             fprintf(logFile,"%s",message); fflush(logFile);
             lastTFailed = currT ;
           };


           //indent level within time range
           if (PCMode && !allflagged){

             if(verbose){printf(" Computing gains\n");fflush(stdout);};

/////////
// GAIN:
    // FIRST GAIN IN NORMAL MODE, 0:

             gchanged = allgains[currAntIdx][0]->setInterpolationTime(currT);
             for (ij=0; ij<currNant; ij++) {
               if (Weight[currAntIdx][ij]) {
               allgains[currAntIdx][0]->applyInterpolation(ij,0,AnG[currAntIdx][ij]); };
             };
             if(verbose){printf(" Normal Mode 0\n");fflush(stdout);};

// FURTHER GAIN, IN PRODUCT MODE, 2:
             for (ik=1; ik<ngainTabs[currAntIdx]; ik++) {
               auxB1 = allgains[currAntIdx][ik]->setInterpolationTime(currT) ;
               gchanged = gchanged || auxB1;
               for (ij=0; ij<currNant; ij++) {
                 if (Weight[currAntIdx][ij]) {
                   allgains[currAntIdx][ik]->applyInterpolation(
                       ij,2,AnG[currAntIdx][ij]);  
                 };
               };
             };
             if(verbose){printf(" Product Mode 2\n");fflush(stdout);};

// CROSS-PHASE GAIN AT THE ALMA REFERENCE ANTENNA:
             cplx32f AuxRatio; 
             for (ik=0; ik<ngainTabs[currAntIdx]; ik++) {
               if (ALMARefAnt>=0 && !(allgains[currAntIdx][ik]->isBandpass())){
                 if (allgains[currAntIdx][ik]->getInterpolation(
                     ALMARefAnt,0,gainXY)){
                       for (ij=0; ij<nchans[ii]; ij++){
                          if (std::abs(gainXY[1])>0.0 && std::abs(gainXY[0])>0.0){
                             AuxRatio = gainXY[0]/gainXY[1];
                             gainRatio[ij] *= AuxRatio/std::abs(AuxRatio); };
                       };
                 } else {
                    sprintf(message,
                        "ERROR with ALMA Ref. Ant. in gain table!\n");
                    fprintf(logFile,"%s",message); fflush(logFile);
                    DifXData->finish(); 
                    return ret;
                 };
               }; 
             };
             if(verbose){printf(" Cross Phases Mode\n");fflush(stdout);};


/////////
// DTERM:
             dtchanged = alldterms[currAntIdx]->setInterpolationTime(currT);
             for (ij=0; ij<currNant; ij++) {
               if (Weight[currAntIdx][ij]) {
                 alldterms[currAntIdx]->applyInterpolation(
                     ij,0,AnDt[currAntIdx][ij]);  
               };
             };
             if(verbose){printf(" D-terms Mode\n");fflush(stdout);};


//////////////////////////////////

           };   // Comes from if(!allflagged)



// FORCE RE-COMPUTATION (TO SET UNITY MATRIX) IF ALL ANTENNAS ARE FLAGGED
           if (allflagged || !PCMode){
             gchanged=false; dtchanged=false;
             for (j=0; j<nchans[ii]; j++) {
               if(XYSWAP[currAntIdx]){
                 Ktotal[currAntIdx][0][0][j] = HSw[0][0]*oneOverSqrt2; //*gainRatio[j];
                 Ktotal[currAntIdx][0][1][j] = HSw[0][1]*oneOverSqrt2/gainRatio[j];
                 Ktotal[currAntIdx][1][0][j] = HSw[1][0]*oneOverSqrt2; //*gainRatio[j];
                 Ktotal[currAntIdx][1][1][j] = HSw[1][1]*oneOverSqrt2/gainRatio[j];} 
               else {
                 Ktotal[currAntIdx][0][0][j] = H[0][0]*oneOverSqrt2; //*gainRatio[j];
                 Ktotal[currAntIdx][0][1][j] = H[0][1]*oneOverSqrt2/gainRatio[j];
                 Ktotal[currAntIdx][1][0][j] = H[1][0]*oneOverSqrt2; //*gainRatio[j];
                 Ktotal[currAntIdx][1][1][j] = H[1][1]*oneOverSqrt2/gainRatio[j];
               };
             //  Ktotal[currAntIdx][0][1][j] *= gainRatio[j];
             //  Ktotal[currAntIdx][1][1][j] *= gainRatio[j];
             };
           };


////////////
// Compute the elements of the K matrix (only those that changed):

   //indent level within time range
           if (PCMode && (dtchanged || gchanged) && !allflagged) {
             //indent level if dt or g changed   

// INITIATE K MATRIX:
             auxD = 0.0;
             for (ij=0; ij<2; ij++) {
               for (ik=0; ik<2; ik++) {
                 for (j=0; j<nchans[ii]; j++) {
                   Ktotal[currAntIdx][ij][ik][j] = 0.0; 
                 };
               };
             };

// ADD-UP ALL GAINS:

             //indent level if dt or g changed
             for (ij=0; ij<currNant; ij++) {

     // BUT ONLY IF ANTENNA WAS USED IN THE PHASING
     //indent level if ANTENNA WAS USED IN THE PHASING
               if (Weight[currAntIdx][ij]) {

     // Total weight:
                 auxD += 1.0;

    // Kfrozen is unlikely to change much with time:
                 if (dtchanged) {
                   for (j=0; j<nchans[ii]; j++) {
                     gainXY[0] = 1.0 ; 
                     gainXY[1] = 1.0 ;
                     Kfrozen[currAntIdx][0][1][ij][j] = 
                         gainXY[0]*AnDt[currAntIdx][ij][0][j];
                     Kfrozen[currAntIdx][1][0][ij][j] = 
                         gainXY[1]*AnDt[currAntIdx][ij][1][j];
                     Kfrozen[currAntIdx][0][0][ij][j] = gainXY[0];
                     Kfrozen[currAntIdx][1][1][ij][j] = gainXY[1];
                   };
                 };

                 for (j=0; j<nchans[ii]; j++) {
                   K[currAntIdx][0][0][ij][j] = 
                       Kfrozen[currAntIdx][0][0][ij][j]*AnG[currAntIdx][ij][0][j];
                   K[currAntIdx][1][1][ij][j] = 
                       Kfrozen[currAntIdx][1][1][ij][j]*AnG[currAntIdx][ij][1][j];
                   K[currAntIdx][0][1][ij][j] = 
                       Kfrozen[currAntIdx][0][1][ij][j]*AnG[currAntIdx][ij][0][j];
                   K[currAntIdx][1][0][ij][j] = 
                       Kfrozen[currAntIdx][1][0][ij][j]*AnG[currAntIdx][ij][1][j];
                 };
                 // indent within if ANTENNA WAS USED IN THE PHASING

// Add-up all elements:
                 for (il=0; il<2; il++) {
                   for (ik=0; ik<2; ik++) {
                     for (j=0; j<nchans[ii]; j++) {
                       Ktotal[currAntIdx][il][ik][j] += K[currAntIdx][il][ik][ij][j];
                     };
                   };
                 };



               }; // Comes from if(Weight....)
             };//indent level if dt or g changed



             NormFac[0] = 0.0; NormFac[1] = 0.0; 


// Get the antenna-wise gain average:

//indent level if dt or g changed
             for (j=0; j<nchans[ii]; j++) {

//indent level within getting average loop
               for (ij=0; ij<2; ij++) {
                 for (ik=0; ik<2; ik++) {
                   Ktotal[currAntIdx][ij][ik][j] /= auxD;
                 };
               };
//indent level within getting average loop

// Correct the phase offset at the reference antenna:
               Ktotal[currAntIdx][0][1][j] *= gainRatio[j];
               Ktotal[currAntIdx][1][1][j] *= gainRatio[j];

////////////
               //indent level within getting average loop
               if(verbose && j==0){
                 printf("gainRatio (j=0): %.3e %.3e\n",
                      gainRatio[j].real(), gainRatio[j].imag());
                 printf("Ktot00: %.3e %.3e\n",
                      Ktotal[currAntIdx][0][0][j].real(), Ktotal[currAntIdx][0][0][j].imag());
                 printf("Ktot01: %.3e %.3e\n",
                      Ktotal[currAntIdx][0][1][j].real(), Ktotal[currAntIdx][0][1][j].imag());
                 printf("Ktot10: %.3e %.3e\n",
                      Ktotal[currAntIdx][1][0][j].real(), Ktotal[currAntIdx][1][0][j].imag());
                 printf("Ktot11: %.3e %.3e\n",
                      Ktotal[currAntIdx][1][1][j].real(), Ktotal[currAntIdx][1][1][j].imag()); 
                 if (allflagged) {printf("All flagged\n"); }
                 else {printf("Not flagged\n"); };
               };



///////////////////////////
// THIS CODE IS INDEPENDENT OF HOW THE AVERAGE FOR K MATRIX IS IMPLEMENTED

               //indent level within getting average loop
               AD = Ktotal[currAntIdx][0][0][j]*Ktotal[currAntIdx][1][1][j];
               BC = Ktotal[currAntIdx][0][1][j]*Ktotal[currAntIdx][1][0][j];
   // Determinant:
               DetInv = (AD - BC); // 

   // Inverse of K matrix:
               if (allflagged) {
                 Kinv[0][0] = 1.0; 
                 Kinv[0][1] = 0.0;
                 Kinv[1][0] = 0.0;
                 Kinv[1][1] = 1.0;} 
               else {
                 Kinv[0][0] = Ktotal[currAntIdx][1][1][j]/DetInv;
                 Kinv[1][1] = Ktotal[currAntIdx][0][0][j]/DetInv;
// BEWARE THAT THIS MUST BE IN ACCORDANCE TO THE DEFINITION OF Dx AND Dy!!!
                 Kinv[0][1] = -Ktotal[currAntIdx][0][1][j]/DetInv;
                 Kinv[1][0] = -Ktotal[currAntIdx][1][0][j]/DetInv;
               };

              //indent level within getting average loop
               if(doNorm){ 
                 NormFac[0] += std::abs(Kinv[0][0]); 
                 NormFac[1] += std::abs(Kinv[1][1]);
               };

   // Multiply by conversion (hybrid) matrix and save
   // result in the "Ktotal" matrix:
               //indent level within getting average looop
               if(XYSWAP[currAntIdx]){
                 Ktotal[currAntIdx][0][0][j] = 
                    (Kinv[0][0]*HSw[0][0]+Kinv[1][0]*HSw[0][1])*oneOverSqrt2;
                 Ktotal[currAntIdx][0][1][j] = 
                    (Kinv[0][1]*HSw[0][0]+Kinv[1][1]*HSw[0][1])*oneOverSqrt2;
                 Ktotal[currAntIdx][1][0][j] = 
                    (Kinv[0][0]*HSw[1][0]+Kinv[1][0]*HSw[1][1])*oneOverSqrt2;
                 Ktotal[currAntIdx][1][1][j] = 
                    (Kinv[0][1]*HSw[1][0]+Kinv[1][1]*HSw[1][1])*oneOverSqrt2;} 
               else {
                 Ktotal[currAntIdx][0][0][j] = 
                    (Kinv[0][0]*H[0][0]+Kinv[1][0]*H[0][1])*oneOverSqrt2;
                 Ktotal[currAntIdx][0][1][j] = 
                    (Kinv[0][1]*H[0][0]+Kinv[1][1]*H[0][1])*oneOverSqrt2;
                 Ktotal[currAntIdx][1][0][j] = 
                    (Kinv[0][0]*H[1][0]+Kinv[1][0]*H[1][1])*oneOverSqrt2;
                 Ktotal[currAntIdx][1][1][j] = 
                    (Kinv[0][1]*H[1][0]+Kinv[1][1]*H[1][1])*oneOverSqrt2;
               };
               //indent level within getting average looop

////////////////////////////////////
////////////////////

             };   // Comes from: for(j=0; j<nchans[ii]; j++) 
             //indent level if dt or g changed


           //indent level within time range
           } else {
             //indent level if dt or g changed
             NormFac[0]=((float) nchans[ii]); 
             NormFac[1]=((float) nchans[ii]);


           }; // Comes from the else of "if(dtchanged||gchanged)"


           //indent level within time range

 // Norm. factor will be the geometrical average of gains.
           if(doNorm && (dtchanged||gchanged)){
             AntTab = std::sqrt(NormFac[0]*NormFac[1])/((float) nchans[ii]);
             fprintf(gainsFile, "%i  %i  %.10e  %.5e \n",
                  ii+1, currAnt, currT/86400.,AntTab*AntTab/std::abs(auxD));
             for(j=0; j<nchans[ii]; j++){
               Ktotal[currAntIdx][0][0][j] /= AntTab;
               Ktotal[currAntIdx][0][1][j] /= AntTab;
               Ktotal[currAntIdx][1][0][j] /= AntTab;
               Ktotal[currAntIdx][1][1][j] /= AntTab;
             };
           };
           //indent level within time range


  // Correct for amplitude ratios (put amplitudes back):
           if(!PCMode){
          //   printf("%.2f %i |",std::abs(gainRatio[10]),currAntIdx);fflush(stdout);
             for(j=0; j<nchans[ii]; j++){
              //  AntTab = std::abs(gainRatio[j]); 
                AntTab = 1./std::abs(Ktotal[currAntIdx][0][0][j]*Ktotal[currAntIdx][1][1][j] - Ktotal[currAntIdx][0][1][j]*Ktotal[currAntIdx][1][0][j]);
             //   if(j==0 && currAntIdx==2){printf("%.2e ",AntTab);fflush(stdout);};
                Ktotal[currAntIdx][0][0][j] *= AntTab;
                Ktotal[currAntIdx][0][1][j] *= AntTab;
                Ktotal[currAntIdx][1][0][j] *= AntTab;
                Ktotal[currAntIdx][1][1][j] *= AntTab;
             };
           };

// Calibrate and convert to circular:

// Shall we write in plot file?
           auxB2 = (currT>=plRange[0] && currT<=plRange[1] && (calField<0 || currF==calField));
           if (IFplot < 0) { auxB2 = false; };

// NOTE: These files are used to plot in the ALMA case; but are also used
// when solving for the cross-polarization gains!
// So they are not only "plot" files.

// Convert:
           if(Phased){
             // note that if IFplot < 0, plotFile[IFplot] is
             // garbage; but auxB2 (just set) prevents its use
             DifXData->applyMatrix(
                 Ktotal[currAntIdx],XYSWAP[currAntIdx],auxB2,
                 currAntIdx,plotFile[IFplot]);
           } else {
             sprintf(message,"WARNING! Zero-ing weights at time %.8f!\n",currT);
             fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
             DifXData->zeroWeight();
           };

// Write:
           if (!doTest){DifXData->setCurrentMixedVis();};



         };// All this is done only if currT is within doRange.
         //indent level for time range
 
       };  // Go to next mixed-vis in this IF.
       // next mixed-vis indent level
  
     }; // Comes from the check of IF.

   }; ///////////////////////////////
// End of iteration over IFs
////////////////////////////////////




  sprintf(message,"\nDONE WITH ITERATION over IFs!\n");
  fprintf(logFile,"%s",message); std::cout << message; fflush(logFile);

  for (ij=0;ij<nIFplot;ij++){
    fclose(plotFile[ij]);
  };
  if(doNorm){fclose(gainsFile);};

  sprintf(message,"\nDONE WITH plot and gain files!\n");
  fprintf(logFile,"%s",message); std::cout << message; fflush(logFile);

// Close data file(s):
  DifXData->finish();

  sprintf(message,"\nFinishing DifXData!\n");
  fprintf(logFile,"%s",message); std::cout << message; fflush(logFile);

// (almost) END OF PROGRAM.
  std::cout << "\n";
  sprintf(message,"\nDONE WITH POLCONVERT!\n");
  fprintf(logFile,"%s",message); std::cout << message; fflush(logFile);


// finished with no errors:
  ret = Py_BuildValue("i",0);



/////////////////////////////////////////////////
// Free memory:


  for (ij=0; ij<nALMA; ij++) {
    auxI = nsumArr[ij];
    for (ii=0; ii<auxI; ii++) {
      delete[] AnG[ij][ii][0];
      delete[] AnG[ij][ii][1];
      delete[] AnDt[ij][ii][0];
      delete[] AnDt[ij][ii][1];
      delete[] AnG[ij][ii];
      delete[] AnDt[ij][ii];
    };
    delete[] AnG[ij];
    delete[] AnDt[ij];
    delete[] Weight[ij];

    for (ii=0; ii<2; ii++) {
      for (ik=0; ik<2; ik++) {
        for (il=0; il<auxI; il++) {
          delete[] K[ij][ii][ik][il];
          delete[] Kfrozen[ij][ii][ik][il];
        };
        delete[] K[ij][ii][ik];
        delete[] Kfrozen[ij][ii][ik];
        delete[] Ktotal[ij][ii][ik];
      };
    };
  };

  for (i=0;i<nALMA;i++){

  if(PCMode){
    for (j=0;j<nsumArr[i];j++){
      delete[] dttimesArr[i][j];
    };
    for (j=0; j<ngainTabs[i]; j++){
      delete[] ntimeArr[i][j];
      delete[] timesArr[i][j];
      delete[] gainsArrR1[i][j];
      delete[] gainsArrI1[i][j];
      delete[] gainsArrR2[i][j];
      delete[] gainsArrI2[i][j];
      delete[] gainflag[i][j];
    };
  };

    for (j=0; j<ngainTabs[i]; j++){
      delete allgains[i][j];
    };
    delete alldterms[i];
    delete[] allgains[i];

    if(PCMode){
     delete[]  ntimeArr[i]; 
     delete[]  timesArr[i];
     delete[]  gainsArrR1[i];
     delete[]  gainsArrI1[i];
     delete[]  gainsArrR2[i];
     delete[]  gainsArrI2[i];
     delete[]  gainflag[i];
     delete[]  kind[i];
     delete[]  nchanArr[i];
     delete[]  freqsArr[i];
     delete[] dttimesArr[i];
     delete[] ndttimeArr[i];
     delete[] dtflag[i];
     delete[] dtermsArrR1[i];
     delete[] dtermsArrI1[i];
     delete[] dtermsArrR2[i];
     delete[] dtermsArrI2[i];
    };

  };

  for (j=0;j<nSWINFiles;j++){
    for (i=0;i<NPGain;i++){
      delete[] PrioriGains[j][i];
    };
    delete[] PrioriGains[j];
  };

  delete[] PrioriGains;
  delete[] alldterms;
  delete[] allgains;
  delete[] nsumArr;
  delete[] almanums;

 if(PCMode){
  delete[] kind;
  delete[]  ntimeArr;
  delete[]  nchanArr;
  delete[]  timesArr;
  delete[]  gainsArrR1;
  delete[]  gainsArrI1;
  delete[]  gainsArrR2;
  delete[]  gainsArrI2;
  delete[]  gainflag;
  delete[]  freqsArr;
  delete[] ndttimeArr;
  delete[] dttimesArr;
  delete[] dtermsArrR1;
  delete[] dtermsArrI1;
  delete[] dtermsArrR2;
  delete[] dtermsArrI2;
  delete[] dtflag;
  delete[] nchanDt; 
  delete[] dtfreqsArr;
 };

  delete[] isLinear;
  delete[] XYSWAP;
  delete[] ngainTabs;

/////////////////////////////////////////////////////



  sprintf(message,"\nfinished POLCONVERT cleanup!\n");
  fprintf(logFile,"%s",message); std::cout << message; fflush(logFile);
  fclose(logFile);
 
//finished with no errors:
  ret = PyLong_FromLong(0L);

// avoid some lower-level exception that is still set?
  std::cout << "Clearing internal errors" << std::endl;
  PyErr_PrintEx(0);
  PyErr_Clear();
  std::cout << "Returning with success" << std::endl;
  return ret;




}


// vim: set nospell:
// eof

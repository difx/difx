/* POLGAINSOLVE - Solving cross-polarizer gains for PolConvert

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
#include <math.h>
#include <complex>
#include <dirent.h>
#include <fftw3.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_linalg.h>

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
// For PyArray_FromDimsAndData -> PyArray_SimpleNewFromData
#define INTEGER long
#define INTEGERCAST  (const npy_intp*)
#else
// For PyArray_FromDimsAndData -> PyArray_SimpleNewFromData
#define INTEGER int
#define INTEGERCAST (long int *)
#endif

// and after some hacking
#if PY_MAJOR_VERSION >= 3
#define PyString_AsString(obj) PyUnicode_AsUTF8(obj)
#endif

typedef std::complex<double> cplx64f;
typedef std::complex<float> cplx32f;

/* Docstrings */
static char module_docstring[] =
    "This module provides the cross-polarization gain-solver engine.";
static char PolGainSolve_docstring[] =
    "Solves for cross-polarization gains from mixed-polarization visibilities";
static char ReadData_docstring[] =
    "Reads data (one IF) for PolGainSolve";
static char FreeData_docstring[] =
    "Releases the data pointers of PolGainSolve";
static char GetChi2_docstring[] =
    "Computes the Chi2 for a given set of cross-pol gains";
static char GetIFs_docstring[] =
    "Returns the array of frequencies for a given IF";
static char DoGFF_docstring[] =
    "Performs a simplified GFF (delays and rates). The reference antenna is set by not adding it to the list of fittable antennas";
static char SetFringeRates_docstring[] =
    "Forces the antenna fringe-rates to the list give, before the GCPFF is performed";
static char GetNScan_docstring[] =
    "Returns the number of scans for the given IF.";
static char SetFit_docstring[] =
    "Allocates memory for the GCPFF.";



/* Available functions */
static PyObject *PolGainSolve(PyObject *self, PyObject *args);
static PyObject *ReadData(PyObject *self, PyObject *args);
static PyObject *GetChi2(PyObject *self, PyObject *args);
static PyObject *GetIFs(PyObject *self, PyObject *args);
static PyObject *DoGFF(PyObject *self, PyObject *args);
static PyObject *SetFringeRates(PyObject *self, PyObject *args);
static PyObject *GetNScan(PyObject *self, PyObject *args);
static PyObject *FreeData(PyObject *self, PyObject *args);
static PyObject *SetFit(PyObject *self, PyObject *args);

/* Module specification */
static PyMethodDef module_methods[] = {
    {"PolGainSolve", PolGainSolve, METH_VARARGS, PolGainSolve_docstring},
    {"ReadData", ReadData, METH_VARARGS, ReadData_docstring},
    {"GetChi2", GetChi2, METH_VARARGS, GetChi2_docstring},
    {"GetIFs", GetIFs, METH_VARARGS, GetIFs_docstring},
    {"DoGFF", DoGFF, METH_VARARGS, DoGFF_docstring},
    {"SetFringeRates", SetFringeRates, METH_VARARGS, SetFringeRates_docstring},
    {"GetNScan",GetNScan, METH_VARARGS, GetNScan_docstring},
    {"FreeData", FreeData, METH_VARARGS, FreeData_docstring},
    {"SetFit", SetFit, METH_VARARGS, SetFit_docstring},
    {NULL, NULL, 0, NULL}   /* terminated by list of NULLs, apparently */
};

// normally abort() is called on problems, which breaks CASA.
// here we report and save the error condition which can be
// noticed for a cleaner exit.
int gsl_death_by = GSL_SUCCESS;
static void gsl_death(const char * reason, const char * file,
    int line, int gsl_errno) {
    // stderr does not end up synchronized with stdout
    printf("GSL Death by '%s' in file %s at line %d: GSL Error %d\n",
        reason, file, line, gsl_errno);
    fflush(stdout); std::cout << std::flush;
    gsl_death_by = gsl_errno;
}

static long chisqcount = 0;
static long twincounter = 0;

/* Initialize the module */
#if PY_MAJOR_VERSION >= 3
static struct PyModuleDef pc_module_def = {
    PyModuleDef_HEAD_INIT,
    "_PolGainSolve",         /* m_name */
    module_docstring,       /* m_doc */
    -1,                     /* m_size */
    module_methods,         /* m_methods */
    NULL,NULL,NULL,NULL     /* m_reload, m_traverse, m_clear, m_free */
};
PyMODINIT_FUNC PyInit__PolGainSolve(void)
{
    PyObject *m = PyModule_Create(&pc_module_def);
    import_array();
    (void)gsl_set_error_handler(gsl_death);
    return(m);
}
#else
PyMODINIT_FUNC init_PolGainSolve(void)
{
    PyObject *m = Py_InitModule3("_PolGainSolve", module_methods, module_docstring);import_array();
    (void)gsl_set_error_handler(gsl_death);
    if (m == NULL)
        return;
}
#endif

///////////////////////


   char message[512];

   static double TWOPI = 6.283185307179586;

   int MaxChan = 1;
   int MAXIF = 8; // Will reallocate if needed.
   int NCalAnt, Nlin, Ncirc, *Nchan, SolMode, SolAlgor;
   int *IFNum;
   int *Lant, *Cant, *NVis, *NLVis, *NCVis, *CalAnts, *NScan;
   int *Twins[2], Ntwin;
   int solveAmp, useCov, solveQU;
   int *LinBasNum, NLinBas;
   int NIF, NIFComp;
   int NBas, NantFit, Npar = -1;
   int npix = 0;
   double **Frequencies, ***Rates, ***Delays00, ***Delays11; 
   double *Tm = 0;
   int *doIF, *antFit; 
   
   double *feedAngle;
   double **DStokes; // = new double*[1];

   double Chi2Old = 0.0;
   double TAvg = 1.0;
   double RelWeight = 1.0;
   double T0, T1, DT;
   int **Ant1, **Ant2, **BasNum, **Scan;
   double **Times, *CovMat, *IndVec, *SolVec;
   cplx64f **PA1, **PA2, **auxC00, **auxC01, **auxC10, **auxC11;
   cplx64f ***RR, ***RL, ***LR, ***LL, **CrossSpec00, **CrossSpec11;

   double Lambda;
   bool doCov;
   double Stokes[4];

   gsl_matrix_view m; 
   gsl_vector_view x, v;
   gsl_permutation *perm;

   bool AddCrossHand = true;
   bool AddParHand = true;
   bool StokesSolve;

   cplx64f *G1,*G2, *G1nu, *G2nu; 
   double *DirDer,*MBD1,*MBD2;
   int *DerIdx,*AvVis;


   FILE *logFile = 0;


static PyObject *GetNScan(PyObject *self, PyObject *args){
  int cIF;
  PyObject *ret;

  // append after first call
  if (!logFile) logFile = fopen("PolConvert.GainSolve.log","a");
  fprintf(logFile,"into GetNScan...\n"); fflush(logFile);

  if (!PyArg_ParseTuple(args, "i",&cIF)){
     sprintf(message,"Failed GetNScan! Check inputs!\n"); 
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
     ret = Py_BuildValue("i",-1);
     return ret;
  };

  fprintf(logFile,"... and got %d\n", NScan[cIF]); fflush(logFile);
  ret = Py_BuildValue("i",NScan[cIF]);
  return ret;

};



static PyObject *PolGainSolve(PyObject *self, PyObject *args){

  // truncate for first call
  //logFile = fopen("PolConvert.GainSolve.log","w");
  if (!logFile) logFile = fopen("PolConvert.GainSolve.log","a");
  PyObject *calant, *linant, *solints, *flagBas;;

  if (!PyArg_ParseTuple(args, "dOOOO",&RelWeight, &solints, &calant,
    &linant, &flagBas)){
     sprintf(message,"Failed initialization of PolGainSolve! Check inputs!\n"); 
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
     fclose(logFile);
    PyObject *ret = Py_BuildValue("i",-1);
    return ret;
  };

  sprintf(message,"Entered PolGainSolve with RelWeight %g\n\n", RelWeight);
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
  
// Assign dummy sizes to all variables:  

NIF = 0;
Npar=0;
DStokes = new double*[1];
DStokes[0] = new double[4];
CovMat = new double[1];
IndVec = new double[1];
SolVec = new double[1];
G1 = new cplx64f[1];
G2 = new cplx64f[1];
G1nu = new cplx64f[1];
G2nu = new cplx64f[1];
DirDer = new double[1];
MBD1 = new double[1];
MBD2 = new double[1];
DerIdx = new int[1];
AvVis = new int[1];
Tm = new double[1];
doIF = new int[1];
antFit = new int[1];



TAvg = (double) PyInt_AsLong(PyList_GetItem(solints,1));
SolAlgor = (int) PyInt_AsLong(PyList_GetItem(solints,0));

Twins[0] = (int *)PyArray_DATA(PyList_GetItem(flagBas,0));
Twins[1] = (int *)PyArray_DATA(PyList_GetItem(flagBas,1));
Ntwin = PyArray_DIM(PyList_GetItem(flagBas,0),0);

sprintf(message,"Will divide each calibration scan into %.1f chunks\n",TAvg);
fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  


AddParHand = RelWeight>0.0;
AddCrossHand = true;

  Lant = (int *)PyArray_DATA(linant);
  Nlin = PyArray_DIM(linant,0);

  CalAnts = (int *)PyArray_DATA(calant);
  NCalAnt = PyArray_DIM(calant,0);

  
  int i,j,k,l;
  k=0;

  int MaxAnt = 0;
  for(i=0;i<NCalAnt;i++){if(CalAnts[i]>MaxAnt){MaxAnt=CalAnts[i];};};
  sprintf(message,"MaxAnt is %d\n", MaxAnt);
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

  BasNum = new int*[MaxAnt];
  LinBasNum = new int[MaxAnt];
  NLinBas = 0;
  
  bool isCal1, isCal2; // not used: isFlag;

  for(i=0;i<MaxAnt;i++){
    BasNum[i] = new int[MaxAnt];

//  for(j=0;j<MaxAnt;j++){BasNum[i][j] = -1;};
//  isFlag = false;
    for(j=0;j<MaxAnt;j++){
      BasNum[i][j] = -1;
   // does not go here, look down about 1500 lines to flagging baselines
   //    for(k=0;k<Ntwin;k++){
   //       if((Twins[0][k]==i+1 && Twins[1][k]==j+1) || (Twins[1][k]==i+1 && Twins[0][k]==j+1)){isFlag=true;break;};
   //    };
    }

    for(j=i+1;j<MaxAnt;j++){

      isCal1=false; isCal2=false;

      for(l=0;l<NCalAnt;l++){
        if(i==CalAnts[l]-1){isCal1=true;}; 
        if(j==CalAnts[l]-1){isCal2=true;};
      };


      if (isCal1 && isCal2){
        BasNum[i][j] = k;
        
        for(l=0;l<Nlin; l++){if(i==Lant[l] || j==Lant[l]){LinBasNum[NLinBas]=k; NLinBas += 1; break; };};

        sprintf(message,"BL %d is %d - %d\n", k, i+1, j+1);
        fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
        
        // BasNum[j][i] = k;
        k += 1;
      };

    };
  };
  sprintf(message,"NLinBas is %d\n", NLinBas);
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

  
  auxC00 = new cplx64f*[k]; 
  auxC01 = new cplx64f*[k]; 
  auxC10 = new cplx64f*[k];
  auxC11 = new cplx64f*[k];

  for(i=0;i<k;i++){
    auxC00[i] = new cplx64f[3*NCalAnt+1];
    auxC11[i] = new cplx64f[3*NCalAnt+1];
    auxC01[i] = new cplx64f[3*NCalAnt+1];
    auxC10[i] = new cplx64f[3*NCalAnt+1];
  };

  NBas = k;
  sprintf(message,"NBas is %d\n", NBas);
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

  CrossSpec00 = (cplx64f **) malloc(NBas*sizeof(cplx64f*));
  CrossSpec11 = (cplx64f **) malloc(NBas*sizeof(cplx64f*));
  for(i=0;i<NBas;i++){
    CrossSpec00[i] = (cplx64f *) malloc(MaxChan*sizeof(cplx64f));
    CrossSpec11[i] = (cplx64f *) malloc(MaxChan*sizeof(cplx64f));
  };






  NIF = 0;


// Set Memory:

  NVis = (int *) malloc(MAXIF*sizeof(int));
  IFNum = (int *) malloc(MAXIF*sizeof(int));
  NCVis = (int *) malloc(MAXIF*sizeof(int));
  NLVis = (int *) malloc(MAXIF*sizeof(int));
  Nchan = (int *) malloc(MAXIF*sizeof(int));
  Frequencies = (double **) malloc(MAXIF*sizeof(double*));
  Ant1 = (int**) malloc(MAXIF*sizeof(int*));
  Ant2 = (int**) malloc(MAXIF*sizeof(int*));
  Scan = (int**) malloc(MAXIF*sizeof(int*));
  NScan = (int*) malloc(MAXIF*sizeof(int));
  Times = (double**) malloc(MAXIF*sizeof(double*));
  PA1 = (cplx64f**) malloc(MAXIF*sizeof(cplx64f*));
  PA2 = (cplx64f**) malloc(MAXIF*sizeof(cplx64f*));
  RR = (cplx64f***) malloc(MAXIF*sizeof(cplx64f**));
  LR = (cplx64f***) malloc(MAXIF*sizeof(cplx64f**));
  RL = (cplx64f***) malloc(MAXIF*sizeof(cplx64f**));
  LL = (cplx64f***) malloc(MAXIF*sizeof(cplx64f**));
  Rates = (double ***) malloc(MAXIF*sizeof(double**));
  Delays00 = (double ***) malloc(MAXIF*sizeof(double**));
  Delays11 = (double ***) malloc(MAXIF*sizeof(double**));

    PyObject *ret = Py_BuildValue("i",0);
    return ret;

};







static PyObject *FreeData(PyObject *self, PyObject *args) {

int i,j; 

if (!logFile) logFile = fopen("PolConvert.GainSolve.log","a");
sprintf(message,"Freeing Data NIF = %d\n", NIF);
fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

for(i=0;i<NIF;i++){
  for(j=0;j<NVis[i]+1;j++){
    free(RR[i][j]);free(RL[i][j]);
    free(LR[i][j]);free(LL[i][j]);
  };
  free(Ant1[i]);free(Ant2[i]);free(Scan[i]);free(Times[i]);
  free(PA1[i]);free(PA2[i]);free(RR[i]);free(RL[i]);
  free(LR[i]);free(LL[i]);
  delete Frequencies[i];
};

if(NIF>0){
  free(NScan);free(Nchan);free(NVis);
  free(NCVis);free(NLVis);free(IFNum);
  free(Frequencies); free(Scan);
  NIF = -1;

PyObject *ret = Py_BuildValue("i",0);
return ret;

};

sprintf(message,"Closing Logfile\n");
fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
fclose(logFile);

PyObject *ret = Py_BuildValue("i",1);
return ret;

};







static PyObject *ReadData(PyObject *self, PyObject *args) {

int IFN;
const char *file1, *file2;
std::ifstream CPfile, MPfile;

if (!logFile) logFile = fopen("PolConvert.GainSolve.log","a");
fprintf(logFile,"ReadData entered...\n"); fflush(logFile);
if (!PyArg_ParseTuple(args, "iss", &IFN,&file1, &file2)){
     sprintf(message,"Failed ReadData! Check inputs! (return -1)\n"); 
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
     fclose(logFile);
    PyObject *ret = Py_BuildValue("i",-1);
    return ret;
};
fprintf(logFile,"ReadData parsed...\n"); fflush(logFile);




int i, j, k;
double AuxT, AuxPA1, AuxPA2;

bool is1, is2;


CPfile.open(file1, std::ios::in | std::ios::binary);
MPfile.open(file2, std::ios::in | std::ios::binary);
printf("  file1: %s\n  file2: %s\n", file1, file2);



NIF += 1;

if (NIF > MAXIF){
// Set memory for new IF:
  fprintf(logFile,"(realloc) %d > %d\n", NIF, MAXIF); fflush(logFile);
  MAXIF *= 2;
  Nchan = (int*) realloc(Nchan,MAXIF*sizeof(int));
  Frequencies = (double**) realloc(Frequencies,MAXIF*sizeof(double*));
  NVis = (int*) realloc(NVis,MAXIF*sizeof(int));
  NCVis = (int*) realloc(NCVis,MAXIF*sizeof(int));
  NLVis = (int*) realloc(NLVis,MAXIF*sizeof(int));
  IFNum = (int*) realloc(IFNum,MAXIF*sizeof(int));
  if(!Nchan || !Frequencies || !NVis || !NCVis || !NLVis || !IFNum){
    Nchan=NULL; Frequencies=NULL; NVis=NULL; NCVis=NULL; NLVis=NULL; IFNum=NULL;
    fprintf(logFile,"(return -2)"); fflush(logFile);
    PyObject *ret = Py_BuildValue("i",-2);
    return ret;
  };


// Set memory for the visibilities and metadata:
  Ant1 = (int**) realloc(Ant1,MAXIF*sizeof(int*));
  Ant2 = (int**) realloc(Ant2,MAXIF*sizeof(int*));
  Scan = (int**) realloc(Scan,MAXIF*sizeof(int*));
  NScan = (int*) realloc(NScan,MAXIF*sizeof(int));
  Times = (double**) realloc(Times,MAXIF*sizeof(double*));
  PA1 = (cplx64f**) realloc(PA1,MAXIF*sizeof(cplx64f*));
  PA2 = (cplx64f**) realloc(PA2,MAXIF*sizeof(cplx64f*));
  RR = (cplx64f***) realloc(RR,MAXIF*sizeof(cplx64f**));
  LR = (cplx64f***) realloc(LR,MAXIF*sizeof(cplx64f**));
  RL = (cplx64f***) realloc(RL,MAXIF*sizeof(cplx64f**));
  LL = (cplx64f***) realloc(LL,MAXIF*sizeof(cplx64f**));
  Rates = (double ***) realloc(Rates,MAXIF*sizeof(double**));
  Delays00 = (double ***) realloc(Delays00,MAXIF*sizeof(double**));
  Delays11 = (double ***) realloc(Delays11,MAXIF*sizeof(double**));

  if(!Ant1 || !Ant2 || !Times || !PA1 || !PA2 || !RR || !LR || !RL || !LL){
    Ant1=NULL; Ant2=NULL; Times=NULL; PA1=NULL; PA2=NULL; RR=NULL;
    LR=NULL; RL=NULL; LL=NULL;
    fprintf(logFile,"(return -3)"); fflush(logFile);
    PyObject *ret = Py_BuildValue("i",-3);
    return ret;
  };

  if(!Rates || !Delays00 || !Delays11){
    Rates=NULL; Delays00=NULL; Delays11=NULL;
    fprintf(logFile,"(return -4)"); fflush(logFile);
    PyObject *ret = Py_BuildValue("i",-4);
    return ret;
  };


};

// IF NUMBER:
IFNum[NIF-1] = IFN;

// Number of channels for this IF:
CPfile.read(reinterpret_cast<char*>(&Nchan[NIF-1]), sizeof(int));
fprintf(logFile, "IF%d has %i channels\n",IFN,Nchan[NIF-1]); fflush(logFile);


// Maximum number of channels:
if (Nchan[NIF-1] > MaxChan){
  MaxChan=Nchan[NIF-1];
  for (i=0; i< NBas; i++) {
    CrossSpec00[i] = (cplx64f *) realloc(CrossSpec00[i],MaxChan*sizeof(cplx64f));
    CrossSpec11[i] = (cplx64f *) realloc(CrossSpec11[i],MaxChan*sizeof(cplx64f));
    if(!CrossSpec00[i] || !CrossSpec11[i]){
      CrossSpec00[i]=NULL; CrossSpec11[i]=NULL;
      fprintf(logFile,"(return -5)"); fflush(logFile);
      PyObject *ret = Py_BuildValue("i",-5);
      return ret;
    };

  };
};


// ignores noI
MPfile.ignore(sizeof(int));


//Frequencies[NIF-1] = new double[Nchan[NIF-1]];



// Get frequencies for this IF:
Frequencies[NIF-1] = new double[Nchan[NIF-1]];
CPfile.read(reinterpret_cast<char*>(Frequencies[NIF-1]), Nchan[NIF-1]*sizeof(double));


fprintf(logFile,"Freqs. %.8e  %.8e\n",
    Frequencies[NIF-1][0],Frequencies[NIF-1][Nchan[NIF-1]-1]);
fflush(logFile);


// Number of integration times:
int NDiffTimes = 0;
int TimeBuff = 1000;
double *DiffTimes = (double *) malloc(TimeBuff*sizeof(double));
bool isTime;


// Get Number of visibilities observed by the CalAnts (Circ Pol):

NCVis[NIF-1] = 0;
int AuxA1, AuxA2;

fprintf(logFile,"Reading CPfile...(NCalAnt=%d)\n", NCalAnt); fflush(logFile);
// eof() doesn't do what everyone thinks....
while(!CPfile.eof() && CPfile.peek() >= 0){

  is1 = false; is2 = false;
  CPfile.ignore(sizeof(double));    // daytemp
  CPfile.read(reinterpret_cast<char*>(&AuxA1), sizeof(int));
  CPfile.read(reinterpret_cast<char*>(&AuxA2), sizeof(int));
  for (i=0; i<NCalAnt; i++) {
    if (AuxA1 == CalAnts[i]){is1=true;}; 
    if (AuxA2 == CalAnts[i]){is2=true;};
  };

  if(is1 && is2 && AuxA1 != AuxA2){
    NCVis[NIF-1] += 1;
  };
  //  printf("%i %i - %i\n",AuxA1,AuxA2,NCVis[NIF-1]);

  // here we are ignoring all the visibility data
  CPfile.ignore(2*sizeof(double)+4*Nchan[NIF-1]*sizeof(cplx32f)); 

};
fprintf(logFile,"Finished CPfile...\n"); fflush(logFile);








// Get Number of visibilities observed by the CalAnts (Mix Pol):
NLVis[NIF-1] = 0;



fprintf(logFile,"Reading MPfile... (NCalAnt=%d)\n", NCalAnt); fflush(logFile);
// eof() doesn't do what everyone thinks....
while(!MPfile.eof() && MPfile.peek() >= 0){

  is1 = false; is2 = false;
  MPfile.ignore(sizeof(double));    // Time
  MPfile.read(reinterpret_cast<char*>(&AuxA1), sizeof(int));
  MPfile.read(reinterpret_cast<char*>(&AuxA2), sizeof(int));
  for (i=0; i<NCalAnt; i++) {
    if (AuxA1 == CalAnts[i]){is1=true;}; 
    if (AuxA2 == CalAnts[i]){is2=true;};
//    printf("%i %i - %i - %i %i\n",AuxA1,AuxA2,CalAnts[i],is1,is2);

  };

  if(is1 && is2 && AuxA1 != AuxA2){
    NLVis[NIF-1] += 1;
  };

//  printf("%i %i - %i\n",AuxA1,AuxA2,NLVis[NIF-1]);

  MPfile.ignore(2*sizeof(double)+12*Nchan[NIF-1]*sizeof(cplx32f)); 
};
fprintf(logFile,"Finished MPfile...\n"); fflush(logFile);


// Total number of visibilities:
NVis[NIF-1] = NCVis[NIF-1]+ NLVis[NIF-1];

sprintf(message,"Found %i vis in CPol and %i vis in LPol for a total of %i\n",
    NCVis[NIF-1],NLVis[NIF-1],NVis[NIF-1]); 
fprintf(logFile,"%s",message);  fflush(logFile);


// Set memory for the visibilities:
j = NVis[NIF-1]+1;
Ant1[NIF-1] = (int*) malloc(j*sizeof(int)); // new int[j];

Ant2[NIF-1] = (int*) malloc(j*sizeof(int)); // new int[j];

Scan[NIF-1] = (int*) malloc(j*sizeof(int)); // new int[j];

Times[NIF-1] = (double*) malloc(j*sizeof(double)); // new double[j];

PA1[NIF-1] = (cplx64f*) malloc(j*sizeof(cplx64f)); // new cplx64f[j];

PA2[NIF-1] = (cplx64f*) malloc(j*sizeof(cplx64f)); //  new cplx64f[j];

RR[NIF-1] = (cplx64f**) malloc(j*sizeof(cplx64f*)); // new cplx64f*[j];

LR[NIF-1] = (cplx64f**) malloc(j*sizeof(cplx64f*)); // new cplx64f*[j];

RL[NIF-1] = (cplx64f**) malloc(j*sizeof(cplx64f*)); // new cplx64f*[j];

LL[NIF-1] = (cplx64f**) malloc(j*sizeof(cplx64f*)); // new cplx64f*[j];

for (i=0;i<j;i++){
  RR[NIF-1][i] = (cplx64f*) malloc(Nchan[NIF-1]*sizeof(cplx64f)); // new cplx64f[Nchan[NIF-1]];
  LR[NIF-1][i] = (cplx64f*) malloc(Nchan[NIF-1]*sizeof(cplx64f)); //new cplx64f[Nchan[NIF-1]];
  RL[NIF-1][i] = (cplx64f*) malloc(Nchan[NIF-1]*sizeof(cplx64f)); //new cplx64f[Nchan[NIF-1]];
  LL[NIF-1][i] = (cplx64f*) malloc(Nchan[NIF-1]*sizeof(cplx64f)); //new cplx64f[Nchan[NIF-1]];
};



// Rewind files:
CPfile.clear();
CPfile.seekg(sizeof(int)+Nchan[NIF-1]*sizeof(double),CPfile.beg);

MPfile.clear();
MPfile.seekg(sizeof(int),MPfile.beg);

fprintf(logFile, "\nFiles rewound\n"); fflush(logFile);

// Read visibilities (Mix Pol):
int currI = 0;
bool isGood, isFlipped;
//bool  is1c, is2c;
cplx64f Exp1, Exp2;

cplx32f AuxRR, AuxRL, AuxLR, AuxLL;

i=0;
while(!MPfile.eof() && MPfile.peek() >= 0){
  // MPfile timestamp is here
  MPfile.read(reinterpret_cast<char*>(&AuxT), sizeof(double));
  MPfile.read(reinterpret_cast<char*>(&AuxA1), sizeof(int));
  MPfile.read(reinterpret_cast<char*>(&AuxA2), sizeof(int));
  MPfile.read(reinterpret_cast<char*>(&AuxPA1), sizeof(double));
  MPfile.read(reinterpret_cast<char*>(&AuxPA2), sizeof(double));

// Check if visib is observed by CalAnts:
  isGood = false; is1 = false; is2 = false;
//  is1c = true; is2c = true;

  isFlipped = false;

  for (j=0; j<NCalAnt; j++) {
    if (AuxA1 == CalAnts[j]){is1=true;}; 
    if (AuxA2 == CalAnts[j]){is2=true;};
  };


    if (is1 && is2 && AuxA1 != AuxA2){

      isFlipped = AuxA1 > AuxA2;

      Exp1 = std::polar(1.0,AuxPA1);
      Exp2 = std::polar(1.0,AuxPA2);

// Check if there is (are) Circ Pol antenna(s):
//      for (l=0; l<Nlin; l++){
//        if (AuxA1 == Lant[l]){
//          if(isFlipped){is2c = false;}else{is1c=false;};
//        };
//        if (AuxA2 == Lant[l]){
//          if(isFlipped){is1c = false;}else{is2c=false;};
//        };
//     };

     Times[NIF-1][currI] = AuxT;
 //    printf("DT: %.5e\n",Times[NIF-1][currI]-Times[NIF-1][0]);

     isTime=false;
     for(j=0;j<NDiffTimes;j++){
        if(DiffTimes[j]==AuxT){isTime=true;break;};
     };

     if (!isTime){
       DiffTimes[NDiffTimes]=AuxT;
       NDiffTimes += 1;
       if (NDiffTimes >= TimeBuff){
         TimeBuff += 1000;
         DiffTimes = (double *) realloc(DiffTimes,TimeBuff*sizeof(double));
       };
     };



     if (isFlipped){
       Ant1[NIF-1][currI] = AuxA2;
       Ant2[NIF-1][currI] = AuxA1;
       PA1[NIF-1][currI] = Exp2;
       PA2[NIF-1][currI] = Exp1;
     } else {
       Ant1[NIF-1][currI] = AuxA1;
       Ant2[NIF-1][currI] = AuxA2;
       PA1[NIF-1][currI] = Exp1;
       PA2[NIF-1][currI] = Exp2;
     };

     for (k=0;k<Nchan[NIF-1];k++){
// Jump Uncal Data:
       MPfile.ignore(4*sizeof(cplx32f));
// Read Cal Data:
       MPfile.read(reinterpret_cast<char*>(&AuxRR), sizeof(cplx32f));
       MPfile.read(reinterpret_cast<char*>(&AuxRL), sizeof(cplx32f));
       MPfile.read(reinterpret_cast<char*>(&AuxLR), sizeof(cplx32f));
       MPfile.read(reinterpret_cast<char*>(&AuxLL), sizeof(cplx32f));
// Jump Matrix:
       MPfile.ignore(4*sizeof(cplx32f));

// Apply ParAng to antennas with Circ Pol:

       if (isFlipped){
         RR[NIF-1][currI][k] = conj((cplx64f) AuxRR);
         RL[NIF-1][currI][k] = conj((cplx64f) AuxLR);
         LR[NIF-1][currI][k] = conj((cplx64f) AuxRL);
         LL[NIF-1][currI][k] = conj((cplx64f) AuxLL);
       } else {
         RR[NIF-1][currI][k] = (cplx64f) AuxRR;
         RL[NIF-1][currI][k] = (cplx64f) AuxRL;
         LR[NIF-1][currI][k] = (cplx64f) AuxLR;
         LL[NIF-1][currI][k] = (cplx64f) AuxLL;
       };




     };
     currI += 1; isGood = true;


   };

  if(!isGood){
    MPfile.ignore(12*Nchan[NIF-1]*sizeof(cplx32f));
  };


};
printf("Reached MP eof\n");







// Read visibilities (Circ Pol):

while(!CPfile.eof() && CPfile.peek() >= 0){

  // CPfile timestamp is here
  CPfile.read(reinterpret_cast<char*>(&AuxT), sizeof(double));
  CPfile.read(reinterpret_cast<char*>(&AuxA1), sizeof(int));
  CPfile.read(reinterpret_cast<char*>(&AuxA2), sizeof(int));
  CPfile.read(reinterpret_cast<char*>(&AuxPA1), sizeof(double));
  CPfile.read(reinterpret_cast<char*>(&AuxPA2), sizeof(double));

// Check if visib is observed by CalAnts:
  isGood = false; is1 = false; is2 = false;
  isFlipped = false;

  for (j=0; j<NCalAnt; j++) {
    if (AuxA1 == CalAnts[j]){is1=true;}; 
    if (AuxA2 == CalAnts[j]){is2=true;};
  };

    if (is1 && is2 && AuxA1 != AuxA2){

// Both antennas HAVE to be in Circ Pol:
     Exp1 = std::polar(1.0,AuxPA1);
     Exp2 = std::polar(1.0,AuxPA2);

     isFlipped = AuxA1 > AuxA2;

     Times[NIF-1][currI] = AuxT;


     isTime=false;
     for(j=0;j<NDiffTimes;j++){
        if(DiffTimes[j]==AuxT){isTime=true;break;};
     };

     if (!isTime){
       DiffTimes[NDiffTimes]=AuxT;
       NDiffTimes += 1;
       if (NDiffTimes >= TimeBuff){
         TimeBuff += 1000;
         DiffTimes = (double *) realloc(DiffTimes,TimeBuff*sizeof(double));
       };
     };




     if (isFlipped){
       Ant1[NIF-1][currI] = AuxA2;
       Ant2[NIF-1][currI] = AuxA1;
       PA1[NIF-1][currI] = Exp2;
       PA2[NIF-1][currI] = Exp1;
     } else {
       Ant1[NIF-1][currI] = AuxA1;
       Ant2[NIF-1][currI] = AuxA2;
       PA1[NIF-1][currI] = Exp1;
       PA2[NIF-1][currI] = Exp2;
     };

     for (k=0;k<Nchan[NIF-1];k++){
       CPfile.read(reinterpret_cast<char*>(&AuxRR), sizeof(cplx32f));
       CPfile.read(reinterpret_cast<char*>(&AuxRL), sizeof(cplx32f));
       CPfile.read(reinterpret_cast<char*>(&AuxLR), sizeof(cplx32f));
       CPfile.read(reinterpret_cast<char*>(&AuxLL), sizeof(cplx32f));


       if (isFlipped){
         RR[NIF-1][currI][k] = conj((cplx64f) AuxRR);
         RL[NIF-1][currI][k] = conj((cplx64f) AuxLR);
         LR[NIF-1][currI][k] = conj((cplx64f) AuxRL);
         LL[NIF-1][currI][k] = conj((cplx64f) AuxLL);
       } else {
         RR[NIF-1][currI][k] = (cplx64f) AuxRR;
         RL[NIF-1][currI][k] = (cplx64f) AuxRL;
         LR[NIF-1][currI][k] = (cplx64f) AuxLR;
         LL[NIF-1][currI][k] = (cplx64f) AuxLL;
       };



     };

     currI += 1; isGood = true;
 //    printf("%i of %i  /  %i - %i\n",currI,NVis[NIF-1],AuxA1,AuxA2);

   };

  if(!isGood){
 //   printf("NO GOOD! %i / %i - %i\n",currI,AuxA1,AuxA2);

    CPfile.ignore(4*Nchan[NIF-1]*sizeof(cplx32f));
  };

};

printf("DONE READ!\n");

CPfile.close();
MPfile.close();



// Sort times out:
printf("There are %i int. times.\n",NDiffTimes);
double temp;
bool isOut = true;
while (isOut){
  isOut = false;
  for(j=0;j<NDiffTimes-1;j++){
    if(DiffTimes[j]>DiffTimes[j+1]){
      isOut=true;
      temp = DiffTimes[j];
      DiffTimes[j] = DiffTimes[j+1];
      DiffTimes[j+1] = temp;
    };
  };
};
// sharing to log
printf("  difftimes %f %f .. %f %f\n",
    DiffTimes[0], DiffTimes[1],
    DiffTimes[NDiffTimes-2], DiffTimes[NDiffTimes-1]);


// Get scans:
double ScanTimes[NDiffTimes];
NScan[NIF-1] = 1;
ScanTimes[0] = DiffTimes[0];

for(j=1;j<NDiffTimes;j++){
  if (std::abs(DiffTimes[j]-DiffTimes[j-1])>100.){
    ScanTimes[NScan[NIF-1]] = DiffTimes[j];
//    printf("%i -- %.3f -- %i\n",NScan[NIF-1],std::abs(ScanTimes[NScan[NIF-1]]-ScanTimes[NScan[NIF-1]-1]),j);
    NScan[NIF-1] += 1;
  };
};
ScanTimes[NScan[NIF-1]] = DiffTimes[NDiffTimes-1]+1.;
Scan[NIF-1][0] = 0;

// Assign scan number to each visibility:
for(j=1;j<NVis[NIF-1];j++){
  for(i=0;i<NScan[NIF-1];i++){
    if(Times[NIF-1][j]>=ScanTimes[i] && Times[NIF-1][j]<ScanTimes[i+1]){Scan[NIF-1][j]=i;break;};
  };
};


  Rates[NIF-1] = (double **) malloc(NCalAnt*sizeof(double*));
  Delays00[NIF-1] = (double **) malloc(NCalAnt*sizeof(double*));
  Delays11[NIF-1] = (double **) malloc(NCalAnt*sizeof(double*));
  for(i=0;i<NCalAnt;i++){
    Rates[NIF-1][i] = (double *) malloc(NScan[NIF-1]*sizeof(double));
    Delays00[NIF-1][i] = (double *) malloc(NScan[NIF-1]*sizeof(double));
    Delays11[NIF-1][i] = (double *) malloc(NScan[NIF-1]*sizeof(double));
    for(k=0;k<NScan[NIF-1];k++){
      Rates[NIF-1][i][k] = 0.0;
      Delays00[NIF-1][i][k] = 0.0;
      Delays11[NIF-1][i][k] = 0.0;
    };
  };



sprintf(message,"Found %i scans for IF %i\n",NScan[NIF-1],IFN);
fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  

// what are the times?
for(j=0;j<NScan[NIF-1];j++)
    printf("  scan %d time is %f\n", j, ScanTimes[j]);

free(DiffTimes);

//printf("CLOSED!\n");

    PyObject *ret = Py_BuildValue("i",0);
    return ret;


};









static PyObject *GetIFs(PyObject *self, PyObject *args) {
//(int Npar, int IF0, intint Ch0, int Ch1, double *pars) 
int i,j,k;
//PyObject *IFlist;

if (!logFile) logFile = fopen("PolConvert.GainSolve.log","a");
if (!PyArg_ParseTuple(args, "i", &i)){
     sprintf(message,"Failed GetIFs! Check inputs!\n"); 
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
     fclose(logFile);
    PyObject *ret = Py_BuildValue("i",-1);
    return ret;
};
sprintf(message,"Locating IFNum as %d (aka IF %d)\n", i, i+1);
fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

k=-1;
for (j=0; j<NIF; j++){
  if(IFNum[j] == i){k=j;break;};
};
if(k<0){
    sprintf(message,"IF %d not found\n", i);
    fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
    PyObject *ret = Py_BuildValue("i",-2);
    return ret;
};

INTEGER dims[1];
dims[0] = Nchan[k];

double *copyFreq = new double[Nchan[k]];

for (j=0; j<Nchan[k]; j++){
  copyFreq[j] = Frequencies[k][j];
};

// PyArray_FromDimsAndDataAndDescr and PyArray_FromDims are gone...
// Use PyArray_NewFromDescr and PyArray_SimpleNew instead. (gh-14100)
// PyArrayObject *out_Freq = (PyArrayObject *) PyArray_FromDimsAndData(1,dims,PyArray_DOUBLE, (char *) copyFreq);
PyArrayObject *out_Freq = (PyArrayObject *)PyArray_SimpleNewFromData(
    1, INTEGERCAST dims, NPY_DOUBLE, copyFreq);
PyArray_ENABLEFLAGS(out_Freq, NPY_ARRAY_OWNDATA);
sprintf(message,"IF %d with %d chans\n", i, Nchan[k]);
fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
std::cout << std::flush;
return PyArray_Return(out_Freq);
};






///////////////////////////////
// Quinn Estimator of the FFT peak with sub-bin precision:

double QuinnTau(double x){
//  return 0.25*log(3.*x*x + 6.*x + 1.) - sqrt(6.)/24.*log((x+1.-sqrt(2./3.))/(x+1.+sqrt(2./3.)));
  return 0.25*log1p(3.*x*x + 6.*x) - sqrt(6.)/24.*log1p(-2.*sqrt(2./3.)/(x+1.+sqrt(2./3.)));

};

double QuinnEstimate(cplx64f *FFTVec){

  double Denom = FFTVec[1].real()*FFTVec[1].real() + FFTVec[1].imag()*FFTVec[1].imag();
  double AP = (FFTVec[2].real()*FFTVec[1].real() + FFTVec[2].imag()*FFTVec[1].imag())/Denom;
  double AM = (FFTVec[0].real()*FFTVec[1].real() + FFTVec[0].imag()*FFTVec[1].imag())/Denom;
  
  double DP = -AP/(1.-AP);
  double DM = AM/(1.-AM);

  return (DP + DM)/2. + QuinnTau(DP*DP) - QuinnTau(DM*DM);
};

///////////////////////////////










static PyObject *SetFringeRates(PyObject *self, PyObject *args) {

int i,j,NantFix,cIF,cScan;
double *rates;

PyObject *ratesArr, *antList;

if (!logFile) logFile = fopen("PolConvert.GainSolve.log","a");
if (!PyArg_ParseTuple(args, "iiOO", &cIF, &cScan, &ratesArr, &antList)){
     sprintf(message,"Failed SetFringeRates! Check inputs!\n"); 
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
     fclose(logFile);
    PyObject *ret = Py_BuildValue("i",-1);
    return ret;
};


NantFix = (int) PyList_Size(antList);

rates = (double *) PyArray_DATA(ratesArr);

for (i=0; i<NCalAnt; i++){
  Rates[cIF][i][cScan] = 0.0;
};

for (i=0; i<NantFix; i++){
  j = (int) PyInt_AsLong(PyList_GetItem(antList,i));
  Rates[cIF][j-1][cScan] = rates[i];
};

//Py_DECREF(ratesArr);
//Py_DECREF(antList);

PyObject *ret = Py_BuildValue("i",0);
return ret;

};





static PyObject *DoGFF(PyObject *self, PyObject *args) {

int i,j,k,l, a1,a2, af1, af2, BNum,cScan;


//double Drate, T0;
//cplx32f Grate;

double *T0 = new double[NBas];  
double *T1 = new double[NBas];
bool isFirst = true;
int applyRate;
bool showMe, gotAnts;

cplx64f *aroundPeak00 = new cplx64f[3];
cplx64f *aroundPeak11 = new cplx64f[3];


//T0 = Times[0][0];

PyObject *antList;

if (!logFile) logFile = fopen("PolConvert.GainSolve.log","a");
if (!PyArg_ParseTuple(args, "Oiii", &antList,&npix, &applyRate,&cScan)){
     sprintf(message,"Failed DoGFF! Check inputs!\n"); 
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
     fclose(logFile);
    PyObject *ret = Py_BuildValue("i",-1);
    return ret;
};



if (applyRate==0){
  sprintf(message,"\n\n   DoGFF: Residual rate will NOT be estimated\n\n");
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
} else {
  sprintf(message,"\n\n   DoGFF: Residual rate WILL be estimated\n\n");
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
}

NantFit = (int) PyList_Size(antList);
delete[] antFit;
antFit = new int[NantFit];

for (i=0; i<NantFit; i++){
  antFit[i] = (int) PyInt_AsLong(PyList_GetItem(antList,i));
};


double **BLRates00 = new double *[NIF] ; 
double **BLRates11 = new double *[NIF] ;
double **BLDelays00 = new double *[NIF] ; 
double **BLDelays11 = new double *[NIF] ; 
double **Weights = new double *[NIF] ; 

for (i=0; i<NIF;i++){
  BLRates00[i] = new double[NBas];
  BLRates11[i] = new double[NBas];
  BLDelays00[i] = new double[NBas];
  BLDelays11[i] = new double[NBas];
  Weights[i] = new double[NBas];
};



int prevChan = Nchan[0];
int prevNvis = NVis[0]/NBas;



// FFT FOR EACH BASELINE:
int TotDim = NVis[0]*Nchan[0];
int MaxDim = TotDim;

for (j=1; j<NIF; j++){if(NVis[j]*Nchan[j]>MaxDim){MaxDim=NVis[j]*Nchan[j];};};

fftw_complex *BufferVis00 = (fftw_complex *) fftw_malloc(sizeof(fftw_complex) * MaxDim);
fftw_complex *BufferVis11 = (fftw_complex *) fftw_malloc(sizeof(fftw_complex) * MaxDim);
fftw_complex *AUX = (fftw_complex *) fftw_malloc(sizeof(fftw_complex) * MaxDim);

fftw_complex *outXX = (fftw_complex *) fftw_malloc(sizeof(fftw_complex) * MaxDim);
fftw_complex *outYY = (fftw_complex *) fftw_malloc(sizeof(fftw_complex) * MaxDim);


  
fftw_plan pXX = fftw_plan_dft_2d(prevNvis, Nchan[0], BufferVis00, outXX, FFTW_FORWARD, FFTW_MEASURE);
fftw_plan pYY = fftw_plan_dft_2d(prevNvis, Nchan[0], BufferVis11, outYY, FFTW_FORWARD, FFTW_MEASURE);

    


cplx64f *TempXX, *TempYY;
cplx64f *BufferCXX, *BufferCYY;


TempXX = reinterpret_cast<std::complex<double> *>(outXX);
TempYY = reinterpret_cast<std::complex<double> *>(outYY);

BufferCXX = reinterpret_cast<std::complex<double> *>(BufferVis00);
BufferCYY = reinterpret_cast<std::complex<double> *>(BufferVis11);



for(j=0; j<NBas; j++){  // baseline loop

for (i=0; i<NIF; i++){ // IF loop

  isFirst = true;
  showMe = false;

  BLRates00[i][j] = 0.0;
  BLRates11[i][j] = 0.0;
  BLDelays00[i][j] = 0.0;
  BLDelays11[i][j] = 0.0;

  int NcurrVis = 0;



// Arrange data for this baseline:
    for (k=0; k<NVis[i]; k++){
      a1 = Ant1[i][k];
      a2 = Ant2[i][k];
      BNum = BasNum[a1-1][a2-1];
      if (BNum==j && Scan[i][k]==cScan){
        if(isFirst){
          T0[j] = Times[i][k];
          T1[j] = Times[i][k];
          isFirst = false;
        };
        if (T0[j] > Times[i][k]){T0[j] = Times[i][k];};

        memcpy(&BufferCXX[NcurrVis*Nchan[i]],&RR[i][k][0],Nchan[i]*sizeof(cplx64f));
        memcpy(&BufferCYY[NcurrVis*Nchan[i]],&LL[i][k][0],Nchan[i]*sizeof(cplx64f));

        NcurrVis += 1;
        if (T1[j] < Times[i][k]){T1[j] = Times[i][k];};

      };
    };
   
   if (NcurrVis > 1) showMe = true;


 //   printf("READ A TOTAL OF  %i VISIBS.\n",NcurrVis);
   if (showMe) {
     sprintf(message,"   DoGFF: read %i visiblities\n",NcurrVis);
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
   }

/////////////////
// FFT the fringe and find the peak:
   if (NcurrVis > 1){
    sprintf(message,"FFT on baseline %i IF %i\n",j,i+1);
    fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  

// Re-define the FFTW plan if dimensions changed:
    if (Nchan[i] != prevChan || NcurrVis != prevNvis){


      prevChan = Nchan[i]; prevNvis = NcurrVis;

      memcpy(&AUX[0],&BufferVis00[0],NcurrVis*Nchan[i]*sizeof(fftw_complex));
      fftw_destroy_plan(pXX);  
      pXX = fftw_plan_dft_2d(NcurrVis, Nchan[i], BufferVis00, outXX, FFTW_FORWARD, FFTW_MEASURE);
      memcpy(&BufferVis00[0],&AUX[0],NcurrVis*Nchan[i]*sizeof(fftw_complex));

      memcpy(&AUX[0],&BufferVis11[0],NcurrVis*Nchan[i]*sizeof(fftw_complex));
      fftw_destroy_plan(pYY);
      pYY = fftw_plan_dft_2d(NcurrVis, Nchan[i], BufferVis11, outYY, FFTW_FORWARD, FFTW_MEASURE);
      memcpy(&BufferVis11[0],&AUX[0],NcurrVis*Nchan[i]*sizeof(fftw_complex));

    };

    fftw_execute(pXX); 
    fftw_execute(pYY); 

   };



  double Peak00 = 0.0;
  double Peak11 = 0.0;
  double AbsP;
  int nu00[3],ti00[3], nu11[3],ti11[3], row;

  int Chi, Chf, ti, tf;

  if (Nchan[i]>npix){Chi = npix/2; Chf = Nchan[i]-npix/2;}
              else  {Chi = Nchan[i]/2; Chf = Nchan[i]/2;};


  if (NcurrVis>npix){ti = npix/2; tf = NcurrVis-npix/2;}
              else  {ti = NcurrVis/2; tf = NcurrVis/2;};

  nu00[1] = 0; ti00[1] = 0; nu11[1] = 0; ti11[1] = 0;

  if (NcurrVis >1){
   sprintf(message,"Peaks on baseline %i IF %i\n",j,i+1);
   fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  

// First Quadrant:
  for (l=0; l<ti; l++){
    row = l*Nchan[i];
    for (k=0; k<Chi;k++){
      AbsP = std::abs(TempXX[row + k]);
      if (AbsP>Peak00){
         Peak00 = AbsP;
         nu00[1] = k; ti00[1] = l; 
      };
      AbsP = std::abs(TempYY[row + k]);
      if (AbsP>Peak11){
         Peak11 = AbsP;
         nu11[1] = k; ti11[1] = l; 
      };
    };
  };

printf("\n\nPEAK 1st: %.2e, %.2e / %i, %i\n",Peak00,Peak11,nu00[1],nu11[1]);

// Second Quadrant:
  for (l=tf; l<NcurrVis; l++){
    row = l*Nchan[i];
    for (k=0; k<Chi;k++){
      AbsP = std::abs(TempXX[row + k]);
      if (AbsP>Peak00){
         Peak00 = AbsP;
         nu00[1] = k; ti00[1] = l; 
      };
      AbsP = std::abs(TempYY[row + k]);
      if (AbsP>Peak11){
         Peak11 = AbsP;
         nu11[1] = k; ti11[1] = l; 
      };
    };
  };

printf("PEAK 2nd: %.2e, %.2e / %i, %i\n",Peak00,Peak11,nu00[1],nu11[1]);


// Third Quadrant:
  for (l=0; l<ti; l++){
    row = l*Nchan[i];
    for (k=Chf; k<Nchan[i];k++){
      AbsP = std::abs(TempXX[row + k]);
      if (AbsP>Peak00){
         Peak00 = AbsP;
         nu00[1] = k; ti00[1] = l; 
      };
      AbsP = std::abs(TempYY[row + k]);
      if (AbsP>Peak11){
         Peak11 = AbsP;
         nu11[1] = k; ti11[1] = l; 
      };
    };
  };


printf("PEAK 3rd: %.2e, %.2e / %i, %i\n",Peak00,Peak11,nu00[1],nu11[1]);


// Fourth Quadrant:
  for (l=tf; l<NcurrVis; l++){
    row = l*Nchan[i];
    for (k=Chf; k<Nchan[i];k++){
      AbsP = std::abs(TempXX[row + k]);
      if (AbsP>Peak00){
         Peak00 = AbsP;
         nu00[1] = k; ti00[1] = l; 
      };
      AbsP = std::abs(TempYY[row + k]);
      if (AbsP>Peak11){
         Peak11 = AbsP;
         nu11[1] = k; ti11[1] = l; 
      };
    };
  };

printf("PEAK 4th: %.2e, %.2e / %i, %i\n",Peak00,Peak11,nu00[1],nu11[1]);
fflush(stdout);


/////////
// Unwrap:
if (nu00[1]==0){nu00[0]=Nchan[i]-1;} else{nu00[0]=nu00[1]-1;};
if (nu11[1]==0){nu11[0]=Nchan[i]-1;} else{nu11[0]=nu11[1]-1;};
if (nu00[1]==Nchan[i]-1){nu00[2]=0;} else{nu00[2]=nu00[1]+1;};
if (nu11[1]==Nchan[i]-1){nu11[2]=0;} else{nu11[2]=nu11[1]+1;};

if (ti00[1]==0){ti00[0]=NcurrVis-1;} else{ti00[0]=ti00[1]-1;};
if (ti11[1]==0){ti11[0]=NcurrVis-1;} else{ti11[0]=ti11[1]-1;};
if (ti00[1]==NcurrVis-1){ti00[2]=0;} else{ti00[2]=ti00[1]+1;};
if (ti11[1]==NcurrVis-1){ti11[2]=0;} else{ti11[2]=ti11[1]+1;};

//////////////////

//printf("PEAKS NR: %i, %i, %i, %i\n",nu00[0],nu00[1], nu00[2]);
//printf("PEAKS TR: %i, %i, %i, %i\n",ti00[0],ti00[1], ti00[2]);

//printf("PEAKS NL: %i, %i, %i, %i\n",nu00[0],nu00[1], nu00[2]);
//printf("PEAKS TL: %i, %i, %i, %i\n",ti00[0],ti00[1], ti00[2]);

////////////////////
// Estimate the rate with sub-bin precision:

    aroundPeak00[0] = TempXX[nu00[1] + Nchan[i]*(ti00[0])];
    aroundPeak00[1] = TempXX[nu00[1] + Nchan[i]*(ti00[1])];
    aroundPeak00[2] = TempXX[nu00[1] + Nchan[i]*(ti00[2])];
    BLRates00[i][j] = ((double) ti00[1]);
    BLRates00[i][j] += QuinnEstimate(aroundPeak00);


  if (BLRates00[i][j] > ((double) NcurrVis)/2.){
    BLRates00[i][j] = BLRates00[i][j] - (double) NcurrVis;
  }; 


  BLRates00[i][j] *= 1./(T1[j]-T0[j]);


    aroundPeak11[0] = TempYY[nu11[1] + Nchan[i]*(ti11[0])];
    aroundPeak11[1] = TempYY[nu11[1] + Nchan[i]*(ti11[1])];
    aroundPeak11[2] = TempYY[nu11[1] + Nchan[i]*(ti11[2])];
    BLRates11[i][j] = ((double) ti11[1]);
    BLRates11[i][j] += QuinnEstimate(aroundPeak11);

//printf("aroundPeak: %.2e %.2e %.2e %.2e %.2e %.2e\n",aroundPeak00[0],aroundPeak00[1],aroundPeak00[2],aroundPeak11[0],aroundPeak11[1],aroundPeak11[2]);


  if (BLRates11[i][j] > ((double) NcurrVis)/2.){
    BLRates11[i][j] = BLRates11[i][j] - (double) NcurrVis;
  }; 

  BLRates11[i][j] *= 1./(T1[j]-T0[j]);

  //  printf("IF %i, RR: %i -> %.2e with dT = %.2f\n",i,ti00[1], BLRates00[i][j],(T1[j]-T0[j]));
  //  printf("IF %i, LL: %i -> %.2e with dT = %.2f\n",i,ti11[1], BLRates11[i][j],(T1[j]-T0[j]));


////////////////////
// Estimate the delay with sub-bin precision:

    aroundPeak00[0] = TempXX[nu00[0] + Nchan[i]*(ti00[1])];
    aroundPeak00[1] = TempXX[nu00[1] + Nchan[i]*(ti00[1])];
    aroundPeak00[2] = TempXX[nu00[2] + Nchan[i]*(ti00[1])];
    BLDelays00[i][j] = ((double) nu00[1]);
    BLDelays00[i][j] += QuinnEstimate(aroundPeak00);


  if (BLDelays00[i][j] > ((double) Nchan[i])/2.){
    BLDelays00[i][j] = BLDelays00[i][j] - (double) Nchan[i];
  }; 

  BLDelays00[i][j] *= 1./(Frequencies[i][Nchan[i]-1]-Frequencies[i][0]);


    aroundPeak11[0] = TempYY[nu11[0] + Nchan[i]*(ti11[1])];
    aroundPeak11[1] = TempYY[nu11[1] + Nchan[i]*(ti11[1])];
    aroundPeak11[2] = TempYY[nu11[2] + Nchan[i]*(ti11[1])];
    BLDelays11[i][j] = ((double) nu11[1]);
    BLDelays11[i][j] += QuinnEstimate(aroundPeak11);

  if (BLDelays11[i][j] > ((double) Nchan[i])/2.){
    BLDelays11[i][j] = BLDelays11[i][j] - (double) Nchan[i];
  }; 

  BLDelays11[i][j] *= 1./(Frequencies[i][Nchan[i]-1]-Frequencies[i][0]);

  Weights[i][j] = 1.0;

  } else {   // Comes from if(NcurrVis > 2)
     BLRates00[i][j] = 0.0; BLRates11[i][j] = 0.0; 
     BLDelays00[i][j] = 0.0; BLDelays11[i][j] = 0.0; 
     Weights[i][j] = 0.0;

     sprintf(message,"WARNING! BASELINE %i HAS NO DATA IN IF %i ",j,i+1);
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  

     for (gotAnts = false, a1=0; a1<NCalAnt; a1++){
       for (a2=a1+1;a2<NCalAnt;a2++){
         if(j == BasNum[CalAnts[a1]-1][CalAnts[a2]-1]){
          sprintf(message,"ANTS: %i-%i!\n",a1+1,a2+1);
          fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
          gotAnts = true;
          break;
         };
       };
     };
     if (!gotAnts) {
       sprintf(message,"NO-ANTS:\n");
       fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
     }

  };

//////
  if (showMe) {
    printf(
      "RR FRINGE PEAK OF %.5e AT INDEX %i-%i (RATE %.5e Hz, DELAY: %.5e s)\n",
      Peak00,ti00[1],nu00[1],BLRates00[i][j],BLDelays00[i][j]);
    printf(
      "LL FRINGE PEAK OF %.5e AT INDEX %i-%i (RATE %.5e Hz, DELAY: %.5e s)\n",
      Peak11,ti11[1],nu11[1],BLRates11[i][j],BLDelays00[i][j]);
  };

  }; // end IF loop
////////////////////


}; // end baseline loop



/*
NcalAnt = 0;
calAnt = new int[Nant];
bool GoodRef = false;


for (i=0; i<Nant; i++){ 
  for(j=0;j<Nant;j++){ 
     if((NData[BasNum[i][j]]>0 || NData[BasNum[j][i]]>0)&& i!=j && (Weights[BasNum[i][j]]>0. ||  Weights[BasNum[j][i]]>0.)){
       if(i==REFANT || j==REFANT){GoodRef=true;};
       if(i!=REFANT){calAnt[NcalAnt] = i; NcalAnt ++; break;};
     };  
  };
};
*/




/////////////////
// Globalize the rate and delay solutions:

sprintf(message,"# of free antenna gains (per pol.): %i\n",NantFit);
fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  


double *Hessian = new double[NantFit*NantFit];
double *RateResVec = new double[NantFit];
double *DelResVec00 = new double[NantFit];
double *DelResVec11 = new double[NantFit];
delete[] CovMat;
CovMat = new double[NantFit*NantFit];

for (i=0;i<NantFit*NantFit;i++){
  Hessian[i] = 0.0;
  CovMat[i] = 0.0;
};

for (i=0;i<NantFit;i++){
  RateResVec[i] = 0.0;
  DelResVec00[i] = 0.0;
  DelResVec11[i] = 0.0;
};



////////////////////////////////////
// AT THE MOMENT, WE COMBINE ALL IFS FOR THE SAME RATES AND DELAYS:

if (NantFit>1){

for (i=0; i<NIF; i++){
  for (a1=0; a1<NCalAnt; a1++){
    
    for (a2=a1+1;a2<NCalAnt;a2++){

       af1 = -1; af2 = -1;
       for(j=0; j<NantFit; j++) {
         if (CalAnts[a1]==antFit[j]){af1 = j;};
         if (CalAnts[a2]==antFit[j]){af2 = j;};
       };

       // look up about 1500 lines to flagging baselines
       BNum = BasNum[CalAnts[a1]-1][CalAnts[a2]-1];
       for(j=0;j<Ntwin;j++){
         if((Twins[0][j]==CalAnts[a1] && Twins[1][j]==CalAnts[a2])||(Twins[0][j]==CalAnts[a2] && Twins[1][j]==CalAnts[a1])){
           BNum = -1; break;
         };
      };

      if (BNum>0){

       if (af1>=0){
         RateResVec[af1] += Weights[i][BNum]*(BLRates00[i][BNum] + BLRates11[i][BNum])/2.;
//z      // printf("%i | %i-%i %.2e  %.2e \n",i, CalAnts[a1],CalAnts[a2], BLRates00[i][BNum],BLRates11[i][BNum]);
         DelResVec00[af1] += Weights[i][BNum]*BLDelays00[i][BNum];
         DelResVec11[af1] += Weights[i][BNum]*BLDelays11[i][BNum];
         Hessian[af1*NantFit + af1] += Weights[i][BNum];
       };
       if (af2>=0){
         RateResVec[af2] += Weights[i][BNum]*(-BLRates00[i][BNum] - BLRates11[i][BNum])/2.;
         DelResVec00[af2] -= Weights[i][BNum]*BLDelays00[i][BNum];
         DelResVec11[af2] -= Weights[i][BNum]*BLDelays11[i][BNum];
         Hessian[af2*NantFit + af2] += Weights[i][BNum];
       };
       if (af1>=0 && af2>=0){
         Hessian[af1*NantFit + af2] += -Weights[i][BNum];
         Hessian[af2*NantFit + af1] += -Weights[i][BNum];
       };
      };
    };
  };
};
/////////////////////////////////


} else {

  BNum = BasNum[CalAnts[0]-1][CalAnts[1]-1];

  for (i=0; i<NIF; i++){
    Hessian[0] += Weights[i][BNum];
    RateResVec[0] += Weights[i][BNum]*(BLRates00[i][BNum] + BLRates11[i][BNum])/2.;
    DelResVec00[0] += Weights[i][BNum]*BLDelays00[i][BNum];
    DelResVec11[0] += Weights[i][BNum]*BLDelays11[i][BNum];

    for (j=0; j<NCalAnt; j++){
      Rates[i][j][cScan] = 0.0;
      Delays00[i][j][cScan] = 0.0;
      Delays11[i][j][cScan] = 0.0;

      if(CalAnts[j]==antFit[0]){
        if (applyRate>0){Rates[i][j][cScan] = -RateResVec[0]/((double) NIF);};
        Delays00[i][j][cScan] = -DelResVec00[0]/((double) NIF);
        Delays11[i][j][cScan] = -DelResVec11[0]/((double) NIF);
      };
    };
  };
};


printf("\n\nHessian Globalization Matrix:\n\n");
//zz bool isSingular, tempSing;
bool isSingular;
isSingular=false;
for (i=0; i<NantFit; i++){
  printf("  ");
//z  tempSing = true;
  if (Hessian[i*NantFit+i]==0.0){isSingular=true;};
  for (j=0; j<NantFit; j++){
//z     if (Hessian[i*NantFit+j]!=0.0){tempSing=false;};
     printf("%+.2e ",Hessian[i*NantFit+j]);
  };
//z  if (tempSing){isSingular=true;};
//z printf("\n");
//z };
printf("\n");
};
if(isSingular){printf("Possible singular matrix!\n");};
printf("\n");

printf("\n\n Residual baseline phase quantities:\n\n");
for (i=0; i<NantFit; i++){
  printf("  ");
  printf(" %.2e | %.2e | %.2e\n",DelResVec00[i],DelResVec11[i],RateResVec[i]);
};

// GBC added these which may now be redundant
if (!isSingular) printf("\n(not Singular)\n");
else             printf("\n(is  Singular)\n");

printf("\n");

// The Hessian's inverse can be reused for rates, delays and phases!
gsl_death_by = GSL_SUCCESS;

gsl_matrix_view mm = gsl_matrix_view_array (Hessian, NantFit, NantFit);
//gsl_matrix_view inv = gsl_matrix_view_array(CovMat,NantFit,NantFit);
gsl_vector *xx = gsl_vector_calloc(NantFit);
gsl_vector *dd0 = gsl_vector_calloc(NantFit);
gsl_vector *dd1 = gsl_vector_calloc(NantFit);

gsl_vector_view RateInd = gsl_vector_view_array(RateResVec,NantFit);
gsl_vector_view Del00Ind = gsl_vector_view_array(DelResVec00,NantFit);
gsl_vector_view Del11Ind = gsl_vector_view_array(DelResVec11,NantFit);

printf("  RateResVec DelResVec00 DelResVec11\n");
for (i=0; i<NantFit; i++){
  printf(" %+.4e %+.4e %+.4e\n",
    RateResVec[i], DelResVec00[i], DelResVec11[i]);
}


int s;

gsl_permutation *permm = gsl_permutation_alloc (NantFit);

printf("doing gsl_linalg_LU_decomp...\n");
gsl_linalg_LU_decomp (&mm.matrix, permm, &s);
printf("...done (%d)\n", gsl_death_by);

if (gsl_death_by != GSL_SUCCESS) {
    printf("premature exit 11\n");
    fflush(stdout); std::cout << std::flush;
    PyObject *ret = Py_BuildValue("i",-11);
    return ret;
}

if(!isSingular){
//z     printf("Globalizing solutions\n");
        printf("Globalizing solutions: ");
        printf("doing gsl_linalg_LU_solve 3x...\n");
	gsl_linalg_LU_solve (&mm.matrix, permm, &RateInd.vector, xx);
	gsl_linalg_LU_solve (&mm.matrix, permm, &Del00Ind.vector, dd0);
	gsl_linalg_LU_solve (&mm.matrix, permm, &Del11Ind.vector, dd1);
};
gsl_permutation_free(permm);

if (gsl_death_by != GSL_SUCCESS) {
    printf("premature exit 12\n");
    fflush(stdout); std::cout << std::flush;
    PyObject *ret = Py_BuildValue("i",-12);
    return ret;
}

//gsl_linalg_LU_invert (&m.matrix, perm, &inv.matrix);

// shut down any nans
int nancounter = 0;
for (i=0; i<NantFit; i++){
  if (!std::isnormal(gsl_vector_get(xx,i))) {
    gsl_vector_set(xx,i,0.0);
    nancounter++;
  }
  if (!std::isnormal(gsl_vector_get(dd0,i))) {
    gsl_vector_set(dd0,i,0.0);
    nancounter++;
  }
  if (!std::isnormal(gsl_vector_get(dd1,i))) {
    gsl_vector_set(dd1,i,0.0);
    nancounter++;
  }
}
printf("%d nans were zeroed\n", nancounter);
for (i=0; i<NantFit; i++){
   printf(" %+.4e %+.4e %+.4e\n",
    gsl_vector_get(xx,i), gsl_vector_get(dd0,i), gsl_vector_get(dd1,i));
}

// Derive the rates as CovMat*RateVec and delays as CovMat*DelVec:

if (NantFit>1){


for (i=0; i<NCalAnt; i++){
  Rates[0][i][cScan] = 0.0;
  Delays00[0][i][cScan] = 0.0;
  Delays11[0][i][cScan] = 0.0;
  af1 = -1;
  for(j=0; j<NantFit; j++) {
    if (CalAnts[i]==antFit[j]){af1 = j;};
  };
  if (af1 >=0){
     if(applyRate>0){
	     Rates[0][i][cScan] = -gsl_vector_get(xx,af1);
	     Delays00[0][i][cScan] = -gsl_vector_get(dd0,af1);
	     Delays11[0][i][cScan] = -gsl_vector_get(dd1,af1);
     };
  };

};

for (i=0; i<NIF; i++){
  for (j=0; j<NCalAnt; j++){
    Rates[i][j][cScan] = Rates[0][j][cScan];
    Delays00[i][j][cScan] = Delays00[0][j][cScan];
    Delays11[i][j][cScan] = Delays11[0][j][cScan];
  };
};



};






for (i=0; i<NCalAnt; i++){

//  sprintf(message,"Antenna %i -> Rate: %.3e Hz; RR Delay: %.3e s; LL Delay: %.3e s\n",CalAnts[i],Rates[0][i][cScan],Delays00[0][i][cScan],Delays11[0][i][cScan]);
  sprintf(message,"Antenna %i -> Rate: %.3e Hz.  Delay:  %.3e ns.\n",CalAnts[i],Rates[0][i][cScan],(Delays00[0][i][cScan]+Delays11[0][i][cScan])*0.5*1.e9);
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
};



double MaxRateDev = -1.0;
//int BadA1 = -1; int BadA2 = -1;
//int BadIF = -1;
int BadA1R = -1; int BadA2R = -1;
int BadIFR = -1; int BadBR = -1;

double TempD;

for (i=0; i<NIF; i++){
  for (a1=0; a1<NCalAnt; a1++){
    for (a2=a1+1;a2<NCalAnt;a2++){
       BNum = BasNum[CalAnts[a1]-1][CalAnts[a2]-1];
       TempD = fabs(BLRates00[i][BNum] + (Rates[i][a1][cScan] - Rates[i][a2][cScan]));
       if (TempD > MaxRateDev){
          MaxRateDev=TempD; BadIFR=i;BadA1R=a1;BadA2R=a2; BadBR = BNum;};
       TempD = fabs(BLRates11[i][BNum] + (Rates[i][a1][cScan] - Rates[i][a2][cScan]));
       if (TempD > MaxRateDev){
          MaxRateDev=TempD; BadIFR=i;BadA1R=a1;BadA2R=a2; BadBR = BNum;};
    };
  };
};

//if (NantFit>1){

  sprintf(message,"\n\nMax. fringe deviation (rate): Bas. %i-%i at IF %i (%.1e deg. across window)\n",CalAnts[BadA1R],CalAnts[BadA2R],IFNum[BadIFR],fabs(MaxRateDev*(T1[BadBR]-T0[BadBR])*180.));
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  

//};


/////////////////

fftw_free(BufferVis00); fftw_free(BufferVis11); fftw_free(outXX); fftw_free(outYY);

//delete[] antFit;
delete[] T0;
delete[] T1;
delete[] aroundPeak00;
delete[] aroundPeak11;

for (i=0; i<NIF;i++){
  delete[] BLRates00[i];
  delete[] BLRates11[i];
  delete[] BLDelays00[i];
  delete[] BLDelays11[i];
  delete[] Weights[i];
};

delete[] BLRates00;
delete[] BLRates11;
delete[] BLDelays00;
delete[] BLDelays11;
delete[] Weights;

delete[] Hessian;
delete[] RateResVec;
delete[] DelResVec00;
delete[] DelResVec11;
//delete[] CovMat;



//Py_DECREF(antList);


PyObject *ret = Py_BuildValue("i",0);
return ret;

};




































static PyObject *SetFit(PyObject *self, PyObject *args) {

  int i, j, k, oldNpar = Npar;
  bool foundit;

  if (!logFile) logFile = fopen("PolConvert.GainSolve.log","a");
  sprintf(message,"Entered SetFit with NBas %d oldNpar %d\n\n", NBas, oldNpar);
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
  std::cout << std::flush;

  PyObject *IFlist, *antList, *calstokes, *ret, *feedPy;
  if (!PyArg_ParseTuple(args, "iOOiiOiO",
    &Npar, &IFlist, &antList, &solveAmp, &solveQU, &calstokes, &useCov, &feedPy)){
     sprintf(message,"Failed SetFit! Check inputs!\n"); 
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
     fclose(logFile);
     ret = Py_BuildValue("i",-1);
    return ret;
  };

if (Tm) {
  sprintf(message,"Clearing previous allocation objects\n");
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
  delete[] Tm;
  delete[] doIF;
  delete[] antFit;
  delete[] CovMat;
  delete[] IndVec;
  delete[] SolVec;
  delete[] G1;
  delete[] G2;
  delete[] G1nu;
  delete[] G2nu;
  for(i=0;i<oldNpar+1;i++){delete[] DStokes[i];};
  delete[] DStokes;
  delete[] DirDer;
  delete[] MBD1;
  delete[] MBD2;
  delete[] DerIdx;
  delete[] AvVis;
}
//  delete[] feedAngle;
  sprintf(message,"Allocation objects cleared\n");
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

  Tm = new double[NBas];

  T0 = Times[0][0];
  T1 = Times[0][NVis[0]-1];
  DT = (T1-T0)/TAvg;

  feedAngle = (double *)PyArray_DATA(feedPy);

  NIFComp = (int) PyList_Size(IFlist);
  doIF = new int[NIFComp];
//  printf("\nNIFComp: %i\n",NIFComp);

  NantFit = (int) PyList_Size(antList);
  antFit = new int[NantFit];

  for (i=0; i<NantFit; i++){
    antFit[i] = (int) PyInt_AsLong(PyList_GetItem(antList,i));
  };

for (i=0; i<NIFComp; i++){
  foundit = false;
  j = (int) PyInt_AsLong(PyList_GetItem(IFlist,i));
  for (k=0; k<NIF;k++){
    if (j==IFNum[k]){doIF[i] = k; foundit=true; break;};
  };
  if (!foundit){
     sprintf(message,"BAD IF NUMBER: %i\n",j); 
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
     fclose(logFile);
     ret = Py_BuildValue("i",-1);
     return ret;
  };
};



CovMat = new double[Npar*Npar];
IndVec = new double[Npar];
SolVec = new double[Npar];
m = gsl_matrix_view_array (CovMat, Npar, Npar);
v = gsl_vector_view_array (IndVec, Npar);
x = gsl_vector_view_array (SolVec, Npar);
perm = gsl_permutation_alloc (Npar);

//////////////////////
// if calstokes[0]<0, it means we are SOLVING for Stokes!
// unused:
// double tempD = (double) PyFloat_AsDouble(PyList_GetItem(calstokes,0));
StokesSolve = solveQU;
//if(!StokesSolve){
  for (i=0; i<4; i++){
    Stokes[i] = (double) PyFloat_AsDouble(PyList_GetItem(calstokes,i));
  };
//};

//////////////////////

G1 = new cplx64f[Npar+1];
G2 = new cplx64f[Npar+1]; 
DStokes = new double*[Npar+1];
DirDer = new double[Npar];

MBD1 = new double[Npar+1];
MBD2 = new double[Npar+1];
G1nu = new cplx64f[Npar+1];
G2nu = new cplx64f[Npar+1];
for(i=0;i<Npar+1;i++){DStokes[i]=new double[4];};

DerIdx = new int[Npar+1];
AvVis = new int[NBas];

ret = Py_BuildValue("i",0);
return ret;

};





















static PyObject *GetChi2(PyObject *self, PyObject *args) {

//int NIFComp;
int Ch0, Ch1;
// int i,j,k,l, end;    // original code
int i,k,l, end;
int j = -1;          // used in sprintf below, but never assigned.
double *CrossG;
//int *doIF;



//double *Tm = new double[NBas];

double dx = 1.0e-8;

double Drate1, Drate2, Ddelay1, Ddelay2; 
double *DerAux1, *DerAux2;

DerAux1 = new double[2];
DerAux2 = new double[2];

// unused:
// bool CohAvg = true; // Coherent summ for Chi2  ??
// unused:
// bool FlipIt = false; // Flip 180 degrees the gains (in case R <-> L at all antennas).

PyObject *pars, *ret,*LPy;


if (!logFile) logFile = fopen("PolConvert.GainSolve.log","a");
if (!PyArg_ParseTuple(args, "OOiii", &pars, &LPy, &Ch0, &Ch1,&end)){
     sprintf(message,"Failed GetChi2! Check inputs!\n"); 
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
     fclose(logFile);
    ret = Py_BuildValue("i",-1);
    return ret;
};

chisqcount ++;

  Lambda = PyFloat_AsDouble(LPy);
  doCov = Lambda >= 0.0;

// Find out IFs to compute and do sanity checks:

for (i=0; i<NIFComp; i++){
  if (Ch1 > Nchan[doIF[i]]){
     sprintf(message,"IF %i ONLY HAS %i CHANNELS. CHANNEL %i DOES NOT EXIST! \n",j,Nchan[doIF[i]], Ch1); 
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
     fclose(logFile);
     ret = Py_BuildValue("i",-1);
     return ret;
  };
};

if (Ch0<0 || Ch0>Ch1){
 sprintf(message,"BAD CHANNEL RANGE: %i TO %i. SHOULD ALL BE POSITIVE AND Ch0 < Ch1\n",Ch0,Ch1); 
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
     fclose(logFile);
    ret = Py_BuildValue("i",-1);
    return ret;
};



// Reference frequency for the MBD:
double RefNu = Frequencies[doIF[0]][0];



//Npar = PyArray_DIM(pars,0);
CrossG = (double *) PyArray_DATA(pars);

int s;







if(StokesSolve){
  Stokes[0] = 1.0; Stokes[3] = 0.0;
  for (i=1; i<3; i++){Stokes[i] = CrossG[Npar-3+i];};
};







int currIF, a1, a2, ac1,ac2,af1, af2, currDer, cscan;
int is1, is2, auxI1, auxI2;

auxI1 = 0;



cplx64f Error;
cplx64f oneC, RateFactor, FeedFactor1, FeedFactor2, RRRate, RLRate, LRRate, LLRate; 

cplx64f RM1, RP1; 
cplx64f RM2, RP2; 
cplx64f auxC1, auxC2, auxC3;
cplx64f *AvPA1 = new cplx64f[NBas]; 
cplx64f *AvPA2 = new cplx64f[NBas];

int Nflipped = 0;

double ParHandWgt = 1.0;
double CrossHandWgt = 1.0;
double BasWgt = 1.0;
double Itot = 0.0;
// Avoid overflow errors to very large RelWeights (i.e., divide the crosshand
// instead of multipyling the parhand)
if (RelWeight<1.0){ParHandWgt=RelWeight;}else{CrossHandWgt=1./RelWeight;};


for(i=0;i<Npar;i++){
  IndVec[i] = 0.0;
  for(j=0;j<Npar;j++){
    CovMat[i*Npar+j]=0.0;
  };
};

oneC= cplx64f(1.0,0.0);



int Nder = 0;

int MBD1p, MBD2p, G1pA,G1pF, G2pA, G2pF;

int BNum;

double Chi2 = 0.0;
double auxD1, auxD2;
// double TRatio = 1.0;


/*
printf("\nThere are %i baselines.\n",NBas);
for (i=0;i<NBas;i++){printf(" %.3e ",Tm[i]);};
printf("\nThere are %i IFs.\n",NIFComp);
for (i=0;i<NIFComp;i++){printf(" %i ",doIF[i]);};
printf("\nThere are %i fittable antennas.\n",NantFit);
for (i=0;i<NantFit;i++){printf(" %i ",antFit[i]);};
*/












for(l=0;l<Npar+1;l++){
  for(i=0;i<4;i++){DStokes[l][i] = Stokes[i];};
};



for (i=0; i<NIFComp; i++){
 

  currIF = doIF[i];

  for (k=0;k<NBas;k++){
   for(j=0;j<Npar+1;j++){
    auxC00[k][j] = cplx64f(0., 0.);
    auxC01[k][j] = cplx64f(0., 0.);
    auxC10[k][j] = cplx64f(0., 0.);
    auxC11[k][j] = cplx64f(0., 0.);
   };
    AvVis[k] = 0;
    AvPA1[k] = 0.0; AvPA2[2] = 0.0;
  };



  for(k=0;k<NBas;k++){Tm[k]=Times[currIF][0];};

//z
// printf("NVis: %i\n",NVis[currIF]); fflush(stdout);

  for (k=0; k<NVis[currIF]; k++){

    a1 = Ant1[currIF][k];
    a2 = Ant2[currIF][k];
    cscan = Scan[currIF][k];

    ac1 = -1; ac2 = -1;
    for(j=0; j<NCalAnt; j++) {
      if (a1==CalAnts[j]){ac1 = j;};
      if (a2==CalAnts[j]){ac2 = j;};
    };

    
    af1 = -1; af2 = -1;
    for(j=0; j<NantFit; j++) {
      if (a1==antFit[j]){af1 = j;};
      if (a2==antFit[j]){af2 = j;};
    };










///////////////////////////////////////////////
//
// ONLY IF THE ANTENNAS ARE TO BE FITTED:


// Find which antenna(s) are linear:
    is1 = false ; is2 = false;
    BasWgt = 1.0;

    if (++twincounter < 3) {
      sprintf(message, "THERE ARE %i TWINS.\n",Ntwin);
      fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
    }

    for(j=0; j<Ntwin; j++){
       sprintf(message,
        "Checking %i-%i to %i-%i\n",Twins[0][j],Twins[1][j],a1,a2);
       fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
       if ((Twins[0][j]==a1 && Twins[1][j]==a2) ||
           (Twins[0][j]==a2 && Twins[1][j]==a1)){
          if (twincounter < 3) {
            sprintf(message, "Found Twins!\n");
            fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
          }
          BasWgt=0.0;
          break;
       };
    };

    for(j=0; j<Nlin; j++) {
      if (a1==Lant[j]){is1 = true;};
      if (a2==Lant[j]){is2 = true;};
    };

    
    

    
 //   if(is1||is2){BasWgt*=100.;};

// The crossGains at the ref. channel: 

    Nder = 1;
    DerIdx[0] = 0;
     
    if (solveAmp==0){
        
      if (af1 >= 0){
        G1pA = -1; G1pF = af1;
        G1[0] = std::polar(1.0, CrossG[af1]); 

        if(doCov){
        for(j=1;j<Npar+1;j++){
          G1[j] = G1[0]; 
        };
        G1[G1pF+1] = std::polar(1.0, CrossG[af1]+dx);
        DerIdx[Nder] = G1pF+1;
        Nder += 1;};


      } else { G1[0] = std::polar(1.0, 0.0); G1pA = -1; G1pF = -1;}; 

      if (af2 >= 0){
        G2pA = -1; G2pF = af2;
        G2[0] = std::polar(1.0, -CrossG[af2]); 

        if(doCov){
        for(j=1;j<Npar+1;j++){
          G2[j] = G2[0]; 
        };
        G2[G2pF+1] = std::polar(1.0, -CrossG[af2]-dx);
        DerIdx[Nder] = G2pF+1;
        Nder += 1;};


      } else { G2[0] = std::polar(1.0, 0.0); G2pA = -1; G2pF = -1;}; 

  //    printf("G: %.3e , %.3e ; %i , %i \n",CrossG[af1],CrossG[af2], af1, af2);
      
    } else {

      if (af1 >= 0){
        G1pA = af1*2; G1pF = G1pA+1;
//        G1[0] = cplx64f(CrossG[G1pA], CrossG[G1pF]);   // RE+IM SPACE
        G1[0] = std::polar(CrossG[G1pA], CrossG[G1pF]);  // AMP+PHASE SPACE

        if(doCov){
        for(j=1;j<Npar+1;j++){
          G1[j] = G1[0]; 
        };
//        G1[G1pA+1] = cplx64f(CrossG[G1pA]+dx, CrossG[G1pF]);  // RE+IM SPACE
        G1[G1pA+1] = std::polar(CrossG[G1pA]+dx, CrossG[G1pF]); // AMP+PHASE SPACE
//        G1[G1pF+1] = cplx64f(CrossG[G1pA], CrossG[G1pF]+dx); // RE+IM SPACE
        G1[G1pF+1] = std::polar(CrossG[G1pA], CrossG[G1pF]+dx); // AMP+PHASE SPACE


        DerIdx[Nder] = G1pA+1;
        DerIdx[Nder+1] = G1pF+1;
        Nder += 2;};


      } else { G1[0] = std::polar(1.0, 0.0); G1pA=-1; G1pF=-1;};

      if (af2 >= 0){
        G2pA = af2*2; G2pF = G2pA+1;
//        G2[0] = cplx64f(CrossG[G2pA], -CrossG[G2pF])  ; // RE+IM SPACE
        G2[0] = std::polar(CrossG[G2pA], -CrossG[G2pF]); // AMP+PHASE SPACE

        if(doCov){
        for(j=1;j<Npar+1;j++){
          G2[j] = G2[0]; 
        };
//        G2[G2pA+1] = cplx64f(CrossG[G2pA]+dx, -CrossG[G2pF]); // RE+IM SPACE
        G2[G2pA+1] = std::polar(CrossG[G2pA]+dx, -CrossG[G2pF]); // AMP+PHASE SPACE
//        G2[G2pF+1] = cplx64f(CrossG[G2pA], -CrossG[G2pF]-dx);  // RE+IM SPACE
        G2[G2pF+1] = std::polar(CrossG[G2pA], -CrossG[G2pF]-dx); // AMP+PHASE SPACE

        DerIdx[Nder] = G2pA+1;
        DerIdx[Nder+1] = G2pF+1;
        Nder += 2;};


      } else { G2[0] = std::polar(1.0, 0.0);G2pA=-1; G2pF=-1;}; 

    };


    BNum = BasNum[a1-1][a2-1];



    if(BNum>=0){

//z
    //  printf("Fitting for %i %i\n",a1,a2); fflush(stdout);

    AvVis[BNum] += 1;

    FeedFactor1 = std::polar(1.0, feedAngle[a1-1])*PA1[currIF][k]; 
    FeedFactor2 = std::polar(1.0, feedAngle[a2-1])*PA2[currIF][k];

    AvPA1[BNum] += FeedFactor1;
    AvPA2[BNum] += FeedFactor2;


 //   for (j=Ch0; j<Ch1+1; j++) 
    for (j=Ch0; j<Ch1; j++){

    




// Add the multi-band delays:
     if (SolAlgor == 0){
      if (af1 >= 0){
        if(solveAmp==0){
          MBD1p = NantFit+af1;
          MBD1[0] = CrossG[MBD1p]*(Frequencies[currIF][j]-RefNu);
        } else {
          MBD1p = NantFit*2+af1;
          MBD1[0] = CrossG[MBD1p]*(Frequencies[currIF][j]-RefNu);
      };
      if (j==Ch0 && doCov){MBD1[MBD1p+1] = CrossG[MBD1p] + dx; DerIdx[Nder] = MBD1p+1; Nder += 1;};
      } else {MBD1[0] = 0.0; MBD1p=-1;};
      if (af2 >= 0){
        if(solveAmp==0){
          MBD2p = NantFit+af2;
          MBD2[0] = CrossG[MBD2p]*(Frequencies[currIF][j]-RefNu);
        } else {
          MBD2p = NantFit*2+af2;
          MBD2[0] = CrossG[NantFit*2+af2]*(Frequencies[currIF][j]-RefNu);
      };
      if (j==Ch0 && doCov){MBD2[MBD2p+1] = CrossG[MBD2p] + dx; DerIdx[Nder] = MBD2p+1; Nder += 1;};
      } else {MBD2[0] = 0.0; MBD2p = -1;};

       G1nu[0] = G1[0]*(std::polar(1.0,MBD1[0]));
       G2nu[0] = G2[0]*(std::polar(1.0,-MBD2[0]));


       if(doCov){
       for (l=1;l<Npar+1;l++){
         G1nu[l] = G1nu[0];
         G2nu[l] = G2nu[0];
       };

       G1nu[G1pA+1] = G1[G1pA+1]*(std::polar(1.0,MBD1[0]));
       G2nu[G2pA+1] = G2[G2pA+1]*(std::polar(1.0,-MBD2[0]));
       G1nu[G1pF+1] = G1[G1pF+1]*(std::polar(1.0,MBD1[0]));
       G2nu[G2pF+1] = G2[G2pF+1]*(std::polar(1.0,-MBD2[0]));
       G1nu[MBD1p+1] = G1[0]*(std::polar(1.0,MBD1[MBD1p+1]*(Frequencies[currIF][j]-RefNu)));
       G2nu[MBD2p+1] = G2[0]*(std::polar(1.0,-MBD2[MBD2p+1]*(Frequencies[currIF][j]-RefNu)));
       };

     } else {

     MBD1p=-1;MBD2p=-1;
     G1nu[0] = G1[0]; 
     G2nu[0] = G2[0]; 

     if(doCov){
     for (l=1;l<Npar+1;l++){
        G1nu[l] = G1nu[0];
        G2nu[l] = G2nu[0];
     };
     G1nu[G1pA+1] = G1[G1pA+1];
     G2nu[G2pA+1] = G2[G2pA+1];
     G1nu[G1pF+1] = G1[G1pF+1];
     G2nu[G2pF+1] = G2[G2pF+1];
     };

     };



if(StokesSolve){
  DerIdx[Nder]=Npar-1; DStokes[Npar-1][1] = DStokes[0][1]+dx; Nder += 1;
  DerIdx[Nder]=Npar; DStokes[Npar][2] = DStokes[0][2]+dx; Nder += 1;
};










// Compute the instrumental phases (for all pol. products):


    if(ac1>=0){
            Ddelay1 = TWOPI*((Delays00[0][ac1][cscan]+Delays11[0][ac1][cscan])*0.5*(Frequencies[currIF][j]-RefNu));
	    Drate1 = TWOPI*(Rates[0][ac1][cscan]*(Times[currIF][k]-T0));} else {Ddelay1=0.0;Drate1=0.0;};
    if(ac2>=0){
            Ddelay2 = TWOPI*((Delays00[0][ac2][cscan]+Delays11[0][ac2][cscan])*0.5*(Frequencies[currIF][j]-RefNu));	    
	    Drate2 = TWOPI*(Rates[0][ac2][cscan]*(Times[currIF][k]-T0));} else {Ddelay2=0.0;Drate2=0.0;};

    RateFactor = std::polar(1.0, Drate1-Drate2 + Ddelay1-Ddelay2);
    //   printf("\n\n  RATES:  %.3e  %.3e  %.3e  %.3e  | %.3e %.3e\n\n",Drate1,Drate2,Ddelay1,Ddelay2, RateFactor.real(), RateFactor.imag());

    RRRate = RateFactor*FeedFactor1/FeedFactor2; 
    RLRate = RateFactor*FeedFactor1*FeedFactor2; 
    LRRate = RateFactor/FeedFactor1/FeedFactor2; 
    LLRate = RateFactor/FeedFactor1*FeedFactor2; 

/*
    RRRate = FeedFactor1/FeedFactor2;
    RLRate = FeedFactor1*FeedFactor2;
    LRRate = oneC/FeedFactor1/FeedFactor2; 
    LLRate = oneC/FeedFactor1*FeedFactor2; 
*/






// Compute all derivatives:

    for (l=0; l<Nder; l++){
      currDer = DerIdx[l];
    //  printf("Doing der %i of %i: %i\n",l+1,Nder,currDer); 

  //    if(BNum==256){printf("Reached %.3f  %.3e  %i %i\n",Times[currIF][k]-Tm[BNum],DT,BNum,j); };

// Convert gain ratios into leakages (for linear-pol antennas):
      
  //    Rho1 = (oneC - G1nu[currDer])/(oneC + G1nu[currDer]);
  //    Rho2 = (oneC - G2nu[currDer])/(oneC + G2nu[currDer]);
        RM1 = (oneC - G1nu[currDer]); RM2 = (oneC - G2nu[currDer]); 
        RP1 = (oneC + G1nu[currDer]); RP2 = (oneC + G2nu[currDer]); 

      
// USE MINIMIZATION OF THE CROSS-HAND CORRELATIONS:
     if(AddCrossHand){
       if (is1 && is2){
         auxC01[BNum][currDer] += (RP1*RP2*RL[currIF][k][j] + RM1*RP2*LL[currIF][k][j] + RP1*RM2*RR[currIF][k][j] + RM1*RM2*LR[currIF][k][j])*RLRate;
         auxC10[BNum][currDer] += (RP1*RP2*LR[currIF][k][j] + RM1*RP2*RR[currIF][k][j] + RP1*RM2*LL[currIF][k][j] + RM1*RM2*RL[currIF][k][j])*LRRate;
       } else if (is1){
         auxC01[BNum][currDer] += (RP1*RL[currIF][k][j] + RM1*LL[currIF][k][j])*G2nu[currDer]*RLRate;
         auxC10[BNum][currDer] += (RP1*LR[currIF][k][j] + RM1*RR[currIF][k][j])*LRRate;
       } else if (is2){
         auxC01[BNum][currDer] += (RP2*RL[currIF][k][j] + RM2*RR[currIF][k][j])*RLRate;
         auxC10[BNum][currDer] += (RP2*LR[currIF][k][j] + RM2*LL[currIF][k][j])*G1nu[currDer]*LRRate;
       } else {
         auxC01[BNum][currDer] += (RL[currIF][k][j])*G2nu[currDer]*RLRate;
         auxC10[BNum][currDer] += (LR[currIF][k][j])*G1nu[currDer]*LRRate;
       };
     };

// USE GLOBAL CROSS-POLARIZATION FRINGE FITTING:
  //   if(AddParHand){
       if (is1 && is2){
         auxC00[BNum][currDer] += (RP1*RP2*RR[currIF][k][j] + RP2*RM1*LR[currIF][k][j] + RM2*RP1*RL[currIF][k][j] + RM1*RM2*LL[currIF][k][j])*RRRate;
         auxC11[BNum][currDer] += (RP1*RP2*LL[currIF][k][j] + RP2*RM1*RL[currIF][k][j] + RM2*RP1*LR[currIF][k][j] + RM1*RM2*RR[currIF][k][j])*LLRate;
       } else if (is1){
         auxC00[BNum][currDer] += (RP1*RR[currIF][k][j] + RM1*LR[currIF][k][j])*RRRate;
         auxC11[BNum][currDer] += (RP1*LL[currIF][k][j] + RM1*RL[currIF][k][j])*G2nu[currDer]*LLRate;
       } else if (is2){
         auxC00[BNum][currDer] += (RP2*RR[currIF][k][j] + RM2*RL[currIF][k][j])*RRRate;
         auxC11[BNum][currDer] += (RP2*LL[currIF][k][j] + RM2*LR[currIF][k][j])*G1nu[currDer]*LLRate;
       } else {
         auxC00[BNum][currDer] += RR[currIF][k][j]*RRRate;
         auxC11[BNum][currDer] += LL[currIF][k][j]*G2nu[currDer]*G1nu[currDer]*LLRate;
       };
  //   };



   };  // Comes from   for (l=0; l<Nder; l++)



   };  // Comes from loop over channels.




//  cplx64f CrossMod = cplx64f(Stokes[1],Stokes[2]);
//  unused:
//  double ParMod = (Stokes[0]+Stokes[3])/(Stokes[0]-Stokes[3]);
  cplx64f TempC;




// Did we reach the pre-averaging time?? If so, update the covariance+residuals:
   if (Times[currIF][k]>=Tm[BNum] + DT){
  //   printf("Reached %.3e  %.3e  %.3e\n",Times[currIF][k],Tm[BNum],DT); 

     Tm[BNum] = Times[currIF][k];

////////////////////////////////////////////////////////
// UPDATE THE COVARIANCE MATRIX AND RESIDUALS VECTOR
// 
// 

  Itot = 0.5*(std::abs(auxC00[BNum][0]) + std::abs(auxC11[BNum][0]));

  if (AddParHand && abs(auxC11[BNum][0])>0.0){
      Error = auxC00[BNum][0]/auxC11[BNum][0];
  };

if(doCov){

	
////////////////////////////////////////////
// CONTRIBUTION FROM THE PARALLEL HANDS:
  if (RelWeight>0.0 && abs(auxC11[BNum][0])>0.0){
  for(j=1;j<Nder;j++){
    auxI1 = DerIdx[j];
    auxC1 = (auxC00[BNum][auxI1]/auxC11[BNum][auxI1]-Error)/dx;
// Incoherent approach:
     DerAux1[0] = auxC1.real()*auxC1.real(); DerAux1[1] = auxC1.imag()*auxC1.imag();
     CovMat[(auxI1-1)*(Npar+1)] += (DerAux1[0] + DerAux1[1])*ParHandWgt*BasWgt;

    if(useCov){
     for(l=j+1;l<Nder;l++){
      auxI2 = DerIdx[l];

// Incoherent approach:
        auxC2 = (auxC00[BNum][auxI2]/auxC11[BNum][auxI2]-Error)/dx;
        DerAux2[0] = auxC1.real()*auxC2.real(); DerAux2[1] = auxC1.imag()*auxC2.imag();
        CovMat[(auxI1-1)*Npar+auxI2-1] += (DerAux2[0] + DerAux2[1])*ParHandWgt*BasWgt;
        CovMat[(auxI2-1)*Npar+auxI1-1] = CovMat[(auxI1-1)*Npar+auxI2-1];
     };  
   };

// Incoherent approach:
    DerAux2[0] = (1.-Error.real())*auxC1.real(); DerAux2[1] = -Error.imag()*auxC1.imag();
    IndVec[auxI1-1] += (DerAux2[0] + DerAux2[1])*ParHandWgt*BasWgt; 
  };

  };

  
/////////////////////////////////////////////
// CONTRIBUTION FROM THE CROSS HANDS: RL
  if (AddCrossHand){

  for(j=1;j<Nder;j++){
    auxI1 = DerIdx[j];

      auxC1 = auxC01[BNum][auxI1];
      auxC1 -= auxC01[BNum][0];
      auxC1 /= dx*Itot;

// Incoherent approach:
    DerAux1[0] = auxC1.real()*auxC1.real(); DerAux1[1] = auxC1.imag()*auxC1.imag();
    CovMat[(auxI1-1)*(Npar+1)] += (DerAux1[0] + DerAux1[1])*CrossHandWgt*BasWgt;


   if (useCov){
    for(l=j+1;l<Nder;l++){
      auxI2 = DerIdx[l];

      auxC3 = auxC01[BNum][auxI2];
      auxC3 -= auxC01[BNum][0];
      auxC3 /= dx*Itot;

// Incoherent approach:
      DerAux2[0] = auxC1.real()*auxC3.real(); DerAux2[1] = auxC1.imag()*auxC3.imag();
      CovMat[(auxI1-1)*Npar+auxI2-1] += (DerAux2[0] + DerAux2[1])*CrossHandWgt*BasWgt ;
      CovMat[(auxI2-1)*Npar+auxI1-1] = CovMat[(auxI1-1)*Npar+auxI2-1];
    };  
   };

     auxC3 = auxC01[BNum][0]/Itot;

// Incoherent approach:
    DerAux2[0] = auxC3.real()*auxC1.real(); DerAux2[1] = auxC3.imag()*auxC1.imag();
    IndVec[auxI1-1] -= (DerAux2[0] + DerAux2[1])*CrossHandWgt*BasWgt ; 



/////////////////////////////////////////////
// CONTRIBUTION FROM THE CROSS HANDS: LR
    
   auxC1 = auxC10[BNum][auxI1];
   auxC1 -= auxC10[BNum][0];
   auxC1 /= dx*Itot;


// Incoherent approach:
    DerAux1[0] = auxC1.real()*auxC1.real(); DerAux1[1] = auxC1.imag()*auxC1.imag();
    CovMat[(auxI1-1)*(Npar+1)] += (DerAux1[0] + DerAux1[1])*CrossHandWgt*BasWgt;


   if (useCov){
    for(l=j+1;l<Nder;l++){
      auxI2 = DerIdx[l];

   auxC3 = auxC10[BNum][auxI2];
   auxC3 -= auxC10[BNum][0];
   auxC3 /= dx*Itot;


// Incoherent approach:
      DerAux2[0] = auxC1.real()*auxC3.real(); DerAux2[1] = auxC1.imag()*auxC3.imag();
      CovMat[(auxI1-1)*Npar+auxI2-1] += (DerAux2[0] + DerAux2[1])*CrossHandWgt*BasWgt ;
      CovMat[(auxI2-1)*Npar+auxI1-1] = CovMat[(auxI1-1)*Npar+auxI2-1];

    };  
   };

     auxC3 = auxC10[BNum][0]/Itot;

// Incoherent approach:
    DerAux2[0] = auxC3.real()*auxC1.real(); DerAux2[1] = auxC3.imag()*auxC1.imag();
    IndVec[auxI1-1] -= (DerAux2[0] + DerAux2[1])*CrossHandWgt*BasWgt ; 





  };
  };

}; // Comes from if(doCov)











//////////////////////////
// UPDATE THE CHI SQUARE
  //   printf("C00: %.3e %.3ej ; C01: %.3e %.3ej\n",auxC00[BNum][0].real(),auxC00[BNum][0].imag(),auxC01[BNum][0].real(),auxC01[BNum][0].imag());
  //   printf("C10: %.3e %.3ej ; C11: %.3e %.3ej\n",auxC10[BNum][0].real(),auxC10[BNum][0].imag(),auxC11[BNum][0].real(),auxC11[BNum][0].imag());

     if(AddCrossHand){
    //    auxD1 = std::abs(auxC01[BNum][0]/auxC00[BNum][0]);  auxD2 = std::abs(auxC10[BNum][0]/auxC00[BNum][0]);
    //      Chi2 += (auxD1*auxD1 + auxD2*auxD2)*CrossHandWgt*BasWgt ;
    //    auxD1 = std::abs(auxC01[BNum][0]/auxC11[BNum][0]);  auxD2 = std::abs(auxC10[BNum][0]/auxC11[BNum][0]);
    //      Chi2 += (auxD1*auxD1 + auxD2*auxD2)*CrossHandWgt*BasWgt ;
      auxD1 = std::abs(auxC01[BNum][0])/Itot; auxD2 = std::abs(auxC10[BNum][0])/Itot; ///(std::abs(auxC00[BNum][0])*std::abs(auxC11[BNum][0])); ///(auxC11[BNum][0]*auxC00[BNum][0]));
      Chi2 += (auxD1*auxD1 + auxD2*auxD2)*CrossHandWgt*BasWgt;   
    };

     if (RelWeight>0.0){
        if (abs(auxC11[BNum][0])>0.0){
	  auxC1 = Error - (Stokes[0]+Stokes[3])/(Stokes[0]-Stokes[3]);
          auxD1 = auxC1.real()*auxC1.real() + auxC1.imag()*auxC1.imag();
            Chi2 += auxD1*ParHandWgt*BasWgt ;
        };
     };

      if(end==1){

        for(l=0; l<NLinBas; l++){
          if (LinBasNum[l]==BNum){  
	        double GoodPhase = std::arg(auxC00[BNum][0]/auxC11[BNum][0]);
	        double FlippedPhase = std::arg(auxC00[BNum][0]/auxC11[BNum][0]*(AvPA2[BNum]/AvPA1[BNum]));
            if (std::abs(FlippedPhase)<std::abs(GoodPhase)){Nflipped += 1;}else{Nflipped -= 1;};	 
            break;
          };
        };   
	/* 
         printf("\n GOOD: %.3f  ; BAD: %.3f\n",std::abs(GoodPhase)*180./3.1416,std::abs(FlippedPhase)*180./3.1416);
      printf("\nIF: %i BNum: %i , %i-%i\n",currIF, BNum, a1, a2);
      printf("RR/LL Obs Angle: %.3f | %.3f | %.3f %.3f\n",std::arg(auxC00[BNum][0]/auxC11[BNum][0])*180./3.141593, std::arg((PA1[currIF][k]*PA1[currIF][k])/(PA2[currIF][k]*PA2[currIF][k]))*180./3.141593, std::arg(PA1[currIF][k])*180./3.141593, std::arg(PA2[currIF][k])*180./3.141593);
        */


      };



// Reset temporal visibility averages:
    for(j=0;j<Npar+1;j++){
      auxC00[BNum][j] = cplx64f(0., 0.);
      auxC01[BNum][j] = cplx64f(0., 0.);
      auxC10[BNum][j] = cplx64f(0., 0.);
      auxC11[BNum][j] = cplx64f(0., 0.);
    };
    AvVis[BNum] = 0;


   }; // Comes from:   if (Times[currIF][k]>=Tm[BNum] + DT)

   }; // Comes from if(BNum>=0) 

  };  // Comes from loop over visibilities

}; // comes from loop over NIFComp


double TheorImpr = 0.0;


if(doCov){
// Solve the system:

// Find the largest gradient:
double Largest = 0.0;
for(i=0;i<Npar;i++){
  if(CovMat[i*Npar+i]>Largest){Largest=CovMat[i*Npar+i];};
};

// Fill in the Hessian's lower part:
for(i=0;i<Npar;i++){
  DirDer[i] = CovMat[i*Npar+i];
//  for(j=i+1;j<Npar;j++){
//    if (useCov){CovMat[j*Npar+i] = CovMat[i*Npar + j];} else {
//       CovMat[j*Npar+i] = 0.0;
//       CovMat[i*Npar+j] = 0.0; 
//    };
//  };
  CovMat[i*Npar+i] += Lambda*(Largest);
};

gsl_death_by = GSL_SUCCESS;
if (useCov){
  gsl_linalg_LU_decomp (&m.matrix, perm, &s);
  gsl_linalg_LU_solve(&m.matrix, perm, &v.vector, &x.vector);
} else {
  for(i=0;i<Npar;i++){SolVec[i] = IndVec[i]/CovMat[i*Npar+i];};
};
if (gsl_death_by != GSL_SUCCESS) {
    PyObject *ret = Py_BuildValue("d",-13);
    return ret;
}

for(i=0;i<Npar;i++){
  TheorImpr += DirDer[i]*SolVec[i]*SolVec[i] - 2.*IndVec[i]*SolVec[i];
  CrossG[i] += SolVec[i];
};


}; // comes from id(doCov)

  if (SolAlgor==0){
     printf("\n Chi Squared: %.5e. Expected improvement: %.5e",Chi2,TheorImpr);
   };

  Chi2Old = Chi2;

    if (end==1){Chi2 = (Nflipped>0)?1.0:-1.0;};
 //   if(FlipIt){printf("\nWill flip all phase gains by 180 degrees.\n\n\n\n");};

    ret = Py_BuildValue("d",Chi2);


  delete[] AvPA1;
  delete[] AvPA2;

  
//sprintf(message, "Chisq Ret %g (%ld)\n", Chi2, chisqcount);
//fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
  std::cout << std::flush;

  return ret;

};












// eof

/* POLGAINSOLVE - Solving cross-polarizer gains for PolConvert

             Copyright (C) 2017-2022  Ivan Marti-Vidal
             Nordic Node of EU ALMA Regional Center (Onsala, Sweden)
             Max-Planck-Institut fuer Radioastronomie (Bonn, Germany)
             University of Valencia (Spain)  

	     Revised on Dec 2025

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
//#define NO_IMPORT_ARRAY
//#if PY_MAJOR_VERSION >= 3
//#define NPY_NO_DEPRECATED_API 0x0
//#endif
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
//#include <gsl/gsl_errno.h>
//#include <gsl/gsl_linalg.h>




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
//#define INTEGER long
//#define INTEGERCAST  (const npy_intp*)
//#else
// For PyArray_FromDimsAndData -> PyArray_SimpleNewFromData
//#define INTEGER int
//#define INTEGERCAST (long int *)
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
static char GetNchan_docstring[] =
    "Returns the number of channels for the given IF.";



/* Available functions */
static PyObject *PolGainSolve(PyObject *self, PyObject *args);
static PyObject *ReadData(PyObject *self, PyObject *args);
static PyObject *GetChi2(PyObject *self, PyObject *args);
static PyObject *GetIFs(PyObject *self, PyObject *args);
static PyObject *GetNchan(PyObject *self, PyObject *args);
static PyObject *DoGFF(PyObject *self, PyObject *args);
static PyObject *SetFringeRates(PyObject *self, PyObject *args);
static PyObject *GetNScan(PyObject *self, PyObject *args);
static PyObject *FreeData(PyObject *self, PyObject *args);
static PyObject *SetFit(PyObject *self, PyObject *args);

bool solveSystem(int Neq, double *Hessian, double *Residuals, double *Solution, double *Errors);


/* Module specification */
static PyMethodDef module_methods[] = {
    {"PolGainSolve", PolGainSolve, METH_VARARGS, PolGainSolve_docstring},
    {"ReadData", ReadData, METH_VARARGS, ReadData_docstring},
    {"GetChi2", GetChi2, METH_VARARGS, GetChi2_docstring},
    {"GetIFs", GetIFs, METH_VARARGS, GetIFs_docstring},
    {"DoGFF", DoGFF, METH_VARARGS, DoGFF_docstring},
    {"SetFringeRates", SetFringeRates, METH_VARARGS, SetFringeRates_docstring},
    {"GetNScan",GetNScan, METH_VARARGS, GetNScan_docstring},
    {"GetNchan",GetNchan, METH_VARARGS, GetNchan_docstring},
    {"FreeData", FreeData, METH_VARARGS, FreeData_docstring},
    {"SetFit", SetFit, METH_VARARGS, SetFit_docstring},
    {NULL, NULL, 0, NULL} /* terminated by list of NULLs, apparently */
};





// normally abort() is called on problems, which breaks CASA.
// here we report and save the error condition which can be
// noticed for a cleaner exit.
//int gsl_death_by = GSL_SUCCESS;
//static void gsl_death(const char * reason, const char * file,
//    int line, int gsl_errno) {
//    // stderr does not end up synchronized with stdout
//    printf("GSL Death by '%s' in file %s at line %d: GSL Error %d\n",
//        reason, file, line, gsl_errno);
//    fflush(stdout); std::cout << std::flush;
//    gsl_death_by = gsl_errno;
//}
 
static long chisqcount = 0;





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
    return(m);
}
#else
PyMODINIT_FUNC init_PolGainSolve(void)
{
    PyObject *m = Py_InitModule3("_PolGainSolve", module_methods, module_docstring);import_array();
    if (m == NULL)
        return;

}
#endif
///////////////////////


   char message[512];

   bool doParang;

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
   int NBas, NantFit, Npar=-1;
   int npix = 0;
   double **Frequencies, ***Rates[5], ***Delays[5];
   double *Tm = nullptr;
   double *BasWgt;
   int *doIF, *antFit; 
   int MaxAnt = 0; 
   double *feedAngle;
   double **DStokes; // = new double*[1];
   double UVTAPER = 1.e9;

   double Chi2Old = 0.0;
   double TAvg = 1.0;
   double RelWeight = 1.0;
   double T0, T1, DT;
   int **Ant1, **Ant2, **BasNum, **Scan;
   double **Times, **ScanDur, **Weights, *CovMat, *IndVec, *SolVec;
   double **UVGauss;
   cplx64f **PA1, **PA2, **auxC00, **auxC01, **auxC10, **auxC11;
   cplx64f **auxC00Flp, **auxC11Flp; // To better check Parangle Flip.
   cplx64f ***RR, ***RL, ***LR, ***LL, **CrossSpec00, **CrossSpec11;
   double *UVWeights;
   double SNR_CUTOFF;
   double Lambda;
   bool doCov;
   double Stokes[4];

   bool AddCrossHand = true;
   bool AddParHand = true;
   bool StokesSolve;

   cplx64f *G1,*G2, *G1nu, *G2nu; 
   double *DirDer,*MBD1,*MBD2;
   int *DerIdx,*AvVis;


   FILE *logFile = nullptr;







// Just a simple linear-equation system solver:
bool solveSystem(int Neq, double *HessianOrig, double *ResidualsOrig, double *Solution, double *Errors){

    bool isSingular;
    int i,j,k;
    double *buffer = new double[Neq+1];
    double f;

    double *Hessian = new double[Neq*Neq];
    double *Residuals = new double[Neq];
    memcpy((void *)Hessian, (void *)HessianOrig, sizeof(double)*Neq*Neq);
    memcpy((void *)Residuals, (void *)ResidualsOrig, sizeof(double)*Neq);

    int Nmissing = 0;
    int *missing = new int[Neq];

    double *L = new double[Neq*Neq];
    double *U = new double[Neq*Neq];

    for(i=0;i<Neq;i++){Solution[i]=0.0;};
    for(i=0;i<Neq*Neq;i++){L[i]=0.0;U[i]=0.0;};
    for(i=0;i<Neq;i++){L[i*Neq+i]=1.0;};

// Getting null rows out of the computation:
    for(i=0;i<Neq;i++){
      isSingular=true;
      for(j=0;j<Neq;j++){
        if(HessianOrig[i*Neq+j]!=0.0){isSingular=false;break;};
      };
      if(isSingular){
        missing[Nmissing]=i;Nmissing+=1;
   //     printf("Antenna %i does not seem to have valid data.\n",i);
      };
    };

     /* performing Gaussian elimination */
    for(i=0;i<Neq-1;i++){
// PIVOTING:

    isSingular = false;
    for(k=0;k<Nmissing;k++){
      if(i==missing[k]){isSingular=true;break;};
    };

    if(!isSingular){

      for(j=i+1;j<Neq;j++){

       isSingular = false;
       for(k=0;k<Nmissing;k++){
         if(j==missing[k]){isSingular=true;break;};
       };

       if(!isSingular){

        if(abs(Hessian[i*Neq + i]) < abs(Hessian[j*Neq + i])){
          for(k=0;k<Neq;k++){
            buffer[k] = Hessian[i*Neq + k];
            Hessian[i*Neq + k] = Hessian[j*Neq + k];
            Hessian[j*Neq + k] = buffer[k];
          };
          buffer[Neq] = Residuals[i]; 
          Residuals[i] = Residuals[j]; 
          Residuals[j] = buffer[Neq]; 
        };
      };
    };

      if(Hessian[i*Neq + i]==0.0){  //isSingular=true; return isSingular;
        missing[Nmissing] = i;
        Nmissing += 1;
   //     printf("Antenna %i does not seem to have valid data EITHER.\n",i);
      } else {
      for(j=i+1;j<Neq;j++){

       isSingular = false;
       for(k=0;k<Nmissing;k++){
         if(j==missing[k]){isSingular=true;break;};
       };

       if(!isSingular){
        f=Hessian[j*Neq + i]/Hessian[i*Neq + i];
        L[j*Neq+i] = f;
        for(k=0;k<Neq;k++){
          Hessian[j*Neq + k]=Hessian[j*Neq + k]-f*Hessian[i*Neq + k];
        };
        Residuals[j] = Residuals[j]-f*Residuals[i];
      } else {L[j*Neq+i]=0.0;};

    };
    };
   };

  };


   for(i=0;i<Neq*Neq;i++){U[i]=Hessian[i];};


    /* Backward substitution for discovering values of unknowns */
    for(i=Neq-1;i>=0;i--){
      isSingular=false;
      for(j=0; j<Nmissing; j++){
        if(missing[j]==i){isSingular=true;Solution[i]=0.0;break;};
      };
      if(!isSingular){
        Solution[i]=Residuals[i];
        for(j=i+1;j<Neq;j++){
          if(i!=j){
            Solution[i]=Solution[i]-Hessian[i*Neq + j]*Solution[j];
          };
        };
        Solution[i]=Solution[i]/Hessian[i*Neq + i];
      };
    };
    


  int Nfree;
  Nfree = Neq - Nmissing;
  double *invL = new double[Nfree*Nfree];
  double *invU = new double[Nfree*Nfree];
  double *Ufree = new double[Nfree*Nfree];

  int ifree, jfree;
  ifree=0; jfree=0;



  for(i=0;i<Neq;i++){
    isSingular=false;
    for(k=0;k<Nmissing;k++){
      if(i==missing[k]){isSingular=true;break;};
    };
    if(!isSingular){
      jfree=0;
      for(j=0;j<Neq;j++){
        isSingular = false;
        for(k=0;k<Nmissing;k++){
          if(j==missing[k]){isSingular=true;break;};
        };
        if(!isSingular){
          invL[ifree*Nfree+jfree]=L[i*Neq+j]; Ufree[ifree*Nfree+jfree]=U[i*Neq+j];
          jfree+=1;
        };
      };
      ifree += 1;
    };
  };




// Now, invert the smaller (nonSingular) matrices L and U:
  double *ident = new double[Nfree];

  for(k=0;k<Nfree;k++){
    for(j=0;j<Nfree;j++){ident[j]=0.0;};
      ident[k]=1.0;
      for(i=Nfree-1;i>=0;i--){
        invU[i*Nfree + k]=ident[i];
        for(j=i+1;j<Nfree;j++){
          if(i!=j){
            invU[i*Nfree+k]=invU[i*Nfree+k]-Ufree[i*Nfree + j]*invU[j*Nfree+k];
          };
        };
        invU[i*Nfree+k]=invU[i*Nfree+k]/Ufree[i*Nfree + i];
    };
  };


// The parameter errors are just the (sqrt of) the diagonal cov. matrix:
  ifree = 0;
  for(i=0;i<Neq;i++){
    isSingular=false;
    Errors[i]=0.0;
    for(k=0;k<Nmissing;k++){
      if(missing[k]==i){isSingular=true;Errors[i]=-1.0; break;};
    };
    if(!isSingular){
      for(j=0;j<Nfree;j++){
        Errors[i] += invU[ifree*Nfree+j]*invL[j*Nfree+ifree];
      };
      ifree += 1;
      Errors[i] = std::sqrt(Errors[i]);
    };
  };


  delete[] L;
  delete[] U;
  delete[] missing;
  delete[] buffer;
  delete[] Hessian;
  delete[] Residuals;
  delete[] invU;
  delete[] Ufree;
  delete[] invL;
  delete[] ident;

  isSingular = Nmissing>0;
  return isSingular;


};




// Self-explanatory:
static PyObject *GetNchan(PyObject *self, PyObject *args){
  int cIF, k, j;
  PyObject *ret;

  long Iret = 0;

// append after first call
  if (!logFile) logFile = fopen("PolConvert.GainSolve.log","a");
  fprintf(logFile,"into GetNchan...\n"); fflush(logFile);

  if (!PyArg_ParseTuple(args, "i",&cIF)){
     sprintf(message,"Failed GetNchan! Check inputs!\n"); 
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile); 
     Iret = -10;
     goto abort4; 
  };

   k=-1;
   for (j=0; j<NIF; j++){
     if(IFNum[j] == cIF){k=j;break;};
   };

  if(k<0){
    sprintf(message,"GetNchan: IF %d not found\n", cIF);
    fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
    Iret = -20;
    goto abort4;
  };



  fprintf(logFile,"... and got %d\n", Nchan[k]); fflush(logFile);
  Iret = Nchan[k];

abort4:

  if(logFile){fclose(logFile); logFile = nullptr;};
  ret = PyLong_FromLong(Iret);
  return ret;

};



// Self-explanatory:
static PyObject *GetNScan(PyObject *self, PyObject *args){
  int cIF, k, j;
  PyObject *ret;
  long Iret = 0;

// append after first call
  if (!logFile) logFile = fopen("PolConvert.GainSolve.log","a");
  fprintf(logFile,"into GetNScan...\n"); fflush(logFile);

  if (!PyArg_ParseTuple(args, "i",&cIF)){
     sprintf(message,"Failed GetNScan! Check inputs!\n"); 
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
     Iret = -30;
     goto abort5;  
  };


   k=-1;
   for (j=0; j<NIF; j++){
     if(IFNum[j] == cIF){k=j;break;};
   };

  if(k<0){
    sprintf(message,"GetNScan: IF %d not found\n", cIF);
    fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
    Iret = -40;
    goto abort5;
  };



  fprintf(logFile,"... and got %d\n", NScan[k]); fflush(logFile);
  Iret = NScan[k];

abort5:
  if(logFile) {fclose(logFile); logFile=nullptr;};
  ret = PyLong_FromLong(Iret);
  return ret;

};



// Constructor:
static PyObject *PolGainSolve(PyObject *self, PyObject *args){

  PyObject *solints, *flagBas, *logNameObj;
  PyArrayObject *linant, *calant;

  if (!PyArg_ParseTuple(args, "ddOOOOO",&RelWeight, &UVTAPER, &solints, &calant,
        &linant,&flagBas, &logNameObj)){
     sprintf(message,"Failed initialization of PolGainSolve! Check inputs!\n"); 
     std::cout<<message;
    PyObject *ret = Py_BuildValue("i",-1);
    return ret;
  };

  UVTAPER = 2.*UVTAPER*UVTAPER;

  // truncate for first call
  std::string logName = PyString_AsString(logNameObj);
  if (!logFile) logFile = fopen(logName.c_str(),"a");




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


// Read python data pointers:  
  TAvg = (double) PyInt_AsLong(PyList_GetItem(solints,1));
  SolAlgor = (int) PyInt_AsLong(PyList_GetItem(solints,0));

  Twins[0] = (int *)PyArray_DATA((PyArrayObject *)PyList_GetItem(flagBas,0));
  Twins[1] = (int *)PyArray_DATA((PyArrayObject *)PyList_GetItem(flagBas,1));
  Ntwin = PyArray_DIM((PyArrayObject *)PyList_GetItem(flagBas,0),0);

  sprintf(message,"There are %i baselines to flag\n",Ntwin);
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  

  sprintf(message,"Will pre-average the data in chunks of %.1f seconds\n",TAvg);
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  

  AddParHand = RelWeight>0.0;
  AddCrossHand = true;

  Lant = (int *)PyArray_DATA(linant);
  Nlin = PyArray_DIM(linant,0);

  CalAnts = (int *)PyArray_DATA(calant);
  NCalAnt = PyArray_DIM(calant,0);

  
  int i,j,k,l;
  k=0;

  MaxAnt = 0;
  for(i=0;i<NCalAnt;i++){
    if(CalAnts[i]>MaxAnt){
      MaxAnt=CalAnts[i];
    };
  };

  BasNum = new int*[MaxAnt];
  LinBasNum = new int[MaxAnt*(MaxAnt-1)/2];
  NLinBas = 0;
  
  bool isCal1, isCal2; 

  for(i=0;i<MaxAnt;i++){
    BasNum[i] = new int[MaxAnt];
    for(j=0;j<MaxAnt;j++){
      BasNum[i][j] = -1;
    };
    for(j=i+1;j<MaxAnt;j++){
      isCal1=false; isCal2=false;
      for(l=0;l<NCalAnt;l++){
        if(i==CalAnts[l]-1){isCal1=true;}; 
        if(j==CalAnts[l]-1){isCal2=true;};
      };
      if (isCal1 && isCal2){
        BasNum[i][j] = k;
        for(l=0;l<Nlin; l++){if(i==Lant[l]-1 || j==Lant[l]-1){LinBasNum[NLinBas]=k; NLinBas += 1; break; };};
        k += 1;
      };
    };
  };
  printf("There are %i baselines.\n",k);

  int BNum;
  BasWgt = new double[k];
  for(i=0;i<k;i++){BasWgt[i]=0.0;};

  for(i=0;i<MaxAnt;i++){
    for(j=i+1;j<MaxAnt;j++){
       BNum = BasNum[i][j];
       if (BNum>=0){  
           BasWgt[BNum] = 1.0;};
       for(l=0;l<Ntwin;l++){
         if((Twins[0][l]==i+1 && Twins[1][l]==j+1)||(Twins[0][l]==j+1 && Twins[1][l]==i+1)){
           printf("Flagging baseline %i\n",BNum); BasNum[i][j] = -1; if(BNum>=0){BasWgt[BNum] = 0.0;}; break;
         };
      };
    };
  };

  auxC00 = new cplx64f*[k]; 
  auxC01 = new cplx64f*[k]; 
  auxC10 = new cplx64f*[k];
  auxC11 = new cplx64f*[k];
  auxC00Flp = new cplx64f*[k]; 
  auxC11Flp = new cplx64f*[k];
  UVWeights = new double[k];

  for(i=0;i<k;i++){
    auxC00[i] = new cplx64f[3*NCalAnt+1];
    auxC11[i] = new cplx64f[3*NCalAnt+1];
    auxC01[i] = new cplx64f[3*NCalAnt+1];
    auxC10[i] = new cplx64f[3*NCalAnt+1];
    auxC00Flp[i] = new cplx64f[3*NCalAnt+1];
    auxC11Flp[i] = new cplx64f[3*NCalAnt+1];
  };
  NBas = k;
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
  Weights = (double**) malloc(MAXIF*sizeof(double*));

  ScanDur = (double**) malloc(MAXIF*sizeof(double*));
  PA1 = (cplx64f**) malloc(MAXIF*sizeof(cplx64f*));
  PA2 = (cplx64f**) malloc(MAXIF*sizeof(cplx64f*));
  UVGauss = (double**) malloc(MAXIF*sizeof(double*));
  RR = (cplx64f***) malloc(MAXIF*sizeof(cplx64f**));
  LR = (cplx64f***) malloc(MAXIF*sizeof(cplx64f**));
  RL = (cplx64f***) malloc(MAXIF*sizeof(cplx64f**));
  LL = (cplx64f***) malloc(MAXIF*sizeof(cplx64f**));
  for(i=0;i<5;i++){
    Rates[i] = (double ***) malloc(MAXIF*sizeof(double**));
    Delays[i] = (double ***) malloc(MAXIF*sizeof(double**));
  };


  PyObject *ret = Py_BuildValue("i",0);
  return ret;
};






// Free all pointers.
static PyObject *FreeData(PyObject *self, PyObject *args) {

  int i,j,k; 

  if (!logFile) logFile = fopen("PolConvert.GainSolve.log","a");
  sprintf(message,"Freeing Data NIF = %d\n", NIF);
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);


  if (Tm){
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
    for(i=0;i<Npar+1;i++){delete[] DStokes[i];};
    delete[] DStokes;
    delete[] DirDer;
    delete[] MBD1;
    delete[] MBD2;
    delete[] DerIdx;
    delete[] AvVis;
    sprintf(message,"Clearing previous allocation objects\n");
    fprintf(logFile,"%s",message); // std::cout<<message; fflush(logFile);
  };
  Tm = nullptr;


  for(i=0;i<NIF;i++){
    for(j=0;j<NVis[i]+1;j++){
      free(RR[i][j]);free(RL[i][j]);
      free(LR[i][j]);free(LL[i][j]);
    };
    free(Ant1[i]);free(Ant2[i]);free(Scan[i]);free(Times[i]);
    free(PA1[i]);free(PA2[i]);free(RR[i]);free(RL[i]);free(UVGauss[i]);
    free(LR[i]);free(LL[i]);free(ScanDur[i]);free(Weights[i]);
    delete[] Frequencies[i];
  };

  free(Ant1); free(Ant2); free(Times); free(Weights);
  free(PA1); free(PA2); free(UVGauss);
  free(RR); free(RL); free(LR); free(LL); free(ScanDur);


  for (i=0; i<NBas; i++) { free(CrossSpec00[i]); free(CrossSpec11[i]); }
  free(CrossSpec00); free(CrossSpec11);

  for (i=0; i<NBas; i++) {
        delete[] auxC00[i]; delete[] auxC01[i];
        delete[] auxC10[i]; delete[] auxC11[i];
        delete[] auxC00Flp[i]; delete[] auxC11Flp[i];
    }
    delete[] auxC00; delete[] auxC01; delete[] auxC10; delete[] auxC11;
    delete[] auxC00Flp; delete[] auxC11Flp;

  for (i=0; i<MaxAnt; i++) { delete[] BasNum[i]; }
    delete[] BasNum;
    delete[] LinBasNum;
 
  delete[] BasWgt;
  delete[] UVWeights;


  for (k=0; k<5; k++) {
        for (int i=0; i<NIF; i++) {
            if (Delays[k] && Delays[k][i]) {
                for (int j=0; j<NCalAnt; j++) {
                    free(Delays[k][i][j]);    
                    free(Rates[k][i][j]);
                };
                free(Delays[k][i]);
                free(Rates[k][i]);
            };
        };
        free(Delays[k]); free(Rates[k]);      
    };




  if(NIF>0){
    free(NScan);free(Nchan);free(NVis);
    free(NCVis);free(NLVis);free(IFNum);
    free(Frequencies); free(Scan);
    NIF = -1;
    PyObject *ret = Py_BuildValue("i",0);
    return ret;
  };

// Problem with NIF. Returns error:
  PyObject *ret = Py_BuildValue("i",1);
  return ret;
};






// Read the data in polconvert's binary format.
// In addition, arrange the data in scans.
// MaxDT is the maximum allowed time separation between 
// neighboring entries of the same scan (in seconds).
static PyObject *ReadData(PyObject *self, PyObject *args) {

  int IFN;
  long Iret = 0;
  const char *file1, *file2;
  std::ifstream CPfile, MPfile;
  double MaxDT;


// Number of integration times:
  int NDiffTimes = 0;
  int TimeBuff = 1000;
  double *DiffTimes = (double *) malloc(TimeBuff*sizeof(double));
  bool isTime;



  double *ScanTimes = new double[1];
  bool isOut = true;
  int currI = 0;
  bool isGood, isFlipped;
  cplx64f Exp1, Exp2;
  cplx32f AuxRR, AuxRL, AuxLR, AuxLL;





// Log File:
  if (!logFile) logFile = fopen("PolConvert.GainSolve.log","a");
  fprintf(logFile,"ReadData entered...\n"); fflush(logFile);


  if (!PyArg_ParseTuple(args, "issd", &IFN,&file1, &file2,&MaxDT)){
     sprintf(message,"Failed ReadData! Check inputs! (return -1)\n"); 
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
     Iret = -50;
     goto abort1;  
  };

  fprintf(logFile,"ReadData parsed...\n"); fflush(logFile);


  int i, j, k;
  double AuxT, AuxPA1, AuxPA2, AuxUV;
  bool is1, is2;


  CPfile.open(file1, std::ios::in | std::ios::binary);
  MPfile.open(file2, std::ios::in | std::ios::binary);



  NIF += 1;

//////////
// Set memory for new IF:
  if (NIF > MAXIF){
    fprintf(logFile,"(realloc) %d > %d\n", NIF, MAXIF); fflush(logFile);
    MAXIF *= 2;
    Nchan = (int*) realloc(Nchan,MAXIF*sizeof(int));
    Frequencies = (double**) realloc(Frequencies,MAXIF*sizeof(double*));
    NVis = (int*) realloc(NVis,MAXIF*sizeof(int));
    NCVis = (int*) realloc(NCVis,MAXIF*sizeof(int));
    NLVis = (int*) realloc(NLVis,MAXIF*sizeof(int));
    IFNum = (int*) realloc(IFNum,MAXIF*sizeof(int));
    if(!Nchan || !Frequencies || !NVis || !NCVis || !NLVis || !IFNum){
      fprintf(logFile,"(return -60)"); fflush(logFile);
      Iret = -60;
      goto abort1;
    };

// Set memory for the visibilities and metadata:
    Ant1 = (int**) realloc(Ant1,MAXIF*sizeof(int*));
    Ant2 = (int**) realloc(Ant2,MAXIF*sizeof(int*));
    Scan = (int**) realloc(Scan,MAXIF*sizeof(int*));
    NScan = (int*) realloc(NScan,MAXIF*sizeof(int));
    Times = (double**) realloc(Times,MAXIF*sizeof(double*));
    Weights = (double**) realloc(Weights,MAXIF*sizeof(double*));
    ScanDur = (double**) realloc(ScanDur,MAXIF*sizeof(double*));
    PA1 = (cplx64f**) realloc(PA1,MAXIF*sizeof(cplx64f*));
    PA2 = (cplx64f**) realloc(PA2,MAXIF*sizeof(cplx64f*));
    UVGauss = (double**) realloc(UVGauss,MAXIF*sizeof(double*));
    RR = (cplx64f***) realloc(RR,MAXIF*sizeof(cplx64f**));
    LR = (cplx64f***) realloc(LR,MAXIF*sizeof(cplx64f**));
    RL = (cplx64f***) realloc(RL,MAXIF*sizeof(cplx64f**));
    LL = (cplx64f***) realloc(LL,MAXIF*sizeof(cplx64f**));
    for(i=0;i<5;i++){
      Delays[i] = (double ***) realloc(Delays[i],MAXIF*sizeof(double**));
      Rates[i] = (double ***) realloc(Rates[i],MAXIF*sizeof(double**));
    };
    if(!Ant1 || !Ant2 || !Times || !Weights || !PA1 || !PA2 || !RR || !LR || !RL || !LL){
      fprintf(logFile,"(return -70)"); fflush(logFile);
      Iret = -70;
      goto abort1;
    };
    if(!Rates[0] || !Delays[0] || !Delays[1] || !Delays[2] || !Delays[3]){
      fprintf(logFile,"(return -80)"); fflush(logFile);
      Iret = -80;
      goto abort1;
    };
  };
//////////


// IF NUMBER:
  IFNum[NIF-1] = IFN;

// Number of channels for this IF:
  CPfile.read(reinterpret_cast<char*>(&Nchan[NIF-1]), sizeof(int));
  fprintf(logFile, "IF%d has %i channels\n",IFN,Nchan[NIF-1]); fflush(logFile);
  printf("IF%d (%i) has %i channels\n",IFN,NIF,Nchan[NIF-1]); fflush(stdout);

// Maximum number of channels:
  if (Nchan[NIF-1] > MaxChan){
    MaxChan=Nchan[NIF-1];
    for (i=0; i< NBas; i++) {
      CrossSpec00[i] = (cplx64f *) realloc(CrossSpec00[i],MaxChan*sizeof(cplx64f));
      CrossSpec11[i] = (cplx64f *) realloc(CrossSpec11[i],MaxChan*sizeof(cplx64f));
      if(!CrossSpec00[i] || !CrossSpec11[i]){
        fprintf(logFile,"(return -90)"); fflush(logFile);
        Iret = -90;
	goto abort1;
      };
    };
  };


// ignores noI
  MPfile.ignore(sizeof(int));

// Have we applied parang??
  MPfile.read(reinterpret_cast<char*>(&doParang), sizeof(bool));


// Get frequencies for this IF:
  Frequencies[NIF-1] = new double[Nchan[NIF-1]];
  CPfile.read(reinterpret_cast<char*>(Frequencies[NIF-1]), Nchan[NIF-1]*sizeof(double));

  fprintf(logFile,"Freqs. %.8e  %.8e\n",
      Frequencies[NIF-1][0],Frequencies[NIF-1][Nchan[NIF-1]-1]);
  fflush(logFile);



// Get Number of visibilities observed by the CalAnts (Circ Pol):

  NCVis[NIF-1] = 0;
  int AuxA1, AuxA2;

  fprintf(logFile,"Reading CPfile...(NCalAnt=%d)\n", NCalAnt); fflush(logFile);

// eof() doesn't do what everyone thinks....
  while(!CPfile.eof() && CPfile.peek() >= 0){
    is1 = false; is2 = false;
    CPfile.ignore(sizeof(double));   // daytemp
    CPfile.read(reinterpret_cast<char*>(&AuxA1), sizeof(int));
    CPfile.read(reinterpret_cast<char*>(&AuxA2), sizeof(int));
    for (i=0; i<NCalAnt; i++) {
      if (AuxA1 == CalAnts[i]){is1=true;}; 
      if (AuxA2 == CalAnts[i]){is2=true;};
    };
    if(is1 && is2 && AuxA1 != AuxA2){
      NCVis[NIF-1] += 1;
    };
// here we are ignoring all the visibility data
  CPfile.ignore(3*sizeof(double)+4*Nchan[NIF-1]*sizeof(cplx32f));
  };
  fprintf(logFile,"Finished CPfile...\n"); fflush(logFile);


// Get Number of visibilities observed by the CalAnts (Mix Pol):
  NLVis[NIF-1] = 0;


  fprintf(logFile,"Reading MPfile... (NCalAnt=%d)\n", NCalAnt); fflush(logFile);
// eof() doesn't do what everyone thinks....
  while(!MPfile.eof() && MPfile.peek() >= 0){
    is1 = false; is2 = false;
    MPfile.ignore(sizeof(int));
    MPfile.ignore(sizeof(double)); // Time
    MPfile.read(reinterpret_cast<char*>(&AuxA1), sizeof(int));
    MPfile.read(reinterpret_cast<char*>(&AuxA2), sizeof(int));
    for (i=0; i<NCalAnt; i++) {
      if (AuxA1 == CalAnts[i]){is1=true;}; 
      if (AuxA2 == CalAnts[i]){is2=true;};
    };
    if(is1 && is2 && AuxA1 != AuxA2){
      NLVis[NIF-1] += 1;
    };
    MPfile.ignore(3*sizeof(double)+12*Nchan[NIF-1]*sizeof(cplx32f));
  };

  fprintf(logFile,"Finished MPfile...\n"); fflush(logFile);

// Total number of visibilities:
  NVis[NIF-1] = NCVis[NIF-1]+ NLVis[NIF-1];
  sprintf(message,"Found %i vis in CPol and %i vis in LPol for a total of %i\n",
      NCVis[NIF-1],NLVis[NIF-1],NVis[NIF-1]); 
  fprintf(logFile,"%s",message);  fflush(logFile);

// Set memory for the visibilities:
  j = NVis[NIF-1]+1;
  Ant1[NIF-1] = (int*) malloc(j*sizeof(int));
  Ant2[NIF-1] = (int*) malloc(j*sizeof(int));
  Scan[NIF-1] = (int*) malloc(j*sizeof(int)); 
  Times[NIF-1] = (double*) malloc(j*sizeof(double));
  Weights[NIF-1] = (double*) malloc(j*sizeof(double));
  PA1[NIF-1] = (cplx64f*) malloc(j*sizeof(cplx64f)); 
  PA2[NIF-1] = (cplx64f*) malloc(j*sizeof(cplx64f)); 
  UVGauss[NIF-1] = (double*) malloc(j*sizeof(double));
  RR[NIF-1] = (cplx64f**) malloc(j*sizeof(cplx64f*)); 
  LR[NIF-1] = (cplx64f**) malloc(j*sizeof(cplx64f*));
  RL[NIF-1] = (cplx64f**) malloc(j*sizeof(cplx64f*)); 
  LL[NIF-1] = (cplx64f**) malloc(j*sizeof(cplx64f*)); 
  for (i=0;i<j;i++){
    RR[NIF-1][i] = (cplx64f*) malloc(Nchan[NIF-1]*sizeof(cplx64f)); 
    LR[NIF-1][i] = (cplx64f*) malloc(Nchan[NIF-1]*sizeof(cplx64f)); 
    RL[NIF-1][i] = (cplx64f*) malloc(Nchan[NIF-1]*sizeof(cplx64f)); 
    LL[NIF-1][i] = (cplx64f*) malloc(Nchan[NIF-1]*sizeof(cplx64f)); 
  };

// Rewind files:
  CPfile.clear();
  CPfile.seekg(sizeof(int)+Nchan[NIF-1]*sizeof(double),CPfile.beg);

  MPfile.clear();
  MPfile.seekg(sizeof(int)+sizeof(bool),MPfile.beg);

  fprintf(logFile, "\nFiles rewound\n"); fflush(logFile);


  i=0;
  while(!MPfile.eof() && MPfile.peek() >= 0){
// MPfile timestamp is here
    MPfile.ignore(sizeof(int));
    MPfile.read(reinterpret_cast<char*>(&AuxT), sizeof(double));
    MPfile.read(reinterpret_cast<char*>(&AuxA1), sizeof(int));
    MPfile.read(reinterpret_cast<char*>(&AuxA2), sizeof(int));
    MPfile.read(reinterpret_cast<char*>(&AuxPA1), sizeof(double));
    MPfile.read(reinterpret_cast<char*>(&AuxPA2), sizeof(double));
    MPfile.read(reinterpret_cast<char*>(&AuxUV), sizeof(double));

// Check if visib is observed by CalAnts:
    isGood = false; is1 = false; is2 = false;
    isFlipped = false;

    for (j=0; j<NCalAnt; j++) {
      if (AuxA1 == CalAnts[j]){is1=true;}; 
      if (AuxA2 == CalAnts[j]){is2=true;};
    };

    if (is1 && is2 && AuxA1 != AuxA2){
      isFlipped = AuxA1 > AuxA2;
      Exp1 = std::polar(1.0,AuxPA1);
      Exp2 = std::polar(1.0,AuxPA2);
      Times[NIF-1][currI] = AuxT;
      UVGauss[NIF-1][currI] = std::exp(-AuxUV/UVTAPER);
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

  if(doParang){
         RR[NIF-1][currI][k] *= PA2[NIF-1][currI]/PA1[NIF-1][currI];
         RL[NIF-1][currI][k] /= PA2[NIF-1][currI]*PA1[NIF-1][currI];
         LR[NIF-1][currI][k] *= PA2[NIF-1][currI]*PA1[NIF-1][currI];
         LL[NIF-1][currI][k] *= PA1[NIF-1][currI]/PA2[NIF-1][currI];
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
    CPfile.read(reinterpret_cast<char*>(&AuxUV), sizeof(double));
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
    UVGauss[NIF-1][currI] = std::exp(-AuxUV/UVTAPER);
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
  };


  if(!isGood){
    CPfile.ignore(4*Nchan[NIF-1]*sizeof(cplx32f));
  };


  };

  printf("DONE READ!\n"); fflush(stdout);
  CPfile.close();
  MPfile.close();



// Sort times out:
  printf("There are %i int. times.\n",NDiffTimes);
  double temp;
  isOut = true;
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
  delete[] ScanTimes;
  ScanTimes = new double[NDiffTimes];
  NScan[NIF-1] = 1;
  ScanTimes[0] = DiffTimes[0];
  ScanDur[NIF-1] = (double*) malloc(NDiffTimes*sizeof(double));

// By default, a difference of just 2 int. times (for all antennas) implies a scan change:
  if(MaxDT==0.0){MaxDT=2.*(DiffTimes[1]-DiffTimes[0]);};

  for(j=1;j<NDiffTimes;j++){
    if (std::abs(DiffTimes[j]-DiffTimes[j-1])>MaxDT){
      ScanTimes[NScan[NIF-1]] = DiffTimes[j];
      ScanDur[NIF-1][NScan[NIF-1]-1] = DiffTimes[j-1]-ScanTimes[NScan[NIF-1]-1];
      NScan[NIF-1] += 1;
    };
  };
  ScanTimes[NScan[NIF-1]] = DiffTimes[NDiffTimes-1]+1.;
  ScanDur[NIF-1][NScan[NIF-1]-1] = ScanTimes[NScan[NIF-1]] - ScanTimes[NScan[NIF-1]-1];
  Scan[NIF-1][0] = 0;



// Assign scan number to each visibility:
  for(j=0;j<NVis[NIF-1];j++){
    for(i=0;i<NScan[NIF-1];i++){
      if(Times[NIF-1][j]>=ScanTimes[i] && Times[NIF-1][j]<ScanTimes[i+1]){Scan[NIF-1][j]=i;break;};
    };
  };




  for(i=0;i<5;i++){
    Delays[i][NIF-1] = (double **) malloc(NCalAnt*sizeof(double*));
    Rates[i][NIF-1] = (double **) malloc(NCalAnt*sizeof(double*));
  };
  for(i=0;i<NCalAnt;i++){
    for(k=0;k<5;k++){
      Delays[k][NIF-1][i] = (double *) malloc(NScan[NIF-1]*sizeof(double));
      Rates[k][NIF-1][i] = (double *) malloc(NScan[NIF-1]*sizeof(double));
    };
    for(k=0;k<NScan[NIF-1];k++){
      for (j=0;j<5;j++){Delays[j][NIF-1][i][k] = 0.0; Rates[j][NIF-1][i][k] = 0.0;};
    };
  };

  sprintf(message,"Found %i scans for IF %i\n",NScan[NIF-1],IFN);
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  

// what are the times?
  for(j=0;j<NScan[NIF-1];j++){
    sprintf(message,"  scan %d time is %f; duration is %f\n", j, ScanTimes[j],ScanDur[NIF-1][j]);
    fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
  };

  free(DiffTimes);


abort1:
  delete[] ScanTimes;
  if(Iret!=0){
      Nchan=nullptr; Frequencies=nullptr; NVis=nullptr; NCVis=nullptr; NLVis=nullptr; IFNum=nullptr;
      Ant1=nullptr; Ant2=nullptr; Times=nullptr; PA1=nullptr; PA2=nullptr; UVGauss=nullptr;
      RR=nullptr; LR=nullptr; RL=nullptr; LL=nullptr; ScanDur=nullptr; Weights=nullptr;
      for(i=0;i<5;i++){
        Rates[i] = nullptr; Delays[i]=nullptr; CrossSpec00[i]=nullptr; CrossSpec11[i]=nullptr;
      };
  };
  if (CPfile.is_open()) CPfile.close();
  if (MPfile.is_open()) MPfile.close();
  if(logFile) {fclose(logFile); logFile=nullptr;};
  PyObject *ret = PyLong_FromLong(Iret);
  return ret;
};








/// GetIFs(ifNr) for invocation from Python like
static PyObject *GetIFs(PyObject *self, PyObject *args) {  
int i,j,k;
long Iret = 0;

  PyArrayObject *FreqsObj;
  double *copyFreq;

  if (!logFile) logFile = fopen("PolConvert.GainSolve.log","a");

  if (!PyArg_ParseTuple(args, "iO", &i,&FreqsObj)){
     sprintf(message,"Failed GetIFs! Check inputs!\n"); 
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
     Iret = -100;
     goto abort2;
  };

  sprintf(message,"Locating IFNum as %d \n", i);
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

  k=-1;
  for (j=0; j<NIF; j++){
    if(IFNum[j] == i){k=j;break;};
  };

  if(k<0){
    sprintf(message,"IF %d not found\n", i);
    fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
    Iret = -110;
    goto abort2;
  };

  copyFreq =  (double *)PyArray_DATA(FreqsObj);

  for (j=0; j<Nchan[k]; j++){
    copyFreq[j] = Frequencies[k][j];
  };


  sprintf(message,"IF %d with %d chans\n", i, Nchan[k]);
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
  std::cout << std::flush;

abort2:

  if (logFile) {fclose(logFile); logFile=nullptr;};
  PyObject *ret = PyLong_FromLong(Iret); 
  return ret;
};





///////////////////////////////
// Quinn Estimator of the FFT peak with sub-bin precision:

double QuinnTau(double x){
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









// Forces fringe rates (not used at the moment).
static PyObject *SetFringeRates(PyObject *self, PyObject *args) {


  int i,j,k,NantFix,cIF,cScan;
  double *rates[4];
  long Iret = 0;

  PyObject *antList;
  PyArrayObject *ratesArr;

  if (!logFile) logFile = fopen("PolConvert.GainSolve.log","a");

  if (!PyArg_ParseTuple(args, "iiOO", &cIF, &cScan, &ratesArr, &antList)){
     sprintf(message,"Failed SetFringeRates! Check inputs!\n"); 
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
     Iret = -120;
     goto abort3;
  };


  NantFix = (int) PyList_Size(antList);

  for(k=0;k<4;k++){
    rates[k] = (double *) PyArray_DATA(ratesArr);
    for (i=0; i<NCalAnt; i++){
      Rates[k][cIF][i][cScan] = 0.0;
    };
    for (i=0; i<NantFix; i++){
      j = (int) PyInt_AsLong(PyList_GetItem(antList,i));
      Rates[k][cIF][j-1][cScan] = rates[k][i];
    };
  };

abort3:
  if(logFile) {fclose(logFile); logFile=nullptr;};
  PyObject *ret = PyLong_FromLong(Iret);
  return ret;
};




// A simplified Global Fringe Fitting 
// (useful to pre-correct for high delays and/or rates).
static PyObject *DoGFF(PyObject *self, PyObject *args) {

  int i,j,k,l,m, a1,a2, af1, af2, BNum,cScan;
  double *T0 = new double[NBas];  
  double *T1 = new double[NBas];
  bool isFirst = true;
  int applyRate;
  bool showMe, gotAnts;
  cplx64f **aroundPeak = new cplx64f *[4]; 


  for(i=0;i<4;i++){
    aroundPeak[i] = new cplx64f[3];
  };

  PyObject *antList;

  if (!logFile) logFile = fopen("PolConvert.GainSolve.log","a");
  if (!PyArg_ParseTuple(args, "Oiiid", &antList,&npix, &applyRate,&cScan,&SNR_CUTOFF)){
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
  };


  NantFit = (int) PyList_Size(antList);
  delete[] antFit;
  antFit = new int[NantFit];

  for (i=0; i<NantFit; i++){
    antFit[i] = (int) PyInt_AsLong(PyList_GetItem(antList,i));
  };

// One element per polarization product:
  double ***BLRates = new double **[4];
  double ***BLDelays = new double **[4];
  double ***BLWeights = new double **[4];
  for (i=0;i<4;i++){
    BLRates[i] = new double *[NIF];
    BLDelays[i] = new double *[NIF];
    BLWeights[i] = new double *[NIF];
    for (j=0; j<NIF;j++){
      BLRates[i][j] = new double[NBas];
      BLDelays[i][j] = new double[NBas];
      BLWeights[i][j] = new double[NBas];
    };
  };

  int prevChan = Nchan[0];
  int prevNvis = NVis[0]/NBas;

// FFT FOR EACH BASELINE:
  int TotDim = NVis[0]*Nchan[0];
  int MaxDim = TotDim;
  for (j=1; j<NIF; j++){if(NVis[j]*Nchan[j]>MaxDim){MaxDim=NVis[j]*Nchan[j];};};

    fftw_complex **BufferVis = new fftw_complex *[4];
    fftw_complex **out = new fftw_complex *[4];
    BufferVis[0] = (fftw_complex *) fftw_malloc(sizeof(fftw_complex) * MaxDim);
    BufferVis[1] = (fftw_complex *) fftw_malloc(sizeof(fftw_complex) * MaxDim);
    BufferVis[2] = (fftw_complex *) fftw_malloc(sizeof(fftw_complex) * MaxDim);
    BufferVis[3] = (fftw_complex *) fftw_malloc(sizeof(fftw_complex) * MaxDim);
    fftw_complex *AUX = (fftw_complex *) fftw_malloc(sizeof(fftw_complex) * MaxDim);
    out[0] = (fftw_complex *) fftw_malloc(sizeof(fftw_complex) * MaxDim);
    out[1] = (fftw_complex *) fftw_malloc(sizeof(fftw_complex) * MaxDim);
    out[2] = (fftw_complex *) fftw_malloc(sizeof(fftw_complex) * MaxDim);
    out[3] = (fftw_complex *) fftw_malloc(sizeof(fftw_complex) * MaxDim);
    fftw_plan *pFT = new fftw_plan[4];
    pFT[0] = fftw_plan_dft_2d(prevNvis, Nchan[0], BufferVis[0], out[0], FFTW_FORWARD, FFTW_MEASURE);
    pFT[1] = fftw_plan_dft_2d(prevNvis, Nchan[0], BufferVis[1], out[1], FFTW_FORWARD, FFTW_MEASURE);
    pFT[2] = fftw_plan_dft_2d(prevNvis, Nchan[0], BufferVis[2], out[2], FFTW_FORWARD, FFTW_MEASURE);
    pFT[3] = fftw_plan_dft_2d(prevNvis, Nchan[0], BufferVis[3], out[3], FFTW_FORWARD, FFTW_MEASURE);

    cplx64f *Temp[4];
    cplx64f *BufferC[4];
    for (i=0; i<4; i++){
      Temp[i] = reinterpret_cast<std::complex<double> *>(out[i]);
      BufferC[i] = reinterpret_cast<std::complex<double> *>(BufferVis[i]);
    };

    int *inMatrix, NinMatrix;
    double Peak[4] = {0.,0.,0.,0.};
    double rmsFringe[4] = {0.,0.,0.,0.}; 
    double avgFringe[4] = {0.,0.,0.,0.}; 
    double AbsP;
    double Dnpix;
    int Chi, Chf, ti, tf;
    int *nu[4], *time[4], row;
    for(k=0;k<4;k++){
      nu[k] = new int[3]; 
      time[k]=new int[3];
    };
    sprintf(message,"Will fringe-fit %i baselines.\n",NBas); 
    fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  


    for(j=0; j<NBas; j++){   // baseline loop
             for (gotAnts = false, a1=0; a1<NCalAnt; a1++){
              for (a2=a1+1;a2<NCalAnt;a2++){
                if(j == BasNum[CalAnts[a1]-1][CalAnts[a2]-1]){
                  sprintf(message,"PROCESSING BASELINE %i (ANTS %i-%i)\n",j,CalAnts[a1]-1,CalAnts[a2]-1);
                  fprintf(logFile,"%s",message); // std::cout<<message;
                  fflush(logFile);
                  gotAnts = true;
                  break;
                };
              };
            };
            if (!gotAnts) {
              sprintf(message,"NO ANTENNAS FOUND FOR BASELINE %i!\n",j);
              fprintf(logFile,"%s",message); // std::cout<<message;
              fflush(logFile);
            };
      for (i=0; i<NIF; i++){  // IF loop
        inMatrix = new int[NVis[i]];
        NinMatrix = 0;
        isFirst = true;
        showMe = false;

        for(k=0;k<4;k++){
          BLRates[k][i][j] = 0.0;
          BLDelays[k][i][j] = 0.0;
        };
        int NcurrVis = 0;

// Arrange data for this baseline:
        for (k=0; k<NVis[i]; k++){
          a1 = Ant1[i][k];
          a2 = Ant2[i][k];
          BNum = BasNum[a1-1][a2-1];
          if (BNum==j && Scan[i][k]==cScan){
            inMatrix[NinMatrix]=k;
            NinMatrix += 1;
            if(isFirst){
              T0[j] = Times[i][k];
              T1[j] = Times[i][k];
              isFirst = false;
            };
            if (T0[j] > Times[i][k]){
              T0[j] = Times[i][k];
            };
            memcpy(&BufferC[0][NcurrVis*Nchan[i]],&RR[i][k][0],Nchan[i]*sizeof(cplx64f));
            memcpy(&BufferC[1][NcurrVis*Nchan[i]],&LL[i][k][0],Nchan[i]*sizeof(cplx64f));
            memcpy(&BufferC[2][NcurrVis*Nchan[i]],&RL[i][k][0],Nchan[i]*sizeof(cplx64f));
            memcpy(&BufferC[3][NcurrVis*Nchan[i]],&LR[i][k][0],Nchan[i]*sizeof(cplx64f));

            NcurrVis += 1;
            if (T1[j] < Times[i][k]){
              T1[j] = Times[i][k];
            };
          };
        };

        if (NcurrVis > 1) showMe = true;

        if (showMe) {
          sprintf(message,"   DoGFF: read %i visiblities\n",NcurrVis);
          fprintf(logFile,"%s",message); // std::cout<<message; 
          fflush(logFile);  
        };

/////////////////
// FFT the fringe and find the peak:
        if (NcurrVis > 2){
          sprintf(message,"FFT on IF %i\n",i+1);
          fprintf(logFile,"%s",message); // std::cout<<message; 
          fflush(logFile);  

// Re-define the FFTW plan if dimensions changed:
          if (Nchan[i] != prevChan || NcurrVis != prevNvis){
            prevChan = Nchan[i]; prevNvis = NcurrVis;

            for(k=0;k<4;k++){
              memcpy(&AUX[0],&BufferVis[k][0],NcurrVis*Nchan[i]*sizeof(fftw_complex));
              fftw_destroy_plan(pFT[k]);  
              pFT[k] = fftw_plan_dft_2d(NcurrVis, Nchan[i], BufferVis[k], out[k], FFTW_FORWARD, FFTW_MEASURE);
              memcpy(&BufferVis[k][0],&AUX[0],NcurrVis*Nchan[i]*sizeof(fftw_complex));
            };
          };
          for(k=0;k<4;k++){fftw_execute(pFT[k]);};
        };


        if (npix>0 && Nchan[i]>npix){Chi = npix/2; Chf = Nchan[i]-npix/2;}
        else  {Chi = Nchan[i]/2; Chf = Nchan[i]/2;};
        if (npix>0 && NcurrVis>npix){ti = npix/2; tf = NcurrVis-npix/2;}
        else  {ti = NcurrVis/2; tf = NcurrVis/2;};

        for(l=0;l<4;l++){ 
          for(k=0;k<3;k++){
             nu[l][k]=0; time[l][k]=0;
          }; 
          rmsFringe[l]=0.0; avgFringe[l]=0.0;
        };


        if (NcurrVis >2){
          Dnpix = (double) Nchan[i]*NcurrVis; 


          Peak[0]=0.0; Peak[1]=0.0; Peak[2]=0.0; Peak[3]=0.0;

// First Quadrant:
          for (l=0; l<ti; l++){
            row = l*Nchan[i];
            for (k=0; k<Chi;k++){
              for(m=0;m<4;m++){
                AbsP = std::abs(Temp[m][row + k]);
                if (AbsP>Peak[m]){
                  Peak[m] = AbsP;
                  nu[m][1] = k; time[m][1] = l; 
                };
                rmsFringe[m] += AbsP*AbsP; avgFringe[m] += AbsP;
              };
            };
          };

// Second Quadrant:
          for (l=tf; l<NcurrVis; l++){
            row = l*Nchan[i];
            for (k=0; k<Chi;k++){
              for(m=0;m<4;m++){
                AbsP = std::abs(Temp[m][row + k]);
                if (AbsP>Peak[m]){
                  Peak[m] = AbsP;
                  nu[m][1] = k; time[m][1] = l; 
                };
                rmsFringe[m] += AbsP*AbsP; avgFringe[m] += AbsP;
              };
            };
          };

// Third Quadrant:
          for (l=0; l<ti; l++){
            row = l*Nchan[i];
            for (k=Chf; k<Nchan[i];k++){
              for(m=0;m<4;m++){
                AbsP = std::abs(Temp[m][row + k]);
                if (AbsP>Peak[m]){
                  Peak[m] = AbsP;
                  nu[m][1] = k; time[m][1] = l; 
                };
                rmsFringe[m] += AbsP*AbsP; avgFringe[m] += AbsP;
              };
            };
          };

// Fourth Quadrant:
          for (l=tf; l<NcurrVis; l++){
            row = l*Nchan[i];
            for (k=Chf; k<Nchan[i];k++){
              for(m=0;m<4;m++){
                AbsP = std::abs(Temp[m][row + k]);
                if (AbsP>Peak[m]){
                  Peak[m] = AbsP;
                  nu[m][1] = k; time[m][1] = l; 
                };
                rmsFringe[m] += AbsP*AbsP; avgFringe[m] += AbsP;
              };
            };
          };

/////////
// Unwrap:

          for(m=0;m<4;m++){
            sprintf(message,"%i /  %i %i \n",m,time[m][1],nu[m][1]);
            fprintf(logFile,"%s",message); // std::cout<<message;
            if (nu[m][1]==0){nu[m][0]=Nchan[i]-1;} else{nu[m][0]=nu[m][1]-1;};
            if (nu[m][1]==Nchan[i]-1){nu[m][2]=0;} else{nu[m][2]=nu[m][1]+1;};
            if (time[m][1]==0){time[m][0]=NcurrVis-1;} else{time[m][0]=time[m][1]-1;};
            if (time[m][1]==NcurrVis-1){time[m][2]=0;} else{time[m][2]=time[m][1]+1;};
          };


// Get the SNR of the fringe (i.e., peak over RMS, but without the peak):

  // First, remove the peak (and pixels around it) from the RMS computation:
          for (l=0; l<3; l++){
            for (k=0; k<3; k++){
              for(m=0;m<4;m++){
                AbsP = std::abs(Temp[m][time[m][l]*Nchan[i] + nu[m][k]]);
                rmsFringe[m] -= AbsP*AbsP; avgFringe[m] -= AbsP;
              };
            };
          };

  // Then, compute the SNR for each polarization:
          for(m=0;m<4;m++){
            BLWeights[m][i][j] = Peak[m]/pow(rmsFringe[m]/(Dnpix-9.) - pow(avgFringe[m]/(Dnpix-9.),2.),0.5);
          };
        
  // Set Weight of visibilities:
          for(l=0;l<NinMatrix;l++){
             k = inMatrix[l];
             Weights[i][k] = 0.0;
             for(m=0;m<4;m++){Weights[i][k] += BLWeights[m][i][j];};
             Weights[i][k] /= 4.0;
          };  


//////////////////
////////////////////

// Estimate the rates with sub-bin precision:

          for(m=0;m<4;m++){
            aroundPeak[m][0] = Temp[m][nu[m][1] + Nchan[i]*(time[m][0])];
            aroundPeak[m][1] = Temp[m][nu[m][1] + Nchan[i]*(time[m][1])];
            aroundPeak[m][2] = Temp[m][nu[m][1] + Nchan[i]*(time[m][2])];
            BLRates[m][i][j] = ((double) time[m][1]);
            BLRates[m][i][j] += QuinnEstimate(aroundPeak[m]);
            if (BLRates[m][i][j] > ((double) NcurrVis)/2.){
              BLRates[m][i][j] = BLRates[m][i][j] - (double) NcurrVis;
            }; 
            BLRates[m][i][j] *= 1./(T1[j]-T0[j]);
          };

////////////////////
// Estimate the delays with sub-bin precision:

          for(m=0;m<4;m++){
            aroundPeak[m][0] = Temp[m][nu[m][0] + Nchan[i]*(time[m][1])];
            aroundPeak[m][1] = Temp[m][nu[m][1] + Nchan[i]*(time[m][1])];
            aroundPeak[m][2] = Temp[m][nu[m][2] + Nchan[i]*(time[m][1])];
            BLDelays[m][i][j] = ((double) nu[m][1]);
            BLDelays[m][i][j] += QuinnEstimate(aroundPeak[m]);
            if (BLDelays[m][i][j] > ((double) Nchan[i])/2.){
              BLDelays[m][i][j] = BLDelays[m][i][j] - (double) Nchan[i];
            }; 
            BLDelays[m][i][j] *= 1./(Frequencies[i][Nchan[i]-1]-Frequencies[i][0]);
          };

          } else {   // Comes from if(NcurrVis > 2)

            for(m=0;m<4;m++){
              BLRates[m][i][j]=0.0;BLDelays[m][i][j]=0.0;BLWeights[m][i][j]=0.0;
            };
            sprintf(message,"WARNING! BASELINE %i HAS NO DATA IN IF %i!\n",j,i+1);
            fprintf(logFile,"%s",message); // std::cout<<message; 
            fflush(logFile);  
          
          };

//////


          if (showMe){
            for(m=0;m<4;m++){
              sprintf(message,"POL %i (BL %i): PEAK OF %.3e AT INDEX %i-%i (RATE %.3e Hz, DELAY: %.3e s); SNR: %.3e\n",
                   m,j,Peak[m],time[m][1],nu[m][1],BLRates[m][i][j],BLDelays[m][i][j],BLWeights[m][i][j]);
              fprintf(logFile,"%s",message); // std::cout<<message; 
              fflush(logFile);  
            };
          };



        delete[] inMatrix;


        };  // end IF loop
////////////////////
      };  // end baseline loop



//////////////////////
//// PRELIMINARY ESTIMATE OF CROSS-POLARIZATION GAINS!







/////////////////
// Globalize the rate and delay solutions:

    sprintf(message,"# of free antenna gains (per pol.): %i\n",NantFit);
    fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  


// Setup memory:
    double **HessianDel = new double*[4];
    for(m=0;m<4;m++){HessianDel[m] = new double[NantFit*NantFit];};
    double *RateResVec[4]; // = new double[NantFit];
    double *DelResVec[4];
    for(m=0;m<4;m++){DelResVec[m] = new double[NantFit];RateResVec[m] = new double[NantFit];};
    delete[] CovMat;
    CovMat = new double[NantFit*NantFit];
    for (i=0;i<NantFit*NantFit;i++){
      for(m=0;m<4;m++){HessianDel[m][i]=0.0;}
      CovMat[i] = 0.0;
    };
    for (i=0;i<NantFit;i++){
      for(m=0;m<4;m++){DelResVec[m][i] = 0.0;RateResVec[m][i] = 0.0;};
    };




////////////////////////////////////
// AT THE MOMENT, WE COMBINE ALL IFS FOR THE SAME RATES AND DELAYS:

 for(i=0; i<NIF; i++){
   sprintf(message,"For IF %i:\n",IFNum[i]);fprintf(logFile,"%s",message);
   for (a1=0; a1<NCalAnt; a1++){
     for (a2=a1+1;a2<NCalAnt;a2++){
        BNum = BasNum[CalAnts[a1]-1][CalAnts[a2]-1];
        for(j=0; j<Ntwin; j++) {
           if((Twins[0][j]==CalAnts[a1] && Twins[1][j]==CalAnts[a2])||
             (Twins[0][j]==CalAnts[a2] && Twins[1][j]==CalAnts[a1])){
               BNum = -1; break;
           };
        };
        if(BNum>=0){
          sprintf(message,"Bas: %i-%i | Rates: %.2e %.2e %.2e %.2e | Delays: %.2e %.2e %.2e %.2e\n",CalAnts[a1],CalAnts[a2],BLRates[0][i][BNum],BLRates[1][i][BNum],BLRates[2][i][BNum],BLRates[3][i][BNum],BLDelays[0][i][BNum],BLDelays[1][i][BNum],BLDelays[2][i][BNum],BLDelays[3][i][BNum]);fprintf(logFile,"%s",message);
	  fflush(logFile);
        };
     };
   };
 };


    if (NantFit>1){

      for (i=0; i<NIF; i++){

        for (a1=0; a1<NCalAnt; a1++){
          for (a2=a1+1;a2<NCalAnt;a2++){

            af1 = -1; af2 = -1;
            for(j=0; j<NantFit; j++) {
              if (CalAnts[a1]==antFit[j]){af1 = j;};
              if (CalAnts[a2]==antFit[j]){af2 = j;};
            };
            BNum = BasNum[CalAnts[a1]-1][CalAnts[a2]-1];

            for(j=0;j<Ntwin;j++){
              if((Twins[0][j]==CalAnts[a1] && Twins[1][j]==CalAnts[a2])||
                 (Twins[0][j]==CalAnts[a2] && Twins[1][j]==CalAnts[a1])){
                    BNum = -1; break;
              };
            };

            if (BNum>=0 && BLWeights[0][i][BNum]>SNR_CUTOFF && BLWeights[1][i][BNum]>SNR_CUTOFF){

              if (af1>=0){
                for(m=0;m<4;m++){
                  RateResVec[m][af1] += BLWeights[m][i][BNum]*BLRates[m][i][BNum];
                  DelResVec[m][af1] += BLWeights[m][i][BNum]*BLDelays[m][i][BNum];
                  HessianDel[m][af1*NantFit+af1] += BLWeights[m][i][BNum];
                };
              };

              if (af2>=0){
                for(m=0;m<4;m++){
                  RateResVec[m][af2] -= BLWeights[m][i][BNum]*BLRates[m][i][BNum];
                  DelResVec[m][af2] -= BLWeights[m][i][BNum]*BLDelays[m][i][BNum];
                  HessianDel[m][af2*NantFit+af2] += BLWeights[m][i][BNum];
                };
              };

              if (af1>=0 && af2>=0){
                for(m=0;m<4;m++){
                  HessianDel[m][af1*NantFit+af2] -= BLWeights[m][i][BNum];
                  HessianDel[m][af2*NantFit+af1] -= BLWeights[m][i][BNum];
                };
              };
            };
          };
        };
      };

/////////////////////////////////
    } else {

      BNum = BasNum[CalAnts[0]-1][CalAnts[1]-1];

      for (i=0; i<NIF; i++){

        for(m=0;m<4;m++){
          HessianDel[m][0] += BLWeights[m][i][BNum];
          RateResVec[m][0] += BLWeights[m][i][BNum]*BLRates[m][i][BNum];
          DelResVec[m][0] += BLWeights[m][i][BNum]*BLDelays[m][i][BNum];
        };

        for (j=0; j<NCalAnt; j++){
          for(m=0;m<5;m++){
            Rates[m][i][j][cScan] = 0.0;
            Delays[m][i][j][cScan] = 0.0;
          };

          if(CalAnts[j]==antFit[0]){
            for(m=0;m<4;m++){
              if (applyRate>0){
                Rates[m][i][j][cScan] = -RateResVec[m][0]/((double) NIF);};
                Delays[m][i][j][cScan] = -DelResVec[m][0]/((double) NIF);
            };
          };
        };
      };
    };

    sprintf(message,"\n\nHessian Globalization Matrix (RR):\n\n");
    fprintf(logFile,"%s",message);fflush(logFile);
    bool isSingular;
    isSingular=false;
    for (i=0; i<NantFit; i++){
      sprintf(message,"  ");fprintf(logFile,"%s",message);
      for (j=0; j<NantFit; j++){
        sprintf(message,"%.2e ",HessianDel[0][i*NantFit+j]);fprintf(logFile,"%s",message);
      };
      sprintf(message,"\n");fprintf(logFile,"%s",message);
    };
    if(isSingular){
      sprintf(message,"Possible singular matrix!\n"); 
      fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
    };
    //printf("\n");
    sprintf(message,"\n\n Residual baseline phase quantities (DELA || RATE):\n\n");
    fprintf(logFile,"%s",message);
    for (i=0; i<NantFit; i++){
      for(m=0;m<4;m++){sprintf(message," %.2e |", DelResVec[m][i]);fprintf(logFile,"%s",message);};
      sprintf(message,"| ");fprintf(logFile,"%s",message);
      for(m=0;m<4;m++){sprintf(message," %.2e |", RateResVec[m][i]);fprintf(logFile,"%s",message);};
      sprintf(message,"\n");fprintf(logFile,"%s",message);
    };
    //printf("\n");


    double **SolRat = new double*[4];
    double **SolDel = new double*[4];
    double **SolErrRat = new double*[4];
    double **SolErrDel = new double*[4];
    for (i=0; i<4; i++){
      SolDel[i] = new double[NantFit]; SolRat[i] = new double[NantFit];
      SolErrDel[i] = new double[NantFit]; SolErrRat[i] = new double[NantFit];
    };



    isSingular = false;
      for(i=0;i<4;i++){
          isSingular = solveSystem(NantFit,HessianDel[i],DelResVec[i],SolDel[i], SolErrDel[i]);
          isSingular = solveSystem(NantFit,HessianDel[i],RateResVec[i],SolRat[i],SolErrRat[i]);
      };


    double WgtTemp, WgtTot;

    if (NantFit>1){

    for (i=0; i<NCalAnt; i++){

      WgtTot = 0.0; WgtTemp = 0.0;


      for(j=0;j<5;j++){Delays[j][0][i][cScan] = 0.0; Rates[j][0][i][cScan] = 0.0;};
        af1 = -1;

        for(j=0; j<NantFit; j++) {
          if (CalAnts[i]==antFit[j]){
            af1 = j; 
            break;
          };
        };
       if (af1 >=0){
         if(applyRate>0){
           Rates[4][0][i][cScan] = 0.0;
           Delays[4][0][i][cScan] = 0.0;
           for(j=0;j<4;j++){
             Rates[j][0][i][cScan] = -SolRat[j][af1]; 
             Delays[j][0][i][cScan] = -SolDel[j][af1]; 
             WgtTemp = 1./(SolErrDel[j][af1]*SolErrDel[j][af1]);
             WgtTot += WgtTemp;
             Delays[4][0][i][cScan] += -SolDel[j][af1]*WgtTemp;
             Rates[4][0][i][cScan] += -SolRat[j][af1]*WgtTemp;
           };
           Rates[4][0][i][cScan] /= WgtTot;
           Delays[4][0][i][cScan] /= WgtTot;
         };
       };
    };




    for (i=1; i<NIF; i++){
      for (j=0; j<NCalAnt; j++){
        for (m=0;m<4;m++){
          Rates[m][i][j][cScan] = Rates[m][0][j][cScan];
          Delays[m][i][j][cScan] = Delays[m][0][j][cScan];
        };
      };
    };


  };



// Print globalized results:
  for (i=0; i<NCalAnt; i++){

    sprintf(message, "\n -- Antenna %i: [ Rate (Hz) / Delay (ns) ]:\n",i); fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
    for(j=0; j<4; j++){
      sprintf(message, "POL %i: [ %.3e / %.3e ] \n",j,Rates[j][0][i][cScan],Delays[j][0][i][cScan]);
      fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
    };
  };


// Release memory:

  delete[] T0;
  delete[] T1;

  for (m=0;m<4;m++){
    for (i=0; i<NIF;i++){
      delete[] BLRates[m][i];
      delete[] BLDelays[m][i];
      delete[] BLWeights[m][i];
    };
    delete[] BLRates[m];
    delete[] BLDelays[m];
    delete[] BLWeights[m];
    delete[] HessianDel[m];
    delete[] DelResVec[m];
    delete[] RateResVec[m];
    delete[] aroundPeak[m];
  };
  delete[] BLRates;
  delete[] BLDelays;
  delete[] BLWeights;
  delete[] aroundPeak;
  delete[] HessianDel;

  fftw_free(AUX);                      
  for (i=0; i<4; i++) {
    fftw_destroy_plan(pFT[i]);
    fftw_free(BufferVis[i]);
    fftw_free(out[i]);
    delete[] SolRat[i]; delete[] SolDel[i];
    delete[] SolErrRat[i]; delete[] SolErrDel[i];
    delete[] nu[i]; delete[] time[i];
  };
  delete[] pFT;
  delete[] SolRat; delete[] SolDel;
  delete[] SolErrRat; delete[] SolErrDel;
  delete[] BufferVis; delete[] out;
  fftw_cleanup();

// Return success:
  PyObject *ret = Py_BuildValue("i",0);
  return ret;

};



































// Setup fit parameters and book memory.
static PyObject *SetFit(PyObject *self, PyObject *args) {

  int i, j, k, oldNpar = Npar;
  bool foundit;

  if (!logFile) logFile = fopen("PolConvert.GainSolve.log","a");



  PyObject *IFlist, *antList, *calstokes, *ret;
  PyArrayObject *feedPy;

  if (!PyArg_ParseTuple(args, "iOOiiOiO", 
     &Npar, &IFlist, &antList, &solveAmp, &solveQU, &calstokes, &useCov, &feedPy)){
        sprintf(message,"Failed SetFit! Check inputs!\n"); 
        fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
        fclose(logFile); logFile=nullptr;
        ret = Py_BuildValue("i",-1);
      return ret;
  };


  if (Tm){
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
    sprintf(message,"Clearing previous allocation objects\n");
    fprintf(logFile,"%s",message); // std::cout<<message; fflush(logFile);
  };

  Tm = new double[NBas];

  T0 = Times[0][0];
  T1 = Times[0][NVis[0]-1];
  DT = TAvg;

  feedAngle = (double *)PyArray_DATA(feedPy);

  NIFComp = (int) PyList_Size(IFlist);
  doIF = new int[NIFComp];

  NantFit = (int) PyList_Size(antList);
  antFit = new int[NantFit];


// Temporary code for testing: reset the solution algorithm, depending on whether
// we have SBDs in the parameter list:
  if (Npar<NantFit*3){SolAlgor=1;}else{SolAlgor=0;};


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
      fclose(logFile); logFile = nullptr;
      ret = Py_BuildValue("i",-1);
      return ret;
    };
  };

  CovMat = new double[Npar*Npar];
  IndVec = new double[Npar];
  SolVec = new double[Npar];

//////////////////////
  StokesSolve = solveQU;
  for (i=0; i<4; i++){
    Stokes[i] = (double) PyFloat_AsDouble(PyList_GetItem(calstokes,i));
  };

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




















// Compute the Chi Square of a model of cross-polarization gains
// (on both circular and linear antennas) using all four
// correlation products.
static PyObject *GetChi2(PyObject *self, PyObject *args) { 

  int Ch0, Ch1;
  int i, k,l, end;
  int j= -1;
  long Iret = 0;
  double *CrossG;
  double Drate1R, Drate1L, Drate2R, Drate2L, Drate1, Drate2; 
  double Ddelay1R, Ddelay2R, Ddelay1L, Ddelay2L, Ddelay1, Ddelay2;
  PyObject *ret,*LPy;
  PyArrayObject *pars;
  bool useRates, useDelays;

  int MBD1p, MBD2p, G1pA,G1pF, G2pA, G2pF;
  int BNum;
  double Chi2 = 0.0;
  double auxD1, auxD2;
  double *chanFreq;

 int currIF, a1, a2, ac1,ac2,af1, af2, currDer, currScan, nextScan;
  int is1, is2; 

  cplx64f Error;
  cplx64f oneC, RateFactor, FeedFactor1, FeedFactor2, RRRate, RLRate, LRRate, LLRate;

  cplx64f RM1, RP1;
  cplx64f RM2, RP2;
  cplx64f auxC1, auxC2, auxC3;

  int Nflipped = 0;

  double ParHandWgt = 1.0;
  double CrossHandWgt = 1.0;
  double Itot = 0.0;


// Reference frequency for the MBD:
  double RefNu = Frequencies[doIF[0]][0];


  if (!logFile) logFile = fopen("PolConvert.GainSolve.log","a");
  if (!PyArg_ParseTuple(args, "OOiiibb", &pars, &LPy, &Ch0, &Ch1,&end,&useRates,&useDelays)){
     sprintf(message,"Failed GetChi2! Check inputs!\n"); 
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
     Iret = -130;
     goto abortChi;
  };




  chisqcount++;


  Lambda = PyFloat_AsDouble(LPy);
  doCov = Lambda >= 0.0;



// Find out IFs to compute and do sanity checks:

  for (i=0; i<NIFComp; i++){
    if (Ch1 > Nchan[doIF[i]]){
      sprintf(message,"IF %i ONLY HAS %i CHANNELS. CHANNEL %i DOES NOT EXIST! \n",j,Nchan[doIF[i]], Ch1); 
      fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
      Iret = -140;
      goto abortChi;
    };
  };



  if (Ch0<0 || Ch0>Ch1){
    sprintf(message,"BAD CHANNEL RANGE: %i TO %i. SHOULD ALL BE POSITIVE AND Ch0 < Ch1\n",Ch0,Ch1); 
    fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
    Iret = -150;
    goto abortChi;
  };



// Memory to store the current gain ratios (aka cross-polarization gains):
  CrossG = (double *) PyArray_DATA(pars);





// Store memory for Stokes parameters (if we solve for them):
  if(StokesSolve){
    Stokes[0] = 1.0; Stokes[3] = 0.0;
    for (i=1; i<3; i++){Stokes[i] = CrossG[Npar-3+i];};
  };



// Avoid overflow errors to very large RelWeights (i.e., divide the crosshand
// instead of multipyling the parhand)
  if (RelWeight<1.0){ParHandWgt=RelWeight;}else{CrossHandWgt=1./RelWeight;};





  oneC= cplx64f(1.0,0.0);




/// Loop over IFs:
  for (i=0; i<NIFComp; i++){

    chanFreq = new double[Nchan[doIF[i]]];
    for (j=0;j<Nchan[doIF[i]];j++){
      chanFreq[j] = (double) (j-Nchan[doIF[i]]/2);
    };



// Reset temporary arrays to store visibilities:
    currIF = doIF[i];
    for (k=0;k<NBas;k++){
      UVWeights[k]= 0.0;
      for(j=0;j<Npar+1;j++){
        auxC00[k][j] = cplx64f(0., 0.);
        auxC01[k][j] = cplx64f(0., 0.);
        auxC10[k][j] = cplx64f(0., 0.);
        auxC11[k][j] = cplx64f(0., 0.);
        auxC00Flp[k][j] = cplx64f(0., 0.);
        auxC11Flp[k][j] = cplx64f(0., 0.);
      };
      AvVis[k] = 0;
    };
    for(k=0;k<NBas;k++){
      Tm[k]=Times[currIF][0];
    };


// Figure out which antennas do we have now
// and whether we solve for them:
    for (k=0; k<NVis[currIF]; k++){
      a1 = Ant1[currIF][k];
      a2 = Ant2[currIF][k];
      currScan = Scan[currIF][k];
      if(k<NVis[currIF]-1){nextScan=Scan[currIF][k+1];}else{nextScan=currScan+1;};
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
// ONLY IF THE ANTENNA GAINS ARE TO BE FITTED:

// Find which antenna(s) are linear:
    is1 = false ; is2 = false;
// (Notice that we can also solve for R/L gains 
// for the circular antennas)

    BNum = BasNum[a1-1][a2-1];
    for(j=0; j<Nlin; j++) {
      if (a1==Lant[j]){is1 = true;};
      if (a2==Lant[j]){is2 = true;};
    };



// The crossGains at the ref. channel: 


// Figure out the antenna gains being used now:
    if (solveAmp==0){
        
      if (af1 >= 0){
        G1pA = -1; G1pF = af1;
        G1[0] = std::polar(1.0, CrossG[af1]); 
      } else { G1[0] = std::polar(1.0, 0.0); G1pA = -1; G1pF = -1;}; 

        if (af2 >= 0){
          G2pA = -1; G2pF = af2;
          G2[0] = std::polar(1.0, -CrossG[af2]); 

        } else { G2[0] = std::polar(1.0, 0.0); G2pA = -1; G2pF = -1;}; 

      
    } else {


      if (af1 >= 0){
        G1pA = af1*2; G1pF = G1pA+1;
        G1[0] = std::polar(CrossG[G1pA], CrossG[G1pF]);  // AMP+PHASE SPACE

      } else { G1[0] = std::polar(1.0, 0.0); G1pA=-1; G1pF=-1;};

        if (af2 >= 0){
          G2pA = af2*2; G2pF = G2pA+1;
          G2[0] = std::polar(CrossG[G2pA], -CrossG[G2pF]); // AMP+PHASE SPACE
      } else { G2[0] = std::polar(1.0, 0.0);G2pA=-1; G2pF=-1;}; 

    };




    if(BNum>=0){

      AvVis[BNum] += 1;
      FeedFactor1 = std::polar(1.0, feedAngle[a1-1])*PA1[currIF][k]; 
      FeedFactor2 = std::polar(1.0, feedAngle[a2-1])*PA2[currIF][k];

      for (j=Ch0; j<Ch1; j++){

// Add the multi-band delays:
        if (SolAlgor == 0){
          if (af1 >= 0){
            if(solveAmp==0){
              MBD1p = NantFit+af1;
              MBD1[0] = CrossG[MBD1p]*(chanFreq[j]);
            } else {
              MBD1p = NantFit*2+af1;
              MBD1[0] = CrossG[MBD1p]*(chanFreq[j]);
            };
        } else {
           MBD1[0] = 0.0; MBD1p=-1;
        };
        if (af2 >= 0){
          if(solveAmp==0){
            MBD2p = NantFit+af2;
            MBD2[0] = CrossG[MBD2p]*(chanFreq[j]);
          } else {
            MBD2p = NantFit*2+af2;
            MBD2[0] = CrossG[MBD2p]*(chanFreq[j]);
          };
        } else {
           MBD2[0] = 0.0; MBD2p = -1;
        };
        G1nu[0] = G1[0]*(std::polar(1.0,MBD1[0]));
        G2nu[0] = G2[0]*(std::polar(1.0,-MBD2[0]));


      } else {

        MBD1p=-1;MBD2p=-1;
        G1nu[0] = G1[0]; 
        G2nu[0] = G2[0]; 

     };





// Compute the instrumental phases (for all pol. products):
// First, we center the fringes using the (circular-pol) antenna gains derived from the GFF:
// NOTE: if useDelay=false, the cross-pol delays from GFF are NOT used. Only the rates:

  RateFactor=1.0;
  Ddelay1R = 0.0; Ddelay1L = 0.0; Drate1R = 0.0; Drate1L = 0.0;
  Ddelay2R = 0.0; Ddelay2L = 0.0; Drate2R = 0.0; Drate2L = 0.0;
  Ddelay1 = 0.0; Ddelay2 = 0.0; Drate1 = 0.0; Drate2 = 0.0;

    if(ac1>=0){ // and !is1){
            Ddelay1R = TWOPI*((Delays[0][0][ac1][currScan])*(Frequencies[currIF][j]-RefNu));
            Ddelay1L = TWOPI*((Delays[1][0][ac1][currScan])*(Frequencies[currIF][j]-RefNu));
            Ddelay1 = 0.5*(Ddelay1R + Ddelay1L);
	    if(useRates){
              Drate1R =  TWOPI*((Rates[0][0][ac1][currScan])*(Times[currIF][k]-T0));
              Drate1L =  TWOPI*((Rates[1][0][ac1][currScan])*(Times[currIF][k]-T0));
              Drate1 = 0.5*(Drate1R + Drate1L);
            } else {Drate1R=0.0;Drate1L=0.0;Drate1=0.0;};
            
    };
 
    if(ac2>=0){ // and !is2){
            Ddelay2R = TWOPI*((Delays[0][0][ac2][currScan])*(Frequencies[currIF][j]-RefNu));
            Ddelay2L = TWOPI*((Delays[1][0][ac2][currScan])*(Frequencies[currIF][j]-RefNu));
            Ddelay2 = 0.5*(Ddelay2R + Ddelay2L);
            if(useRates){
	      Drate2R =  TWOPI*((Rates[0][0][ac2][currScan])*(Times[currIF][k]-T0));
	      Drate2L =  TWOPI*((Rates[1][0][ac2][currScan])*(Times[currIF][k]-T0));
              Drate2 = 0.5*(Drate2R + Drate2L);
            } else { Drate2R=0.0;Drate2L=0.0;Drate2=0.0;};
    };

    if(useDelays){
      RateFactor = std::polar(1.0, Drate1-Drate2 + Ddelay1-Ddelay2);
      RRRate = RateFactor*FeedFactor1/FeedFactor2; 
      RLRate = RateFactor*FeedFactor1/FeedFactor2; 
      LRRate = RateFactor*FeedFactor1/FeedFactor2; 
      LLRate = RateFactor*FeedFactor1/FeedFactor2; 
    } else {
      // TODO: Activate the rate correction, to improve gain estimates.
      // BUT some baselines usually give crazy rates (around 1Hz!!)
      RateFactor = std::polar(1.0, Drate1-Drate2);  
      RRRate = RateFactor*FeedFactor1/FeedFactor2;
      LLRate = RateFactor/FeedFactor1*FeedFactor2; 
      RLRate = RateFactor*FeedFactor1*FeedFactor2; 
      LRRate = RateFactor/FeedFactor1/FeedFactor2; 
    };

//  RRRate = 1.;
//  RLRate = 1.;
//  LRRate = 1.;
//  LLRate = 1.;











// Compute all the model derivatives:

      currDer = 0;
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
       if (is1 && is2){
         auxC1 = (RP1*RP2*RR[currIF][k][j] + RP2*RM1*LR[currIF][k][j] + RM2*RP1*RL[currIF][k][j] + RM1*RM2*LL[currIF][k][j])*RRRate;
         auxC2 = (RP1*RP2*LL[currIF][k][j] + RP2*RM1*RL[currIF][k][j] + RM2*RP1*LR[currIF][k][j] + RM1*RM2*RR[currIF][k][j])*LLRate;
         auxC00[BNum][currDer] += auxC1;
         auxC11[BNum][currDer] += auxC2;
       } else if (is1){
         auxC1 = (RP1*RR[currIF][k][j] + RM1*LR[currIF][k][j])*RRRate;
         auxC2 = (RP1*LL[currIF][k][j] + RM1*RL[currIF][k][j])*G2nu[currDer]*LLRate;
         auxC00[BNum][currDer] += auxC1;
         auxC11[BNum][currDer] += auxC2;
       } else if (is2){
         auxC1 = (RP2*RR[currIF][k][j] + RM2*RL[currIF][k][j])*RRRate;
         auxC2 = (RP2*LL[currIF][k][j] + RM2*LR[currIF][k][j])*G1nu[currDer]*LLRate;
         auxC00[BNum][currDer] += auxC1;
         auxC11[BNum][currDer] += auxC2;
       } else {
         auxC1 = RR[currIF][k][j]*RRRate;
         auxC2 = LL[currIF][k][j]*G2nu[currDer]*G1nu[currDer]*LLRate;
         auxC00[BNum][currDer] += auxC1;
         auxC11[BNum][currDer] += auxC2;
       };

// Accumulate the parallel hands with the flipped parangle:
       if(currDer==0){
         auxC3 = (PA2[currIF][k]/PA1[currIF][k])*(PA2[currIF][k]/PA1[currIF][k]);
         auxC00Flp[BNum][0] += auxC1*auxC3;
         auxC11Flp[BNum][0] += auxC2/auxC3;
         UVWeights[BNum] += UVGauss[currIF][k];
       };



    };  // Comes from loop over channels.



    cplx64f TempC;


////////////////////////////////////////////////////////
// UPDATE THE COVARIANCE MATRIX AND RESIDUALS VECTOR
// 
// 
// Did we reach the pre-averaging time?? If so, update the covariance+residuals:
    if ((Times[currIF][k]>=Tm[BNum] + DT) || !(currScan==nextScan)){


      if(k<NVis[currIF]-1){Tm[BNum] = Times[currIF][k+1];};

      Itot = 0.5*(std::abs(auxC00[BNum][0]) + std::abs(auxC11[BNum][0]));

      if (abs(auxC11[BNum][0])>0.0){
        Error = auxC00[BNum][0] - auxC11[BNum][0];
      };






//////////////////////////
// UPDATE THE CHI SQUARE
     if(AddCrossHand){
       auxD1 = std::abs(auxC01[BNum][0])/Itot; auxD2 = std::abs(auxC10[BNum][0])/Itot; 
       Chi2 += (auxD1*auxD1 + auxD2*auxD2)*CrossHandWgt*BasWgt[BNum]*Weights[currIF][k]*UVWeights[BNum];
     };
     if (RelWeight>0.0){
       if (abs(auxC11[BNum][0])>0.0){
// ALTERNATIVE OPTION: DIFFERENCE OF PARALLEL HANDS:
         auxC1 = Error - ((Stokes[0]+Stokes[3]) - (Stokes[0]-Stokes[3]));
         auxD1 = auxC1.real()*auxC1.real() + auxC1.imag()*auxC1.imag();
         Chi2 += auxD1*ParHandWgt*BasWgt[BNum]*Weights[currIF][k]*UVWeights[BNum];

// XPOLGFF OPTION: RATIO OF PARALLEL HANDS:
//         auxC1 = Error - (Stokes[0]+Stokes[3])/(Stokes[0]-Stokes[3]);
//         auxD1 = auxC1.real()*auxC1.real() + auxC1.imag()*auxC1.imag();
//         Chi2 += auxD1*ParHandWgt*BasWgt[BNum]*Weights[currIF][k]*UVWeights[BNum];
       };
     };
     if(end==1){
// Figure out if flipping R->L improves the phase of the RR/LL ratio:
       for(l=0; l<NLinBas; l++){
         if (LinBasNum[l]==BNum){  
// ALTERNATIVE OPTION:
           double GoodAmp = std::abs(Error);
	   double FlippedAmp = std::abs(auxC00Flp[BNum][0]-auxC11Flp[BNum][0]);
           if (FlippedAmp<GoodAmp){Nflipped += 1;}else{Nflipped -= 1;};

// XPOLGFF OPTION:
//           double GoodPhase = std::arg(Error);
//	   double FlippedPhase = std::arg(auxC00Flp[BNum][0]/auxC11Flp[BNum][0]);
//           if (std::abs(FlippedPhase)<std::abs(GoodPhase)){Nflipped += 1;}else{Nflipped -= 1;};
           break;
         };
       };   
     };



// Reset temporal visibility averages:
    for(j=0;j<Npar+1;j++){
      auxC00[BNum][j] = cplx64f(0., 0.);
      auxC01[BNum][j] = cplx64f(0., 0.);
      auxC10[BNum][j] = cplx64f(0., 0.);
      auxC11[BNum][j] = cplx64f(0., 0.);
      auxC00Flp[BNum][j] = cplx64f(0., 0.);
      auxC11Flp[BNum][j] = cplx64f(0., 0.);

    };
    AvVis[BNum] = 0;
    UVWeights[BNum] = 0.0;

   }; // Comes from:   if (Times[currIF][k]>=Tm[BNum] + DT)

   }; // Comes from if(BNum>=0) 

  };  // Comes from loop over visibilities

  delete[] chanFreq;

};  // Comes from loop over NIFComp




  Chi2Old = Chi2;

    if (end==1){Chi2 = (Nflipped>0)?1.0:-1.0;};

abortChi:
// We return the ChiSq (if OK) or a negative error code:
    if(Iret==0){
	    ret = Py_BuildValue("d",Chi2);}
    else {
	    ret = PyLong_FromLong(Iret);
    };


  if(logFile){fclose(logFile); logFile = nullptr;};
  return ret;

};


// eof











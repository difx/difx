/* CROSSPHASECAL - Cross-polarization phasecal extraction for PolConvert

             Copyright (C) 2018-2022  Ivan Marti-Vidal
             Centro Astronomico de Yebes (Spain)
             University of Valencia (Spain)
              
             Co-developer: Javier Gonzalez (CAY, Spain).


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

#include <stdio.h>  
#include <stdlib.h>
#include <sys/types.h>
#include <new>
#include <ctime>
#include <sys/stat.h>
#include <string.h>
#include <dirent.h>
#include <iostream>
#include <fstream>
#include <complex>
#include <sstream> 
#include <iomanip>

#define EPSILON 0.00001

typedef std::complex<float> cplx32f;
typedef std::complex<double> cplx64d;



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
    "Interface to read/write phasecal metadata into PolConvert.";

static char XPCalMF_docstring[] =
    "Reads and interpolates phasecal-tone phase values, given in DiFX format. \n Supports multi-file. It can either return the time average of the cross-polarization phase \n or overwrite the pcal file with the polconverted solutions.\n";


/* Available functions */
static PyObject *XPCalMF(PyObject *self, PyObject *args);


/* Module specification */
static PyMethodDef module_methods[] = {
    {"XPCalMF", XPCalMF, METH_VARARGS, XPCalMF_docstring},
    {NULL, NULL, 0, NULL}   /* terminated by list of NULLs, apparently */
};


/* Initialize the module */

#if PY_MAJOR_VERSION >= 3
static struct PyModuleDef pc_module_def = {
    PyModuleDef_HEAD_INIT,
    "_XPCalMF",               /* m_name */
    module_docstring,       /* m_doc */
    -1,                     /* m_size */
    module_methods,         /* m_methods */
    NULL,NULL,NULL,NULL     /* m_reload, m_traverse, m_clear, m_free */
};
PyMODINIT_FUNC PyInit__XPCalMF(void)
{
    PyObject *m = PyModule_Create(&pc_module_def);
    import_array();
    return(m);
}
#else

PyMODINIT_FUNC init_XPCalMF(void)
{
    import_array();
    PyObject *m = Py_InitModule3("_XPCalMF", module_methods, module_docstring);
    if (m == NULL)
        return;

}
#endif 




/* For double comparisons */
bool areSame(double a, double b){
	return fabs(a-b) < EPSILON;
}


//////////////////////////////////
// MAIN FUNCTION: 
static PyObject *XPCalMF(PyObject *self, PyObject *args)
{

      
  double PI = 3.1415926535; 
  double R2D = 180./PI; 

  int overWrite;
  int iMode;
//  double PI = 3.1415926535; 
//  double R2D = 180./PI; 
//  double GrDel;
  // Object to return:
  PyObject *ret; 

  ret = Py_BuildValue("i",-1);
  
  // Function arguments:
//  int Ref, NIFs;
  PyObject *pFName, *pZero;
  if (!PyArg_ParseTuple(args, "OOii", &pFName, &pZero, &overWrite, &iMode)){printf("FAILED XPCalMF! Wrong arguments!\n"); fflush(stdout);  return ret;};

 // if (Ref<0 || Ref>2){printf("ERROR! Ref should be >=0 and <= 2\n"); fflush(stdout); return ret;}

  
  // GrDel *= 1.e-3*(2.*PI);

  bool connectPhase = iMode<=0;
  if(iMode <0){iMode = -iMode;}; 
 
// OPEN PHASECAL FILE:
  std::string PcalFile = PyString_AsString(pFName);
  std::ifstream PcalF;
  PcalF.open(PcalFile.c_str(), std::ios::in);



  
 // double T, inT, Tini, Tbuf; 
  double Re=0.0, Im=0.0, nui=0.0;
  double Tbuf, inT;
  int BUFF = 1024;


  int NTone=0, i,j,l, Aux, Aux2, Aux3;
  long currP, lastP, Nbytes;


// LISTs OF FREQUENCIES TO ZERO:
  int nIFZero = PyList_Size(pZero);
  double *IFini = new double[nIFZero];
  double *IFend = new double[nIFZero];
  for (i=0;i<nIFZero;i++){
    IFini[i] = (int)PyFloat_AsDouble( PyList_GetItem(PyList_GetItem(pZero,i),0) );
    IFend[i] = (int)PyFloat_AsDouble( PyList_GetItem(PyList_GetItem(pZero,i),1) );
  };



  char Pol;
  std::string TelName, line, auxStr;
  std::istringstream tempStr, auxTempStr;

  cplx64d **PCalsX= new cplx64d*[BUFF];
  cplx64d **PCalsY= new cplx64d*[BUFF]; 
  bool **goodX = new bool*[BUFF];
  bool **goodY = new bool*[BUFF];

  double **PCalTimes= new double*[BUFF];
  double *PCalNus= new double[BUFF];
  bool *ZeroIt = new bool[BUFF];
  int *NTimes= new int[BUFF];

  bool RepNu;

  int NuOver = 1;
  int *TOver = new int[BUFF];
  for(i=0;i<BUFF;i++){
    TOver[i] = 1;
  };

// Read line by line. Update frequency list on the fly:

  while (!PcalF.eof()){
    std::getline(PcalF,line);
    if (line.length() > 10 && line[0] != '#'){  // good line
       tempStr.str(line);
       tempStr.clear();

         tempStr >> TelName;
         tempStr >> Tbuf;
         tempStr >> inT;
         tempStr >> Aux;
         tempStr >> Aux2;
         tempStr >> Aux3;

         i=0; j=0; l=0;
       
         while(std::getline(tempStr,auxStr,' ')){
           if (auxStr.length() > 0){
             switch(i){

	       // Remember the format: 'FREQ POL RE IM':
	       // (i.e., case 0, 1, 2, 3).
               case 0: nui = atof(auxStr.c_str());

                 j=0; l=-1;


                 RepNu = false; // Is this tone NEW (i.e., not found in the previous times??)
                 if (nui<=0.0){j=-1;} else {
	  	 for(j=0;j<NTone;j++){
                     if(nui==PCalNus[j]){
                       RepNu = true;
                       break;
                     };
                   };
                 };

                // In case of a new tone, add it to the data:
                 if (!RepNu && nui >0.0){

                  // printf(" NEW TONE at %s (%i): %.3f MHz.\n",TelName.c_str(),NTone,nui);

                   if(NTone+1>BUFF*NuOver){
                    // printf("Resizing Tones\n");fflush(stdout);
                     NuOver +=1;
                     ZeroIt = (bool *) realloc((void *)ZeroIt, BUFF*NuOver*sizeof(bool));
                     PCalNus = (double *) realloc((void *)PCalNus, BUFF*NuOver*sizeof(double));
                     PCalsX = (cplx64d **) realloc((void *)PCalsX,  BUFF*NuOver*sizeof(cplx64d*));
                     PCalsY = (cplx64d **) realloc((void *)PCalsY,  BUFF*NuOver*sizeof(cplx64d*));
                     PCalTimes = (double **) realloc((void *)PCalTimes, BUFF*NuOver*sizeof(double*));
                     goodX = (bool **) realloc((void *)goodX,  BUFF*NuOver*sizeof(bool*));
                     goodY = (bool **) realloc((void *)goodY,  BUFF*NuOver*sizeof(bool*));
                     NTimes = (int *) realloc((void *)NTimes, BUFF*NuOver*sizeof(int));
                     TOver = (int *) realloc((void *)TOver,  BUFF*NuOver*sizeof(int));
                     for(l=0;l<BUFF;l++){TOver[BUFF*(NuOver-1)+l] = 1;};
                   };

                   PCalNus[NTone]=nui;
                 //  printf("SET %i to %.8e\n",NTone,nui);fflush(stdout); 
                   ZeroIt[NTone] = false;
                   for(j=0;j<nIFZero;j++){
                     if (PCalNus[NTone]>=IFini[j] && PCalNus[NTone]<=IFend[j]){ZeroIt[NTone] = true; break;};
                   };                 
  
                   PCalTimes[NTone] = new double[BUFF];
                   PCalsX[NTone] = new cplx64d[BUFF];
                   PCalsY[NTone] = new cplx64d[BUFF];
                   goodX[NTone] = new bool[BUFF];
                   goodY[NTone] = new bool[BUFF];
                   NTimes[NTone]=0;

                   j=NTone; NTone+=1; 
                 };


             // Check whether this is a new integration time:
             if(j>=0){
               RepNu = false;
               for(l=0;l<NTimes[j];l++){
                 if( areSame(Tbuf,PCalTimes[j][l])){RepNu=true;break;};
               };
               if(!RepNu){

                  if(NTimes[j]+1>BUFF*TOver[j]){
                   //  printf("Resizing Times\n");fflush(stdout);
                    TOver[j] += 1;
                    PCalTimes[j] = (double *) realloc((void *)PCalTimes[j],BUFF*TOver[j]*sizeof(double));
                    PCalsX[j] = (cplx64d *) realloc((void *)PCalsX[j],BUFF*TOver[j]*sizeof(cplx64d));
                    PCalsY[j] = (cplx64d *) realloc((void *)PCalsY[j],BUFF*TOver[j]*sizeof(cplx64d));
                    goodX[j] = (bool *) realloc((void *)goodX[j],BUFF*TOver[j]*sizeof(bool));
                    goodY[j] = (bool *) realloc((void *)goodY[j],BUFF*TOver[j]*sizeof(bool));
                  };

                  PCalTimes[j][NTimes[j]] = Tbuf;
                  goodX[j][NTimes[j]] = false; goodY[j][NTimes[j]] = false;
                  l=NTimes[j]; NTimes[j] += 1;
               };
             };

             i+= 1; break;

             case 1: Pol = auxStr.c_str()[0]; i += 1; break;
             case 2: Re = atof(auxStr.c_str()); i += 1; break;
             case 3: Im = atof(auxStr.c_str()); i=0;
               if(j>=0 && l >=0){
                 if (Pol == 'X' || Pol == 'R'){PCalsX[j][l] = cplx64d(Re,Im); goodX[j][l]=true;}; 
                 if (Pol == 'Y' || Pol == 'L'){PCalsY[j][l] = cplx64d(Re,Im); goodY[j][l]=true;};  
               }; 
               break;
		
             };
           };
         };
       	
    };
  };



  PcalF.close();

 // printf("NTONE: %i\n",NTone);fflush(stdout);

// Variables to store final results:
  double *Phases = new double[NTone];
  double *Amps = new double[NTone];
  double AmpsX, AmpsY;

// Auxiliary variables:
//int FirstGood = -1;

  //printf("STAGE 2\n");fflush(stdout);

// Compute the average cross-polarization phases:
  cplx64d PCalTemp;
  int NPCals;
  for(j=0;j<NTone;j++){
    PCalTemp = 0.0;
    NPCals = 0;
    AmpsX = 0.0; AmpsY = 0.0;
    for (i=0; i<NTimes[j]; i++){
    //  printf("TONE %i; TIME %i of %i \n",j,i,NTimes[j]);fflush(stdout);
      if(goodX[j][i] && goodY[j][i]){
        NPCals += 1;
        switch (iMode) {
          case 0:
            PCalTemp += PCalsY[j][i]/PCalsX[j][i]; break;
          case 1:
            PCalTemp += PCalsX[j][i]; break;
          default: PCalTemp += PCalsY[j][i]/PCalsX[j][i];
        };
        AmpsX += std::abs(PCalsX[j][i]);
        AmpsY += std::abs(PCalsY[j][i]);
      };
    };
    if(NPCals>0){
      Phases[j] = (double) std::arg(PCalTemp);
      switch (iMode){
        case 0:
          Amps[j] = AmpsY/AmpsX; break;
        case 1:
          Amps[j] = AmpsX; break;
        default: Amps[j] = AmpsY/AmpsX;
      };
    } else {
      Phases[j] = 0.0; Amps[j] = 1.;
    };
  };







// Update the PCAL file (if asked to do it):

  if(overWrite != 0){

//  bool isX;

// MAKE A BACKUP OF THE PCAL FILE:
  PcalF.open(PcalFile.c_str(), std::ios::in | std::ios::binary);
  std::string ORIGSUFFIX(".ORIGINAL");
  std::string outname = PcalFile + ORIGSUFFIX;  
  std::ofstream OutPcal(outname.c_str(), std::ios::out | std::ios::binary);
  OutPcal << PcalF.rdbuf();

  PcalF.close();
  OutPcal.close();

////////////////////////////////
// OPEN PHASECAL FILE:
  PcalF.open(outname.c_str(), std::ios::in);


// OPEN OUTPUT FILE:
  OutPcal.open(PcalFile.c_str(), std::ios::out);


  
//  for (i=0; i<NTone; i++){
//     printf(" %i  -   %.3f  %i \n",nIFZero,PCalNus[i],ZeroIt[i]);
//  };

  std::string TelName, line, auxStr;
  std::stringstream tempStr;
 // std::stringbuf *linePos;


  while (!PcalF.eof()){

    // Read original line:
    std::getline(PcalF,line);
    tempStr.str(line);

    // Change entries: (X,Y) -> (R,L) and R = L = X.
    if (line.length() > 10 && line[0] != '#'){
       tempStr.clear();
       tempStr.precision(5);

	// First elements in line:       
         tempStr >> TelName;
         tempStr >> Tbuf;
         tempStr >> inT;
         tempStr >> Aux;
         tempStr >> Aux2;
         tempStr >> Aux3;


       i=0; j=0; currP = 0; //isX = true;
       while(std::getline(tempStr,auxStr,' ')){
         if (auxStr.length() > 0){
           switch(i){

             case 0: nui = atoi(auxStr.c_str());

               RepNu = false; // Is this tone NEW (i.e., not found in the previous times??)
               if (nui<0){j=-1; l=-1;} else {
                 for(j=0;j<NTone;j++){
                   if(nui==PCalNus[j]){
                     RepNu = true;
                     break;
                   };
                 };

                 for(l=0;l<NTimes[j];l++){
                     if(areSame(Tbuf,PCalTimes[j][l])){break;};
                 };
               };

               if(ZeroIt[j]){PCalsX[j][l]=cplx64d(-1.0,0.0); PCalsY[j][l]=cplx64d(-1.0,0.0);};

               i += 1; break;

             case 1: if(nui<=0){i+=1; break;} Pol = auxStr.c_str()[0];

	       if (Pol=='X' || Pol=='R'){  // If X, keep next Re and Im; Change to R
               //  isX = true;
		 currP = tempStr.tellg();
                 tempStr.seekp(currP-2);
		 tempStr << 'R';
	       } else if (Pol=='Y' || Pol=='L'){ // If Y, change Re and Im; Change to L
               //  isX = false;
                 currP = tempStr.tellg();
                 tempStr.seekp(currP-2);
		 tempStr << 'L';
	       };	       	     
	       i += 1; break;

             case 2: if(nui<=0){i+=1; break;};
	//	if (!isX){
	          lastP = tempStr.tellg();		
                  tempStr.seekp(currP);
                  Nbytes = lastP - currP-1;
		  currP = lastP;
		  tempStr << std::setw(Nbytes) << std::scientific << PCalsX[j][l].real();
	//	};	
		i += 1; break;


             case 3: if(nui<=0){i=0; break;};
	//	if (!isX){
	          lastP = tempStr.tellg();		
                  tempStr.seekp(currP);
                  Nbytes = lastP - currP-1;
		  currP = lastP;
		  tempStr << std::setw(Nbytes) << std::scientific << PCalsX[j][l].imag();
	//	};	
		i = 0; break;

           };
         };
       };


    };

  // Write new lines:
  OutPcal << tempStr.str() << std::endl ;



  };

  PcalF.close();
  OutPcal.close();

  };







  if(overWrite!=0){
    //printf("Overwrite\n");fflush(stdout);
    ret = Py_BuildValue("i",0);
    return ret;

  };

  double FrAux, PhAux, AuxA; 
  double ZeroAux;
// Sort the data in order of increasing frequency:
  for (i=0; i<NTone-1; i++){
    for (j=i+1; j<NTone; j++){
      if (PCalNus[i]>PCalNus[j]){
        FrAux=PCalNus[i]; PCalNus[i]=PCalNus[j]; PCalNus[j]=FrAux;
        PhAux=Phases[i]; Phases[i]=Phases[j]; Phases[j]=PhAux;
        AuxA = Amps[i]; Amps[i] = Amps[j]; Amps[j] = AuxA;
        ZeroAux = ZeroIt[i]; ZeroIt[i]=ZeroIt[j]; ZeroIt[j] = ZeroAux;
      };
    }; 
  //  printf("TONE %i: %.8e\n",i,PCalNus[i]);fflush(stdout);
  };



// Connect phases among tones:


  double DNu = PCalNus[1] - PCalNus[0];
  int iJump = 0;
  double FracP, IntP;
  double IFDel00, IFDel01, IFDel0, IFDel1, NtoneIF;
  double *NWrap = new double[NTone];



  for (i=0; i<NTone-1; i++){
    /////////////////////////	  
    // First, a simple connection between neighoring tones:	
    if (Phases[i+1]-Phases[i] > PI){
      for(j=i+1;j<NTone;j++){Phases[j] -= 2.*PI;};
    };

    if (Phases[i+1]-Phases[i] < -PI){
      for(j=i+1;j<NTone;j++){Phases[j] += 2.*PI;};
    };
    /////////////////////////

  if(connectPhase){

    ///////////////////////////////////////////////////////////////
    // Between IFs, we extrapolate the phase from the edge tone using the
    // group delay of the whole IF, as estimated from the tones within that IF:
    // The group delay of the IF is estimated from a simple linear regression:

    if(i>1 && PCalNus[i+1]-PCalNus[i] > 1.1*DNu){ // Condition to assume that we have jumped to another IF.
      IFDel00 = 0.0; IFDel01 = 0.0; IFDel0 = 0.0; IFDel1 = 0.0; 

      // iJump is the index of the first tone in the IF to be fitted.
      NtoneIF = (double) (i-iJump+1); // Number of phasecal tones within the IF.

      for(j=iJump; j<=i; j++){
        IFDel00 += PCalNus[j]*PCalNus[j]; IFDel0 += PCalNus[j];
	IFDel01 += PCalNus[j]*Phases[j]; IFDel1 += Phases[j];
      };	
      iJump = i+1; // Now, iJump will be the index of the first tone in the NEW IF.
      IFDel0 /= NtoneIF ; IFDel1 /= NtoneIF;

      // Estimated phase of the iJump-th tone (group-delay extrapolation from the previous IF):
      NWrap[i] = (IFDel01 - NtoneIF*IFDel0*IFDel1)/(IFDel00 - NtoneIF*IFDel0*IFDel0)*(PCalNus[i+1]-PCalNus[i]) + Phases[i]-Phases[i+1];

      // Convert this difference into an integer number of wraps:
      NWrap[i] /= 2.*PI; FracP = modf(NWrap[i], &IntP);
      if (FracP>0.5){IntP += 1.;} else if(FracP<-0.5){IntP -= 1.;}; // Difference will always be <180 degrees.


      for(j=i+1;j<NTone;j++){Phases[j] += 2.*PI*IntP;};

    };

  };

  };


  std::string SUFFIX(".CROSSPOL"); // = PyString_AsString(SuffixObj);
  std::string outname = PcalFile + SUFFIX;  
  FILE *outFile = fopen(outname.c_str(),"w");

 // printf("NTone = %i\n",NTone); fflush(stdout);
  
  for (i=0;i<NTone;i++){
   //   printf(" WRITING %i\n",i);fflush(stdout);
   //    printf("%i %.8e  %.8e  %.8e\n",i,PCalNus[i],Phases[i],Amps[i]); fflush(stdout);
      fprintf(outFile,"%.8e  %.8e  %.8e\n",PCalNus[i],Phases[i]*R2D,Amps[i]);
  };

  fflush(outFile);

 // Release memory:
  for (i=0; i<NTone; i++){
    //printf(" DELETING %i\n",i);fflush(stdout);    
    delete[] PCalsX[i];
    delete[] PCalsY[i];
    delete[] PCalTimes[i];
    delete[] goodX[i];
    delete[] goodY[i];
  };
    delete[] PCalsX;
    delete[] PCalsY;
    delete[] PCalTimes;
    delete[] goodX;
    delete[] goodY;
    delete[] PCalNus;
    delete[] NTimes;
    delete[] ZeroIt;

/*
  // Arrange data for output to Python:
  npy_intp dims[1];
  dims[0] = NTone;
  PyObject *XYadd = (PyObject *) PyArray_SimpleNewFromData(1, dims, NPY_FLOAT64, (void *) Phases);
  PyObject *XYnu =  (PyObject *) PyArray_SimpleNewFromData(1, dims, NPY_FLOAT64, (void *) PCalNus);
  PyObject *XAmps = (PyObject *) PyArray_SimpleNewFromData(1, dims, NPY_FLOAT64, (void *) Amps);
  ret = Py_BuildValue("[O,O,O]",XYnu,XYadd,XAmps);
*/

  //printf("Xcross %i\n",NTone);fflush(stdout);
  
  ret = Py_BuildValue("s",outname.c_str());
  return ret;


};










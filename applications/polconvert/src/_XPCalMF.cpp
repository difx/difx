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

  const char *FREQLAB = "FREQ (MHZ)";
  const char *SIGNLAB = "SIGN";
  const char *BWLAB = "BW (MHZ)";

  int overWrite;
  int iMode, IFoffset;
  // Object to return:
  PyObject *ret; 
  long Iret;

  int NIF = 0;
  double *FRINI;
  double *FREND;
  double AuxF0, AuxF1, AuxF2;
  int i;
 

  //ret = Py_BuildValue("i",-1);
  Iret = -1;

  // Function arguments:
//  int Ref, NIFs;
  PyObject *pFName, *pZero, *pFreqInfo;
  if (!PyArg_ParseTuple(args, "OOiiOi", &pFName, &pZero, &overWrite, &iMode, &pFreqInfo, &IFoffset)){printf("FAILED XPCalMF! Wrong arguments!\n"); fflush(stdout);  return PyLong_FromLong(Iret);};



  
 // READ IF FREQUENCY LIMITS: 
   if ( PyList_Size(PyDict_Items(pFreqInfo)) > 0){

    PyObject *PyFreqs = PyDict_GetItemString(pFreqInfo,FREQLAB);
    PyObject *PyBW = PyDict_GetItemString(pFreqInfo,BWLAB);
    PyObject *PySign = PyDict_GetItemString(pFreqInfo,SIGNLAB);
    
    NIF = PyList_Size(PyFreqs);
    if (NIF>IFoffset){NIF = IFoffset;};
    FRINI = new double[NIF];
    FREND = new double[NIF];
    
    for (i=0;i<NIF;i++){
      AuxF0 = PyFloat_AsDouble( PyList_GetItem(PyFreqs,i) );
      AuxF1 = PyFloat_AsDouble( PyList_GetItem(PyBW,i) );
      AuxF2 = PyFloat_AsDouble( PyList_GetItem(PySign,i) );
      if (AuxF2<0.0){
        FRINI[i] = AuxF0 - AuxF1; FREND[i] = AuxF0;
      } else {
        FRINI[i] = AuxF0; FREND[i] = AuxF0 + AuxF1;
      };
     // printf("IF %i: FROM %.4e TO %.4e\n",i,FRINI[i],FREND[i]);
    };
  } else {FRINI = new double[1]; FREND = new double[1];};

  
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


  int NTone=0,j,k,l, Aux, Aux2, Aux3;
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

         i=0; j=0; l=0; Pol = 'R';
       
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

  double *Delays = new double[NTone];
  double *RefPhases = new double[NTone];
  double *RefFreqs = new double[NTone];
  int *IF = new int[NTone];   

// Auxiliary variables:


// Compute the average cross-polarization phases:
  cplx64d PCalTemp;
  int NPCals;
  for(j=0;j<NTone;j++){
    Delays[j] = 0.0;
    RefPhases[j] = 0.0;
    RefFreqs[j] = 0.0;
    IF[j] = 0;
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
       l=-1; Pol='R';
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




  double FrAux, PhAux, AuxA; 
  double ZeroAux;

  double DNu = PCalNus[1] - PCalNus[0];
  double IFDel00, IFDel01, IFDel0, IFDel1, NtoneIF;
  double *NWrap = new double[NTone];
  cplx64d AvPhasor;
  double fitDelay, IFPhase;

  int nUsableTones;
  int *usableTones = new int[NTone];
  FILE *outFile;
  std::string outname; 
  std::string SUFFIX(".CROSSPOL");
 

  if(overWrite!=0){
    Iret = 0;
    ret = PyLong_FromLong(Iret);
    goto finish;
  };



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



// Connect phases among tones for each IF:


 // If freq. info was not provided, guess the IFs:
 
   if (NIF==0){
     int AuxIF = 0;
     delete[] FRINI; delete[] FREND;
     FRINI = new double[NTone]; FREND = new double[NTone];
     for (i=0; i<NTone-1; i++){
        if(PCalNus[i+1]-PCalNus[i] > 1.01*DNu){
          FRINI[NIF] = PCalNus[AuxIF]; FREND[NIF] = PCalNus[i]; NIF += 1; AuxIF = i+1;
        };
     };
     FRINI[NIF] = PCalNus[AuxIF]; FREND[NIF] = PCalNus[NTone-1]; NIF += 1;
   }; 





  if(connectPhase){

  for (i=0; i<NTone; i++){
    /////////////////////////	  
    // First, a simple connection between neighoring tones:
   
   if(i<NTone-1){ 
    
     if(PCalNus[i+1]-PCalNus[i] < 1.01*DNu){
       if (Phases[i+1]-Phases[i] > PI){
         for(j=i+1;j<NTone;j++){Phases[j] -= 2.*PI;};
       };

      if (Phases[i+1]-Phases[i] < -PI){
        for(j=i+1;j<NTone;j++){Phases[j] += 2.*PI;};
      };
    };
    
    };
    
  };
  
  };
  
  
//// LOOP OVER IFS:  

   
 for (k=0; k<NIF; k++){
 
   //printf("%i %i\n",NIF,k);
 
   // Figure out list of pcal tones inside this IF:
   nUsableTones = 0;
   for (j=0;j<NTone;j++){
     if (PCalNus[j]>= FRINI[k] && PCalNus[j]<= FREND[k]){
       usableTones[nUsableTones] = j;
       nUsableTones += 1;
     };
   }; 
    
    /////////////////////////

  if(connectPhase){

      IFDel00 = 0.0; IFDel01 = 0.0; IFDel0 = 0.0; IFDel1 = 0.0; 

      NtoneIF = (double) (nUsableTones); // Number of phasecal tones within the IF.

      
      for(j=0; j<nUsableTones; j++){
        l = usableTones[j];
        IFDel00 += PCalNus[l]*PCalNus[l]; IFDel0 += PCalNus[l];
	IFDel01 += PCalNus[l]*Phases[l]; IFDel1 += Phases[l];
      };	

      IFDel0 /= NtoneIF ; IFDel1 /= NtoneIF;

      // Estimate the cross-polarization tone delay for this IF:
      fitDelay = (IFDel01 - NtoneIF*IFDel0*IFDel1)/(IFDel00 - NtoneIF*IFDel0*IFDel0);

      // Estimate the cross-polarization tone phase:
      AvPhasor = 0.0;
      for(j=0; j<nUsableTones; j++){
        l = usableTones[j];
        AvPhasor += std::polar(1.0,(Phases[l] - fitDelay*(PCalNus[l] - IFDel0)));
      };
      IFPhase = std::arg(AvPhasor);

      // Store the results:
      for(j=0; j<nUsableTones; j++){
        l = usableTones[j];
        Delays[l] = fitDelay/(2.*PI); RefPhases[l] = IFPhase; 
        RefFreqs[l] = IFDel0; 
        IF[l] = k+1;
     //   printf("%.8e   %.8e   %.8e  %i\n",Delays[j],RefPhases[j],RefFreqs[j],j);
      };
      
      // Estimated phase of the iJump-th tone (group-delay extrapolation from the previous IF):
      NWrap[i] = fitDelay*(PCalNus[i+1]-PCalNus[i]) + Phases[i]-Phases[i+1];

      // Convert this difference into an integer number of wraps:
  //    NWrap[i] /= 2.*PI; FracP = modf(NWrap[i], &IntP);
  //    if (FracP>0.5){IntP += 1.;} else if(FracP<-0.5){IntP -= 1.;}; // Difference will always be <180 degrees.


  //    for(j=i+1;j<NTone;j++){Phases[j] += 2.*PI*IntP;};

    

  };

  };


  outname = PcalFile + SUFFIX;  
  outFile = fopen(outname.c_str(),"w");

  
  fprintf(outFile,"# Freq (MHz) | X-Y Phase (deg.) | Amps (Norm.) | X-Y Delay (mus) | Av Phase (deg.) | Ref. Freq. (MHz) | IF \n");
  
  for (i=0;i<NTone;i++){
      fprintf(outFile,"%.8e  %.8e  %.8e  %.8e  %.8e  %.8e  %i\n", PCalNus[i], Phases[i]*R2D, Amps[i], Delays[i], RefPhases[i]*R2D, RefFreqs[i], IF[i]);
  };

  fflush(outFile);

// Return filename (if created):  
  ret = Py_BuildValue("s",outname.c_str());

finish:

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
    delete[] Phases;
    delete[] Amps;
    delete[] Delays;
    delete[] RefPhases;
    delete[] RefFreqs;
    delete[] NTimes;
    delete[] ZeroIt;

    delete[] FRINI;
    delete[] FREND;
    delete[] IFini;
    delete[] IFend;
    delete[] TOver;
    delete[] IF;
    delete[] usableTones;


  return ret;


};








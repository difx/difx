/* POLGAINSOLVE - Solving cross-polarizer gains for PolConvert

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
#include <math.h>
#include <complex>
#include <dirent.h>
#include <fftw.h>
#include <gsl/gsl_linalg.h>


typedef std::complex<double> cplx64f;
typedef std::complex<float> cplx32f;

/* Docstrings */
static char module_docstring[] =
    "This module provides the cross-polarization gain-solver engine.";
static char PolGainSolve_docstring[] =
    "Solves for cross-polarization gains from mixed-polarization visibilities";
static char ReadData_docstring[] =
    "Reads data (one IF) for PolGainSolve";
static char GetChi2_docstring[] =
    "Computes the Chi2 for a given set of cross-pol gains";
static char GetIFs_docstring[] =
    "Returns the array of frequencies for a given IF";
static char DoGFF_docstring[] =
    "Performs a simplified GFF (delays and rates). The reference antenna is set by not adding it to the list of fittable antennas";

/* Available functions */
static PyObject *PolGainSolve(PyObject *self, PyObject *args);
static PyObject *ReadData(PyObject *self, PyObject *args);
static PyObject *GetChi2(PyObject *self, PyObject *args);
static PyObject *GetIFs(PyObject *self, PyObject *args);
static PyObject *DoGFF(PyObject *self, PyObject *args);


/* Module specification */
static PyMethodDef module_methods[] = {
    {"PolGainSolve", PolGainSolve, METH_VARARGS, PolGainSolve_docstring},
    {"ReadData", ReadData, METH_VARARGS, ReadData_docstring},
    {"GetChi2", GetChi2, METH_VARARGS, GetChi2_docstring},
    {"GetIFs", GetIFs, METH_VARARGS, GetIFs_docstring},
    {"DoGFF", DoGFF, METH_VARARGS, DoGFF_docstring},
    {NULL, NULL, 0, NULL}
};


/* Initialize the module */
PyMODINIT_FUNC init_PolGainSolve(void)
{
    PyObject *m = Py_InitModule3("_PolGainSolve", module_methods, module_docstring);import_array();
    if (m == NULL)
        return;

}

///////////////////////


   char message[512];

   static double TWOPI = 6.283185307179586;

   int MaxChan = 1;
   int MAXIF = 8; // Will reallocate if needed.
   int NCalAnt, Nlin, Ncirc, *Nchan, SolMode, SolAlgor;
   int *IFNum;
   int *Lant, *Cant, *NVis, *NLVis, *NCVis, *CalAnts;

   int NIF = 0;
   int NBas;
   int npix = 0;
   double **Frequencies, *Rates, *Delays00, *Delays11;
   double Chi2Old = 0.0;
   double TAvg = 1.0;
   double RelWeight = 1.0;
   int **Ant1, **Ant2, **BasNum;
   double **Times;
   cplx64f **PA1, **PA2, *auxC00, *auxC01, *auxC10, *auxC11;
   cplx64f ***RR, ***RL, ***LR, ***LL, **CrossSpec00, **CrossSpec11;

   bool AddCrossHand = true;
   bool AddParHand = true;
 
   FILE *logFile;




//PolGainSolve::~PolGainSolve() {
//};




static PyObject *PolGainSolve(PyObject *self, PyObject *args){

  logFile = fopen("PolConvert.GainSolve.log","w");
  PyObject *calant, *linant, *solints;

  if (!PyArg_ParseTuple(args, "dOOO",&RelWeight, &solints, &calant, &linant)){
     sprintf(message,"Failed initialization of PolGainSolve! Check inputs!\n"); 
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
     fclose(logFile);
    PyObject *ret = Py_BuildValue("i",-1);
    return ret;
  };

TAvg = (double) PyInt_AsLong(PyList_GetItem(solints,1));
SolAlgor = (int) PyInt_AsLong(PyList_GetItem(solints,0));

printf("Will divide the calibration iscan into %.1f chunks\n",TAvg);

AddParHand = RelWeight>0.0;
AddCrossHand = true;


/*
  if (TAvg >0){
    SolMode = 1;
  } else if (TAvg <0){
    SolMode = -1;
    TAvg = -TAvg;
  } else {
    SolMode = 0;
    TAvg = 1.0;
  };

  if(TAvg >= 100.){
    AddCrossHand = true;
    TAvg -=100;
    if (TAvg<1.0){TAvg=1.0;}; 
  } else {
    AddCrossHand = false;
  };
*/

  CalAnts = (int *)PyArray_DATA(calant);
  NCalAnt = PyArray_DIM(calant,0);

  int i,j,k,l;
  k=0;

  int MaxAnt = 0;
  for(i=0;i<NCalAnt;i++){if(CalAnts[i]>MaxAnt){MaxAnt=CalAnts[i];};};
  BasNum = new int*[MaxAnt];

  bool isCal1, isCal2;

  for(i=0;i<MaxAnt;i++){
    BasNum[i] = new int[MaxAnt];

    for(j=0;j<MaxAnt;j++){BasNum[i][j] = -1;};

    for(j=i+1;j<MaxAnt;j++){

      isCal1=false; isCal2=false;

      for(l=0;l<NCalAnt;l++){
        if(i==CalAnts[l]-1){isCal1=true;}; 
        if(j==CalAnts[l]-1){isCal2=true;};
      };

  //    printf("LOOP %i %i -  %i %i\n",i,j,isCal1,isCal2);

      if (isCal1 && isCal2){
        BasNum[i][j] = k;
   //     BasNum[j][i] = k;
        k += 1;
 //       printf("%i - %i | %i\n",i+1,j+1,BasNum[i][j]);
      };



    };
  };
  

  auxC00 = new cplx64f[k];
  auxC01 = new cplx64f[k];
  auxC10 = new cplx64f[k];
  auxC11 = new cplx64f[k];
  NBas = k;

  Rates = (double *) malloc(NCalAnt*sizeof(double));
  Delays00 = (double *) malloc(NCalAnt*sizeof(double));
  Delays11 = (double *) malloc(NCalAnt*sizeof(double));

  for(i=0;i<NCalAnt;i++){
     Rates[i] = 0.0;
     Delays00[i] = 0.0;
     Delays11[i] = 0.0;
  };

  CrossSpec00 = (cplx64f **) malloc(NBas*sizeof(cplx64f*));
  CrossSpec11 = (cplx64f **) malloc(NBas*sizeof(cplx64f*));
  for(i=0;i<NBas;i++){
    CrossSpec00[i] = (cplx64f *) malloc(MaxChan*sizeof(cplx64f));
    CrossSpec11[i] = (cplx64f *) malloc(MaxChan*sizeof(cplx64f));
  };


  Lant = (int *)PyArray_DATA(linant);
  Nlin = PyArray_DIM(linant,0);

//  printf("N linears: %i  | N Cal. Antennas: %i\n",Nlin,NCalAnt);

//  int i;
//  for (i=0; i<NCalAnt;i++){printf("ANT: %i\n",CalAnts[i]);};

// Re-allocate memory:
  if (NIF >0){
    free(NVis); // = (int *) malloc(MAXIF*sizeof(int));// new int[1];
    free(IFNum); // = (int *) malloc(MAXIF*sizeof(int));// new int[1];
    free(NCVis);// = (int *) malloc(MAXIF*sizeof(int));// new int[1];
    free(NLVis);// = (int *) malloc(MAXIF*sizeof(int));// new int[1];
    free(Nchan); //= (int *) malloc(MAXIF*sizeof(int));// new int[1];
    free(Frequencies);// = (double **) malloc(MAXIF*sizeof(double*)); // new double*[1];
    free(Ant1); //= (int**) malloc(MAXIF*sizeof(int*));
    free(Ant2); //= (int**) malloc(MAXIF*sizeof(int*));
    free(Times);// = (double**) malloc(MAXIF*sizeof(double*));
    free(PA1); //= (cplx32f**) malloc(MAXIF*sizeof(cplx32f*));
    free(PA2); //= (cplx32f**) malloc(MAXIF*sizeof(cplx32f*));
    free(RR); //= (cplx32f***) malloc(MAXIF*sizeof(cplx32f**));
    free(LR); //= (cplx32f***) malloc(MAXIF*sizeof(cplx32f**));
    free(RL); //= (cplx32f***) malloc(MAXIF*sizeof(cplx32f**));
    free(LL); //= (cplx32f***) malloc(MAXIF*sizeof(cplx32f**));
  };


  NIF = 0;


// Set Memory:

  NVis = (int *) malloc(MAXIF*sizeof(int));// new int[1];
  IFNum = (int *) malloc(MAXIF*sizeof(int));// new int[1];
  NCVis = (int *) malloc(MAXIF*sizeof(int));// new int[1];
  NLVis = (int *) malloc(MAXIF*sizeof(int));// new int[1];
  Nchan = (int *) malloc(MAXIF*sizeof(int));// new int[1];
  Frequencies = (double **) malloc(MAXIF*sizeof(double*)); // new double*[1];
  Ant1 = (int**) malloc(MAXIF*sizeof(int*));
  Ant2 = (int**) malloc(MAXIF*sizeof(int*));
  Times = (double**) malloc(MAXIF*sizeof(double*));
  PA1 = (cplx64f**) malloc(MAXIF*sizeof(cplx64f*));
  PA2 = (cplx64f**) malloc(MAXIF*sizeof(cplx64f*));
  RR = (cplx64f***) malloc(MAXIF*sizeof(cplx64f**));
  LR = (cplx64f***) malloc(MAXIF*sizeof(cplx64f**));
  RL = (cplx64f***) malloc(MAXIF*sizeof(cplx64f**));
  LL = (cplx64f***) malloc(MAXIF*sizeof(cplx64f**));




    PyObject *ret = Py_BuildValue("i",0);
    return ret;

};






static PyObject *ReadData(PyObject *self, PyObject *args) { //std::string CPFileName, std::string MPFileName){

int IFN;
const char *file1, *file2;
std::ifstream CPfile, MPfile;

if (!PyArg_ParseTuple(args, "iss", &IFN,&file1, &file2)){
     sprintf(message,"Failed ReadData! Check inputs!\n"); 
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
     fclose(logFile);
    PyObject *ret = Py_BuildValue("i",-1);
    return ret;
};




int i, j, k, l;
double AuxT, AuxPA1, AuxPA2;

bool is1, is2;


CPfile.open(file1, std::ios::in | std::ios::binary);
MPfile.open(file2, std::ios::in | std::ios::binary);



NIF += 1;

if (NIF > MAXIF){
// Set memory for new IF:
  MAXIF *= 2;
  Nchan = (int*) realloc(Nchan,MAXIF*sizeof(int));
  Frequencies = (double**) realloc(Frequencies,MAXIF*sizeof(double*));
  NVis = (int*) realloc(NVis,MAXIF*sizeof(int));
  NCVis = (int*) realloc(NCVis,MAXIF*sizeof(int));
  NLVis = (int*) realloc(NLVis,MAXIF*sizeof(int));
  IFNum = (int*) realloc(IFNum,MAXIF*sizeof(int));

// Set memory for the visibilities and metadata:
  Ant1 = (int**) realloc(Ant1,MAXIF*sizeof(int*));
  Ant2 = (int**) realloc(Ant2,MAXIF*sizeof(int*));
  Times = (double**) realloc(Times,MAXIF*sizeof(double*));
  PA1 = (cplx64f**) realloc(PA1,MAXIF*sizeof(cplx64f*));
  PA2 = (cplx64f**) realloc(PA2,MAXIF*sizeof(cplx64f*));
  RR = (cplx64f***) realloc(RR,MAXIF*sizeof(cplx64f**));
  LR = (cplx64f***) realloc(LR,MAXIF*sizeof(cplx64f**));
  RL = (cplx64f***) realloc(RL,MAXIF*sizeof(cplx64f**));
  LL = (cplx64f***) realloc(LL,MAXIF*sizeof(cplx64f**));
};

// IF NUMBER:
IFNum[NIF-1] = IFN;

// Number of channels for this IF:
CPfile.read(reinterpret_cast<char*>(&Nchan[NIF-1]), sizeof(int));
//printf("There are %i channels\n",Nchan[NIF-1]);


// Maximum number of channels:
if (Nchan[NIF-1] > MaxChan){
  MaxChan=Nchan[NIF-1];
  for (i=0; i< NBas; i++) {
    CrossSpec00[i] = (cplx64f *) realloc(CrossSpec00[i],MaxChan*sizeof(cplx64f));
    CrossSpec11[i] = (cplx64f *) realloc(CrossSpec11[i],MaxChan*sizeof(cplx64f));
  };
};



MPfile.ignore(sizeof(int));


Frequencies[NIF-1] = new double[Nchan[NIF-1]];



// Get frequencies for this IF:
Frequencies[NIF-1] = new double[Nchan[NIF-1]];
CPfile.read(reinterpret_cast<char*>(Frequencies[NIF-1]), Nchan[NIF-1]*sizeof(double));


//printf("Freqs. %.5e  %.5e\n",Frequencies[NIF-1][0],Frequencies[NIF-1][Nchan[NIF-1]-1]);

// Get Number of visibilities observed by the CalAnts (Circ Pol):

NCVis[NIF-1] = 0;
int AuxA1, AuxA2;

while(!CPfile.eof()){

  is1 = false; is2 = false;
  CPfile.ignore(sizeof(double)); 
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

CPfile.ignore(2*sizeof(double)+4*Nchan[NIF-1]*sizeof(cplx32f)); 

};








// Get Number of visibilities observed by the CalAnts (Mix Pol):
NLVis[NIF-1] = 0;

while(!MPfile.eof()){

  is1 = false; is2 = false;
  MPfile.ignore(sizeof(double)); 
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


// Total number of visibilities:
NVis[NIF-1] = NCVis[NIF-1]+ NLVis[NIF-1];

sprintf(message,"\nFound %i vis in CPol and %i vis in LPol\n",NCVis[NIF-1],NLVis[NIF-1]); 
fprintf(logFile,"%s",message);  



// Set memory for the visibilities:
j = NVis[NIF-1]+1;
Ant1[NIF-1] = (int*) malloc(j*sizeof(int)); // new int[j];

Ant2[NIF-1] = (int*) malloc(j*sizeof(int)); // new int[j];

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


// Read visibilities (Mix Pol):
int currI = 0;
bool isGood, is1c, is2c, isFlipped;
cplx64f Exp1, Exp2;

cplx32f AuxRR, AuxRL, AuxLR, AuxLL;

i=0;
while(!MPfile.eof()){
  MPfile.read(reinterpret_cast<char*>(&AuxT), sizeof(double));
  MPfile.read(reinterpret_cast<char*>(&AuxA1), sizeof(int));
  MPfile.read(reinterpret_cast<char*>(&AuxA2), sizeof(int));
  MPfile.read(reinterpret_cast<char*>(&AuxPA1), sizeof(double));
  MPfile.read(reinterpret_cast<char*>(&AuxPA2), sizeof(double));


// Check if visib is observed by CalAnts:
  isGood = false; is1 = false; is2 = false;
  is1c = true; is2c = true;

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
      for (l=0; l<Nlin; l++){
        if (AuxA1 == Lant[l]){
          if(isFlipped){is2c = false;}else{is1c=false;};
        };
        if (AuxA2 == Lant[l]){
          if(isFlipped){is1c = false;}else{is2c=false;};
        };
     };

     Times[NIF-1][currI] = AuxT;
 //    printf("DT: %.5e\n",Times[NIF-1][currI]-Times[NIF-1][0]);

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

// test rate
  // if (Ant1[NIF-1][currI]==1){RR[NIF-1][currI][k] = std::polar(1.0,TWOPI*(Times[NIF-1][currI]-Times[NIF-1][0])/100.);};

       if (is1c){
         RR[NIF-1][currI][k] /= Exp1;
         RL[NIF-1][currI][k] /= Exp1;
         LR[NIF-1][currI][k] *= Exp1;
         LL[NIF-1][currI][k] *= Exp1;
       };
       if (is2c) {
         RR[NIF-1][currI][k] *= Exp2;
         RL[NIF-1][currI][k] /= Exp2;
         LR[NIF-1][currI][k] *= Exp2;
         LL[NIF-1][currI][k] /= Exp2;
       };

//     if (currI<100 && k==10 && NIF==1){
//     printf("RR for %i - %i | %i, %i = %.5e   %.5e |  %.5e   %.5e | %.5e %.5e \n",Ant1[NIF-1][currI],Ant2[NIF-1][currI],currI,k,AuxRR.real(),AuxRR.imag(),RR[NIF-1][currI][k].real(),RR[NIF-1][currI][k].imag(), abs(AuxRR), abs(RR[NIF-1][currI][k]));
//     };


     };
     currI += 1; isGood = true;


   };

  if(!isGood){
    MPfile.ignore(12*Nchan[NIF-1]*sizeof(cplx32f));
  };

//printf("currI: %i\n",currI);

};








// Read visibilities (Circ Pol):

while(!CPfile.eof()){

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

// Apply ParAng:
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

       RR[NIF-1][currI][k] /= Exp1;
       RL[NIF-1][currI][k] /= Exp1;
       LR[NIF-1][currI][k] *= Exp1;
       LL[NIF-1][currI][k] *= Exp1;

       RR[NIF-1][currI][k] *= Exp2;
       RL[NIF-1][currI][k] /= Exp2;
       LR[NIF-1][currI][k] *= Exp2;
       LL[NIF-1][currI][k] /= Exp2; 

     };

     currI += 1; isGood = true;
 //    printf("%i of %i  /  %i - %i\n",currI,NVis[NIF-1],AuxA1,AuxA2);

   };

  if(!isGood){
 //   printf("NO GOOD! %i / %i - %i\n",currI,AuxA1,AuxA2);

    CPfile.ignore(4*Nchan[NIF-1]*sizeof(cplx32f));
  };

};

//printf("DONE READ!\n");

CPfile.close();
MPfile.close();

//printf("CLOSED!\n");

    PyObject *ret = Py_BuildValue("i",0);
    return ret;


};









static PyObject *GetIFs(PyObject *self, PyObject *args) {     //(int Npar, int IF0, intint Ch0, int Ch1, double *pars) {

int i,j,k;

//PyObject *IFlist;

if (!PyArg_ParseTuple(args, "i", &i)){
     sprintf(message,"Failed GetIFs! Check inputs!\n"); 
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
     fclose(logFile);
    PyObject *ret = Py_BuildValue("i",-1);
    return ret;
};

k=-1;
for (j=0; j<NIF; j++){
  if(IFNum[j] == i){k=j;break;};
};

if(k<0){
    PyObject *ret = Py_BuildValue("i",-1);
    return ret;
};

int dims[1];
dims[0] = Nchan[k];

double *copyFreq = new double[Nchan[k]];

for (j=0; j<Nchan[k]; j++){
  copyFreq[j] = Frequencies[k][j];
};

PyArrayObject *out_Freq = (PyArrayObject *) PyArray_FromDimsAndData(1,dims,PyArray_DOUBLE, (char *) copyFreq);

return PyArray_Return(out_Freq);

};






///////////////////////////////
// Quinn Estimator of the FFT peak with sub-bin precision:

double QuinnTau(double x){
  return 0.25*log(3.*x*x + 6.*x + 1.) - sqrt(6.)/24.*log((x+1.-sqrt(2./3.))/(x+1.+sqrt(2./3.)));
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


int i,j,NantFix;
double *rates;

PyObject *ratesArr, *antList;

if (!PyArg_ParseTuple(args, "OO", &ratesArr, &antList)){
     sprintf(message,"Failed SetFringeRates! Check inputs!\n"); 
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
     fclose(logFile);
    PyObject *ret = Py_BuildValue("i",-1);
    return ret;
};

NantFix = (int) PyList_Size(antList);

rates = (double *) PyArray_DATA(ratesArr);

for (i=0; i<NCalAnt; i++){
  Rates[i] = 0.0;
};

for (i=0; i<NantFix; i++){
  j = (int) PyInt_AsLong(PyList_GetItem(antList,i));
  Rates[j-1] = rates[i];
};


PyObject *ret = Py_BuildValue("i",0);
return ret;


};





static PyObject *DoGFF(PyObject *self, PyObject *args) {

int i,j,k,l, a1,a2, af1, af2, BNum,NantFit;
int *antFit;

//double Drate, T0;
//cplx32f Grate;

double *T0 = new double[NBas];  
double *T1 = new double[NBas];
bool isFirst = true;
int applyRate;

cplx64f *aroundPeak00 = new cplx64f[3];
cplx64f *aroundPeak11 = new cplx64f[3];


//T0 = Times[0][0];

PyObject *antList;

if (!PyArg_ParseTuple(args, "Oii", &antList,&npix, &applyRate)){
     sprintf(message,"Failed DoGFF! Check inputs!\n"); 
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
     fclose(logFile);
    PyObject *ret = Py_BuildValue("i",-1);
    return ret;
};


if (applyRate==0){printf("\n\n   Residual rate will NOT be estimated\n\n");};
NantFit = (int) PyList_Size(antList);
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


fftwnd_plan p = fftw2d_create_plan(NVis[0]/NBas, Nchan[0], FFTW_FORWARD, FFTW_MEASURE | FFTW_IN_PLACE);

int prevChan = Nchan[0];
int prevNvis = NVis[0]/NBas;



// FFT FOR EACH BASELINE:
int TotDim = NVis[0]*Nchan[0];
cplx64f *BufferVis00 = (cplx64f *) malloc(TotDim*sizeof(cplx64f));
cplx64f *BufferVis11 = (cplx64f *) malloc(TotDim*sizeof(cplx64f));


for(j=0; j<NBas; j++){


for (i=0; i<NIF; i++){

  isFirst = true;

  BLRates00[i][j] = 0.0;
  BLRates11[i][j] = 0.0;
  BLDelays00[i][j] = 0.0;
  BLDelays11[i][j] = 0.0;

//  if (isFirst){
    int NcurrVis = 0;

    if (TotDim != NVis[i]*Nchan[i]){
      TotDim = NVis[i]*Nchan[i];
      cplx64f *BufferVis00 = (cplx64f *) realloc(BufferVis00,TotDim*sizeof(cplx64f));
      cplx64f *BufferVis11 = (cplx64f *) realloc(BufferVis11,TotDim*sizeof(cplx64f));
    };
//    printf("\n IF %i HAS %i CHANNELS AND %i VIS. THERE ARE %i BASELINES\n",i+1,Nchan[i],NVis[i],NBas);
//  };


// Arrange data for this baseline:
    for (k=0; k<NVis[i]; k++){
      a1 = Ant1[i][k];
      a2 = Ant2[i][k];
      BNum = BasNum[a1-1][a2-1];
  //    printf("Bnum: %i, %i-%i, for baseline %i\n",BNum,a1,a2,j);
      if (BNum==j){
        if(isFirst){
          T0[j] = Times[i][k];
          T1[j] = Times[i][k];
          isFirst = false;
     //     printf("DOING IF %i (%i CHANS) FOR BASELINE %i (ANTS: %i-%i)\n",i,Nchan[i],j,a1,a2);
        };
        if (T0[j] > Times[i][k]){T0[j] = Times[i][k];};

        memcpy(&BufferVis00[NcurrVis*Nchan[i]],&RR[i][k][0],Nchan[i]*sizeof(cplx64f));
        memcpy(&BufferVis11[NcurrVis*Nchan[i]],&LL[i][k][0],Nchan[i]*sizeof(cplx64f));

//  if (k<100 && i==0){
//  printf("RR: IF %i - K %i - Nv %i - %.5e  %.5e\n",i,k,NcurrVis, RR[i][k][10].real(), BufferVis00[NcurrVis*Nchan[i] + 10].real());
//  };
//    printf("LL: %i - %i - %.5e  %.5e\n",150,25,NcurrVis, 25, LL[i][k][25], BufferVis11[NcurrVis*Nchan[i] + 25]);

        NcurrVis += 1;
     //   T1[j] = Times[i][k];
        if (T1[j] < Times[i][k]){T1[j] = Times[i][k];};

  //      printf("COPIED VISIB. %i\n",k);
      };
    };


//    printf("READ A TOTAL OF  %i VISIBS.\n",NcurrVis);

/////////////////
// FFT the fringe and find the peak:
   if (NcurrVis > 1){
    if (Nchan[i] != prevChan || NcurrVis != prevNvis){
      prevChan = Nchan[i]; prevNvis = NcurrVis;
      fftwnd_destroy_plan(p);  
      p = fftw2d_create_plan(NcurrVis,Nchan[i], FFTW_FORWARD, FFTW_IN_PLACE);
    };

    fftwnd_one(p, reinterpret_cast<fftw_complex*>(BufferVis00), NULL);
    fftwnd_one(p, reinterpret_cast<fftw_complex*>(BufferVis11), NULL);
   };

 //   printf("DONE WITH FFT!\n");

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

// First Quadrant:
  for (l=0; l<ti; l++){
    row = l*Nchan[i];
    for (k=0; k<Chi;k++){
      AbsP = std::abs(BufferVis00[row + k]);
      if (AbsP>Peak00){
         Peak00 = AbsP;
         nu00[1] = k; ti00[1] = l; 
      };
      AbsP = std::abs(BufferVis11[row + k]);
      if (AbsP>Peak11){
         Peak11 = AbsP;
         nu11[1] = k; ti11[1] = l; 
      };
    };
  };


// Second Quadrant:
  for (l=tf; l<NcurrVis; l++){
    row = l*Nchan[i];
    for (k=0; k<Chi;k++){
      AbsP = std::abs(BufferVis00[row + k]);
      if (AbsP>Peak00){
         Peak00 = AbsP;
         nu00[1] = k; ti00[1] = l; 
      };
      AbsP = std::abs(BufferVis11[row + k]);
      if (AbsP>Peak11){
         Peak11 = AbsP;
         nu11[1] = k; ti11[1] = l; 
      };
    };
  };

// Third Quadrant:
  for (l=0; l<ti; l++){
    row = l*Nchan[i];
    for (k=Chf; k<Nchan[i];k++){
      AbsP = std::abs(BufferVis00[row + k]);
      if (AbsP>Peak00){
         Peak00 = AbsP;
         nu00[1] = k; ti00[1] = l; 
      };
      AbsP = std::abs(BufferVis11[row + k]);
      if (AbsP>Peak11){
         Peak11 = AbsP;
         nu11[1] = k; ti11[1] = l; 
      };
    };
  };

// Fourth Quadrant:
  for (l=tf; l<NcurrVis; l++){
    row = l*Nchan[i];
    for (k=Chf; k<Nchan[i];k++){
      AbsP = std::abs(BufferVis00[row + k]);
      if (AbsP>Peak00){
         Peak00 = AbsP;
         nu00[1] = k; ti00[1] = l; 
      };
      AbsP = std::abs(BufferVis11[row + k]);
      if (AbsP>Peak11){
         Peak11 = AbsP;
         nu11[1] = k; ti11[1] = l; 
      };
    };
  };



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


////////////////////
// Estimate the rate with sub-bin precision:

    aroundPeak00[0] = BufferVis00[nu00[1] + Nchan[i]*(ti00[0])];
    aroundPeak00[1] = BufferVis00[nu00[1] + Nchan[i]*(ti00[1])];
    aroundPeak00[2] = BufferVis00[nu00[1] + Nchan[i]*(ti00[2])];
    BLRates00[i][j] = ((double) ti00[1]);
    BLRates00[i][j] += QuinnEstimate(aroundPeak00);
//    printf("IF %i, RR: %i -> %.2f with dT = %.2f\n",i,ti00[1], BLRates00[i][j],(T1[j]-T0[j]));

  if (BLRates00[i][j] > ((double) NcurrVis)/2.){
    BLRates00[i][j] = BLRates00[i][j] - (double) NcurrVis;
  }; 


  BLRates00[i][j] *= 1./(T1[j]-T0[j]);


    aroundPeak11[0] = BufferVis11[nu11[1] + Nchan[i]*(ti11[0])];
    aroundPeak11[1] = BufferVis11[nu11[1] + Nchan[i]*(ti11[1])];
    aroundPeak11[2] = BufferVis11[nu11[1] + Nchan[i]*(ti11[2])];
    BLRates11[i][j] = ((double) ti11[1]);
    BLRates11[i][j] += QuinnEstimate(aroundPeak11);

  if (BLRates11[i][j] > ((double) NcurrVis)/2.){
    BLRates11[i][j] = BLRates11[i][j] - (double) NcurrVis;
  }; 

  BLRates11[i][j] *= 1./(T1[j]-T0[j]);

////////////////////
// Estimate the delay with sub-bin precision:

    aroundPeak00[0] = BufferVis00[nu00[0] + Nchan[i]*(ti00[1])];
    aroundPeak00[1] = BufferVis00[nu00[1] + Nchan[i]*(ti00[1])];
    aroundPeak00[2] = BufferVis00[nu00[2] + Nchan[i]*(ti00[1])];
    BLDelays00[i][j] = ((double) nu00[1]);
    BLDelays00[i][j] += QuinnEstimate(aroundPeak00);


  if (BLDelays00[i][j] > ((double) Nchan[i])/2.){
    BLDelays00[i][j] = BLDelays00[i][j] - (double) Nchan[i];
  }; 

  BLDelays00[i][j] *= 1./(Frequencies[i][Nchan[i]-1]-Frequencies[i][0]);


    aroundPeak11[0] = BufferVis11[nu11[0] + Nchan[i]*(ti11[1])];
    aroundPeak11[1] = BufferVis11[nu11[1] + Nchan[i]*(ti11[1])];
    aroundPeak11[2] = BufferVis11[nu11[2] + Nchan[i]*(ti11[1])];
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
     printf("WARNING! BASELINE %i HAS NO DATA IN IF %i!\n",j,i+1);
  };

//////
//  printf("RR FRINGE PEAK OF %.5e AT INDEX %i-%i (RATE %.5e Hz, DELAY: %.5e s)\n",Peak00,ti00[1],nu00[1],BLRates00[i][j],BLDelays00[i][j]);
//  printf("LL FRINGE PEAK OF %.5e AT INDEX %i-%i (RATE %.5e Hz, DELAY: %.5e s)\n",Peak11,ti11[1],nu11[1],BLRates11[i][j],BLDelays00[i][j]);

  };
////////////////////


};


/////////////////
// Globalize the rate and delay solutions:

printf("# of free antenna gains (per pol.): %i\n",NantFit);

double *Hessian = new double[NantFit*NantFit];
double *RateResVec = new double[NantFit];
double *DelResVec00 = new double[NantFit];
double *DelResVec11 = new double[NantFit];
double *CovMat = new double[NantFit*NantFit];

for (i=0;i<NantFit*NantFit;i++){
  Hessian[i] = 0.0;
  CovMat[i] = 0.0;
};

for (i=0;i<NantFit;i++){
  RateResVec[i] = 0.0;
  DelResVec00[i] = 0.0;
  DelResVec11[i] = 0.0;
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

      if (BNum>0){

       if (af1>=0){
         RateResVec[af1] += Weights[i][BNum]*(BLRates00[i][BNum] + BLRates11[i][BNum])/2.;
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


} else {

  BNum = BasNum[CalAnts[0]-1][CalAnts[1]-1];

  for (i=0; i<NIF; i++){
    Hessian[0] += Weights[i][BNum];
    RateResVec[0] += Weights[i][BNum]*(BLRates00[i][BNum] + BLRates11[i][BNum])/2.;
    DelResVec00[0] += Weights[i][BNum]*BLDelays00[i][BNum];
    DelResVec11[0] += Weights[i][BNum]*BLDelays11[i][BNum];
  };
//  Rates[0] = 0.0; //RateResVec[0]/((double) NIF);
//  Delays00[0] = 0.0; // DelResVec00[0]/((double) NIF);
//  Delays11[0] = 0.0; // DelResVec11[0]/((double) NIF);

  for (i=0; i<NCalAnt; i++){
    Rates[i] = 0.0;
    Delays00[i] = 0.0;
    Delays11[i] = 0.0;

    if(CalAnts[i]==antFit[0]){
      if (applyRate>0){Rates[i] = -RateResVec[0]/((double) NIF);};
      Delays00[i] = -DelResVec00[0]/((double) NIF);
      Delays11[i] = -DelResVec11[0]/((double) NIF);
    };
  };

};


printf("\n\nHessian Globalization Matrix:\n\n");
for (i=0; i<NantFit; i++){
  printf("  ");
  for (j=0; j<NantFit; j++){
     printf("%.2e ",Hessian[i*NantFit+j]);
  };
printf("\n");
};
printf("\n");

//printf("Baseline rates (Hz):\n");
//for (i=0; i<NBas; i++){
//     printf("R: %.2e L: %.2e | ",BLRates00[i][0],BLRates11[i][0]);
//};
//printf("\n\n");

//printf("ANTENNA RATES:\n");
//for (i=0; i<NantFit; i++){
//     printf("%.2e | ",Rates[i]);
//};
//printf("\n\n");



// The Hessian's inverse can be reused for rates, delays and phases!

gsl_matrix_view m = gsl_matrix_view_array (Hessian, NantFit, NantFit);
gsl_matrix_view inv = gsl_matrix_view_array(CovMat,NantFit,NantFit);

int s;

gsl_permutation *perm = gsl_permutation_alloc (NantFit);

gsl_linalg_LU_decomp (&m.matrix, perm, &s);
gsl_linalg_LU_invert (&m.matrix, perm, &inv.matrix);


// Derive the rates as CovMat*RateVec and delays as CovMat*DelVec:

if (NantFit>1){


for (i=0; i<NCalAnt; i++){
  Rates[i] = 0.0;
  Delays00[i] = 0.0;
  Delays11[i] = 0.0;
  af1 = -1;
  for(j=0; j<NantFit; j++) {
    if (CalAnts[i]==antFit[j]){af1 = j;};
  };
  if (af1 >=0){
    for (j=0; j<NantFit; j++){
      if(applyRate>0){Rates[i] -= RateResVec[j]*gsl_matrix_get(&inv.matrix,af1,j);};
      Delays00[i] -= DelResVec00[j]*gsl_matrix_get(&inv.matrix,af1,j);
      Delays11[i] -= DelResVec11[j]*gsl_matrix_get(&inv.matrix,af1,j);
    };
  };

};

};

for (i=0; i<NCalAnt; i++){

  printf("Antenna %i -> Rate: %.3e Hz; RR Delay: %.3e s; LL Delay: %.3e s\n",CalAnts[i],Rates[i],Delays00[i],Delays11[i]);
};



double MaxRateDev = -1.0;
double MaxDelDev = -1.0;
int BadA1 = -1; int BadA2 = -1;
int BadIF = -1;
int BadA1R = -1; int BadA2R = -1;
int BadIFR = -1; int BadBR = -1;

double TempD;

for (i=0; i<NIF; i++){
  for (a1=0; a1<NCalAnt; a1++){
    for (a2=a1+1;a2<NCalAnt;a2++){
       BNum = BasNum[CalAnts[a1]-1][CalAnts[a2]-1];
       TempD = fabs(BLRates00[i][BNum] + (Rates[a1] - Rates[a2]));
//       printf("Check %i %i %i (%.3e %.3e %.3e)\n",i,a1,a2,Rates[a1],Rates[a2],(BLRates11[i][BNum]+BLRates00[i][BNum])*0.5);
       if (TempD > MaxRateDev){
          MaxRateDev=TempD; BadIFR=i;BadA1R=a1;BadA2R=a2; BadBR = BNum;};
       TempD = fabs(BLRates11[i][BNum] + (Rates[a1] - Rates[a2]));
       if (TempD > MaxRateDev){
          MaxRateDev=TempD; BadIFR=i;BadA1R=a1;BadA2R=a2; BadBR = BNum;};
       TempD = fabs(BLDelays00[i][BNum] + (Delays00[a1] - Delays00[a2]));
       if (TempD > MaxDelDev){MaxDelDev=TempD; BadIF=i;BadA1=a1;BadA2=a2;};
       TempD = fabs(BLDelays11[i][BNum] + (Delays11[a1] - Delays11[a2]));
       if (TempD > MaxDelDev){MaxDelDev=TempD; BadIF=i;BadA1=a1;BadA2=a2;};
    };
  };
};

//if (NantFit>1){

  printf("\n\nMax. fringe deviation (rate): Bas. %i-%i at IF %i (%.1e deg. across window)\n",CalAnts[BadA1R],CalAnts[BadA2R],IFNum[BadIFR],fabs(MaxRateDev*(T1[BadBR]-T0[BadBR])*180.));
  printf("Max. fringe deviation (delay): Bas. %i-%i at IF %i (%.1e deg. across window)\n\n",CalAnts[BadA1],CalAnts[BadA2],IFNum[BadIF],fabs(MaxDelDev*(Frequencies[BadIF][Nchan[BadIF]-1]-Frequencies[BadIF][0])*180.));

//};


/////////////////




PyObject *ret = Py_BuildValue("i",0);
return ret;

};
























static PyObject *GetChi2(PyObject *self, PyObject *args) { 

int NIFComp, Npar, NantFit;
int Ch0, Ch1,solveAmp;
int i,j,k;
double *CrossG;
int *doIF, *antFit;

double T0 = Times[0][0];
double T1 = Times[0][NVis[0]-1];
double DT = (T1-T0)/TAvg;
double *Tm = new double[NBas];


double Drate1, Drate2; 

PyObject *pars, *IFlist, *antList;


if (!PyArg_ParseTuple(args, "OOOiii", &pars, &IFlist, &antList, &Ch0, &Ch1, &solveAmp)){
     sprintf(message,"Failed GetChi2! Check inputs!\n"); 
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
     fclose(logFile);
    PyObject *ret = Py_BuildValue("i",-1);
    return ret;
};


NIFComp = (int) PyList_Size(IFlist);
doIF = new int[NIFComp];


NantFit = (int) PyList_Size(antList);
antFit = new int[NantFit];

for (i=0; i<NantFit; i++){
  antFit[i] = (int) PyInt_AsLong(PyList_GetItem(antList,i));
};

// Find out IFs to compute and do sanity checks:
bool foundit;

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
     PyObject *ret = Py_BuildValue("i",-1);
     return ret;
  };
  if (Ch1 > Nchan[doIF[i]]){
     sprintf(message,"IF %i ONLY HAS %i CHANNELS. CHANNEL %i DOES NOT EXIST! \n",j,Nchan[doIF[i]], Ch1); 
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
     fclose(logFile);
     PyObject *ret = Py_BuildValue("i",-1);
     return ret;
  };
};

if (Ch0<0 || Ch0>Ch1){
 sprintf(message,"BAD CHANNEL RANGE: %i TO %i. SHOULD ALL BE POSITIVE AND Ch0 < Ch1\n",Ch0,Ch1); 
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);  
     fclose(logFile);
    PyObject *ret = Py_BuildValue("i",-1);
    return ret;
};



// Reference frequency for the MBD:
double RefNu = Frequencies[doIF[0]][0];



Npar = PyArray_DIM(pars,0);
CrossG = (double *) PyArray_DATA(pars);

//printf("There are %i fitting parameters\n",Npar);







int currIF, a1, a2, ac1,ac2,af1, af2;
int is1, is2;

cplx64f G1, G2, Rho1, Rho2; //, auxC00, auxC01,auxC10,auxC11;
cplx64f Error;
cplx64f oneC, G1nu, G2nu, RateFactor; 

oneC= cplx64f(1.0,0.0);


int *AvVis = new int[NBas];

int BNum;

double MBD1, MBD2;
double Chi2 = 0.0;
double auxD1, auxD2;
double TRatio = 1.0;

//double MaxAmp = 2.0;

//bool DoParHand = true; //SolMode > 0;
//bool DoCrossHand = AddCrossHand;


//printf("\nPREAVER: %i\n",preAver);

for (i=0; i<NIFComp; i++){
 
  currIF = doIF[i];

  for (k=0;k<NBas;k++){
    auxC00[k] = cplx64f(0., 0.);
    auxC01[k] = cplx64f(0., 0.);
    auxC10[k] = cplx64f(0., 0.);
    auxC11[k] = cplx64f(0., 0.);
    AvVis[k] = 0;
  };

  for(k=0;k<NBas;k++){Tm[k]=Times[currIF][0];};


  for (k=0; k<NVis[currIF]; k++){

    a1 = Ant1[currIF][k];
    a2 = Ant2[currIF][k];


    ac1 = -1; ac2 = -1;
    for(j=0; j<NCalAnt; j++) {
      if (a1==CalAnts[j]){ac1 = j;};
      if (a2==CalAnts[j]){ac2 = j;};
    };

    BNum = BasNum[a1-1][a2-1];
    if(ac1>=0){Drate1 = TWOPI*Rates[ac1]*(Times[currIF][k]-T0);} else {Drate1=0.0;};
    if(ac2>=0){Drate2 = TWOPI*Rates[ac2]*(Times[currIF][k]-T0);} else {Drate2=0.0;};

    RateFactor = std::polar(1.0,Drate1-Drate2);

//    printf("%i %i %.2e %.2e \n",ac1,ac2,Drate1,Drate2);

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
    for(j=0; j<Nlin; j++) {
      if (a1==Lant[j]){is1 = true;};
      if (a2==Lant[j]){is2 = true;};
    };


// The crossGains at the ref. channel: 
     
    if (solveAmp==0){

      if (af1 >= 0){
        G1 = std::polar(1.0, CrossG[af1]); // + Drate1);
      } else { G1 = std::polar(1.0, 0.0);}; //Drate1);};

      if (af2 >= 0){
        G2 = std::polar(1.0, -CrossG[af2]); // - Drate2);
      } else { G2 = std::polar(1.0, 0.0);}; //-Drate2);};

    } else {

      if (af1 >= 0){
        G1 = std::polar(CrossG[(af1)*2], CrossG[(af1)*2+1]); // + Drate1);
      } else { G1 = std::polar(1.0, 0.0);}; //Drate1);};

      if (af2 >= 0){
        G2 = std::polar(CrossG[(af2)*2], -CrossG[(af2)*2+1]); // - Drate2);
      } else { G2 = std::polar(1.0, 0.0);}; //-Drate2);};

    };

    if(BNum>=0){

    AvVis[BNum] += 1;

//    printf("\nAvVis[%i] = %i\n",BNum,AvVis[BNum]);

    for (j=Ch0; j<Ch1+1; j++){

// Add the multi-band delays:
     if (SolAlgor == 0){
      if (af1 >= 0){
        if(solveAmp==0){MBD1 = CrossG[NantFit+af1]*(Frequencies[currIF][j]-RefNu);} else {
          MBD1 = CrossG[NantFit*2+af1]*(Frequencies[currIF][j]-RefNu);};
      } else {MBD1 = 0.0;};
      if (af2 >= 0){
        if(solveAmp==0){MBD2 = CrossG[NantFit+af2]*(Frequencies[currIF][j]-RefNu);} else {
        MBD2 = CrossG[NantFit*2+af2]*(Frequencies[currIF][j]-RefNu);};
      } else {MBD2 = 0.0;};
       G1nu = G1*(std::polar(1.0,MBD1));
       G2nu = G2*(std::polar(1.0,-MBD2));
     } else {G1nu = G1; G2nu = G2;};

// Convert gain ratios into leakages (for linear-pol antennas:
         Rho1 = (oneC - G1nu)/(oneC + G1nu);
         Rho2 = (oneC - G2nu)/(oneC + G2nu);


// USE MINIMIZATION OF THE CROSS-HAND CORRELATIONS:
     if(AddCrossHand){
       if (is1 && is2){
         auxC01[BNum] += (RL[currIF][k][j] + Rho1*LL[currIF][k][j] + Rho2*RR[currIF][k][j] + Rho1*Rho2*LR[currIF][k][j])/PA1[currIF][k]/PA2[currIF][k]*RateFactor;
         auxC10[BNum] += (LR[currIF][k][j] + Rho1*RR[currIF][k][j] + Rho2*LL[currIF][k][j] + Rho1*Rho2*RL[currIF][k][j])*PA1[currIF][k]*PA2[currIF][k]*RateFactor;
       } else if (is1){
         auxC01[BNum] += (RL[currIF][k][j] + Rho1*LL[currIF][k][j])/PA1[currIF][k]*G2nu*RateFactor;
         auxC10[BNum] += (LR[currIF][k][j] + Rho1*RR[currIF][k][j])*PA1[currIF][k]*RateFactor;
       } else if (is2){
         auxC01[BNum] += (RL[currIF][k][j] + Rho2*RR[currIF][k][j])/PA2[currIF][k]*RateFactor;
         auxC10[BNum] += (LR[currIF][k][j] + Rho2*LL[currIF][k][j])*PA2[currIF][k]*G1nu*RateFactor;
       };
     };

// USE GLOBAL CROSS-POLARIZATION FRINGE FITTING:
     if(AddParHand){
       if (is1 && is2){
         auxC00[BNum] += (RR[currIF][k][j] + Rho1*LR[currIF][k][j] + Rho2*RL[currIF][k][j] + Rho1*Rho2*LL[currIF][k][j])/PA1[currIF][k]*PA2[currIF][k]*RateFactor;
         auxC11[BNum] += (LL[currIF][k][j] + Rho1*RL[currIF][k][j] + Rho2*LR[currIF][k][j] + Rho1*Rho2*RR[currIF][k][j])*PA1[currIF][k]/PA2[currIF][k]*RateFactor;
       } else if (is1){
         auxC00[BNum] += (RR[currIF][k][j] + Rho1*LR[currIF][k][j])/PA1[currIF][k]*RateFactor;
         auxC11[BNum] += (LL[currIF][k][j] + Rho1*RL[currIF][k][j])*PA1[currIF][k]*G2nu*RateFactor;
       } else if (is2){
         auxC00[BNum] += (RR[currIF][k][j] + Rho2*RL[currIF][k][j])*PA2[currIF][k]*RateFactor;
         auxC11[BNum] += (LL[currIF][k][j] + Rho2*LR[currIF][k][j])/PA2[currIF][k]*G1nu*RateFactor;
       } else {
         auxC00[BNum] += RR[currIF][k][j]*RateFactor;
         auxC11[BNum] += LL[currIF][k][j]*G2nu*G1nu*RateFactor;
       };
     };

    };  // Comes from loop over channels.


   if (Times[currIF][k]>=Tm[BNum] + DT){

     Tm[BNum] = Times[currIF][k];


// Avoid overflow errors to very large RelWeights (i.e., divide the crosshand
// instead of multipyling the parhand)
     if(AddCrossHand){
        auxD1 = std::abs(auxC01[BNum]);  auxD2 = std::abs(auxC10[BNum]);
        if (RelWeight>1.0){
          Chi2 += (auxD1*auxD1 + auxD2*auxD2)/RelWeight;
        } else {
          Chi2 += (auxD1*auxD1 + auxD2*auxD2);
        };
     };

     if (AddParHand){
        if (abs(auxC11[BNum])>0.0){
          Error = auxC00[BNum]/auxC11[BNum];
          auxD1 = std::abs(Error - oneC);
//          Error = auxC00[BNum] - auxC11[BNum];
//          auxD1 = std::abs(Error);
          if (RelWeight>1.0){
            Chi2 += auxD1*auxD1;
          } else {
            Chi2 += auxD1*auxD1*RelWeight;
          };
        };
     };

    auxC00[BNum] = cplx64f(0., 0.);
    auxC01[BNum] = cplx64f(0., 0.);
    auxC10[BNum] = cplx64f(0., 0.);
    auxC11[BNum] = cplx64f(0., 0.);
    AvVis[BNum] = 0;

   };


   }; // Comes from if(BNum>=0) 


  };  // Comes from loop over visibilities


  for (k=0;k<NBas;k++){
    if (AvVis[k]>0){

     if (Tm[k]>T0){TRatio = (T1-Tm[k])/(Tm[k]-T0);} else {TRatio = 1.0;};

     if(AddCrossHand){
        auxD1 = std::abs(auxC01[k]);  auxD2 = std::abs(auxC10[k]);
        if (RelWeight>1.0){
          Chi2 += (auxD1*auxD1 + auxD2*auxD2)*TRatio/RelWeight;
        } else {
          Chi2 += (auxD1*auxD1 + auxD2*auxD2)*TRatio;
        };
     };

     if (AddParHand){
        if (abs(auxC11[k])>0.0){
          Error = auxC00[k]/auxC11[k];
          auxD1 = std::abs(Error - oneC);
//          Error = auxC00[k] - auxC11[k];
//          auxD1 = std::abs(Error);
        if (RelWeight>1.0){
          Chi2 += (auxD1*auxD1)*TRatio;
        } else {
          Chi2 += (auxD1*auxD1)*TRatio*RelWeight;
        };
        };
     };

      auxC00[k] = cplx64f(0., 0.);
      auxC01[k] = cplx64f(0., 0.);
      auxC10[k] = cplx64f(0., 0.);
      auxC11[k] = cplx64f(0., 0.);
      AvVis[k] = 0;

    };
  };



};



//  for (i=0;i<NantFit;i++){
//    printf("Antenna %i: AMP: %.2e PHASE: %.2e MBD: %.2e\n\n",antFit[i],CrossG[i*2],CrossG[i*2+1],CrossG[NantFit*2+i]);
//  };


  if (SolAlgor==0 && Chi2Old != 0.0){
     printf("\r Chi Squared: %.5e. Improvement of %.2e \%",Chi2,100.*(Chi2-Chi2Old)/Chi2);
   };

  Chi2Old = Chi2;

    PyObject *ret = Py_BuildValue("d",Chi2);
    return ret;

};














/* CROSSPHASECAL - Cross-polarization phasecal extraction for PolConvert

             Copyright (C) 2018  Ivan Marti-Vidal
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
// compiler warning that we use a deprecated NumPy API
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

typedef std::complex<float> cplx32f;
typedef std::complex<double> cplx64d;



/* Docstrings */
static char module_docstring[] =
    "Interface to read phasecal metadata into PolConvert.";
static char XPCal_docstring[] =
    "Reads and interpolates phasecal-tone phase values, given in DiFX format.\n The first argument is the name of the phasecal file.\n The second argument is an integer: \n 0 = return the scan-averaged cross-polarization phases. \n 1 = return the scan-averaged phases in the firs pol. channel, referred to the tone of lowest frequency. \n 3 = return the phasecals in the first pol. channel for each tone and integration time.\n\n The output is a list of numpy arrays: frequency (MHz), phase (deg), rate (Hz), reference time (MJD), time offsets (s), phasecal matrix (i.e., phases for each tone and time offset).\n\nThe third argument is a group delay (in ns) to be substracted from the phasecals before the phase connection among tones.";


static char XPConvert_docstring[] =
    "Overwrites the pcal DiFX file with a new version, where X and Y are substitued by R and L. In this version, both the R and L entries are exact copies of X.";


/* Available functions */
static PyObject *XPCal(PyObject *self, PyObject *args);
static PyObject *XPConvert(PyObject *self, PyObject *args);


/* Module specification */
static PyMethodDef module_methods[] = {
    {"XPCal", XPCal, METH_VARARGS, XPCal_docstring},
    {"XPConvert", XPConvert, METH_VARARGS, XPConvert_docstring},
    {NULL, NULL, 0, NULL}
};


/* Initialize the module */
PyMODINIT_FUNC init_XPCal(void)
{
    import_array();
    PyObject *m = Py_InitModule3("_XPCal", module_methods, module_docstring);
    if (m == NULL)
        return;

}

//////////////////////////////////
// MAIN FUNCTION: 
static PyObject *XPCal(PyObject *self, PyObject *args)
{

      
  double PI = 3.1415926535; 
  double R2D = 180./PI; 
  double GrDel;
  // Object to return:
  PyObject *ret; 

  ret = Py_BuildValue("i",-1);
  
  // Function arguments:
  int Ref;
  PyObject *pFName;
  if (!PyArg_ParseTuple(args, "Oid", &pFName, &Ref, &GrDel)){printf("FAILED XPCal! Wrong arguments!\n"); fflush(stdout);  return ret;};


  if (Ref<0 || Ref>2){printf("ERROR! Ref should be >=0 and <= 2\n"); fflush(stdout); return ret;}

  
  GrDel *= 1.e-3*(2.*PI);
  
// OPEN PHASECAL FILE:
  std::string PcalFile = PyString_AsString(pFName);
  std::ifstream PcalF;
  PcalF.open(PcalFile.c_str(), std::ios::in);


  
  double T, inT, Tini; 
  double Re, Im, nui;
  double NEntry, *NWrap;
  int Aux = 0; int Aux2 = 0;
  int NTone, NToneHf, TPI, i,j,l;
  long k;
  char Pol;
  std::string TelName, line, auxStr;
  std::istringstream tempStr;

  cplx64d *PCalsX, *PCalsY, PCalTemp, *PCalsD; 
  double *PCalsAX, *PCalsAY; 
  double *PCalNus, MinNu;
  double *RateFitXtt, *RateFitXpt, *RateFitXt, *RateFitXp;
  double *PrevPhX, *NGoods;
  bool *goodX, *goodY, RepNu;

  int *NTimes, *TBUFF;

  double **Ti, **PDi;


  bool start = false;

  k=0;
  Tini = 0.0;
  NEntry = 0.0;


// Read line by line. The frequencies of all tones will be read from the first line 
// (i.e., when start = false).  

  while (!PcalF.eof()){
    std::getline(PcalF,line);
    if (line.length() > 10 && line[0] != '#'){  // good line
       tempStr.str(line);
       tempStr.clear();
       if (!start){
	// First elements in line:       
         tempStr >> TelName;
         tempStr >> T;
         tempStr >> inT;
         tempStr >> Aux;
         tempStr >> NTone;
         tempStr >> TPI; // Tone per IF
         start = true;

	 // NTone is for both pols. So the actual number of tones is half of it:
         NToneHf = NTone*TPI/2;

         Tini = T;
         
	 /////////////////////////////////////////
         // Arrange memory to store results:

	 // Phasecals in each pol. channel: 
         PCalsX = new cplx64d[NToneHf]; 
         PCalsY = new cplx64d[NToneHf]; 

	 // Where results are accumulated:
         PCalsD = new cplx64d[NToneHf]; // phases
         PCalsAX = new double[NToneHf]; // Amplitudes X
         PCalsAY = new double[NToneHf]; // Amplitudes Y


	 // Frequency of each tone:
         PCalNus = new double[NToneHf];

	 // Auxiliary variables to fit the phase rate of each tone:
         RateFitXtt = new double[NToneHf];
         RateFitXpt = new double[NToneHf];
         RateFitXt = new double[NToneHf];
         RateFitXp = new double[NToneHf];

	 // Previous phase values:
         PrevPhX = new double[NToneHf];

	 // Number of accumulated 2pi wraps (one value per tone):
         NWrap = new double[NToneHf];

	 // Time offset at which the tones are given:
	 Ti = new double*[NToneHf]; 

	 // Storage of all phases (i.e., in time and frequency):
	 PDi = new double*[NToneHf];

	 // Number of phase values given for each tone:
         NTimes = new int[NToneHf];

	 // Maximum number of phase values per tone
	 // (if needed, it is reset at runtime)
         TBUFF = new int[NToneHf];

	 // Number of successful tone detections for each frequency
	 // (defined as a double, for practical reasons)
         NGoods = new double[NToneHf];

	 // Whether there have been good detections at each pol. channel
	 // in the current integration time:
         goodX = new bool[NToneHf]; goodY = new bool[NToneHf];

	 // Fill- in default values for all variables:
         for(j=0;j<NToneHf; j++){
	       TBUFF[j] = 128;
           Ti[j] = (double *) malloc(TBUFF[j]*sizeof(double));
           PDi[j] = (double *) malloc(TBUFF[j]*sizeof(double));
           NTimes[j] = 0; NGoods[j] = 0.0;
           PCalsX[j]=cplx64d(0.,0.); PCalsY[j]=cplx64d(0.,0.);
           PCalsD[j]=cplx64d(0.,0.); PCalNus[j]=-1.0; 
           PCalsAX[j] = 0.0; PCalsAY[j] = 0.0;
           RateFitXtt[j] = 0.0; RateFitXpt[j] = 0.0; 
           RateFitXt[j] = 0.0; RateFitXp[j] = 0.0; 
           PrevPhX[j] = 0.0;
	       NWrap[j] = 0.0;
           goodX[j]=false; goodY[j]=false;
         };
         j = 0; i = 0; Aux = 0;

	 // START READING THE PHASE VALUES!!!
	 // The format is 'FREQ POL RE IM', and now 
	 // we are only interested in FREQ:
         while(std::getline(tempStr,auxStr,' ')){
           if (auxStr.length() > 0){
             switch(i){
               case 0: nui = atof(auxStr.c_str()); i+= 1; 
                 RepNu = false; // Is this frequency repeated???
                 for (j=0;j<Aux;j++){
                   if (PCalNus[j]==nui){RepNu=true;break;};
                 }; // Add to the list if it is not repeated:
                 if(!RepNu && nui>0.0){PCalNus[Aux]=nui;Aux+=1;}; 
                 break;
               case 1: i+= 1; break;
               case 2: i+= 1; break;
               case 3: i=0; break;
             };
           };
         };
         tempStr.clear();
         
       };

       // After this line, 'Aux' is the number of tones with
       // successful detections.

       // Determine the lowest tone frequency so far:
       MinNu = 1.e12;
       for (i=0; i<Aux; i++){
          if(PCalNus[i]<MinNu){MinNu = PCalNus[i];};  
       }; 
       
       // Now, we read the phase values:

         tempStr >> TelName;
         tempStr >> T;
         tempStr >> inT;
         tempStr >> Aux2;
         tempStr >> NTone;
         tempStr >> TPI;
       
       // Time in seconds, referred to the first integration: 
       T -= Tini; T *= 86400. ;
       

       i=0; j=0;
       while(std::getline(tempStr,auxStr,' ')){
         if (auxStr.length() > 0){
           switch(i){

	     // Remember the format: 'FREQ POL RE IM':
	     // (i.e., case 0, 1, 2, 3).

             case 0: nui = atof(auxStr.c_str());
               RepNu = false; // Is this tone NEW (i.e., not found in the previous times??)
               if (nui<0.0){j=-1;} else {
                 for(j=0;j<Aux;j++){
                   if(nui==PCalNus[j]){
                     RepNu = true;
                     break;
                   };
                 };
               }; // In case of a new tone, add it to the data (and update 'Aux'):
               if (!RepNu && nui >0.0){PCalNus[Aux]=nui; j=Aux; Aux+=1; printf("NEW %.3f, %.3f\n",nui);}; 
               i+= 1; break;

             case 1: Pol = auxStr.c_str()[0]; i += 1; break;

             case 2: Re = atof(auxStr.c_str()); i += 1; break;

             case 3: Im = atof(auxStr.c_str()); i=0; k+=1;
              if(j>=0){
               if (Pol == 'X' || Pol == 'R'){PCalsX[j] = cplx64d(Re,Im); goodX[j]=true;}; 
               if (Pol == 'Y' || Pol == 'L'){PCalsY[j] = cplx64d(Re,Im); goodY[j]=true;};  
              }; 
               break;

           };
         };
       };


       // After reading all tones for this int. time, accumulate the solutions:

       double AuxPhase;
       
       for(l=0;l<NToneHf;l++){

         if(goodX[l] && goodY[l]){
      
	  // Depending on what the user asked (i.e., cross-pol, relative X phase,
	  // or total phase):	 
           switch(Ref){
               case 0: PCalTemp = PCalsY[l]/PCalsX[l]; break;
               case 1: PCalTemp = PCalsX[l]/PCalsX[0]; break;
               case 2: PCalTemp = PCalsX[l];           break;
           };     
             
         // Accumulate phases (correct first for the group delay, referred to NuMin):
           PCalTemp *= std::polar(1.0,GrDel*(PCalNus[l]-MinNu));
           PCalsD[l]+= PCalTemp;
           
         // Accumulate amplitudes:
           PCalsAX[l] += std::abs(PCalsX[l]) ; PCalsAY[l] += std::abs(PCalsY[l]);
           
	   // Unwrap:
           AuxPhase = ((double) std::arg(PCalTemp)) + 2.*PI*NWrap[l];
           
	   // Update the number of accumulated wraps:
           if (AuxPhase - PrevPhX[l] > PI){
		   AuxPhase -= 2.*PI; NWrap[l] -= 1.;} 
	   else if (AuxPhase - PrevPhX[l] < -PI){
		   AuxPhase += 2.*PI; NWrap[l] += 1.;
	   };
           
	   // Update vectors to fit the phase rate of each tone:
           RateFitXpt[l] += AuxPhase*T;
           RateFitXtt[l] += T*T;
           RateFitXt[l] += T;
           RateFitXp[l] += AuxPhase;
           
	   // Store current phase (useful to figure out wraps):
	   PrevPhX[l] = AuxPhase;

	   // Add this integration time to the full-data output:
	   // If this is the first data point, we add another one 
	   // at Tini - 10s, to avoid silly extrapolarion scipy issues.
          // if (Ref>0){
             if (NTimes[l] == 0){Ti[l][0] = T - 10.; PDi[l][0] = R2D*AuxPhase; NTimes[l] += 1;};	     
             Ti[l][NTimes[l]] = T; PDi[l][NTimes[l]] = R2D*AuxPhase;
	     NTimes[l] += 1;
          // };
           
           NGoods[l] += 1.0;  // only if we have good detections at BOTH polarizations.

             
           
        } else { // If there are no detections for this tone, just use the previous value:
          if (NTimes[l] == 0){Ti[l][0] = T - 10.; PDi[l][0] = 0.0; NTimes[l] += 1;};	     	
	  Ti[l][NTimes[l]] = T; PDi[l][NTimes[l]] = R2D*PrevPhX[l];	
	  NTimes[l] += 1;
	
	};

	// Update the data size if needed: 
        if (NTimes[l] == TBUFF[l]){
          TBUFF[l] += 64; 
          Ti[l] = (double *) realloc(Ti[l],TBUFF[l]*sizeof(double));
          PDi[l] = (double *) realloc(PDi[l],TBUFF[l]*sizeof(double));
        };

        goodX[l]=false; goodY[l]=false;


       }; 
  
    NEntry += 1.0;   
    };
  };




// Variables to store final results:
double *Phases = new double[Aux];
double *DFreqs = new double[Aux];
double *Rates = new double[Aux];

// Auxiliary variables:
double AvT, AvP;
int FirstGood = -1;

// Arrange some results:
for(j=0;j<Aux;j++){

  // We add a new point at 10s after the last int. time,
  // to avoid silly scipy extrapolation errors:	
  Ti[j][NTimes[j]] = Ti[j][NTimes[j]-1] + 10.;
  PDi[j][NTimes[j]] = PDi[j][NTimes[j]-1];
  NTimes[j] += 1;

  // If the first integrations for this tone were not good,
  // fill them with the first valid phase for this tone:
  for (i=0; i<NTimes[j]; i++){
    if (PDi[j][i] != 0.0){FirstGood = i; break;};
  };	
  for (i=0; i<FirstGood; i++){PDi[j][i] = PDi[j][FirstGood];};  

  // If the last integrations for this tone were not good,
  // fill them with the last valid phase for this tone:
  for (i=NTimes[j]-1; i>=0; i--){
    if (PDi[j][i] != 0.0){FirstGood = i; break;};
  };	
  for (i=FirstGood+1; i<NTimes[j]; i++){PDi[j][i] = PDi[j][FirstGood];};  


  // Now, fit the phase rate, using a simple linear-regression approach:
  AvT = RateFitXt[j]/NGoods[j];
  AvP = RateFitXp[j]/NGoods[j];
  DFreqs[j]= PCalNus[j];
  Rates[j] = (RateFitXpt[j] - NGoods[j]*AvP*AvT)/(RateFitXtt[j] - NGoods[j]*AvT*AvT)/(2.*PI);

  // If the cross-pol phasecals are not being computed (Ref>0), we provide the model 
  // prediction at the first integration time, as taken from the linear regression: 
  if (Ref>0){
    Phases[j] = R2D*(AvP - (2.*PI)*Rates[j]*AvT);
  } else { // if not, just provide the accumulated cross-polarization value:
    Phases[j] = R2D*std::arg(PCalsD[j]);};


  // Now, prepare the accumulated amplitudes and store them in PCalAX:
  switch(Ref){
      case 0: PCalsAX[j] = PCalsAY[j]/PCalsAX[j]; break;
      case 1: PCalsAX[j] = PCalsAX[j]/PCalsAX[0]; break;
  };    
    
    
  };


  double FrAux, PhAux, RAux;
  double *AuxD;
  double AuxA;

// Sort the data in order of increasing frequency:
  for (i=0; i<Aux-1; i++){
    for (j=i+1; j<Aux; j++){
      if (DFreqs[i]>DFreqs[j]){
        FrAux=DFreqs[i]; DFreqs[i]=DFreqs[j]; DFreqs[j]=FrAux;
        PhAux=Phases[i]; Phases[i]=Phases[j]; Phases[j]=PhAux;
        RAux=Rates[i];   Rates[i] = Rates[j]; Rates[j] =RAux;
        AuxD = Ti[i]; Ti[i] = Ti[j]; Ti[j] = AuxD;
        AuxD = PDi[i]; PDi[i] = PDi[j]; PDi[j] = AuxD;
        AuxA = PCalsAX[i]; PCalsAX[i] = PCalsAX[j]; PCalsAX[j] = AuxA;
      };
    }; 
  };

// Minimum frequency difference among tones (assume first 2 tones at the same IF):
  double DNu = DFreqs[1] - DFreqs[0];

  // Auxiliary variables to fit the group delay at each IF:
  double FracP, IntP;
  double IFDel00, IFDel01, IFDel0, IFDel1, NtoneIF;
  int iJump = 0;


  // Connect phases among IFs:
  for(i=0;i<Aux-1;i++){

    /////////////////////////	  
    // First, a simple connection between neighoring tones:	  
    if (Phases[i+1]-Phases[i] > 180.){for(j=i+1;j<Aux;j++){Phases[j] -= 360.; for(Aux2=0; Aux2<NTimes[j]; Aux2++){PDi[j][Aux2] -= 360.;};}; };
    if (Phases[i+1]-Phases[i] < -180.){for(j=i+1;j<Aux;j++){Phases[j] += 360.; for(Aux2=0; Aux2<NTimes[j]; Aux2++){PDi[j][Aux2] += 360.;};};};
    /////////////////////////
    
    
    ///////////////////////////////////////////////////////////////
    // Between IFs, we extrapolate the phase from the edge tone using the
    // group delay of the whole IF, as estimated from the tones within that IF:
    // The group delay of the IF is estimated from a simple linear regression:

    if(i>1 && DFreqs[i+1]-DFreqs[i] > DNu){ // Condition to assume that we have jumped to another IF.
 
      IFDel00 = 0.0; IFDel01 = 0.0; IFDel0 = 0.0; IFDel1 = 0.0; 

      // iJump is the index of the first tone in the IF to be fitted.
      NtoneIF = (double) (i-iJump+1); // Number of phasecal tones within the IF.

      for(j=iJump; j<=i; j++){
        IFDel00 += DFreqs[j]*DFreqs[j]; IFDel0 += DFreqs[j];
	    IFDel01 += DFreqs[j]*Phases[j]; IFDel1 += Phases[j];
      };	      
      iJump = i+1; // Now, iJump will be the index of the first tone in the NEW IF.
      IFDel0 /= NtoneIF ; IFDel1 /= NtoneIF;

      // Estimated phase of the iJump-th tone (group-delay extrapolation from the previous IF):
      NWrap[i] = (IFDel01 - NtoneIF*IFDel0*IFDel1)/(IFDel00 - NtoneIF*IFDel0*IFDel0)*(DFreqs[i+1]-DFreqs[i]) + Phases[i]-Phases[i+1];

      // Convert this difference into an integer number of wraps:
      NWrap[i] /= 360.; FracP = modf(NWrap[i], &IntP);
      if (FracP>0.5){IntP += 1.;} else if(FracP<-0.5){IntP -= 1.;}; // Difference will always be <180 degrees.

      // Correct all data accordingly:
      for(j=i+1;j<Aux;j++){Phases[j] += 360.*IntP; for(Aux2=0; Aux2<NTimes[j]; Aux2++){PDi[j][Aux2] += 360.*IntP;};}
    };    
  };
  


 // Release memory:
  delete[] PCalsX;
  delete[] PCalsY;
  delete[] PCalsAY;
  delete[] PCalsD;
  delete[] PCalNus;
  delete[] RateFitXpt;
  delete[] RateFitXtt;
  delete[] RateFitXp;
  delete[] RateFitXt;
  delete[] PrevPhX;
  delete[] goodX; 
  delete[] goodY;
  delete[] NWrap;


  // Arrange data for output to Python:
  long dims[1];
  dims[0] = (long) Aux;
  PyObject *XYadd = PyArray_SimpleNewFromData(1, dims, NPY_FLOAT64, Phases);
  PyObject *XYnu =  PyArray_SimpleNewFromData(1, dims, NPY_FLOAT64, DFreqs);
  PyObject *XRates =  PyArray_SimpleNewFromData(1, dims, NPY_FLOAT64, Rates);
  PyObject *XAmps =  PyArray_SimpleNewFromData(1, dims, NPY_FLOAT64, PCalsAX);


  // Return the 2D phase matrix (i.e., contiguous in time and frequency), if asked to:
      dims[0] = (long) NTimes[0];	  
      PyObject *TimesOutput = PyArray_SimpleNewFromData(1, dims, NPY_FLOAT64, Ti[0]);
    double *AllOutput = new double[NTimes[0]*Aux];
    for (i=0; i<Aux; i++){
       for (j=0; j<NTimes[0]; j++){
         AllOutput[i*NTimes[0] + j] = PDi[i][j];
       };	       
    };	    
    long dims2[2];
    dims2[0] = (long) Aux;
    dims2[1] = (long) NTimes[0];
    PyObject *PDiOutput = PyArray_SimpleNewFromData(2, dims2, NPY_FLOAT64, AllOutput);

    delete[] NTimes;
    ret = Py_BuildValue("[O,O,O,d,O,O,O]",XYnu,XYadd,XRates,Tini,XAmps,TimesOutput,PDiOutput);
    return ret;



};













static PyObject *XPConvert(PyObject *self, PyObject *args)
{

      
  // Object to return:
  PyObject *ret; 

  ret = Py_BuildValue("i",-1);
  
  // Function arguments:
  PyObject *pFName;
  if (!PyArg_ParseTuple(args, "O", &pFName)){printf("FAILED XPConvert! Wrong arguments!\n"); fflush(stdout);  return ret;};


  double T;
  int i, j, k, nui; 
  bool isX = false, RepNu = false;
  char Pol;
  long currP, lastP, Nbytes;

// Size of pcal buffer:
  int BuffSize = 1024;
  int NewBuffSize = BuffSize;

// Buffers for pcals:
  int *PCalNus = (int *) malloc(BuffSize*sizeof(int));
  double *LastPcalRe = (double *) malloc(BuffSize*sizeof(double));
  double *LastPcalIm = (double *) malloc(BuffSize*sizeof(double));
  int NPCals = 0;

  for (i=0; i<BuffSize; i++){
    PCalNus[i] = -1; LastPcalRe[i] = 0.0; LastPcalIm[i] = 0.0;
  };



// MAKE A BACKUP OF THE PCAL FILE:
  std::string PcalFile = PyString_AsString(pFName);
  std::ifstream PcalF;
  PcalF.open(PcalFile.c_str(), std::ios::in | std::ios::binary);

  std::string SUFFIX(".ORIGINAL");
  std::string outname = PcalFile + SUFFIX;  
  std::ofstream OutPcal(outname.c_str(), std::ios::out | std::ios::binary);

//  char buf[1028];
//  while(PcalF.gcount()>0){PcalF.read(&buf[0],1028); OutPcal.write(&buf[0],PcalF.gcount());};

  OutPcal << PcalF.rdbuf();

  PcalF.close();
  OutPcal.close();


    

///////////////////////////////
// OPEN PHASECAL FILE:
  PcalF.open(outname.c_str(), std::ios::in);

// OPEN OUTPUT FILE:
  OutPcal.open(PcalFile.c_str(), std::ios::out);



  std::string TelName, line, auxStr;
  std::stringstream tempStr;
  std::stringbuf *linePos;


  while (!PcalF.eof()){

    // Read original line:
    std::getline(PcalF,line);
    tempStr.str(line);

    // Change entries: (X,Y) -> (R,L) and R = L = X.
    if (line.length() > 10 && line[0] != '#'){
       tempStr.clear();
       tempStr.precision(5);
     //  tempStr.setw(12);

       linePos = tempStr.rdbuf();

	// First elements in line (dummy reads):       
         tempStr >> TelName;
         tempStr >> T;
         tempStr >> T;
         tempStr >> i;
         tempStr >> i;
         tempStr >> i;


       i=0; j=0;
       while(std::getline(tempStr,auxStr,' ')){
         if (auxStr.length() > 0){
           switch(i){

	     // Remember the format: 'FREQ POL RE IM':
	     // (i.e., case 0, 1, 2, 3).

             case 0: nui = atoi(auxStr.c_str());
               RepNu = false; // Is this tone NEW (i.e., not found in the previous times??)
               if (nui<0){j=-1;} else {
                 for(j=0;j<NPCals;j++){
                   if(nui==PCalNus[j]){
                     RepNu = true;
                     break;
                   };
                 };
               }; // In case of a new tone, add it to the data (and update 'NPCals'):
               if (!RepNu && nui >0){PCalNus[NPCals]=nui; j=NPCals; NPCals+=1;}; 

	       ///////////////
	       // Unlikely re-buffering:
	       if (NPCals >= BuffSize){
		 NewBuffSize += BuffSize;      
		 PCalNus = (int *) realloc(PCalNus, NewBuffSize*sizeof(int));
		 LastPcalRe = (double *) realloc(LastPcalRe, NewBuffSize*sizeof(double));
		 LastPcalIm = (double *) realloc(LastPcalIm, NewBuffSize*sizeof(double));
                 for (k=NPCals; k<NewBuffSize; k++){
		   PCalNus[k] = -1; LastPcalRe[k] = 0.0; LastPcalIm[k] = 0.0; 
		 };    
	       };
               //////////////

               i+= 1; break;


             case 1: if(nui<=0){i+=1; break;} Pol = auxStr.c_str()[0];

	       if (Pol=='X'){  // If X, keep next Re and Im; Change to R
                 isX = true;
		 currP = tempStr.tellg();
                 tempStr.seekp(currP-2);
		 tempStr << 'R';


	       } else if (Pol=='Y'){ // If Y, change Re and Im; Change to L
                 isX = false;
                 currP = tempStr.tellg();
                 tempStr.seekp(currP-2);
		 tempStr << 'L';

	       };	       
		     
	       i += 1; break;


             case 2: if(nui<=0){i+=1; break;};
		if (isX){
		  LastPcalRe[j] = atof(auxStr.c_str());
		} else {
	          lastP = tempStr.tellg();		
                  tempStr.seekp(currP);
                  Nbytes = lastP - currP-1;
		  currP = lastP;
		  tempStr << std::setw(Nbytes) << std::scientific << LastPcalRe[j];
		};	
		i += 1; break;


             case 3: if(nui<=0){i = 0; isX = false; break;};
		if(isX){
		  LastPcalIm[j] = atof(auxStr.c_str());
		} else {
	          lastP = tempStr.tellg();		
                  tempStr.seekp(currP);
                  Nbytes = lastP - currP-1;
		  tempStr << std::setw(Nbytes) << LastPcalIm[j];
		};

		i = 0; isX = false; break;

           };
         };
       };



  };

  // Write new lines:
  OutPcal << tempStr.str() << std::endl ;


};

  OutPcal.close();
  PcalF.close();
  
  free(PCalNus); 
  free(LastPcalRe);
  free(LastPcalIm);
  

// Return with no problems:
ret = Py_BuildValue("i",0);
return ret;


};


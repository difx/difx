/*

# Copyright (c) Ivan Marti-Vidal 2015. 
#               EU ALMA Regional Center. Nordic node.
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


#include <Python.h>
// compiler warning that we use a deprecated NumPy API
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
#include "DataIO.h"
#include "DataIOFITS.h"
#include "DataIOSWIN.h"
#include "CalTable.h"
#include "Weighter.h"
#include <sstream> 



/* Docstrings */
static char module_docstring[] =
    "This module provides an interface for PolConvert.";
static char PolConvert_docstring[] =
    "Converts mixed-polarization visibilities into pure circular-polarization basis";

/* Available functions */
static PyObject *PolConvert(PyObject *self, PyObject *args);


/* Module specification */
static PyMethodDef module_methods[] = {
    {"PolConvert", PolConvert, METH_VARARGS, PolConvert_docstring},
    {NULL, NULL, 0, NULL}
};


/* Initialize the module */
PyMODINIT_FUNC init_PolConvert(void)
{
    PyObject *m = Py_InitModule3("_PolConvert", module_methods, module_docstring);
    if (m == NULL)
        return;

}

//////////////////////////////////
// MAIN FUNCTION: 
static PyObject *PolConvert(PyObject *self, PyObject *args)
{


  //static const std::complex<float> oneOverSqrt2 = 0.7071067811;
  static const std::complex<float> Im = (std::complex<float>) std::polar(1.0,1.570796326);  

    long i,j,k,auxI;
    int ii, ij, ik, il, im;

    PyObject *ngain, *nsum, *gains, *ikind, *dterms, *plotRange, *IDI, *antnum, *tempPy, *ret; 
    PyObject *allphants, *nphtimes, *phanttimes, *Range, *SWAP, *doIF, *metadata, *refAnts;
    PyObject *asdmTimes; 
    int nALMA, plIF, plAnt, nPhase, doTest, doConj, doNorm;
    double XYadd;
    bool isSWIN; 

    if (!PyArg_ParseTuple(args, "iiiiOOOOOOOOOOOOOOOOiiidO",&nALMA, &plIF, &plAnt, &nPhase, &doIF, &SWAP, &ngain,&nsum, &ikind, &gains, &dterms, &IDI, &antnum, &plotRange, &Range, &allphants, &nphtimes, &phanttimes, &refAnts, &asdmTimes, &doTest, &doConj, &doNorm, &XYadd, &metadata)){printf("FAILED PolConvert! Wrong arguments!\n"); fflush(stdout);  return NULL;};



// OPEN LOG FILE:
 //   time_t now = time(0); 
 //   char logname[28] = {0};
    char message[512];

  //  if (now != -1){
  //     strftime(logname, 28, "PolConvert.%j_%H_%M_%S.log", gmtime(&now));
  //  };

    FILE *logFile = fopen("PolConvert.log","w");



// Sort out if SWIN files or FITS-IDI files are gonig to be converted:
// (if the length of the metadata list is zero, this is a FITS-IDI file)
    int SWINnIF = (int) PyList_Size(metadata)-1 ;
    isSWIN = SWINnIF > 0;

    int nSWINFiles = 0; // compiler warning
    std::string* SWINFiles, outputfits;

    if (isSWIN) {
       nSWINFiles = (int) PyList_Size(IDI) ;
       SWINFiles = new std::string[nSWINFiles];
       for (ii=0; ii<nSWINFiles; ii++){
         SWINFiles[ii] = PyString_AsString(PyList_GetItem(IDI,ii));
       }; 
       sprintf(message,"\nCONVERTING %i SWIN (DiFX) FILES\n\n",nSWINFiles);
       fprintf(logFile,message); std::cout<<message; fflush(logFile);

       fflush(logFile);
    } else {
      SWINFiles = new std::string[nSWINFiles];  // compiler warning
      outputfits = PyString_AsString(IDI);
      sprintf(message,"\nOUTPUT FITS-IDI FILE:  %s \n\n",outputfits.c_str());
      fprintf(logFile,message); std::cout<<message; fflush(logFile);

      fflush(logFile);
    };


// If SWIN, read the frequency channels from the metadata: 
   int *SWINnchan = 0;  // compiler warning
   double **SWINFreqs = 0, jd0 = 0.0;   // compiler warning

   if (isSWIN) {
     SWINFreqs = new double*[SWINnIF];
     SWINnchan = new int[SWINnIF];
     for (ii=0; ii<SWINnIF; ii++) {
      SWINFreqs[ii] =  (double *)PyArray_DATA(PyList_GetItem(metadata,ii));
      SWINnchan[ii] = ((int *) PyArray_DIMS(PyList_GetItem(metadata,ii)))[0] ;
     };
     jd0 = PyFloat_AsDouble(PyList_GetItem(metadata,SWINnIF));
   };



// Do we just test?
    if (doTest) {sprintf(message,"\n Will only compute, but not update the output file(s)\n");fprintf(logFile,message); std::cout<<message; fflush(logFile);
};



// return value if there is an error:
    ret = Py_BuildValue("i",1);


// Times to analyse:
    double *plRange = (double *)PyArray_DATA(plotRange);
    double *doRange = (double *)PyArray_DATA(Range);



// Read info for linear-polarization antennas:
    double *time0 = (double *)PyArray_DATA(PyList_GetItem(asdmTimes,0));
    double *time1 = (double *)PyArray_DATA(PyList_GetItem(asdmTimes,1));
    long nASDMEntries = ((long *) PyArray_DIMS(PyList_GetItem(asdmTimes,0)))[0];  


///////////
// Useful to determine which ALMA antennas are phased up at each time:
    int *ASDMant = new int[nPhase];
    int *ALMARef; 
    double **ASDMtimes = new double*[nPhase];
    long *nASDMtimes = new long[nPhase];

    ALMARef = (int *)PyArray_DATA(refAnts);

    for (ii=0; ii<nPhase; ii++){
      nASDMtimes[ii] = (long)PyInt_AsLong(PyList_GetItem(nphtimes,ii));
      ASDMant[ii] = (int)PyInt_AsLong(PyList_GetItem(allphants,ii));
      ASDMtimes[ii] = (double *)PyArray_DATA(PyList_GetItem(phanttimes,ii));
    };

    Weighter *ALMAWeight = new Weighter(nPhase,nASDMtimes,nASDMEntries,ASDMant,ASDMtimes,ALMARef,time0,time1);

///////////

//////////////////////////////////////////////
// ALLOCATE MEMORY
    int **kind = new int*[nALMA];
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
      XYSWAP[i] = (bool)PyInt_AsLong(PyList_GetItem(SWAP,i));
      ngainTabs[i] = (int)PyInt_AsLong(PyList_GetItem(ngain,i));
      kind[i] = new int[ngainTabs[i]];
      nsumArr[i] = (int)PyInt_AsLong(PyList_GetItem(nsum,i));
      almanums[i] = (int)PyInt_AsLong(PyList_GetItem(antnum,i));
      if(XYSWAP[i]){sprintf(message,"\nWill swap X/Y channels for antenna #%i.\n",almanums[i]);fprintf(logFile,message); std::cout<<message; fflush(logFile);
};
      ntimeArr[i] = new long*[ngainTabs[i]];
      nchanArr[i] = new long[ngainTabs[i]];
      timesArr[i] = new double**[ngainTabs[i]];
      gainsArrR1[i] = new double**[ngainTabs[i]];
      gainsArrI1[i] = new double**[ngainTabs[i]];
      gainsArrR2[i] = new double**[ngainTabs[i]];
      gainsArrI2[i] = new double**[ngainTabs[i]];
      gainflag[i] = new bool**[ngainTabs[i]];
      freqsArr[i] = new double*[ngainTabs[i]];
    };
//////////////////////////////////////////////



//////////////////////////////////////////////
// READ CALIBRATION TABLES:
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
      dtermsArrR1[i][j] = (double *)PyArray_DATA(PyList_GetItem(PyList_GetItem(PyList_GetItem(dterms,i),j+1),0));
      dtermsArrI1[i][j] = (double *)PyArray_DATA(PyList_GetItem(PyList_GetItem(PyList_GetItem(dterms,i),j+1),1));
      dtermsArrR2[i][j] = (double *)PyArray_DATA(PyList_GetItem(PyList_GetItem(PyList_GetItem(dterms,i),j+1),2));
      dtermsArrI2[i][j] = (double *)PyArray_DATA(PyList_GetItem(PyList_GetItem(PyList_GetItem(dterms,i),j+1),3));
      dtflag[i][j] = (bool *)PyArray_DATA(PyList_GetItem(PyList_GetItem(PyList_GetItem(dterms,i),j+1),4));
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
       timesArr[i][j][k] = (double *)PyArray_DATA(PyList_GetItem(PyList_GetItem(tempPy,k+1),0));
       gainsArrR1[i][j][k] = (double *)PyArray_DATA(PyList_GetItem(PyList_GetItem(tempPy,k+1),1));
       gainsArrI1[i][j][k] = (double *)PyArray_DATA(PyList_GetItem(PyList_GetItem(tempPy,k+1),2));
       gainsArrR2[i][j][k] = (double *)PyArray_DATA(PyList_GetItem(PyList_GetItem(tempPy,k+1),3));
       gainsArrI2[i][j][k] = (double *)PyArray_DATA(PyList_GetItem(PyList_GetItem(tempPy,k+1),4));
       gainflag[i][j][k] = (bool *)PyArray_DATA(PyList_GetItem(PyList_GetItem(tempPy,k+1),5));
      };
    };
   };


// ALLOCATE MEMORY FOR CALIBRATION INSTANCES:
   CalTable ***allgains = new CalTable**[nALMA];
   CalTable **alldterms = new CalTable*[nALMA];

// CREATE CALIBRATION INSTANCES:   
   for (i=0; i<nALMA; i++){
     allgains[i] = new CalTable*[ngainTabs[i]];
     alldterms[i] = new CalTable(2,dtermsArrR1[i],dtermsArrI1[i],dtermsArrR2[i],dtermsArrI2[i],dtfreqsArr[i],dttimesArr[i],nsumArr[i],ndttimeArr[i], nchanDt[i],dtflag[i],logFile);
     for (j=0; j<ngainTabs[i];j++){
       allgains[i][j] = new CalTable(kind[i][j],gainsArrR1[i][j],gainsArrI1[i][j],gainsArrR2[i][j],gainsArrI2[i][j],freqsArr[i][j],timesArr[i][j],nsumArr[i],ntimeArr[i][j], nchanArr[i][j],gainflag[i][j],logFile);

//////////////////////////////////////////////





  };

   };




  fflush(logFile);

/////////////////////////////////
// READ VLBI DATA:

     DataIO *DifXData ;  // Polymorphism to SWIN or FITS-IDI.
     bool OverWrite= true; // Always force overwrite (for now)

     if (isSWIN) {
       sprintf(message,"\n\n Opening and preparing SWIN files.\n");
       fprintf(logFile,message); std::cout<<message; fflush(logFile);

       DifXData = new DataIOSWIN(nSWINFiles, SWINFiles, nALMA, almanums, doRange, SWINnIF, SWINnchan, SWINFreqs, OverWrite, doTest, jd0,logFile);
     } else {
       sprintf(message,"\n\n Opening FITS-IDI file and reading header.\n");
       fprintf(logFile,message); std::cout<<message; fflush(logFile);

       DifXData = new DataIOFITS(outputfits, nALMA, almanums, doRange, OverWrite, doConj,logFile);
     };

     if(!DifXData->succeed()){
          sprintf(message,"\nERROR WITH DATA FILE(S)!\n");
          fprintf(logFile,message); std::cout<<message; fflush(logFile);

          return ret;
     };


   sprintf(message,"\n\nFirst observing Julian day: %11.2f\n",DifXData->getDay0());
   fprintf(logFile,message); std::cout<<message; fflush(logFile);



// How many IFs do we convert?
    int nIFconv = (int) PyList_Size(doIF) ;
    bool doAll = false;

// If no IF list was given, convert all of them:
    if (nIFconv==0){nIFconv = DifXData->getNfreqs(); doAll=true;};

// Which IFs do we convert?
    int IFs2Conv[nIFconv];
    for (ii=0; ii<nIFconv; ii++) {
      if (doAll){IFs2Conv[ii]=ii+1;} else {
        IFs2Conv[ii] = (int)PyInt_AsLong(PyList_GetItem(doIF,ii));
      };
    }; 



   int nnu = DifXData->getNfreqs();
   int ALMARefAnt = -1; // If no calAPP is used, do not look for any extra X-Y phase offset.

   int nchans[nnu]; 
   int maxnchan=0;
   for (ii=0; ii<nnu; ii++) {
    nchans[ii] = DifXData->getNchan(ii); // printf("%i -- %i\n",ii,nchans[ii]);
    if (nchans[ii]>maxnchan) {maxnchan=nchans[ii];};
   };

  sprintf(message,"\n The VLBI IFs have a maximum of %i channels\n",maxnchan);
  fprintf(logFile,message); std::cout<<message; fflush(logFile);


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
  int currAnt, currAntIdx, currNant, otherAnt; 
  bool notinlist, gchanged, dtchanged, toconj;

  lastTFailed = 0.0;

  long countNvis;

  std::complex<float> AD, BC, auxD;
  float NormFac[2];
  std::complex<float> DetInv;
  std::complex<float> Kinv[2][2];
  std::complex<float> gainXY[2], gainRatio;

  bool allflagged, auxB ;

  sprintf(message,"\n Will modify %li visibilities (lin-lin counted twice).\n\n",DifXData->getMixedNvis());
  fprintf(logFile,message); std::cout<<message; fflush(logFile);



// TIME RANGE FOR PLOTTING:
   plRange[0] = ((plRange[0]+DifXData->getDay0())-2400000.5)*86400.;
   plRange[1] = ((plRange[1]+DifXData->getDay0())-2400000.5)*86400.;

// TIME RANGE FOR CORRECTING:
   doRange[0] = ((doRange[0]+DifXData->getDay0())-2400000.5)*86400.;
   doRange[1] = ((doRange[1]+DifXData->getDay0())-2400000.5)*86400.;

/////////////////////////////////




    FILE *plotFile;
    FILE *gainsFile;

    plotFile = fopen("POLCONVERT.FRINGE","wb");

    if(doNorm){ 
//      for(ii=0; ii<nnu;ii++){
//         sprintf(message,"POLCONVERT.GAINS.IF%i",ii);
//         gainsFile[ii] = fopen(message,"w");
//      };
    gainsFile = fopen("POLCONVERT.GAINS","wb");

    };


    int noI = -1;
    plIF -= 1; // IF number to zero-based.

    if (plIF>=0 && plIF < nnu){fwrite(&nchans[plIF],sizeof(int),1,plotFile);}else{fwrite(&noI,sizeof(int),1,plotFile);};





//////////////////////////////////////////////////////////
///////////////////////////////////
// MAIN LOOP FOR CORRECTION (LOOP OVER IFs):

//  for (ii=0; ii<nnu; ii++){
  for (im=0; im<nIFconv; im++) {

    ii = IFs2Conv[im] - 1;

    if (ii >= nnu) {
      sprintf(message,"ERROR! DATA DO NOT HAVE IF #%i !!\n",ii);  
      fprintf(logFile,message); std::cout<<message; fflush(logFile);

    return ret;};

    sprintf(message,"\nDoing subband %i of %i\n",ii+1,nnu);
    fprintf(logFile,message); 
    fflush(logFile);
    printf("\rDoing subband %i of %i   ",ii+1,nnu);
    fflush(stdout);


    if(!DifXData->setCurrentIF(ii)){return ret;};

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

     while(DifXData->getNextMixedVis(currT,currAnt, otherAnt, toconj)){

       countNvis += 1;

    //   if(countNvis%1024==0){printf("\r Doing vis %i",countNvis);fflush(stdout);};

// Check if there was an error in reading:
     if (!DifXData->succeed()){DifXData->finish();return ret;};



// Do we have to correct this visibility?

     if(currT>=doRange[0] && currT<=doRange[1]) {


// Sanity check (if antenna is in the list of linear-pol antennas):
     notinlist = true;
     for (ij=0; ij<nALMA; ij++) {
        if (currAnt == almanums[ij]){ currAntIdx = ij; notinlist=false; break;};
     };
     if (notinlist){
      sprintf(message,"ERROR: Found linear-pol data for antenna number %i.\n",currAnt);
      fprintf(logFile,message); std::cout<<message; fflush(logFile);


      sprintf(message,"This antenna is not in the list of linear-pol antennas!\n");
      fprintf(logFile,message);  std::cout<<message; fflush(logFile);

      DifXData->finish(); return ret;
     };



//////////////////////////////////////////////////////
// Set the interpolation time and compute gains:

     currNant = nsumArr[currAntIdx] ;

// Find the ALMA antennas involved in the phasing:
     allflagged = true;
     for (ij=0; ij<currNant; ij++) {
         Weight[currAntIdx][ij] = ALMAWeight->getWeight(ij,currT);
         if (Weight[currAntIdx][ij]){allflagged = false;};
     };

// get ALMA refant used in the Phasing (to correct for X-Y phase offset):
     ALMARefAnt = ALMAWeight->getRefAnt(currT);
     
     gainRatio = (std::complex<float>) std::polar(1.0,XYadd); 
 

     if(allflagged && currT != lastTFailed){
       double dayFrac = (currT/86400. - DifXData->getDay0()+2400000.5);
    //   printf("%.16e\n",dayFrac);
       int day = (int) dayFrac ;
       int hour = (int) (dayFrac*24.);
       int min = (int) ((dayFrac*24. - ((double) hour))*60.);
       int sec = (int) ((dayFrac*24. - ((double) hour) - ((double) min)/60.)*3600.);
       sprintf(message,"WARNING: NO VALID ALMA ANTENNAS ON %i-%i:%i:%i ?!?!\n WILL CONVERT ON THIS TIME *WITHOUT* CALIBRATION\n",day,hour,min,sec);
       fprintf(logFile,message); fflush(logFile);

       lastTFailed = currT ;
     };



   if (!allflagged){
/////////
// GAIN:


    // FIRST GAIN IN NORMAL MODE, 0:
     gchanged = allgains[currAntIdx][0]->setInterpolationTime(currT);
      for (ij=0; ij<currNant; ij++) {
        if (Weight[currAntIdx][ij]) {
          allgains[currAntIdx][0]->applyInterpolation(ij,0,AnG[currAntIdx][ij]); };
      };


// FURTHER GAIN, IN PRODUCT MODE, 2:
     for (ik=1; ik<ngainTabs[currAntIdx]; ik++) {
      auxB = allgains[currAntIdx][ik]->setInterpolationTime(currT) ;
      gchanged = gchanged || auxB;
      for (ij=0; ij<currNant; ij++) {
        if (Weight[currAntIdx][ij]) {
        allgains[currAntIdx][ik]->applyInterpolation(ij,2,AnG[currAntIdx][ij]);  };
      };
    };


// CROSS-PHASE GAIN AT THE ALMA REFERENCE ANTENNA:
   for (ik=0; ik<ngainTabs[currAntIdx]; ik++) {
     if (ALMARefAnt>=0 && !(allgains[currAntIdx][ik]->isBandpass())){
       if (allgains[currAntIdx][ik]->getInterpolation(ALMARefAnt,0,gainXY)){
         gainRatio *= gainXY[0]/gainXY[1]; 
       } else {
          sprintf(message,"ERROR with ALMA Ref. Ant. in gain table!\n");
          fprintf(logFile,message); fflush(logFile);

          DifXData->finish(); return ret;
       };
     }; 
   };

   gainRatio /= std::abs(gainRatio);  // Normalize gain ratio.

/////////
// DTERM:
     dtchanged = alldterms[currAntIdx]->setInterpolationTime(currT);
      for (ij=0; ij<currNant; ij++) {
        if (Weight[currAntIdx][ij]) {
         alldterms[currAntIdx]->applyInterpolation(ij,0,AnDt[currAntIdx][ij]);  };
      };

//////////////////////////////////

  };   // Comes from if(!allflagged)





// FORCE RE-COMPUTATION (TO SET UNITY MATRIX) IF ALL ANTENNAS ARE FLAGGED
  if (allflagged){gchanged=true; dtchanged=true;};



////////////
// Compute the elements of the K matrix (only those that changed):

   if (dtchanged || gchanged) {


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
    for (ij=0; ij<currNant; ij++) {


      if (Weight[currAntIdx][ij]) {  // ONLY IF ANTENNA WAS USED IN THE PHASING

     // Total weight:
       auxD += 1.0;

    // Kfrozen is unlikely to change much with time:
       if (dtchanged) {
        for (j=0; j<nchans[ii]; j++) {
         gainXY[0] = 1.0 ; 
         gainXY[1] = 1.0 ;
         Kfrozen[currAntIdx][0][1][ij][j] = gainXY[0]*AnDt[currAntIdx][ij][1][j];
         Kfrozen[currAntIdx][1][0][ij][j] = gainXY[1]*AnDt[currAntIdx][ij][0][j];
         Kfrozen[currAntIdx][0][0][ij][j] = gainXY[0];
         Kfrozen[currAntIdx][1][1][ij][j] = gainXY[1];
        };
       };

     for (j=0; j<nchans[ii]; j++) {
       K[currAntIdx][0][0][ij][j] = Kfrozen[currAntIdx][0][0][ij][j]*AnG[currAntIdx][ij][0][j];
       K[currAntIdx][1][1][ij][j] = Kfrozen[currAntIdx][1][1][ij][j]*AnG[currAntIdx][ij][1][j];
       K[currAntIdx][0][1][ij][j] = Kfrozen[currAntIdx][0][1][ij][j]*AnG[currAntIdx][ij][0][j];
       K[currAntIdx][1][0][ij][j] = Kfrozen[currAntIdx][1][0][ij][j]*AnG[currAntIdx][ij][1][j];
     };

// Add-up all elements:
     for (il=0; il<2; il++) {
      for (ik=0; ik<2; ik++) {
        for (j=0; j<nchans[ii]; j++) {
          Ktotal[currAntIdx][il][ik][j] += K[currAntIdx][il][ik][ij][j];
        };
       };
      };


   }; // Comes from if(Weight....)


    };


   NormFac[0] = 0.0; NormFac[1] = 0.0; 

// Get the average:
    for (j=0; j<nchans[ii]; j++) {

     for (ij=0; ij<2; ij++) {
      for (ik=0; ik<2; ik++) {
        Ktotal[currAntIdx][ij][ik][j] /= auxD;
      };
     };

// Correct the phase offset at the reference antenna:
     Ktotal[currAntIdx][0][1][j] *= gainRatio;
     Ktotal[currAntIdx][1][1][j] *= gainRatio;

// Normalize amplitudes:

////////////

///////////////////////////
// THIS CODE IS INDEPENDENT OF HOW THE AVERAGE FOR K MATRIX IS IMPLEMENTED


      AD = Ktotal[currAntIdx][0][0][j]*Ktotal[currAntIdx][1][1][j];
      BC = Ktotal[currAntIdx][0][1][j]*Ktotal[currAntIdx][1][0][j];


   // Determinant:
      DetInv = (AD - BC); // 

   // Inverse of K matrix:

      if (allflagged) {
       Kinv[0][0] = 1.0; 
       Kinv[0][1] = 0.0;
       Kinv[1][0] = 0.0;
       Kinv[1][1] = 1.0; 
      } else {
       Kinv[0][0] = Ktotal[currAntIdx][1][1][j]/DetInv;
       Kinv[0][1] = -Ktotal[currAntIdx][0][1][j]/DetInv;
       Kinv[1][0] = -Ktotal[currAntIdx][1][0][j]/DetInv;
       Kinv[1][1] = Ktotal[currAntIdx][0][0][j]/DetInv;
      };

       if(doNorm){ 
          NormFac[0] += std::abs(Kinv[0][0]); 
          NormFac[1] += std::abs(Kinv[1][1]);
       };

   // Multiply by conversion (hybrid) matrix and save result in the "Ktotal" matrix:
 
        Ktotal[currAntIdx][0][0][j] = (Kinv[0][0]+Im*Kinv[1][0]); // /oneOverSqrt2;
        Ktotal[currAntIdx][0][1][j] = (Kinv[0][1]+Im*Kinv[1][1]); // /oneOverSqrt2;
        Ktotal[currAntIdx][1][0][j] = (Kinv[0][0]-Im*Kinv[1][0]); // /oneOverSqrt2;
        Ktotal[currAntIdx][1][1][j] = (Kinv[0][1]-Im*Kinv[1][1]); // /oneOverSqrt2;

////////////////////////////////////
////////////////////


    };   // Comes from: for(j=0; j<nchans[ii]; j++) 


  if(doNorm){ // Norm. factor will be the geometrical average of gains.
    NormFac[0] = std::sqrt(NormFac[0]*NormFac[1])/((float) nchans[ii]);
    fprintf(gainsFile, "%i  %i  %.10e  %.5e\n",ii+1, currAnt, currT/86400.,NormFac[0]);
    for(j=0; j<nchans[ii]; j++){
      Ktotal[currAntIdx][0][0][j] /= NormFac[0];
      Ktotal[currAntIdx][0][1][j] /= NormFac[0];
      Ktotal[currAntIdx][1][0][j] /= NormFac[0];
      Ktotal[currAntIdx][1][1][j] /= NormFac[0];

    };

  };


 }; // Comes from: if(dtchanged||gchanged)



// Calibrate and convert to circular:

// Shall we write in plot file?
     auxB = (currT>=plRange[0] && currT<=plRange[1] && ii == plIF && plAnt == otherAnt);

// Convert:
     DifXData->applyMatrix(Ktotal[currAntIdx],XYSWAP[currAntIdx],auxB,plotFile);

// Write:
     if (!doTest){DifXData->setCurrentMixedVis();};



    };  // All this is done only if currT is within doRange.


   };  // Go to next mixed-vis in this IF.

  


  }; ///////////////////////////////
// End of iteration over IFs
////////////////////////////////////



   fclose(plotFile);
   if(doNorm){fclose(gainsFile);};

// Close data file(s):
   DifXData->finish();

// END OF PROGRAM.
  std::cout << "\n";
  sprintf(message,"\nDONE WITH POLCONVERT!\n");
  fprintf(logFile,message); std::cout << message; fflush(logFile);

  fclose(logFile);
// finished with no errors:
    ret = Py_BuildValue("i",0);
    return ret;

}




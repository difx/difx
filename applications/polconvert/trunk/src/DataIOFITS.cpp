/* DATAIO - FITS-IDI interface to PolConvert

             Copyright (C) 2013  Ivan Marti-Vidal
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



#include <sys/types.h>
#include <iostream> 
#include <fstream>
#include <stdlib.h>  
#include <string.h>
#include <math.h>
#include <dirent.h>
#include "DataIOFITS.h"
#include "fitsio.h"


//#define   NIOBUF = 1;




DataIOFITS::~DataIOFITS() {

};

/*
outputfile is the name of the output VLBI visibility file,
NlinAnt is the number of linear-polarization stations IN THE VLBI observations (i.e., if
only ALMA has linear-pol receivers, NlinAnt = 1).
LinAnt is the list of antenna indices for the linear-polarization stations.
Range is a list with the initial and final modified Julian Dates (in seconds) when the 
corrections are going to be performed.
If Overwrite == true, the output file is overwritten. If not, it expects the output file
to exist already (it will not make a new one).
*/
DataIOFITS::DataIOFITS(std::string outputfile, int NlinAnt, int *LinAnt, double *Range, bool Overwrite, bool doConj, FILE *logF) {


logFile = logF ;

int i;

// Controls whether there is any error in any function:
success = true;

// Status error for CFITSIO:
status = 0;


///////////////////////////////////
// READ METADATA FROM THE PARAMETERS:

NLinAnt = NlinAnt;

doConjugate = doConj;

linAnts = new int[NlinAnt];
for (i=0;i<NlinAnt;i++){
  linAnts[i] = LinAnt[i];
};

currFreq = 0;
currIF = 0;
currBand = 0;
currVis = 0;
jump = 1;
Nentry = 1;

doRange = Range;

currentVis = new std::complex<float>[8];
bufferVis = new std::complex<float>[8];

/////////////////////////////////



////////////////////////////////////
// OPEN FILES, READ DATA, AND FIND OUT
// ALL THE VISIBILITIES TO BE CORRECTED
  readInput(outputfile);
  if (!success){return;};

  openOutFile(outputfile, Overwrite);
  if (!success){return;};
////////////////////////////////////

};



// CLOSE FILE AT END:
void DataIOFITS::finish(){
   fits_close_file(ofile, &status);
 //  char *message;
   if (status){sprintf(message,"\n\nPROBLEM CLOSING FILE!  ERR: %i\n\n",status);
     fprintf(logFile,message); std::cout<<message; fflush(logFile);
   };
};




// FUNTION TO READ THE DATA AND FIND OUT THE MIXED-POL VISIBILITIES
void DataIOFITS::readInput(std::string inputfile) {


// KEYWORDS FOR FITS-IDI FILE:
   char FREQUENCY[] = "FREQUENCY";
   char TOTALBAND[] = "TOTAL_BANDWIDTH";
   char REFFREQ[] = "REF_FREQ";
   char BANDFREQ[] = "BANDFREQ";
   char CHWIDTH[] = "CH_WIDTH";
   char REFPIXL[] = "REF_PIXL";
   char UVDATA[] = "UV_DATA";
   char BASELINE[] = "BASELINE";
   char FREQID[] = "FREQID";
   char SIDEBAND[] = "SIDEBAND";
   char TIME[] = "TIME";
   char DATE[] = "DATE";
   char NOBAND[] = "NO_BAND";
   char NOCHAN[] = "NO_CHAN";
   char ANTENNA[] = "ANTENNA";

   char card[FLEN_CARD]; 
 //  char *message;

   int Nchan, i, ii, jj, kk, ll, auxI;
   int iband, ibfreq, ichw, isb;
   double reffreq, refpix;

// OPEN
   fits_open_file(&fptr, inputfile.c_str(), READONLY, &status);
   if (status){
     sprintf(message,"\n\nPROBLEM OPENING FILE!  ERR: %i\n\n",status);
     fprintf(logFile,message); std::cout<<message; fflush(logFile);
     success=false; return;
   };

// READ NUMBER OF ANTENNAS. ALLOCATE MEMORY FOR ANTENNA INFO.
   long lNant;
   fits_movnam_hdu(fptr, BINARY_TBL, ANTENNA,1, &status);
   if (status){
     sprintf(message,"\n\nPROBLEM READING BINARY TABLE (ANTENNA)!  ERR: %i\n\n",status);
     fprintf(logFile,message); std::cout<<message; fflush(logFile);

     success=false; return;
   };
   fits_get_num_rows(fptr, &lNant, &status);
   Nants = (int) lNant;

// READ FREQUENCY METADATA. ALLOCATE MEMORY.
   fits_movnam_hdu(fptr, BINARY_TBL, FREQUENCY,1, &status);
   if (status){
     sprintf(message,"\n\nPROBLEM READING BINARY TABLE (FREQUENCY)!  ERR: %i\n\n",status);
     fprintf(logFile,message); std::cout<<message; fflush(logFile);

     success=false;return;
   };

   long lNIF;
   fits_get_num_rows(fptr, &lNIF, &status);
   if (status){
     sprintf(message,"\n\nPROBLEM READING NUMBER OF IFs!  ERR: %i\n\n",status);
     fprintf(logFile,message); std::cout<<message; fflush(logFile);

     success=false;return;
   };
   NIFs = (int) lNIF;

   fits_read_key(fptr, TINT, NOBAND, &Nband, card, &status);
   fits_read_key(fptr, TINT, NOCHAN, &Nchan, card, &status);
   fits_read_key(fptr, TDOUBLE, REFFREQ, &reffreq, card, &status);
   fits_read_key(fptr, TDOUBLE, REFPIXL, &refpix, card, &status);
   if (status){
     sprintf(message,"\n\nPROBLEM READING FREQUENCY METADATA I!  ERR: %i\n\n",status);
    fprintf(logFile,message); std::cout<<message; fflush(logFile);

    success=false;return;
   };

   Nfreqs = NIFs*Nband;

   fits_get_colnum(fptr, CASEINSEN, TOTALBAND, &iband, &status);
   fits_get_colnum(fptr, CASEINSEN, BANDFREQ, &ibfreq, &status);
   fits_get_colnum(fptr, CASEINSEN, CHWIDTH, &ichw, &status);
   fits_get_colnum(fptr, CASEINSEN, SIDEBAND, &isb, &status);
   if (status){
    sprintf(message,"\n\nPROBLEM READING FREQUENCY METADATA II!  ERR: %i\n\n",status);
    fprintf(logFile,message); std::cout<<message; fflush(logFile);

    success=false;return;
   };

   Freqs = new FreqSetup[Nfreqs];
   float BW[Nfreqs], chanwidth[Nfreqs];
   double bandfreq[Nfreqs];
   int SB[Nfreqs];


// READ FREQUENCIES:
   for(jj=0;jj<NIFs;jj++){
     fits_read_col(fptr, TFLOAT, iband, jj+1, 1, Nband, NULL, BW, &ii, &status);
     fits_read_col(fptr, TDOUBLE, ibfreq, jj+1, 1, Nband, NULL, bandfreq, &ii, &status);
     fits_read_col(fptr, TFLOAT, ichw, jj+1, 1, Nband, NULL, chanwidth, &ii, &status);
     fits_read_col(fptr, TINT, isb, jj+1, 1, Nband, NULL, SB, &ii, &status);

     if (status){
       sprintf(message,"\n\nPROBLEM READING FREQUENCY INFO!  ERR: %i\n\n",status);
       fprintf(logFile,message); std::cout<<message; fflush(logFile);

       success=false;return;
     };

     for(ii=0;ii<Nband;ii++){
       Freqs[ii+jj*Nband].Nchan = Nchan;
       Freqs[ii+jj*Nband].BW = (double) BW[ii];
       double dchw = (double) chanwidth[ii] ;
       Freqs[ii+jj*Nband].Nu0 = reffreq + bandfreq[ii] - refpix*dchw;
       Freqs[ii+jj*Nband].SB = SB[ii]==1;
       Freqvals[ii] = new double[Nchan];

       if (SB[ii]==1){
         for (auxI = 1; auxI<Nchan+1; auxI++) {
           Freqvals[ii+jj*Nband][auxI-1] = (Freqs[ii+jj*Nband].Nu0 + dchw*((double) auxI)); 
         };
       } else {
         double auxoff = (double) (1+Nchan);
         for (auxI = 1; auxI<Nchan+1; auxI++) {
           Freqvals[ii+jj*Nband][auxI-1] = (Freqs[ii+jj*Nband].Nu0 + auxoff - dchw*((double) auxI)); 
         };
       };
     };
   };



  sprintf(message,"\n Searching for visibilities with mixed (or linear) polarization.\n\n");
  fprintf(logFile,message); std::cout<<message; fflush(logFile);


  fits_movnam_hdu(fptr, BINARY_TBL, UVDATA,1, &status);


  long il;
  int a1, a2;
  fits_get_num_rows(fptr, &Nvis, &status);

  sprintf(message,"There are  %i visibilities.\n",Nvis);
  fprintf(logFile,message); std::cout<<message; fflush(logFile);


// ALLOCATE MEMORY FOR MIXED-POL. METADATA:
  Basels = new int[2*Nvis];
  Freqids = new int[2*Nvis];
  an1 = new int[2*Nvis];
  an2 = new int[2*Nvis];
  indexes = new long[2*Nvis];
  conjugate = new bool[2*Nvis];
  is1 = new bool[2*Nvis];
  is2 = new bool[2*Nvis];
  is1orig = new bool[2*Nvis];
  is2orig = new bool[2*Nvis];
  JDTimes = new double[2*Nvis];
  double *Times = new double[2*Nvis];
  double *Dates = new double[2*Nvis];
  for(il=0;il<Nvis;il++){is1orig[il]=false; is2orig[il]=false;};


// GET COLUMN NUMBERS:
  fits_get_colnum(fptr, CASEINSEN, BASELINE, &ii, &status);
  fits_get_colnum(fptr, CASEINSEN, FREQID, &jj, &status);
  fits_get_colnum(fptr, CASEINSEN, DATE, &kk, &status);
  fits_get_colnum(fptr, CASEINSEN, TIME, &ll, &status);

  if (status){
    sprintf(message,"\n\nPROBLEM READING NUMBERS OF COLUMNS!  ERR: %i\n\n",status);
    fprintf(logFile,message); std::cout<<message; fflush(logFile);

    success=false;return;
  };




// FIND OUT VISIBILITIES!!!

  NLinVis = 0;

  sprintf(message,"Checking the baseline of each visibility.\n"); //,Nvis);
  fprintf(logFile,message); std::cout<<message; fflush(logFile);


  double currT;

//// USE THIS FOR DEBUGGING
//  Nvis = 150000;
//  Nvis = 15000; 
////////

  for (il=0;il<Nvis;il++){

// READ CURRENT VISIBIITY METADATA:
  fits_read_col(fptr, TINT, ii, il+1, 1, 1, NULL, &Basels[il], &auxI, &status);
  fits_read_col(fptr, TINT, jj, il+1, 1, 1, NULL, &Freqids[il], &auxI, &status);
  fits_read_col(fptr, TDOUBLE, kk, il+1, 1, 1, NULL, &Dates[il], &auxI, &status);
  fits_read_col(fptr, TDOUBLE, ll, il+1, 1, 1, NULL, &Times[il], &auxI, &status);
  if (status){
    sprintf(message,"\n\nPROBLEM READING METADATA!  ERR: %i\n\n",status);
    fprintf(logFile,message); std::cout<<message; fflush(logFile);

    success=false;return;
  };

    if (il % 1024 == 0) {
      printf("\r Checking vis. #%i of #%i",il,Nvis);
      fflush(stdout); 
    };

// CURRENT TIME (REFERRED TO FIRST OBSERVING DAY):
    currT = Dates[il] + Times[il] - Dates[0];

// IS THIS VISIBILITY IN THE RIGHT TIME WINDOW?
    if (currT>=doRange[0] && currT<=doRange[1]) {

// ADD-UP VISIILITY TO THE LIST:

    a1 = Basels[il]/256;
    a2 = Basels[il]%256;

    for (i=0;i<NLinAnt;i++){
       if (a1==linAnts[i]){
           an1[NLinVis] = a1;
           an2[NLinVis] = a2;
           indexes[NLinVis] = il;
           conjugate[NLinVis] = true;
           is1orig[NLinVis] = true;
           JDTimes[NLinVis] = ((Times[il]+Dates[il])-2400000.5)*86400.;
           NLinVis += 1;
       };

       if (a2==linAnts[i]){
           an1[NLinVis] = a1;
           an2[NLinVis] = a2;
           indexes[NLinVis] = il;
           conjugate[NLinVis] = false;
           is2orig[NLinVis] = true;
           JDTimes[NLinVis] = ((Times[il]+Dates[il])-2400000.5)*86400.;
           NLinVis += 1;
       };
    };
   }; // Comes from if(currT>...)
  };

// Reference day for AIPS:
day0 = Dates[0];

fits_close_file(fptr, &status);
if (status){
   sprintf(message,"\n\nPROBLEM CLOSING FILE!  ERR: %i\n\n",status);
   fprintf(logFile,message); std::cout<<message; fflush(logFile);

   success=false;return;
};

return;

};








// SET IF TO CHANGE:
bool DataIOFITS::setCurrentIF(int i){

 // char *message;

  if (i>=Nfreqs){
    sprintf(message,"\nERROR! IF %i CANNOT BE FOUND!\n",i+1); 
    fprintf(logFile,message); std::cout<<message; fflush(logFile);

    success=false; return success;
  };

  currFreq = i;
  currIF = i/Nband;
  currBand = i-currIF*Nband;
  currVis = 0;
  jump = 4*((long) Freqs[currFreq].Nchan)*((long) currBand) + 1;
  Nentry = 4*((long) Freqs[currFreq].Nchan);
  delete currentVis ;
  delete bufferVis ;
  currentVis = new std::complex<float>[4*(Freqs[currFreq].Nchan+1)] ;
  bufferVis = new std::complex<float>[4*(Freqs[currFreq].Nchan+1)] ;
  memcpy(is1, is1orig, 2*Nvis*sizeof(bool));
  memcpy(is2, is2orig, 2*Nvis*sizeof(bool));
  return success;
};



// PREPARE OUTPUT FILE. IF OVERWRITE==FALSE, CREATES A NEW FILE 
// WITH ".POLCONVERT" APPENDED TO THE END OF ITS NAME.
void DataIOFITS::openOutFile(std::string outputfile, bool Overwrite) {

  std::ofstream newfits; 
//  char *message;

  if (!Overwrite) {
    std::string newoutput = outputfile + ".POLCONVERT" ;
    std::ofstream oldfits; 
    oldfits.open(outputfile.c_str(), std::ios::in | std::ios::binary);
    newfits.open(newoutput.c_str(), std::ios::out | std::ios::binary);
    newfits << oldfits.rdbuf();
    newfits.close();
    oldfits.close();
    outputfile = newoutput ;
  };

   fits_open_file(&ofile, outputfile.c_str(), READWRITE, &status);
   if (status){
     sprintf(message,"\n\nPROBLEM OPENING NEW FILE!  ERR: %i\n\n",status);
     fprintf(logFile,message); std::cout<<message; fflush(logFile);

     success=false; return;
   };

   fits_movnam_hdu(ofile, BINARY_TBL, "UV_DATA",1, &status);
   fits_get_colnum(ofile, CASEINSEN, "FLUX", &Flux, &status);
   if (status){
     sprintf(message,"\n\nPROBLEM ACCESSING VISIBILITY DATA IN NEW FILE!  ERR: %i\n\n",status);
     fprintf(logFile,message); std::cout<<message; fflush(logFile);

     success=false; return;
   };

   return;

};









// RETURN THE METADATA OF THE NEXT MIXED-POL. VISIBILITY TO CHANGE.
// RETURNS WHETHER IT FOUND DATA (TRUE) OR IF ALL VISIBS. HAVE ALREADY BEEN RETURNED (FALSE) 
bool DataIOFITS::getNextMixedVis(double &JDTime, int &antenna, int &otherAnt, bool &conj) {

 // char *message;
  bool found = false;
  long i,curridx;

  if (NLinVis==0){return false;};

// Pointer to the data to be read:
//   float *cFl = reinterpret_cast<float*>(currentVis);


  while (true) {
    
    curridx = indexes[currVis];
    if (is1[currVis]){
      fits_read_col(ofile, TCOMPLEX, Flux, curridx+1, jump, Nentry, NULL, currentVis, NULL, &status);
      antenna = an1[currVis];
      otherAnt = an2[currVis];
      JDTime = JDTimes[currVis];
      is1[currVis] = false;
      conj = true;
      found = true; break;
    } else if (is2[currVis]){

      fits_read_col(ofile, TCOMPLEX, Flux, curridx+1, jump, Nentry, NULL, currentVis, NULL, &status);
      antenna = an2[currVis];
      otherAnt = an1[currVis];
      JDTime = JDTimes[currVis];
      is2[currVis] = false;
      conj = false;
      found = true; break;
    } else {
      currVis += 1;
    };




    if (currVis == NLinVis){break;};

  };

////////////////////
// FOR SOME REASON, READ_COL RETURNS THE COMPLEX CONJUGATES????
  if(doConjugate){for (i=0; i<Nentry; i++){currentVis[i].imag(-currentVis[i].imag());};};
////////////////////


  if (status){
    sprintf(message,"\n\nPROBLEM ACCESSING VISIBILITY DATA!  ERR: %i\n\n",status);
    fprintf(logFile,message); std::cout<<message; fflush(logFile);

    success=false; return false;
  };
  currConj = conj ;
  return found;

};




// SET THE DATA FOR THE CURRENT VISIBILITY (I.E., THE LAST ONE FOUND BY getNextMixedVis):

bool DataIOFITS::setCurrentMixedVis() {

   long curridx = indexes[currVis];
   long i;
 //  char *message;

////////////////////
// FOR SOME REASON, WRITE_COL RETURNS THE COMPLEX CONJUGATES????
   if(doConjugate){for (i=0; i<Nentry; i++){bufferVis[i].imag(-bufferVis[i].imag());};};
////////////////////


   fits_write_col(ofile, TCOMPLEX, Flux, curridx+1, jump, Nentry, bufferVis, &status); 

   if (status){
     sprintf(message,"\n\nPROBLEM WRITING VISIBILITY DATA!  ERR: %i\n\n",status);
     fprintf(logFile,message); std::cout<<message; fflush(logFile);

     success=false;
   };
   return success;

};







// Convert the data using the corresponding calibration matrix:

void DataIOFITS::applyMatrix(std::complex<float> *M[2][2], bool swap, bool print, FILE *plotFile) {
 
  long k, a11, a12, a21, a22, ca11, ca12, ca21, ca22 ;
  std::complex<float>  auxVis;


     for (k=0; k<Freqs[currFreq].Nchan; k++) {

       if (swap){

        if (currConj) {
          a21 = k*4;
          a12 = a21+1;
          a22 = a21+2;
          a11 = a21+3;
        } else {
          a12 = k*4;
          a21 = a12+1;
          a11 = a12+2;
          a22 = a12+3;
        };

       } else {

         a11 = k*4;
         a22 = a11+1;
         a12 = a11+2;
         a21 = a11+3;

       };


         ca11 = k*4;
         ca22 = ca11+1;
         ca12 = ca11+2;
         ca21 = ca11+3;




       if (currConj) {

         bufferVis[ca11] = M[0][0][k]*currentVis[a11]+M[0][1][k]*currentVis[a21];
         bufferVis[ca12] = M[0][0][k]*currentVis[a12]+M[0][1][k]*currentVis[a22];
         bufferVis[ca21] = M[1][0][k]*currentVis[a11]+M[1][1][k]*currentVis[a21];
         bufferVis[ca22] = M[1][0][k]*currentVis[a12]+M[1][1][k]*currentVis[a22];

       } else {

         bufferVis[ca11] = std::conj(M[0][0][k])*currentVis[a11]+std::conj(M[1][0][k])*currentVis[a12];
         bufferVis[ca12] = std::conj(M[0][1][k])*currentVis[a11]+std::conj(M[1][1][k])*currentVis[a12];
         bufferVis[ca21] = std::conj(M[0][0][k])*currentVis[a21]+std::conj(M[1][0][k])*currentVis[a22];
         bufferVis[ca22] = std::conj(M[0][1][k])*currentVis[a21]+std::conj(M[1][1][k])*currentVis[a22];

       };


   if (print) {
 //    std::cout << currConj << " "<< swap << "\n";

     if (currConj){

     fwrite(&JDTimes[currVis],sizeof(double),1,plotFile);
     fwrite(&currentVis[a11],sizeof(std::complex<float>),1,plotFile);
     fwrite(&currentVis[a12],sizeof(std::complex<float>),1,plotFile);
     fwrite(&currentVis[a21],sizeof(std::complex<float>),1,plotFile);
     fwrite(&currentVis[a22],sizeof(std::complex<float>),1,plotFile);
     fwrite(&bufferVis[ca11],sizeof(std::complex<float>),1,plotFile);
     fwrite(&bufferVis[ca12],sizeof(std::complex<float>),1,plotFile);
     fwrite(&bufferVis[ca21],sizeof(std::complex<float>),1,plotFile);
     fwrite(&bufferVis[ca22],sizeof(std::complex<float>),1,plotFile);
     fwrite(&M[0][0][k],sizeof(std::complex<float>),1,plotFile);
     fwrite(&M[0][1][k],sizeof(std::complex<float>),1,plotFile);
     fwrite(&M[1][0][k],sizeof(std::complex<float>),1,plotFile);
     fwrite(&M[1][1][k],sizeof(std::complex<float>),1,plotFile);
     } else {
     fwrite(&JDTimes[currVis],sizeof(double),1,plotFile);
     auxVis = std::conj(currentVis[a11]);
     fwrite(&auxVis,sizeof(std::complex<float>),1,plotFile);
     auxVis = std::conj(currentVis[a21]);
     fwrite(&auxVis,sizeof(std::complex<float>),1,plotFile);
     auxVis = std::conj(currentVis[a12]);
     fwrite(&auxVis,sizeof(std::complex<float>),1,plotFile);
     auxVis = std::conj(currentVis[a22]);
     fwrite(&auxVis,sizeof(std::complex<float>),1,plotFile);
     auxVis = std::conj(bufferVis[ca11]);
     fwrite(&auxVis,sizeof(std::complex<float>),1,plotFile);
     auxVis = std::conj(bufferVis[ca21]);
     fwrite(&auxVis,sizeof(std::complex<float>),1,plotFile);
     auxVis = std::conj(bufferVis[ca12]);
     fwrite(&auxVis,sizeof(std::complex<float>),1,plotFile);
     auxVis = std::conj(bufferVis[ca22]);
     fwrite(&auxVis,sizeof(std::complex<float>),1,plotFile);
     auxVis = std::conj(M[0][0][k]);
     fwrite(&auxVis,sizeof(std::complex<float>),1,plotFile);
     auxVis = std::conj(M[1][0][k]);
     fwrite(&auxVis,sizeof(std::complex<float>),1,plotFile);
     auxVis = std::conj(M[0][1][k]);
     fwrite(&auxVis,sizeof(std::complex<float>),1,plotFile);
     auxVis = std::conj(M[1][1][k]);
     fwrite(&auxVis,sizeof(std::complex<float>),1,plotFile);
     };

   };
 };

};


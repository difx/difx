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
DataIOFITS::DataIOFITS(std::string outputfile, int NlinAnt, int *LinAnt, double *Range, bool Overwrite, bool doConj, bool doSave, ArrayGeometry *Geom, FILE *logF) {


logFile = logF ;

doWriteCirc = doSave;

int i;

Geometry = Geom;
//BaseLine[0] = BaseLineD[0];
//BaseLine[1] = BaseLineD[1];
//BaseLine[2] = BaseLineD[2];
//AntLon = Longitude;
//TanLat = Latitude;
//BasNum = BasNumD;
//NtotAnt = NtotAntD;
//NtotSou = NtotSouD;

//for (i=0; i<NtotSou; i++) {
// SinDec[i] = sin(DeclinationD[i]);
// CosDec[i] = cos(DeclinationD[i]);
//};

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
jump = 0;
Nentry = 1;
dsize = 2;
doRange = Range;

currentVis = new std::complex<float>[8];
bufferVis = new std::complex<float>[8];
currentData = new float[12];
bufferData = new float[12];

/////////////////////////////////



////////////////////////////////////
// OPEN FILES, READ DATA, AND FIND OUT
// ALL THE VISIBILITIES TO BE CORRECTED
  readInput(outputfile);
  if (!success){return;};

  openOutFile(outputfile, Overwrite);

  if(doWriteCirc){
    sprintf(message,"\n\nSAVING CIRCULAR-BASIS VISIBILITIES INTO AUXILIARY FILES\n");
    fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

    saveCirculars(outputfile);

};

  if (!success){return;};
////////////////////////////////////

};



// CLOSE FILE AT END:
void DataIOFITS::finish(){
   fits_close_file(ofile, &status);
 //  char *message;
   if (status){sprintf(message,"\n\nPROBLEM CLOSING FILE!  ERR: %i\n\n",status);
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
   };
};












void DataIOFITS::saveCirculars(std::string inputfile) {


  int i, j, a1, a2, i3, souidx, auxI;

  double AuxPA1, AuxPA2;

  long il, ii;

  long a11, a12, a21, a22;

  double *UVW;
  UVW = new double[3];
  float *FUVW;
  FUVW = new float[3];

  char UVDATA[] = "UV_DATA";
  char flux[] = "FLUX";


//  fits_open_file(&fptr, inputfile.c_str(), READONLY, &status);
  fits_movnam_hdu(ofile, BINARY_TBL, UVDATA,1, &status);

//  fits_get_colnum(fptr, CASEINSEN, flux, &flid, &status);
   fits_get_colnum(ofile, CASEINSEN, flux, &Flux, &status);


// AUXILIARY BINARY FILES TO STORE CIRCULAR VISIBILITIES:
  FILE **circFile = new FILE*[Nfreqs];


// AUXILIARY MEMORY SPACE TO WRITE CIRCULAR VISIBS:
   delete bufferVis ;
   delete bufferData ;
   bufferVis = new std::complex<float>[4*(Freqs[0].Nchan+1)] ;
   bufferData = new float[12*(Freqs[0].Nchan+1)] ;

// OPEN AUXILIARY BINARY FILES:
  if (doWriteCirc){
    for (ii=0; ii<Nfreqs; ii++){
      sprintf(message,"POLCONVERT.FRINGE/OTHERS.FRINGE_%li",ii+1);
      circFile[ii] = fopen(message,"wb");
      fwrite(&Freqs[ii].Nchan,sizeof(int),1,circFile[ii]);
      fwrite(Freqvals[ii],Freqs[ii].Nchan*sizeof(double),1,circFile[ii]);
    };
  };



   for (ii=0; ii<NVis2Save;ii++){


      il = Vis2Save[ii];

      a1 = Basels[il]/256;
      a2 = Basels[il]%256;

 //     printf("Vis %i of %i (%i) \n",ii,NVis2Save,il);

      fits_read_col(ofile, TFLOAT, uu, il+1, 1, 1, NULL, &FUVW[0], &auxI, &status);
      fits_read_col(ofile, TFLOAT, vv, il+1, 1, 1, NULL, &FUVW[1], &auxI, &status);
      fits_read_col(ofile, TFLOAT, ww, il+1, 1, 1, NULL, &FUVW[2], &auxI, &status);
      fits_read_col(ofile, TINT, ss, il+1, 1, 1, NULL, &souidx, &auxI, &status);

 //     printf("READ! \n");

      UVW[0] = (double) FUVW[0]; UVW[1] = (double) FUVW[1]; UVW[2] = (double) FUVW[2]; 


 //     printf("READ! \n");

      getParAng(souidx-1,a1-1,a2-1,UVW,AuxPA1,AuxPA2);

 //     printf("COMPUTED: %.2e  %.2e \n",AuxPA1,AuxPA2);

      for(i=0;i<Nfreqs;i++) {
        currIF = i/Nband;
        currBand = i-currIF*Nband;
        jump = 4*((long) Freqs[i].Nchan)*((long) currBand);
        Nentry = 4*((long) Freqs[i].Nchan);
        fits_read_col(ofile, TFLOAT, Flux, il+1, dsize*jump+1, dsize*Nentry, NULL, bufferData, NULL, &status);

        for (j=0; j<Nentry; j++){i3 = dsize*j; bufferVis[j].real(bufferData[i3]);bufferVis[j].imag(bufferData[i3+1]); };

       fwrite(&Times[il],sizeof(double),1,circFile[i]);
       fwrite(&a1,sizeof(int),1,circFile[i]);
       fwrite(&a2,sizeof(int),1,circFile[i]);
       fwrite(&AuxPA1,sizeof(double),1,circFile[i]);
       fwrite(&AuxPA2,sizeof(double),1,circFile[i]);

     for (j=0; j<Freqs[i].Nchan; j++){
       a11 = j*4;
       a22 = a11+1;
       a12 = a11+2;
       a21 = a11+3;
       fwrite(&bufferVis[a11],sizeof(std::complex<float>),1,circFile[i]);
       fwrite(&bufferVis[a12],sizeof(std::complex<float>),1,circFile[i]);
       fwrite(&bufferVis[a21],sizeof(std::complex<float>),1,circFile[i]);
       fwrite(&bufferVis[a22],sizeof(std::complex<float>),1,circFile[i]);
     };
    };
  };






// CLOSE AUXILIARY BINARY FILES:
  if (doWriteCirc){
    for (ii=0; ii<Nfreqs; ii++){
      fclose(circFile[ii]); 
    };
  };

  delete[] circFile;


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

   char SOURCE[] = "SOURCE_ID";
   char SOURCE2[] = "SOURCE";

   char UU0[] = "UU";
   char VV0[] = "VV";
   char WW0[] = "WW";
   char UU1[] = "UU---SIN";
   char VV1[] = "VV---SIN";
   char WW1[] = "WW---SIN";
   char UU2[] = "UU---NCP";
   char VV2[] = "VV---NCP";
   char WW2[] = "WW---NCP";

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
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
     success=false; return;
   };

// READ NUMBER OF ANTENNAS. ALLOCATE MEMORY FOR ANTENNA INFO.
   long lNant;
   fits_movnam_hdu(fptr, BINARY_TBL, ANTENNA,1, &status);
   if (status){
     sprintf(message,"\n\nPROBLEM READING BINARY TABLE (ANTENNA)!  ERR: %i\n\n",status);
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

     success=false; return;
   };
   fits_get_num_rows(fptr, &lNant, &status);
   Nants = (int) lNant;

// READ FREQUENCY METADATA. ALLOCATE MEMORY.
   fits_movnam_hdu(fptr, BINARY_TBL, FREQUENCY,1, &status);
   if (status){
     sprintf(message,"\n\nPROBLEM READING BINARY TABLE (FREQUENCY)!  ERR: %i\n\n",status);
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

     success=false;return;
   };

   long lNIF;
   fits_get_num_rows(fptr, &lNIF, &status);
   if (status){
     sprintf(message,"\n\nPROBLEM READING NUMBER OF IFs!  ERR: %i\n\n",status);
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

     success=false;return;
   };
   NIFs = (int) lNIF;

   fits_read_key(fptr, TINT, NOBAND, &Nband, card, &status);
   fits_read_key(fptr, TINT, NOCHAN, &Nchan, card, &status);
   fits_read_key(fptr, TDOUBLE, REFFREQ, &reffreq, card, &status);
   fits_read_key(fptr, TDOUBLE, REFPIXL, &refpix, card, &status);
   if (status){
     sprintf(message,"\n\nPROBLEM READING FREQUENCY METADATA I!  ERR: %i\n\n",status);
    fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

    success=false;return;
   };

   Nfreqs = NIFs*Nband;

   fits_get_colnum(fptr, CASEINSEN, TOTALBAND, &iband, &status);
   fits_get_colnum(fptr, CASEINSEN, BANDFREQ, &ibfreq, &status);
   fits_get_colnum(fptr, CASEINSEN, CHWIDTH, &ichw, &status);
   fits_get_colnum(fptr, CASEINSEN, SIDEBAND, &isb, &status);
   if (status){
    sprintf(message,"\n\nPROBLEM READING FREQUENCY METADATA II!  ERR: %i\n\n",status);
    fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

    success=false;return;
   };

   Freqs = new FreqSetup[Nfreqs];
   float BW[Nfreqs], chanwidth[Nfreqs];
   double bandfreq[Nfreqs];
   int SB[Nfreqs];





// READ FREQUENCIES:
   TotSize = 0;
   for(jj=0;jj<NIFs;jj++){
     fits_read_col(fptr, TFLOAT, iband, jj+1, 1, Nband, NULL, BW, &ii, &status);
     fits_read_col(fptr, TDOUBLE, ibfreq, jj+1, 1, Nband, NULL, bandfreq, &ii, &status);
     fits_read_col(fptr, TFLOAT, ichw, jj+1, 1, Nband, NULL, chanwidth, &ii, &status);
     fits_read_col(fptr, TINT, isb, jj+1, 1, Nband, NULL, SB, &ii, &status);

     if (status){
       sprintf(message,"\n\nPROBLEM READING FREQUENCY INFO!  ERR: %i\n\n",status);
       fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

       success=false;return;
     };

     for(ii=0;ii<Nband;ii++){
       TotSize += Nchan;
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

  TotSize *= 4 ; // 4 Stokes


  sprintf(message,"\n Searching for visibilities with mixed (or linear) polarization.\n\n");
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);


  fits_movnam_hdu(fptr, BINARY_TBL, UVDATA,1, &status);


  long il;
  int a1, a2;
  fits_get_num_rows(fptr, &Nvis, &status);

  sprintf(message,"There are  %li visibilities.\n",Nvis);
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);


// ALLOCATE MEMORY FOR MIXED-POL. METADATA:
  Basels = new int[2*Nvis];
//  Freqids = new int[2*Nvis];
  an1 = new int[2*Nvis];
  an2 = new int[2*Nvis];
  ParAng[0] = new double[2*Nvis];
  ParAng[1] = new double[2*Nvis];
  indexes = new long[2*Nvis];
  conjugate = new bool[2*Nvis];
  is1 = new bool[2*Nvis];
  is2 = new bool[2*Nvis];
  is1orig = new bool[2*Nvis];
  is2orig = new bool[2*Nvis];
  JDTimes = new double[2*Nvis];
  Times = new double[2*Nvis];
  double *Dates = new double[2*Nvis];
//  sour = new int[2*Nvis];
  for(il=0;il<2*Nvis;il++){is1orig[il]=false; is2orig[il]=false;};
  

// GET COLUMN NUMBERS:
  fits_get_colnum(fptr, CASEINSEN, BASELINE, &ii, &status);
  fits_get_colnum(fptr, CASEINSEN, FREQID, &jj, &status);
  fits_get_colnum(fptr, CASEINSEN, DATE, &kk, &status);
  fits_get_colnum(fptr, CASEINSEN, TIME, &ll, &status);
  fits_get_colnum(fptr, CASEINSEN, SOURCE, &ss, &status);

  if(status){
    status = 0;
    fits_get_colnum(fptr, CASEINSEN, SOURCE2, &ss, &status);
  };
//  printf("SIDX: %i  %i\n",ss,status);

  fits_get_colnum(fptr, CASEINSEN, UU0, &uu, &status);
  if (status){
    status = 0;
    fits_get_colnum(fptr, CASEINSEN, UU1, &uu, &status);
    if (status) {
      status = 0;
      fits_get_colnum(fptr, CASEINSEN, UU2, &uu, &status);
      fits_get_colnum(fptr, CASEINSEN, VV2, &vv, &status);
      fits_get_colnum(fptr, CASEINSEN, WW2, &ww, &status);
    } else {
    fits_get_colnum(fptr, CASEINSEN, VV1, &vv, &status);
    fits_get_colnum(fptr, CASEINSEN, WW1, &ww, &status);
    };
  } else {
  fits_get_colnum(fptr, CASEINSEN, VV0, &uu, &status);
  fits_get_colnum(fptr, CASEINSEN, WW0, &uu, &status);
  };

  if (status){
    sprintf(message,"\n\nPROBLEM READING NUMBERS OF COLUMNS!  ERR: %i\n\n",status);
    fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

    success=false; delete[] Dates; return;
  };




// FIND OUT VISIBILITIES!!!

  NLinVis = 0;

  sprintf(message,"Checking the baseline of each visibility.\n"); //,Nvis);
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

  double AuxPA1, AuxPA2;
  double currT;
  double *UVW;
  UVW = new double[3];
  float *FUVW;
  FUVW = new float[3];
  int souidx;
  bool isLinVis;



  Vis2Save = new long[2*Nvis];
  NVis2Save = 0;


  for (il=0;il<Nvis;il++){

  //    printf("\r Checking vis. #%li of #%li",il,Nvis);


// READ CURRENT VISIBIITY METADATA:
  fits_read_col(fptr, TINT, ii, il+1, 1, 1, NULL, &Basels[il], &auxI, &status);

//  printf("\nBASEL %i - %i\n",status,Basels[il]);

  fits_read_col(fptr, TDOUBLE, kk, il+1, 1, 1, NULL, &Dates[il], &auxI, &status);

//  printf("DATE %i\n",status);

  fits_read_col(fptr, TDOUBLE, ll, il+1, 1, 1, NULL, &Times[il], &auxI, &status);

//  printf("TIME %i\n",status);


  if (status){
    sprintf(message,"\n\nPROBLEM READING METADATA!  ERR: %i | row: %li\n\n",status, il);
    fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

    success=false;return;
  };

    if (il % 1024 == 0) {
      printf("\r Checking vis. #%li of #%li",il,Nvis);
      fflush(stdout); 
    };


// CURRENT TIME (REFERRED TO FIRST OBSERVING DAY):
    currT = Dates[il] + Times[il] - Dates[0]; 


// IS THIS VISIBILITY IN THE RIGHT TIME WINDOW?
    if (currT>=doRange[0] && currT<=doRange[1]) {


// Time to MJD:
    Times[il] = ((Times[il]+Dates[il])-2400000.5)*86400.;


// ADD-UP VISIILITY TO THE LIST:

    a1 = Basels[il]/256;
    a2 = Basels[il]%256;

    isLinVis = false;

    for (i=0;i<NLinAnt;i++){
      if(a1==linAnts[i] || a2==linAnts[i]){isLinVis=true; break;};
    };

    if(isLinVis){
       fits_read_col(fptr, TFLOAT, uu, il+1, 1, 1, NULL, &FUVW[0], &auxI, &status);
 //      printf("U %i\n",status);

       fits_read_col(fptr, TFLOAT, vv, il+1, 1, 1, NULL, &FUVW[1], &auxI, &status);
 //      printf("V %i\n",status);

       fits_read_col(fptr, TFLOAT, ww, il+1, 1, 1, NULL, &FUVW[2], &auxI, &status);
 //      printf("W %i\n",status);

       fits_read_col(fptr, TINT, ss, il+1, 1, 1, NULL, &souidx, &auxI, &status);

 //      printf("SOU %i\n",status);

       UVW[0] = (double) FUVW[0]; UVW[1] = (double) FUVW[1]; UVW[2] = (double) FUVW[2]; 

/////// TODO: SORT OUT a1-1 -> a1
       getParAng(souidx-1,a1-1,a2-1,UVW,AuxPA1,AuxPA2);

    } else {

     Vis2Save[NVis2Save] = il;
     NVis2Save += 1;


    };





    if(isLinVis){
           an1[NLinVis] = a1;
           an2[NLinVis] = a2;
           indexes[NLinVis] = il;
           JDTimes[NLinVis] = Times[il]; //((Times[il]+Dates[il])-2400000.5)*86400.;
           ParAng[0][NLinVis] = AuxPA1;
           ParAng[1][NLinVis] = AuxPA2;

       if (a1==linAnts[i]){
           is1orig[NLinVis] = true;
       };

       if (a2==linAnts[i]){
           is2orig[NLinVis] = true;
       };

       NLinVis += 1;

    };
   }; // Comes from if(currT>...)
  };





// Reference day for AIPS:
day0 = Dates[0];

fits_close_file(fptr, &status);
if (status){
   sprintf(message,"\n\nPROBLEM CLOSING FILE!  ERR: %i\n\n",status);
   fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

   success=false;return;
};

delete[] Dates;

return;

};








// SET IF TO CHANGE:
bool DataIOFITS::setCurrentIF(int i){

 // char *message;

  if ( i>=Nfreqs || i<0 ){
    sprintf(message,"\nERROR! IF %i CANNOT BE FOUND!\n",i+1); 
    fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

    success=false; return success;
  };

  currFreq = i;
  currIF = i/Nband;
  currBand = i-currIF*Nband;
  currVis = 0;
  jump = 4*((long) Freqs[currFreq].Nchan)*((long) currBand);
  Nentry = 4*((long) Freqs[currFreq].Nchan);
  delete currentVis ;
  delete bufferVis ;
  delete bufferData ;
  delete currentData ;
  currentVis = new std::complex<float>[4*(Freqs[currFreq].Nchan+1)] ;
  bufferVis = new std::complex<float>[4*(Freqs[currFreq].Nchan+1)] ;

  currentData = new float[12*(Freqs[currFreq].Nchan+1)] ;
  bufferData = new float[12*(Freqs[currFreq].Nchan+1)] ;

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
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

     success=false; return;
   };

   char uv_data[] = "UV_DATA";   // compiler warning
   char flux[] = "FLUX"; // compiler warning
   fits_movnam_hdu(ofile, BINARY_TBL, uv_data,1, &status);
   fits_get_colnum(ofile, CASEINSEN, flux, &Flux, &status);

   long repeat,NFlux;
   int typecode;
   fits_get_coltype(ofile, Flux, &typecode, &NFlux, &repeat, &status);

   dsize = NFlux/TotSize;

   sprintf(message,"\n\n\n   RECORD SIZE: %li ; VIS. SIZE: %li ; There are %li floats per visibility.\n\n",NFlux,TotSize,dsize);
   std::cout<< message;


   if (status){
     sprintf(message,"\n\nPROBLEM ACCESSING VISIBILITY DATA IN NEW FILE!  ERR: %i\n\n",status);
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

     success=false; return;
   };

   return;

};









// RETURN THE METADATA OF THE NEXT MIXED-POL. VISIBILITY TO CHANGE.
// RETURNS WHETHER IT FOUND DATA (TRUE) OR IF ALL VISIBS. HAVE ALREADY BEEN RETURNED (FALSE) 
bool DataIOFITS::getNextMixedVis(double &JDTime, int &antenna, int &otherAnt, bool &conj) {

 // char *message;
  bool found = false;
  long i,curridx, i3;

  if (NLinVis==0){return false;};

// Pointer to the data to be read:
//   float *cFl = reinterpret_cast<float*>(currentVis);


  while (true) {
    
    curridx = indexes[currVis];
    if (is1[currVis]){
      fits_read_col(ofile, TFLOAT, Flux, curridx+1, dsize*jump+1, dsize*Nentry, NULL, currentData, NULL, &status);
      antenna = an1[currVis];
      otherAnt = an2[currVis];
      JDTime = JDTimes[currVis];
      is1[currVis] = false;
      conj = true;
      found = true; 
      for (i=0; i<Nentry; i++){i3 = dsize*i; currentVis[i].real(currentData[i3]);currentVis[i].imag(currentData[i3+1]); };


  //   sprintf(message,"VISIBS: %.5e %.5e %.5e \n",currentData[30+0],currentData[30+1],currentData[30+2]);
   // fprintf(logFile,"%s",message); 
  //    std::cout<<message; // fflush(logFile);

      break;


    } else if (is2[currVis]){

      fits_read_col(ofile, TFLOAT, Flux, curridx+1, dsize*jump+1, dsize*Nentry, NULL, currentData, NULL, &status);
      antenna = an2[currVis];
      otherAnt = an1[currVis];
      JDTime = JDTimes[currVis];
      is2[currVis] = false;
      conj = false;
      found = true;
      for (i=0; i<Nentry; i++){i3 = dsize*i; currentVis[i].real(currentData[i3]); currentVis[i].imag(currentData[i3+1]); };

  //   sprintf(message,"VISIBS: %.3e %.3e %.3e \n",currentData[30+0],currentData[30+1],currentData[30+2]);
   // fprintf(logFile,"%s",message); 
   //   std::cout<<message; // fflush(logFile);

      break;

    } else {
      currVis += 1;
    };



    if (currVis == NLinVis){break;};

  };

////////////////////
// FOR SOME REASON, READ_COL RETURNS THE COMPLEX CONJUGATES????
  if(doConjugate){for (i=0; i<Nentry; i++){currentVis[i].imag(-currentVis[i].imag());};};
////////////////////

  if (found && (is1[currVis] || is2[currVis])){canPlot=false;} else {canPlot=true;};


  if (status){
    sprintf(message,"\n\nPROBLEM ACCESSING VISIBILITY DATA!  ERR: %i\n\n",status);
    fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

    success=false; return false;
  };
  currConj = conj ;
  return found;

};




// SET THE DATA FOR THE CURRENT VISIBILITY (I.E., THE LAST ONE FOUND BY getNextMixedVis):

bool DataIOFITS::setCurrentMixedVis() {

   long curridx = indexes[currVis];
   long i, i3;
 //  char *message;

////////////////////
// FOR SOME REASON, WRITE_COL RETURNS THE COMPLEX CONJUGATES????
   if(doConjugate){for (i=0; i<Nentry; i++){bufferVis[i].imag(-bufferVis[i].imag());};};

   for (i=0; i<Nentry; i++){i3=dsize*i; currentData[i3]=bufferVis[i].real(); currentData[i3+1]=bufferVis[i].imag();};
////////////////////


   fits_write_col(ofile, TFLOAT, Flux, curridx+1, dsize*jump+1, dsize*Nentry, currentData, &status); 

   if (status){
     sprintf(message,"\n\nPROBLEM WRITING VISIBILITY DATA!  ERR: %i\n\n",status);
     fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

     success=false;
   };
   return success;

};







// Convert the data using the corresponding calibration matrix:

void DataIOFITS::applyMatrix(std::complex<float> *M[2][2], bool swap, bool print, int thisAnt, FILE *plotFile) {
 
  long k, a11, a12, a21, a22, ca11, ca12, ca21, ca22 ;
  std::complex<float>  auxVis;


     for (k=0; k<Freqs[currFreq].Nchan; k++) {


         a11 = k*4;
         a22 = a11+1;
         a12 = a11+2;
         a21 = a11+3;



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

         bufferVis[ca11] = std::conj(M[0][0][k])*currentVis[a11]+std::conj(M[0][1][k])*currentVis[a12];
         bufferVis[ca12] = std::conj(M[1][0][k])*currentVis[a11]+std::conj(M[1][1][k])*currentVis[a12];
         bufferVis[ca21] = std::conj(M[0][0][k])*currentVis[a21]+std::conj(M[0][1][k])*currentVis[a22];
         bufferVis[ca22] = std::conj(M[1][0][k])*currentVis[a21]+std::conj(M[1][1][k])*currentVis[a22];

       };


   if (print && canPlot) {

     if (currConj){
     if (k==0){
       fwrite(&JDTimes[currVis],sizeof(double),1,plotFile);
       fwrite(&an1[currVis],sizeof(int),1,plotFile);
       fwrite(&an2[currVis],sizeof(int),1,plotFile);
       fwrite(&ParAng[0][currVis],sizeof(double),1,plotFile);
       fwrite(&ParAng[1][currVis],sizeof(double),1,plotFile);
     };
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
     if(k==0){
       fwrite(&JDTimes[currVis],sizeof(double),1,plotFile);
       fwrite(&an2[currVis],sizeof(int),1,plotFile);
       fwrite(&an1[currVis],sizeof(int),1,plotFile);
       fwrite(&ParAng[1][currVis],sizeof(double),1,plotFile);
       fwrite(&ParAng[0][currVis],sizeof(double),1,plotFile);
     };
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


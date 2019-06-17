/* Copyright (C) 2013  Nordic Node of EU ALMA Regional Center (Onsala, Sweden)
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
#include "./DataIOSWIN.h"





DataIOSWIN::~DataIOSWIN() {

  int i, j; 
  
  free(Records);
  free(ParAng[0]);
  free(ParAng[1]);
  free(is1orig);
  free(is2orig);
  free(is1);
  free(is2);
  for (i=0; i<nautos; i++){delete[] AutoCorrs[i].AC;};
  free(AutoCorrs);
  
  delete[] linAnts;
  delete[] NAV;
  delete[] Freqs;
  delete[] is1;
  delete[] is2;

  for (i=0; i<4; i++){
    delete[] currentVis[i];
    delete[] bufferVis[i];
    delete[] auxVis[i];
  };

  

  for(i=0; i<NLinAnt;i++){
    for(j=0; j<Nfreqs; j++){delete[] averAutocorrs[i][j];};
      delete[] averAutocorrs[i];
  };
  delete[] averAutocorrs; 

  for(i=0;i<Nfreqs;i++){delete[] Freqvals[i];};
  
  
};


DataIOSWIN::DataIOSWIN(int nfiledifx, std::string* difxfiles, int NlinAnt, int *LinAnt, double *Range, int nIF, int *nChan, int *NchanAC, double **FreqVal, bool Overwrite, bool doTest, bool doSolve, int saveSource, double jd0, ArrayGeometry *Geom, FILE *logF) {


doWriteCirc = doSolve;
nfiles = nfiledifx;
int i, j;


//char *message;

logFile = logF;

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

isTwoLinear = false;
success = true;
NLinAnt = NlinAnt;
linAnts = new int[NlinAnt];

NAV = new int[NlinAnt];

for (i=0;i<NlinAnt;i++){
  linAnts[i] = LinAnt[i];
  NAV[i] = NchanAC[i];
};

AutoCorrs = (AutoCorrelation *) malloc(RECBUFFER*sizeof(AutoCorrelation));
Records = (Record *) malloc(RECBUFFER*sizeof(Record)); // new Record[RECBUFFER];
is1orig =  (bool *) malloc(RECBUFFER*sizeof(bool)); //new bool[RECBUFFER];
is2orig = (bool *) malloc(RECBUFFER*sizeof(bool));  //new bool[RECBUFFER];
ParAng[0] = (double *) malloc(RECBUFFER*sizeof(double));  //new double[RECBUFFER];
ParAng[1] = (double *) malloc(RECBUFFER*sizeof(double));  //new double[RECBUFFER];
//is1 = (bool *) malloc(RECBUFFER*sizeof(bool));
//is2 = (bool *) malloc(RECBUFFER*sizeof(bool));

is1 = new bool[RECBUFFER];
is2 = new bool[RECBUFFER];

day0 = jd0 ;

currFreq = 0;
currVis = 0;

doRange = Range;

isAutoCorr = false;


// READ FREQUENCIES FOR ALL IFs:
Nfreqs = nIF;
int MaxNChan = 0;
Freqs = new FreqSetup[Nfreqs];
   for(i=0;i<nIF;i++){
     if (nChan[i]>MaxNChan){MaxNChan = nChan[i];};
     Freqvals[i] = new double[nChan[i]];
     Freqs[i].Nchan = nChan[i];
     Freqs[i].BW = FreqVal[i][nChan[i]-1] - FreqVal[i][0];
     Freqs[i].Nu0 = FreqVal[i][0];
     Freqs[i].SB = 0;
     memcpy(Freqvals[i], FreqVal[i],nChan[i]*sizeof(double)); 
   };


for (i=0; i<4; i++){
  currentVis[i] = new std::complex<float>[MaxNChan+1];
  bufferVis[i] = new std::complex<float>[MaxNChan+1];
  auxVis[i] = new std::complex<float>[MaxNChan+1];
}

  isOverWrite = Overwrite ;

  openOutFiles(difxfiles);
  readHeader(doTest,saveSource);

  
//  Prepare memory for average autocorrs:
  averAutocorrs = new float**[NLinAnt];
  for(i=0; i<NLinAnt;i++){
    averAutocorrs[i] = new float*[Nfreqs];
    for(j=0; j<Nfreqs; j++){averAutocorrs[i][j] = new float[Freqs[j].Nchan];};    
  };
  averageAutocorrs();
  
};






double getMedian(double *X, int n0, int N){

  int i,j,Nmed;
  double Aux;

  if(N==1){return X[n0];};

  double *Sorted = new double[N];
  for(i=0; i<N; i++){Sorted[i] = X[i+n0];};

  for(i=0;i<N-1;i++){
    for(j=i+1; j<N; j++){
      if(Sorted[i]>Sorted[j]){Aux = Sorted[i]; Sorted[i]=Sorted[j]; Sorted[j] = Aux;};
    };	    
  };	  

  if(N%2){
    Nmed = N/2 -1; 
    return (Sorted[Nmed] + Sorted[Nmed+1])/2.;
  } else {
    Nmed = (N+1)/2 -1;
    return Sorted[Nmed];
  };	  

};	




void DataIOSWIN::averageAutocorrs(){
  
   int i, j, k,l, l2, NHf;
   double Nx, Ny;
   double *TempX, *TempY;
   
   for(i=0;i<NLinAnt;i++){    
	   
     for(j=0; j<Nfreqs; j++){	    
	     
       TempX = new double[Freqs[j].Nchan]; 
       TempY = new double[Freqs[j].Nchan]; 
       for(k=0; k<Freqs[j].Nchan; k++){
	 TempX[k] = 0.0; TempY[k] = 0.0;      
       };
       Nx = 0. ; Ny = 0.;
       for(k=0; k<nautos;k++){
         if(AutoCorrs[k].AntIdx == i && AutoCorrs[k].IF == j){
           if (AutoCorrs[k].Pol == 1){
	      Nx += 1.;	   
              for(l=0; l<Freqs[j].Nchan; l++){		      
		 TempX[l] += AutoCorrs[k].AC[l]; 
	      };
	   };    
           if (AutoCorrs[k].Pol == 2){
              Ny += 1.;		   
              for(l=0; l<Freqs[j].Nchan; l++){		   	    
		TempY[l] += AutoCorrs[k].AC[l]; 
	      };
	   };   
         
	 };
                   
       };
       
       if(NAV[i]>0 && Nx > 0. && Ny > 0.){
       	   NHf = NAV[i]/2;

           for(l=0; l<Freqs[j].Nchan; l++){TempX[l] = std::sqrt(TempY[l]/TempX[l]*Nx/Ny);};
	   for(l=0; l< NHf; l++){
	     averAutocorrs[i][j][l] = getMedian(TempX,0,NAV[i]);
             l2 = Freqs[j].Nchan - l -1;
	     averAutocorrs[i][j][l2] = getMedian(TempX,Freqs[j].Nchan-NAV[i]-1,NAV[i]);
	   };

	   for(l=NHf; l< Freqs[j].Nchan - NHf; l++){
	     l2 = l - NHf;   
	     averAutocorrs[i][j][l] = getMedian(TempX,l2,NAV[i]);
	   };
       //    averAutocorrs[i][j] = (float) std::sqrt(TempY/TempX*Nx/Ny);  // GOOD
       //    averAutocorrs[i][j] = (float) std::sqrt(TempX/TempY*Ny/Nx);  // FOR TESTING
       } else {
           for(l=0; l<Freqs[j].Nchan; l++){averAutocorrs[i][j][l] = 1.0;};
       };

       delete[] TempX;
       delete[] TempY;
     };    
   };
    
};











void DataIOSWIN::finish(){

  int auxI;
  for (auxI=0; auxI<nfiles; auxI++) {
     newdifx[auxI].close();
     if (!isOverWrite){olddifx[auxI].close();};
  };

};






void DataIOSWIN::openOutFiles(std::string* difxfiles) {

  std::string SEP = "NEW/";
  int auxI;

//  char *message;

  olddifx = new std::ifstream[nfiles];
  newdifx = new std::fstream[nfiles];

  long begin, end;
  filesizes = new long[nfiles];

  for (auxI=0; auxI<nfiles; auxI++) {

   olddifx[auxI].open((difxfiles[auxI]).c_str(), std::ios::in | std::ios::binary);

// Get file size:
   begin = olddifx[auxI].tellg();
   olddifx[auxI].seekg(0, olddifx[auxI].end);
   end = olddifx[auxI].tellg();
   filesizes[auxI] = end - begin;
   olddifx[auxI].clear();


   if (!isOverWrite) {
     newdifx[auxI].open((SEP+difxfiles[auxI]).c_str(), std::ios::out | std::ios::binary | std::ios::in);
     newdifx[auxI] << olddifx[auxI].rdbuf();
     newdifx[auxI].close();
     newdifx[auxI].open((SEP+difxfiles[auxI]).c_str(), std::ios::out | std::ios::binary | std::ios::in);
     olddifx[auxI].clear();
   } else {
     newdifx[auxI].open((difxfiles[auxI]).c_str(), std::ios::out | std::ios::binary | std::ios::in);
   };

 };

};



// SET IF TO CHANGE:
bool DataIOSWIN::setCurrentIF(int i){

//  char *message;

  if ( i>=Nfreqs || i<0 ){success=false; 
      sprintf(message,"\nERROR! IF %i CANNOT BE FOUND!\n",i+1); 
      fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

      success=false; return success;
  };
  currFreq = i;
  currVis = 0;
  memcpy(is1, is1orig, nrec*sizeof(bool));
  memcpy(is2, is2orig, nrec*sizeof(bool));
  return success;
};





void DataIOSWIN::readHeader(bool doTest, int saveSource) {

//  char *message;
  long loc, beg, end, polpos;
  int basel, fridx, cfidx, mjd, sidx, ii, jj;
  double secs, daytemp, daytemp2;
  double *UVW = new double[3];
  int UVWsize = 3*sizeof(double);
  char *pol = new char[2];
//  char *auxC = new char[4];

  double AuxPA1, AuxPA2;

// AUXILIARY BINARY FILES TO STORE CIRCULAR VISIBILITIES:
  FILE **circFile = new FILE*[Nfreqs];

// AUXILIARY BINARY FILES TO STORE AUTO-CORRELATIONS:
  
  FILE *autoCorrs = new FILE;

//  for (ii=0; ii<Nfreqs; ii++){
  sprintf(message,"POLCONVERT.FRINGE/AUTOCORRS.dat");
  autoCorrs = fopen(message,"wb");
//  };
  
  
// OPEN AUXILIARY BINARY FILES:
  if (doWriteCirc){
    for (ii=0; ii<Nfreqs; ii++){
      sprintf(message,"POLCONVERT.FRINGE/OTHERS.FRINGE_%i",ii+1);
      circFile[ii] = fopen(message,"wb");
      fwrite(&Freqs[ii].Nchan,sizeof(int),1,circFile[ii]);
      for (jj=0;jj<Freqs[ii].Nchan;jj++){
        fwrite(&Freqvals[ii][jj],sizeof(double),1,circFile[ii]);
      };
    };
  };


  success = true;


  free(AutoCorrs);
  free(Records);
  free(ParAng[0]);
  free(ParAng[1]);
  free(is1orig);
  free(is2orig);
  delete is1;
  delete is2;

  
  
  AutoCorrs = (AutoCorrelation *) malloc(RECBUFFER*sizeof(AutoCorrelation));
  Records = (Record *) malloc(RECBUFFER*sizeof(Record)); // new Record[RECBUFFER];
  is1orig =  (bool *) malloc(RECBUFFER*sizeof(bool)); //new bool[RECBUFFER];
  is2orig = (bool *) malloc(RECBUFFER*sizeof(bool));  //new bool[RECBUFFER];
  ParAng[0] = (double *) malloc(RECBUFFER*sizeof(double));  //new double[RECBUFFER];
  ParAng[1] = (double *) malloc(RECBUFFER*sizeof(double));  //new double[RECBUFFER];
//  is1 = (bool *) malloc(RECBUFFER*sizeof(bool));
//  is2 = (bool *) malloc(RECBUFFER*sizeof(bool));





  nautos = 0;
  nrec = 0;
  long CURRSIZE = RECBUFFER;
  long AUTOSIZE = RECBUFFER;
  int ant1, ant2, auxI, auxJ;
  double auxD;
//Assume binary index (i.e., DiFX version >= 2.0):
  sprintf(message,"\nThere are %i IFs.",Nfreqs);
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

  sprintf(message,"\n\n Searching for visibilities with mixed (or linear) polarization.\n\n");
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

  long bperp, cp;


  long RecordSize = 5*sizeof(int) + sizeof(double) + 2*sizeof(char) + endhead;


 for (auxI=0; auxI<nfiles; auxI++) {

  loc = 8;

// Bits per percentage:
  bperp = filesizes[auxI]/100; if(bperp==0){bperp=1;};

  sprintf(message,"\n\nReading file %i of %i (size %li MB)\n",auxI+1,nfiles,filesizes[auxI]/(1024*1024));
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);

  cp = 0;

  while(!newdifx[auxI].eof()) {


//   if(loc/bperp > cp){cp += 1; printf("\r  %li%% DONE",cp);fflush(stdout);};

   newdifx[auxI].seekg(loc,newdifx[auxI].beg);
   newdifx[auxI].read(reinterpret_cast<char*>(&basel), sizeof(int));
   newdifx[auxI].read(reinterpret_cast<char*>(&mjd), sizeof(int));
   newdifx[auxI].read(reinterpret_cast<char*>(&secs), sizeof(double));
   newdifx[auxI].read(reinterpret_cast<char*>(&cfidx), sizeof(int));
   newdifx[auxI].read(reinterpret_cast<char*>(&sidx), sizeof(int));
   newdifx[auxI].read(reinterpret_cast<char*>(&fridx), sizeof(int));
   polpos = newdifx[auxI].tellg();
   newdifx[auxI].read(pol, 2*sizeof(char));

   newdifx[auxI].ignore(sizeof(int)+sizeof(double)); // Pulsar bin + Weight

   newdifx[auxI].read(reinterpret_cast<char*>(UVW), UVWsize);




////////////////
// WHAT IS THE DIFFERENCE BETWEEN CFIDX AND FRIDX !!!!!!!!
////////////////

  beg = newdifx[auxI].tellg(); 
  if (beg>0) {
 // beg += sizeof(int) + sizeof(double);
  end = beg + (Freqs[fridx].Nchan)*sizeof(cplx32f);
  loc = end + 2*sizeof(int); // Control word and header version



// RE-ALLOCATE MEMORY IF BUFFER IS FULL:
  if (nrec == CURRSIZE) {
    CURRSIZE += RECBUFFER; 
    Records = (Record*) realloc(Records, CURRSIZE*sizeof(Record));
    if(!Records){success = false; goto FREE;};
    is1orig = (bool*) realloc(is1orig, CURRSIZE*sizeof(bool));
    if(!is1orig){success = false; goto FREE;};
    is2orig = (bool*) realloc(is2orig, CURRSIZE*sizeof(bool));
    if(!is2orig){success = false; goto FREE;};
    ParAng[0] = (double *) realloc(ParAng[0], CURRSIZE*sizeof(double));
    ParAng[1] = (double *) realloc(ParAng[1], CURRSIZE*sizeof(double));
    if(!ParAng[0] || !ParAng[1]){success = false; goto FREE;};
  };

// Check if we are in the time window:
    secs /= 86400.;
    daytemp = ((double) mjd) + secs - day0;

    if(daytemp>=doRange[0] && daytemp<=doRange[1]){


// Check if a linear-feed antenna is in the baseline:
  ant1 = basel / 256;
  ant2 = basel % 256;
  is1orig[nrec] = false ; is2orig[nrec] = false;
  for (auxJ=0;auxJ<NLinAnt;auxJ++){
    if(ant1 == linAnts[auxJ]){
       is1orig[nrec] = true;
       if (pol[0] == 'R' || pol[0]=='X'){pol[0] = 'X';} else {pol[0] = 'Y';};
    };
    if(ant2 == linAnts[auxJ]){
       is2orig[nrec] = true;
       if (pol[1] == 'R' || pol[1]=='X'){pol[1] = 'X';} else {pol[1] = 'Y';};
    };
  };


  
  
// Derive the parallactic angles:
  getParAng(sidx,ant1-1,ant2-1,UVW,AuxPA1,AuxPA2);
  daytemp2 = (daytemp + day0)*86400.;

  
  
// Read auto-correlations:
  if (ant1==ant2){
  //    printf("FOUND AUTOCORR: %i %i %c %c %.1f\n",ant1,fridx,pol[0],pol[1],daytemp);
      auxD = 0.0;
      newdifx[auxI].seekg(beg,newdifx[auxI].beg);
      newdifx[auxI].read(reinterpret_cast<char*>(currentVis[0]),end-beg);

   //   for (auxJ=1; auxJ<Freqs[fridx].Nchan-1; auxJ++){
   //     auxD += std::abs(currentVis[0][auxJ]);
   //   };
      
      auxJ = -1;
      if( (pol[0]=='R' || pol[0]=='X') && (pol[1]=='R' || pol[1]=='X')){auxJ=1;};
      if( (pol[0]=='L' || pol[0]=='Y') && (pol[1]=='L' || pol[1]=='Y')){auxJ=2;};
      
      if (auxJ>0){
    //    auxD /= (double(Freqs[fridx].Nchan)-2.);
        AutoCorrs[nautos].AntIdx = ant1-1;
        AutoCorrs[nautos].IF = fridx;
        AutoCorrs[nautos].JD = daytemp;
        AutoCorrs[nautos].Pol = auxJ;
        AutoCorrs[nautos].AC = new double[Freqs[fridx].Nchan];
        for (auxJ=1; auxJ<Freqs[fridx].Nchan-1; auxJ++){
          AutoCorrs[nautos].AC[auxJ] = std::abs(currentVis[0][auxJ]);
        };

     //   AutoCorrs[nautos].AC = auxD;        

	nautos += 1;

        if (nautos==AUTOSIZE){
          AUTOSIZE += RECBUFFER; 
          AutoCorrs = (AutoCorrelation*) realloc(AutoCorrs, AUTOSIZE*sizeof(AutoCorrelation));
        };
        
        fwrite(&ant1,sizeof(int),1,autoCorrs);
        fwrite(&auxJ,sizeof(int),1,autoCorrs);
        fwrite(&fridx,sizeof(int),1,autoCorrs);
        fwrite(&daytemp,sizeof(double),1,autoCorrs);
        fwrite(&auxD,sizeof(double),1,autoCorrs);
      };
  
  };
      
  
  
  
//  if(ant1!=ant2){
//  printf("SOU %i:  |  T: %.2f  |  PA1: %.3f  |  PA2: %.3f\n",sidx,daytemp2,AuxPA1*180./3.1416,AuxPA2*180./3.1416);};

// Write circular visibilities (assume standard pol. ordering):
  if ((saveSource<0 || sidx == saveSource) && doWriteCirc && (!is1orig[nrec] && !is2orig[nrec]) && (pol[0] == 'R' || pol[0]=='X') && (pol[1] == 'R' || pol[1]=='X')){


    for (auxJ=0; auxJ<4; auxJ++){    
      newdifx[auxI].seekg(beg + (RecordSize + (Freqs[fridx].Nchan)*sizeof(cplx32f))*auxJ, newdifx[auxI].beg);
     // newdifxR[auxI].sync();
      newdifx[auxI].read(reinterpret_cast<char*>(currentVis[auxJ]),end-beg);

    };

    fwrite(&daytemp,sizeof(double),1,circFile[fridx]);
    fwrite(&ant1,sizeof(int),1,circFile[fridx]);
    fwrite(&ant2,sizeof(int),1,circFile[fridx]);
    fwrite(&AuxPA1,sizeof(double),1,circFile[fridx]);
    fwrite(&AuxPA2,sizeof(double),1,circFile[fridx]);

 //   printf("\nDayTemp2: %.2f",daytemp2);

    for (auxJ=0;auxJ<Freqs[fridx].Nchan;auxJ++){
     fwrite(&currentVis[0][auxJ],sizeof(std::complex<float>),1,circFile[fridx]);
     fwrite(&currentVis[2][auxJ],sizeof(std::complex<float>),1,circFile[fridx]);
     fwrite(&currentVis[3][auxJ],sizeof(std::complex<float>),1,circFile[fridx]);
     fwrite(&currentVis[1][auxJ],sizeof(std::complex<float>),1,circFile[fridx]);
   };

  };


// Read entry metadata:

  if (is1orig[nrec] || is2orig[nrec]) {


     Records[nrec].Baseline = basel;

     Records[nrec].Source = sidx;
     Records[nrec].fileNumber = auxI;
     Records[nrec].Time = daytemp2; 
     

     Records[nrec].notUsed = true;
     Records[nrec].Antennas[0] = ant1;
     Records[nrec].Antennas[1] = ant2;
     Records[nrec].byteIni = beg;
     Records[nrec].byteEnd = end;
     Records[nrec].Pol[0] = pol[0];
     Records[nrec].Pol[1] = pol[1];

// Derive the parallactic angles:
    ParAng[0][nrec] = AuxPA1 ; ParAng[1][nrec] = AuxPA2;
 

/////////
// Overwrite pol label entry in SWIN file:
     if(!doTest){
       if(pol[0]=='X'){pol[0]='R';} else if(pol[0]=='Y'){pol[0]='L';};
       if(pol[1]=='X'){pol[1]='R';} else if(pol[1]=='Y'){pol[1]='L';};
       newdifx[auxI].seekp(polpos, newdifx[auxI].beg);
       newdifx[auxI].write(reinterpret_cast<char*>(pol),2*sizeof(char));
     };
/////////

     Records[nrec].freqIndex = fridx;
     nrec ++;

    };
  };

  };

  };



// Rewind:
//  newdifxW[auxI].clear();
//  newdifxW[auxI].seekg(0,newdifxW[auxI].beg);
  newdifx[auxI].clear();
  newdifx[auxI].seekg(0,newdifx[auxI].beg);




 };


// day0 is JD (not MJD):
day0 += 2400000.5 ;

FREE:
if (!success){
  free(Records);
  free(ParAng[0]);
  free(ParAng[1]);
  free(is1orig);
  free(is2orig);
  free(is1);
  free(is2);
  Records = NULL; ParAng[0]=NULL; ParAng[1]=NULL;
  is1orig=NULL; is2orig=NULL;
};


// CLOSE AUXILIARY BINARY FILES:
  if (doWriteCirc){
    for (ii=0; ii<Nfreqs; ii++){
      fclose(circFile[ii]); 
    };
  };

  fclose(autoCorrs);

  
  delete[] circFile;
//  delete autoCorrs;



if (nrec==0) {
  sprintf(message,"\n NO VALID DATA FOUND!"); 
  fprintf(logFile,"%s",message); std::cout<<message; fflush(logFile);
  success = false;
} else {
// Allocate memory for booleans:
  NLinVis = nrec/4;
  is1 = new bool[nrec];
  is2 = new bool[nrec];
};

delete[] pol;

};






bool DataIOSWIN::getNextMixedVis(double &JDTime, int &antenna, int &otherAnt, bool &conj, int &calField) {


//char *message;
long rec, rec1, k;
int basel, idx, fnum, field;
double time;
long indices[4];
bool complete;

//  printf("\n NEW VISIB! \n");

  if (NLinVis==0){return false;};

///////////////////
// BEWARE WHETHER currFreq IS ZERO-BASED!!!!!
///////////////////


// Find the four correlation products:

canPlot=false;

while(true){

idx = 0;
for (rec=0; rec<nrec; rec++) {
  if (Records[rec].notUsed && Records[rec].freqIndex==currFreq) {
     indices[idx] = rec;
     complete = !(is1[rec] && is2[rec]) ; 

     if(!complete){isTwoLinear=true;};

     if (complete){
       canPlot = true;
       Records[rec].notUsed = false;};
     idx ++;
     basel = Records[rec].Baseline;
     time = Records[rec].Time;
     field = Records[rec].Source;
     currVis = rec;
     for (rec1=rec+1; rec1<nrec; rec1++) {
       if (Records[rec1].Baseline==basel && Records[rec1].Time==time && Records[rec1].freqIndex==currFreq) {
          indices[idx] = rec1; idx ++;
          if (complete){
            Records[rec1].notUsed = false;};
          if (idx==4) {break;};
       };
     }; break;
  };
};


// If no more mixed-pol visibilities are found, we return false:
// If not all the 4 products are found, report a warning:
if (idx==0) {return false;}
else if (idx <4) {

 sprintf(message,"WARNING: Missing: Baseline: %08x - Time: %f sec: ",
    basel,time - Records[0].Time); 
 fprintf(logFile,"%s",message); fflush(logFile); // std::cout<<message;


 for (rec=0; rec<idx; rec++) {
   sprintf(message,"%c%c ",Records[indices[rec]].Pol[0],Records[indices[rec]].Pol[1]);
   fprintf(logFile,"%s",message); fflush(logFile);// std::cout<<message;
   fflush(logFile);
 };

 if(idx==2){indices[2] = -1; indices[3] = -1; break;};
 if(idx==1){indices[1] = -1; indices[2] = -1; indices[3] = -1; 
   sprintf(message,"\n ERROR! ONLY ONE LINEAR POLARIZATION CHANNEL WILL NOT WORK!!");
   fprintf(logFile,"%s",message); fflush(logFile);// std::cout<<message;
   fflush(logFile);
  break;};


} else {
 break;
};




};




// Classify the correlation products:
if (is1[indices[0]]){is1[indices[0]]=false; conj = true;} else {is2[indices[0]]=false; conj=false;};

//for (rec=1; rec<idx; rec++) {
//  is1[indices[rec]] = is1[indices[0]];
//  is2[indices[rec]] = is2[indices[0]];
//};


if (idx<4){
  sprintf(message," CONJ: %i \n",conj);
  fprintf(logFile,"%s",message); fflush(logFile);
};



int i = conj?0:1;
antenna = conj?Records[indices[0]].Antennas[0]:Records[indices[0]].Antennas[1];
otherAnt = conj?Records[indices[0]].Antennas[1]:Records[indices[0]].Antennas[0];


char p1, p2;
currEntries[currFreq][0] = -1;
currEntries[currFreq][1] = -1;
currEntries[currFreq][2] = -1;
currEntries[currFreq][3] = -1;

for (rec = 0; rec < 4; rec++) {

 if (indices[rec]>0){

  p1 = Records[indices[rec]].Pol[i];
  p2 = Records[indices[rec]].Pol[1-i];

// All pol. products must have consistent booleans (for sanity)
  is1[indices[rec]] = is1[indices[0]];
  is2[indices[rec]] = is2[indices[0]];


  if ((p1=='X' || p1=='R') && (p2=='R' || p2=='X')) {
     currEntries[currFreq][0] = indices[rec];
  }
  else if ((p1=='Y'||p1=='L') && (p2=='R' || p2=='X')) {
     currEntries[currFreq][3] = indices[rec];
  }
  else if ((p1=='X'||p1=='R') && (p2=='L' || p2=='Y')) {
     currEntries[currFreq][2] = indices[rec];
  }
  else if ((p1=='Y'||p1=='L') && (p2=='L' || p2=='Y')) {
     currEntries[currFreq][1] = indices[rec];
  };

 };

};

// Get the data and return them:
for (i=0; i<4; i++) {
  if (currEntries[currFreq][i]>=0){
  rec = currEntries[currFreq][i];
  fnum = Records[rec].fileNumber;
  newdifx[fnum].seekg(Records[rec].byteIni, newdifx[fnum].beg);
  newdifx[fnum].sync();
  newdifx[fnum].read(reinterpret_cast<char*>(currentVis[i]),Records[rec].byteEnd-Records[rec].byteIni);
  } else {

    for (k=0; k<Freqs[currFreq].Nchan; k++) {
      //  IVAN: compiler complained
      //currentVis[i][k] = {0.0,0.0};
      currentVis[i][k] = (std::complex<float>)0;
    };
  };
};


// Case of auto-correlations (in the 2nd round of conversion):

// if (currEntries[currFreq][2]==-1 && currEntries[currFreq][3]==-1 && complete && antenna == otherAnt){

 isAutoCorr = antenna == otherAnt;

// if ( antenna == otherAnt ){
//   isAutoCorr = true;
 if (complete) {
   if (isAutoCorr || isTwoLinear){
     for (k=0;k<Freqs[currFreq].Nchan; k++) {
       currentVis[3][k] = auxVis[3][k];
       currentVis[2][k] = auxVis[2][k];
       currentVis[0][k] = auxVis[0][k];
       currentVis[1][k] = auxVis[1][k];
     };
//   } else if (isTwoLinear){
//     for (k=0;k<Freqs[currFreq].Nchan; k++) {
//       currentVis[3][k] = auxVis[2][k];
//       currentVis[2][k] = auxVis[3][k];
//       currentVis[0][k] = auxVis[0][k];
//       currentVis[1][k] = auxVis[1][k];
//     };
 //   printf("READING  %i  %i\n",antenna,otherAnt);
   };
 };
// } else {isAutoCorr = false;};


calField = field; 
JDTime = time;
currConj = conj ;


return true;



};










bool DataIOSWIN::setCurrentMixedVis() { 


//char *message;
long rec;
int i, fnum = 0;

//  printf("\n WRITE VISIB! \n");

// Write:

for (i=0; i<4; i++) {

 if (currEntries[currFreq][i]>=0){
  rec = currEntries[currFreq][i];
  fnum = Records[rec].fileNumber;
  newdifx[fnum].seekp(Records[rec].byteIni, newdifx[fnum].beg);
  newdifx[fnum].write(reinterpret_cast<char*>(bufferVis[i]),Records[rec].byteEnd-Records[rec].byteIni);
  newdifx[fnum].flush();
  newdifx[fnum].clear();
//  newdifx[fnum].sync();
 };
};


  return true;

};




void DataIOSWIN::zeroWeight(){

long rec;
int i, fnum = 0;
double zero = 0.0;
// Write:

for (i=0; i<4; i++) {

 if (currEntries[currFreq][i]>=0){
  rec = currEntries[currFreq][i];
  fnum = Records[rec].fileNumber;
  newdifx[fnum].seekp(Records[rec].byteIni - 4*sizeof(double), newdifx[fnum].beg);
  newdifx[fnum].write(reinterpret_cast<char*>(&zero),sizeof(double));
  newdifx[fnum].flush();
//  newdifx[fnum].sync();
 };
};

  newdifx[fnum].clear();

};


void DataIOSWIN::applyMatrix(std::complex<float> *M[2][2], bool swap, bool print, int thisAnt, FILE *plotFile) {
 
  long k, a11, a12, a21, a22, ca11, ca12, ca21, ca22;
  std::complex<float>  auxVisApply;
  int i;

       a11 = 0;
       a22 = 1;
       a12 = 2;
       a21 = 3;



       ca11 = 0;
       ca22 = 1;
       ca12 = 2;
       ca21 = 3;



     for (k=0; k<Freqs[currFreq].Nchan; k++) {


       if (currConj) {

         bufferVis[ca11][k] = M[0][0][k]*currentVis[a11][k]+M[0][1][k]*currentVis[a21][k];
         bufferVis[ca12][k] = M[0][0][k]*currentVis[a12][k]+M[0][1][k]*currentVis[a22][k];
         bufferVis[ca21][k] = M[1][0][k]*currentVis[a11][k]+M[1][1][k]*currentVis[a21][k];
         bufferVis[ca22][k] = M[1][0][k]*currentVis[a12][k]+M[1][1][k]*currentVis[a22][k];

       } else {

         bufferVis[ca11][k] = std::conj(M[0][0][k])*currentVis[a11][k]+std::conj(M[0][1][k])*currentVis[a12][k];
         bufferVis[ca12][k] = std::conj(M[1][0][k])*currentVis[a11][k]+std::conj(M[1][1][k])*currentVis[a12][k];
         bufferVis[ca21][k] = std::conj(M[0][0][k])*currentVis[a21][k]+std::conj(M[0][1][k])*currentVis[a22][k];
         bufferVis[ca22][k] = std::conj(M[1][0][k])*currentVis[a21][k]+std::conj(M[1][1][k])*currentVis[a22][k];

       };


 //  rec = currEntries[currFreq][0];

   if (print && canPlot) {
     if (currConj){
     if (k==0){
       fwrite(&Records[currVis].Time,sizeof(double),1,plotFile);
       fwrite(&Records[currVis].Antennas[0],sizeof(int),1,plotFile);
       fwrite(&Records[currVis].Antennas[1],sizeof(int),1,plotFile);
       fwrite(&ParAng[0][currVis],sizeof(double),1,plotFile);
       fwrite(&ParAng[1][currVis],sizeof(double),1,plotFile);
     };
     fwrite(&currentVis[a11][k],sizeof(std::complex<float>),1,plotFile);
     fwrite(&currentVis[a12][k],sizeof(std::complex<float>),1,plotFile);
     fwrite(&currentVis[a21][k],sizeof(std::complex<float>),1,plotFile);
     fwrite(&currentVis[a22][k],sizeof(std::complex<float>),1,plotFile);
     fwrite(&bufferVis[ca11][k],sizeof(std::complex<float>),1,plotFile);
     fwrite(&bufferVis[ca12][k],sizeof(std::complex<float>),1,plotFile);
     fwrite(&bufferVis[ca21][k],sizeof(std::complex<float>),1,plotFile);
     fwrite(&bufferVis[ca22][k],sizeof(std::complex<float>),1,plotFile);
     fwrite(&M[0][0][k],sizeof(std::complex<float>),1,plotFile);
     fwrite(&M[0][1][k],sizeof(std::complex<float>),1,plotFile);
     fwrite(&M[1][0][k],sizeof(std::complex<float>),1,plotFile);
     fwrite(&M[1][1][k],sizeof(std::complex<float>),1,plotFile);
     } else {
     if (k==0){
       fwrite(&Records[currVis].Time,sizeof(double),1,plotFile);
       fwrite(&Records[currVis].Antennas[1],sizeof(int),1,plotFile);
       fwrite(&Records[currVis].Antennas[0],sizeof(int),1,plotFile);
       fwrite(&ParAng[1][currVis],sizeof(double),1,plotFile);
       fwrite(&ParAng[0][currVis],sizeof(double),1,plotFile);
     };
     auxVisApply = std::conj(currentVis[a11][k]);
     fwrite(&auxVisApply,sizeof(std::complex<float>),1,plotFile);
     auxVisApply = std::conj(currentVis[a21][k]);
     fwrite(&auxVisApply,sizeof(std::complex<float>),1,plotFile);
     auxVisApply = std::conj(currentVis[a12][k]);
     fwrite(&auxVisApply,sizeof(std::complex<float>),1,plotFile);
     auxVisApply = std::conj(currentVis[a22][k]);
     fwrite(&auxVisApply,sizeof(std::complex<float>),1,plotFile);
     auxVisApply = std::conj(bufferVis[ca11][k]);
     fwrite(&auxVisApply,sizeof(std::complex<float>),1,plotFile);
     auxVisApply = std::conj(bufferVis[ca21][k]);
     fwrite(&auxVisApply,sizeof(std::complex<float>),1,plotFile);
     auxVisApply = std::conj(bufferVis[ca12][k]);
     fwrite(&auxVisApply,sizeof(std::complex<float>),1,plotFile);
     auxVisApply = std::conj(bufferVis[ca22][k]);
     fwrite(&auxVisApply,sizeof(std::complex<float>),1,plotFile);
     auxVisApply = std::conj(M[0][0][k]);
     fwrite(&auxVisApply,sizeof(std::complex<float>),1,plotFile);
     auxVisApply = std::conj(M[1][0][k]);
     fwrite(&auxVisApply,sizeof(std::complex<float>),1,plotFile);
     auxVisApply = std::conj(M[0][1][k]);
     fwrite(&auxVisApply,sizeof(std::complex<float>),1,plotFile);
     auxVisApply = std::conj(M[1][1][k]);
     fwrite(&auxVisApply,sizeof(std::complex<float>),1,plotFile);
     };

   };
 };




///////////////////////////////////
// UPDATE THE AUXILIAR VISIBILITIES (I.E. FOR AUTOCORRS WITH MISSING CROSS-POLS):


for (i=0; i<4; i++) {


 if (currEntries[currFreq][i]<0 || isAutoCorr || isTwoLinear ) {

  if (!canPlot){ // Case of auto-correlations (2nd round of conversion):   

    for(k=0;k<Freqs[currFreq].Nchan; k++) {
      auxVis[i][k] = bufferVis[i][k];
    };
 //   printf("SETTING\n");

  } else {

    for(k=0;k<Freqs[currFreq].Nchan; k++) {
      //  IVAN: compiler complained
      //auxVis[i][k] = {0.0,0.0};
      auxVis[i][k] = (std::complex<float>)0;
    };
    if(i==3){isTwoLinear=false;};
 //   printf("ZEROING\n");

  };

 }; 

};
///////////////////////////////////





};






/* CALTABLE - calibration table interface to PolConvert

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
#include <math.h>
#include <iostream>  
#include <fstream>
#include <cstring>
#include "CalTable.h"
#define PI 3.141592653589793
#define TWOPI 6.283185307179586
#include <complex>

CalTable::~CalTable() {};


CalTable::CalTable(int kind, double **R1,double **P1,double **R2,double **P2, double *freqs, double **times, int Na, long *Nt, long Nc, bool **flag, bool islinear, FILE *logF)
{



// Initiate variables and declare auxiliary variables:

      Nchan = Nc;
      Nants = Na;

      logFile = logF ;

      isLinear = islinear;      
//      isTsys = istsys;

      firstTime = new bool[Nants];

      isDelay = kind==1;
      isDterm = kind==2;
      isTsys = kind==3;
      gainChanged = true;

      Ntimes = new long[Nants];
      std::memcpy(Ntimes,Nt,sizeof(long)*Nants);

      Time = new double*[Nants];
      flags = new bool*[Nants];

      int ia;
      for (ia=0; ia<Nants; ia++){
        firstTime[ia] = true;
        Time[ia] = new double[Ntimes[ia]];
        flags[ia] = new bool[Ntimes[ia]*Nchan];
        std::memcpy(Time[ia],times[ia],sizeof(double)*Ntimes[ia]);
        std::memcpy(flags[ia],flag[ia],sizeof(bool)*Ntimes[ia]*Nchan);
      };

      Freqs = new double[Nchan];
      std::memcpy(Freqs,freqs,sizeof(double)*Nchan);




      long i, j, k, auxI; //, auxI2, auxI3;
      JDRange[1] = 0.0;
      JDRange[0] = 1.e20;
    //  double auxD;
      GainAmp[0] = new double**[Nants];
      GainAmp[1] = new double**[Nants];
      GainPhase[0] = new double**[Nants];
      GainPhase[1] = new double**[Nants];
   //   flags = flag;
      for (i=0; i<Nants; i++){
        if (Time[i][0]<JDRange[0]){JDRange[0] = Time[i][0];};
        if (Time[i][Ntimes[i]-1]>JDRange[1]){JDRange[1] = Time[i][Ntimes[i]-1];};
        GainAmp[0][i] = new double*[Nchan];
        GainAmp[1][i] = new double*[Nchan];
        GainPhase[0][i] = new double*[Nchan];
        GainPhase[1][i] = new double*[Nchan];
        for (j=0; j<Nchan; j++) {
          GainAmp[0][i][j] = new double[Ntimes[i]];
          GainAmp[1][i][j] = new double[Ntimes[i]];
          GainPhase[0][i][j] = new double[Ntimes[i]];
          GainPhase[1][i][j] = new double[Ntimes[i]];
          for (k=0; k<Ntimes[i]; k++){
            auxI = j*Ntimes[i]+k; 
            GainAmp[0][i][j][k] = R1[i][auxI];
            GainAmp[1][i][j][k] = R2[i][auxI];
            GainPhase[0][i][j][k] = P1[i][auxI];
            GainPhase[1][i][j][k] = P2[i][auxI];
          };
        };
      };

      SignFreq = (Freqs[1]>Freqs[0]);



fillGaps();


// Set default channel mapping to 1->1:

    preKt = new double[Nants];
    pret0 = new long[Nants];
    pret1 = new long[Nants];
    currTime = -1.0;
    bufferGain[0] = new std::complex<float>*[Nants];
    bufferGain[1] = new std::complex<float>*[Nants];

    for (auxI=0;auxI<Nants;auxI++) {
       pret0[auxI] = 0;
       pret1[auxI] = 0;
       preKt[auxI] = -1.0;
       bufferGain[0][auxI] = new std::complex<float>[Nchan];
       bufferGain[1][auxI] = new std::complex<float>[Nchan];
    };
    K0 = new double[Nchan];
    I0 = new long[Nchan];
    I1 = new long[Nchan];

    MSChan = Nchan;
    for (auxI = 0; auxI<Nchan; auxI++) {
      K0[auxI] = 1.0;
      I0[auxI] = auxI;
      I1[auxI] = 0;

    };



};



// Is this a bandpass gain or a time gain?
bool CalTable::isBandpass() {

  return Nchan>1;

};



// Interpolate failed frequency channels of each antenna and for each time:
void CalTable::fillGaps() {

int ant; 
long tidx, chan, index;
long auxI, auxI2;
double frchan;
bool firstflag = false; 
//char *message;
bool allflagged = true;

 auxI = -1; // If all channels are flagged, no interpolation is done.





//////////////////////
// DEBUGGING CODE
//FILE *gainFile = fopen("GAINS.ASSESS.B4","ab");
//fwrite(&Nchan,sizeof(int),1,gainFile);
//   for (chan=0; chan<Nchan; chan++) {
//       fwrite(&GainAmp[0][0][chan][0],sizeof(double),1,gainFile); 
//       fwrite(&GainAmp[1][0][chan][0],sizeof(double),1,gainFile); 
//       fwrite(&GainPhase[0][0][chan][0],sizeof(double),1,gainFile); 
//       fwrite(&GainPhase[1][0][chan][0],sizeof(double),1,gainFile); 
//       fwrite(&flags[0][chan*Ntimes[0]+0],sizeof(bool),1,gainFile);
//   };
//fclose(gainFile);
//////////////////////


 if (Nchan==1){
 for (ant=0; ant<Nants; ant++) {
  allflagged = true;
  for (tidx=0; tidx<Ntimes[ant]; tidx++) { if(!flags[ant][tidx]){allflagged=false;break;};};
  if (allflagged){
    sprintf(message,"\nWARNING: ALMA ANTENNA #%i HAS ALL ITS TIMES FLAGGED!\n",ant);
    fprintf(logFile,"%s",message);
    sprintf(message,"SETTING ITS GAIN TO ZERO. CHECK RESULTS CAREFULLY!\n\n");
    fprintf(logFile,"%s",message);
    fflush(logFile);
    for (tidx=0; tidx<Ntimes[ant]; tidx++) {
         if(isDterm){
           GainAmp[0][ant][0][tidx] = 0.0;
           GainAmp[1][ant][0][tidx] = 0.0;
         } else {
           GainAmp[0][ant][0][tidx] = 0.0;
           GainAmp[1][ant][0][tidx] = 0.0;
         };
         GainPhase[0][ant][0][tidx] = 0.0;
         GainPhase[1][ant][0][tidx] = 0.0;
         flags[ant][tidx] = false;
    };
  };
  // Determine first unflagged time:
    for (tidx=0; tidx<Ntimes[ant]; tidx++) {
       if (!flags[ant][tidx]){auxI=tidx;break;};};   //REVISAR
  // Fill the first flagged channels:
    for (index=0; index<auxI; index++){
       GainAmp[0][ant][0][index] = GainAmp[0][ant][0][auxI];
       GainAmp[1][ant][0][index] = GainAmp[1][ant][0][auxI];
       GainPhase[0][ant][0][index] = GainPhase[0][ant][0][auxI];
       GainPhase[1][ant][0][index] = GainPhase[1][ant][0][auxI];
       flags[ant][index] = false;            // REVISAR
    };
  // Determine last unflagged channel
    for (tidx=Ntimes[ant]-1; tidx>=0; tidx --) {
       if (!flags[ant][tidx]){auxI=tidx;break;};};  // REVISAR
  // Fill the last flagged channels:
    for (index=auxI+1; index<Ntimes[ant]; index++){  // REVISAR
       GainAmp[0][ant][0][index] = GainAmp[0][ant][0][auxI];
       GainAmp[1][ant][0][index] = GainAmp[1][ant][0][auxI];
       GainPhase[0][ant][0][index] = GainPhase[0][ant][0][auxI];
       GainPhase[1][ant][0][index] = GainPhase[1][ant][0][auxI];
       flags[ant][index] = false;            // REVISAR
    };
  // Look for flagged time ranges and interpolate them:
   firstflag = false;
   for (tidx=0; tidx<Ntimes[ant]; tidx++) {
     if (!firstflag && flags[ant][tidx]){firstflag = true; auxI=tidx-1;};  // REVISAR
     if (firstflag && !flags[ant][tidx]){
          firstflag = false;
          for (auxI2=auxI+1; auxI2<tidx; auxI2++) {
            frchan = ((double) (auxI2-auxI))/((double) (tidx-auxI));
            GainAmp[0][ant][0][auxI2] = GainAmp[0][ant][0][tidx]*frchan;
            GainAmp[0][ant][0][auxI2] += GainAmp[0][ant][0][auxI]*(1.-frchan);
            GainAmp[1][ant][0][auxI2] = GainAmp[1][ant][0][tidx]*frchan;
            GainAmp[1][ant][0][auxI2] += GainAmp[1][ant][0][auxI]*(1.-frchan);
            GainPhase[0][ant][0][auxI2] = GainPhase[0][ant][0][tidx]*frchan;
            GainPhase[0][ant][0][auxI2] += GainPhase[0][ant][0][auxI]*(1.-frchan);
            GainPhase[1][ant][0][auxI2] = GainPhase[1][ant][0][tidx]*frchan;
            GainPhase[1][ant][0][auxI2] += GainPhase[1][ant][0][auxI]*(1.-frchan);
            flags[ant][auxI2] = false;            // REVISAR
          };
     };
   };
  };
 };


 if (Nchan>1) {
 for (ant=0; ant<Nants; ant++) {
  for (tidx=0; tidx<Ntimes[ant]; tidx++) {

    index = tidx;   // REVISAR

    for (chan=0; chan<Nchan; chan ++) {
       if(!flags[ant][chan*Ntimes[ant]+index]){allflagged=false;};            // REVISAR
    };

    if (allflagged){
      sprintf(message,"\nWARNING: ALMA ANTENNA #%i HAS ALL CHANNELS FLAGGED AT TIME #%li\n",ant,tidx);
      fprintf(logFile,"%s",message);
      sprintf(message,"SETTING ITS GAIN TO DUMMY. CHECK RESULTS CAREFULLY!\n\n");
      fprintf(logFile,"%s",message);
      fflush(logFile);
      for (chan=0; chan<Nchan; chan ++) {
         if(isDterm){
           GainAmp[0][ant][chan][tidx] = 0.0;
           GainAmp[1][ant][chan][tidx] = 0.0;
         } else {
           GainAmp[0][ant][chan][tidx] = 1.0;
           GainAmp[1][ant][chan][tidx] = 1.0;
         };
         GainPhase[0][ant][chan][tidx] = 0.0;
         GainPhase[1][ant][chan][tidx] = 0.0;
         flags[ant][chan*Ntimes[ant]+index] = false;
      };
    };

// POLARIZATION-WISE:

  // Determine first unflagged channel
    for (chan=0; chan<Nchan; chan ++) {
       if (!flags[ant][chan*Ntimes[ant]+index]){auxI=chan;break;};};   //REVISAR
  // Fill the first flagged channels:
    for (chan=0; chan<auxI; chan++){
       GainAmp[0][ant][chan][tidx] = GainAmp[0][ant][auxI][tidx];
       GainAmp[1][ant][chan][tidx] = GainAmp[1][ant][auxI][tidx];
       GainPhase[0][ant][chan][tidx] = GainPhase[0][ant][auxI][tidx];
       GainPhase[1][ant][chan][tidx] = GainPhase[1][ant][auxI][tidx];
       flags[ant][chan*Ntimes[ant]+index] = false;            // REVISAR
    };

  // Determine last unflagged channel
    for (chan=Nchan-1; chan>=0; chan --) {
       if (!flags[ant][chan*Ntimes[ant]+index]){auxI=chan;break;};};  // REVISAR
  // Fill the last flagged channels:
    for (chan=auxI+1; chan<Nchan; chan++){  // REVISAR
       GainAmp[0][ant][chan][tidx] = GainAmp[0][ant][auxI][tidx];
       GainAmp[1][ant][chan][tidx] = GainAmp[1][ant][auxI][tidx];
       GainPhase[0][ant][chan][tidx] = GainPhase[0][ant][auxI][tidx];
       GainPhase[1][ant][chan][tidx] = GainPhase[1][ant][auxI][tidx];
       flags[ant][chan*Ntimes[ant]+index] = false;            // REVISAR
    };

  // Look for flagged regions within the band and interpolate them:
   firstflag = false;
   for (chan=0; chan<Nchan; chan++) {
     if (!firstflag && flags[ant][chan*Ntimes[ant]+index]){firstflag = true; auxI=chan-1;};  // REVISAR
     if (firstflag && !flags[ant][chan*Ntimes[ant]+index]){
          firstflag = false;
          for (auxI2=auxI+1; auxI2<chan; auxI2++) {
            frchan = ((double) (auxI2-auxI))/((double) (chan-auxI));
       //     std::cout << frchan<<" "<<GainAmp[0][ant][chan][tidx]<<" "<<GainAmp[0][ant][auxI][tidx]<<"\n";
            GainAmp[0][ant][auxI2][tidx] = GainAmp[0][ant][chan][tidx]*frchan;
            GainAmp[0][ant][auxI2][tidx] += GainAmp[0][ant][auxI][tidx]*(1.-frchan);
            GainAmp[1][ant][auxI2][tidx] = GainAmp[1][ant][chan][tidx]*frchan;
            GainAmp[1][ant][auxI2][tidx] += GainAmp[1][ant][auxI][tidx]*(1.-frchan);
            GainPhase[0][ant][auxI2][tidx] = GainPhase[0][ant][chan][tidx]*frchan;
            GainPhase[0][ant][auxI2][tidx] += GainPhase[0][ant][auxI][tidx]*(1.-frchan);
            GainPhase[1][ant][auxI2][tidx] = GainPhase[1][ant][chan][tidx]*frchan;
            GainPhase[1][ant][auxI2][tidx] += GainPhase[1][ant][auxI][tidx]*(1.-frchan);
            flags[ant][auxI2*Ntimes[ant]+index] = false;            // REVISAR

          };

     };
   };

    };
 };
 };


//////////////////////
// DEBUGGING CODE
//FILE *gainFile2 = fopen("GAINS.ASSESS","ab");
//fwrite(&Nchan,sizeof(int),1,gainFile2);
//   for (chan=0; chan<Nchan; chan++) {
//       fwrite(&GainAmp[0][0][chan][0],sizeof(double),1,gainFile2); 
//       fwrite(&GainAmp[1][0][chan][0],sizeof(double),1,gainFile2); 
//       fwrite(&GainPhase[0][0][chan][0],sizeof(double),1,gainFile2); 
//       fwrite(&GainPhase[1][0][chan][0],sizeof(double),1,gainFile2); 
//       fwrite(&flags[0][chan*Ntimes[0]+0],sizeof(bool),1,gainFile2);
//   };
//fclose(gainFile2);
//////////////////////


return;
};


// Simple self-explanatory methods:
int CalTable::getNant() {return Nants;};
long CalTable::getNchan() {long ret=Nchan; return ret;};
long CalTable::getNEntries(int ant) {return Ntimes[ant];};
void CalTable::getTimeRange(double *JD) {JD[0] = JDRange[0];JD[1]=JDRange[1];};

void CalTable::setChanged(bool ch){gainChanged = ch;};

void CalTable::getFreqRange(double *Fr) {

if(Freqs[0]<Freqs[Nchan-1]){
  Fr[0] = Freqs[0]; Fr[1] = Freqs[Nchan-1];
} else {
  Fr[1] = Freqs[0]; Fr[0] = Freqs[Nchan-1];
};

};



// Freqs arrays are protected. Copy them to memory block out of instance:
void CalTable::getFrequencies(double *freqs) {
  std::memcpy(freqs,Freqs,sizeof(double)*Nchan);};

// Time arrays are protected. Copy them to memory block out of instance:
void CalTable::getTimes(int ant, double *times) {
  std::memcpy(times,Time[ant],sizeof(double)*Ntimes[ant]);
 // printf("TIMES: %.8e -  %.8e\n",Time[ant][0],times[0]);
};


// Similar to above (i.e., the Table gains are protected):
void CalTable::getGains(int ant, long timeidx, double *gain[4])
{
  long i;
  for (i=0; i< Nchan; i++) {
   gain[0][i] = GainAmp[0][ant][i][timeidx];
   gain[1][i] = GainAmp[1][ant][i][timeidx];
   gain[2][i] = GainPhase[0][ant][i][timeidx];
   gain[3][i] = GainPhase[1][ant][i][timeidx];
  };
};



/* Prepares the instance for the frequency interpolation. User must provide
   the array of frequencies to interpolate to (*freqs) and the number 
   of channels in that array (mschan). */
void CalTable::setMapping(long mschan, double *freqs) 
{


  gainChanged = true;
  currTime = -1.0;


  long i, auxI;
  delete K0;
  delete I0;
  delete I1;


  for (i=0; i<Nants; i++) {
    preKt[i] = -1.0 ;
    delete bufferGain[0][i];
    delete bufferGain[1][i];
    bufferGain[0][i] = new std::complex<float>[mschan];
    bufferGain[1][i] = new std::complex<float>[mschan];
  };


  deltaNu = ((freqs[1]-freqs[0])/1.e9);
  deltaNu0 = ((freqs[0]-Freqs[0])/1.e9);

  K0 = new double[mschan];
  I0 = new long[mschan];
  I1 = new long[mschan];

  MSChan = mschan;
  double mmod;

if (SignFreq) {

  for (i=0; i<mschan; i++) {
    if (freqs[i] <= Freqs[0]){
      I0[i]=0; I1[i] = 0;
      K0[i] = 1.0;}
    else if (freqs[i] >= Freqs[Nchan-1]){
      I0[i]=Nchan-1; I1[i] = 0;
      K0[i] = 1.0;}
    else {
      for (auxI=Nchan-1; auxI>=0; auxI--) {
        if (freqs[i] >= Freqs[auxI]) {
           I0[i] = auxI;
           I1[i] = auxI+1;
           mmod = Freqs[auxI+1]-Freqs[auxI];
           K0[i] = (1.0-(freqs[i]-Freqs[auxI])/mmod);
           break;
        };
      };
     };
  };

} else {

  for (i=0; i<mschan; i++) {
    if (freqs[i] >= Freqs[0]){
      I0[i]=0; I1[i] = 0;
      K0[i] = 1.0;}
    else if (freqs[i] <= Freqs[Nchan-1]){
      I0[i]=Nchan-1; I1[i] = 0;
      K0[i] = 1.0;}
    else {
      for (auxI=Nchan-1; auxI>=0; auxI--) {
        if (freqs[i] <= Freqs[auxI]) {
           I0[i] = auxI;
           I1[i] = auxI+1;
           mmod = Freqs[auxI+1]-Freqs[auxI];
           K0[i] = (1.0-(freqs[i]-Freqs[auxI])/mmod);
           break;
        };
      };
     };
  };

};


};


bool CalTable::setInterpolationTime(double itime) {


  if (itime == currTime) {gainChanged = false; return gainChanged;};

  long i; //, auxI;
  long ti0 = 0;
  long ti1 = 0; 
  double Kt = 0.0;
  long Nts; 
  double auxD, auxD2;
  int iant;

  gainChanged = false;

// Find the right time interpolation:  

 for (iant=0; iant<Nants; iant++) {

  Nts = Ntimes[iant];

  if (Nts == 1) {
    pret0[iant] = 0;
    pret1[iant] = 0;
    if (preKt[iant]<0.0){gainChanged=true;};
    preKt[iant] = 1.0;
  } else {
  
  if (itime<=Time[iant][0]) {
    ti0 = 0; ti1 = 0; Kt = 1.0;} 
  else if (itime>=Time[iant][Nts-1]) {
    ti0 = Nts-1; ti1 = 0; Kt = 1.0;}
  else {
    for (i=Nts-1; i>=0; i--) {
      if (itime>Time[iant][i]) {
        ti1 = i+1;
        ti0 = i;
        auxD = Time[iant][i];
        auxD2 = Time[iant][i+1];
        if (isLinear){
          Kt = (1.0 - (itime - auxD)/(auxD2-auxD));
        } else {
          Kt = 1.0;
        };
        break;    
      };
    };
  };



   pret0[iant] = ti0;
   pret1[iant] = ti1;
   preKt[iant] = Kt;
   gainChanged = true;
   };

 };


   currTime = itime;
   return gainChanged;



};




/* Interpolates the gains of an antenna (iant) at a given time, itime, 
   and applies them to the arrays re[2] and im[2] (elements of these arrays
   are the different polarizations (X,Y)). User should have run "setMapping" before,
   in case that the table channel frequencies do not match with the array of frequencies
   where the interpolation is desired. 
   Then, if mode==0, the gains are just written in re and im. 
   If mode==1, the gains are ADDED to the already-existing values in re and im.
   If mode==2, the gains are MULTIPLIED to the already-existing values in re and im. 
 -------------------
*/

void CalTable::applyInterpolation(int iant, int mode, std::complex<float> *gain[2]) {

  long i;
  long ti0, ti1;
  double auxD;
  double Kt, Kt2;

  double auxF0, auxF1, auxF2, auxF3, auxT0, auxT1, auxT2, auxT3;

  ti0 = pret0[iant];
  ti1 = pret1[iant];
  Kt = preKt[iant];
  Kt2 = 1.0-Kt;

 // printf("CALLED!\n");

// Interpolate in frequency (first) and time (second):


  if (gainChanged || firstTime[iant]) {

   firstTime[iant]=false;

 // if (true) {

  for (i=0; i<MSChan; i++) {

     auxF0 = GainAmp[0][iant][I0[i]][ti0]*K0[i];
     auxF1 = GainAmp[1][iant][I0[i]][ti0]*K0[i];
     auxF2 = GainPhase[0][iant][I0[i]][ti0]*K0[i];
     auxF3 = GainPhase[1][iant][I0[i]][ti0]*K0[i];

     if (I1[i] >0) {
       auxF0 += GainAmp[0][iant][I1[i]][ti0]*(1.-K0[i]);
       auxF1 += GainAmp[1][iant][I1[i]][ti0]*(1.-K0[i]);
       auxF2 += GainPhase[0][iant][I1[i]][ti0]*(1.-K0[i]);
       auxF3 += GainPhase[1][iant][I1[i]][ti0]*(1.-K0[i]);
     };

  //  if (i==250){
  //    printf("ORIG: %3e %3e %3e %3e | %3e %3e\n",auxF0,auxF1,auxF2,auxF3, GainAmp[0][iant][I0[i]][ti0], K0[i]);
  //    holadola[0]=auxF0;holadola[1]=auxF1;holadola[2]=auxF2;holadola[3]=auxF3;
   // };

     if (ti1 > 0) {

     auxT0 = GainAmp[0][iant][I0[i]][ti1]*K0[i];
     auxT1 = GainAmp[1][iant][I0[i]][ti1]*K0[i];
     auxT2 = GainPhase[0][iant][I0[i]][ti1]*K0[i];
     auxT3 = GainPhase[1][iant][I0[i]][ti1]*K0[i];

     if (I1[i] >0) {
       auxT0 += GainAmp[0][iant][I1[i]][ti1]*(1.-K0[i]);
       auxT1 += GainAmp[1][iant][I1[i]][ti1]*(1.-K0[i]);
       auxT2 += GainPhase[0][iant][I1[i]][ti1]*(1.-K0[i]);
       auxT3 += GainPhase[1][iant][I1[i]][ti1]*(1.-K0[i]);
     };
        auxF0 = auxF0*Kt+auxT0*Kt2;
        auxF1 = auxF1*Kt+auxT1*Kt2;
        auxF2 = auxF2*Kt+auxT2*Kt2;
        auxF3 = auxF3*Kt+auxT3*Kt2;


     };


       if (isDelay){
         auxD = TWOPI*(((double)i + 0.5)*deltaNu+deltaNu0);
         bufferGain[0][iant][i] = (std::complex<float>) std::polar(1.0,auxF0*auxD);
         bufferGain[1][iant][i] = (std::complex<float>) std::polar(1.0,auxF1*auxD);
       } else if (isTsys) {
         bufferGain[0][iant][i].real(1./sqrt(auxF0));
         bufferGain[0][iant][i].imag(0.0);
         bufferGain[1][iant][i].real(1./sqrt(auxF1));
         bufferGain[1][iant][i].imag(0.0);
       } else if (isDterm) {
         bufferGain[0][iant][i].real(auxF0);
         bufferGain[0][iant][i].imag(auxF2);
         bufferGain[1][iant][i].real(auxF1);
         bufferGain[1][iant][i].imag(auxF3);
       } else {
         bufferGain[0][iant][i] = (std::complex<float>) std::polar(auxF0,auxF2);
         bufferGain[1][iant][i] = (std::complex<float>) std::polar(auxF1,auxF3);
       };

        
    //     if(i==0){printf("\nPH INT: %.2f %.2f",std::abs(bufferGain[0][iant][i]),std::arg(bufferGain[0][iant][i]));};


  };

  };  // Comes from if(gainChanged)



  switch (mode) {
    case 0: // Normal mode.

      for (i=0; i< MSChan; i++) {
          gain[0][i] = bufferGain[0][iant][i];
          gain[1][i] = bufferGain[1][iant][i];
      };

        break;

    case 1:   // Addition mode. 
      for (i=0; i< MSChan; i++) {
         gain[0][i] +=  bufferGain[0][iant][i];
         gain[1][i] +=  bufferGain[1][iant][i];
      };

       break;

    case 2:  // Product mode.
      for (i=0; i< MSChan; i++) {
         gain[0][i] *=  bufferGain[0][iant][i]; 
         gain[1][i] *=  bufferGain[1][iant][i];
     };

       break;

  };

 // if (gainChanged) {
 //   printf("MODE: %i  %i  %i | %3e  %3e | %3e  %3e\n",mode,Nchan, iant, std::abs(gain[0][250]), std::abs(bufferGain[0][iant][250]), std::arg(gain[0][250]), std::arg(bufferGain[0][iant][250]));
 //  printf("MODE: %i  %i  %i | %3e  %3e  %3e %3e\n",mode,I0[250],I1[250], K0[250], GainAmp[0][iant][I0[i]][ti0], GainAmp[1][iant][I0[i]][ti0], Kt );
 //  printf("%i %.5e  %.5e  %.5e  %.5e |  %.5e  %.5e  %.5e %.5e  \n",iant, gain[0][250].real(), gain[0][250].imag(), gain[1][250].real(), gain[1][250].imag(), bufferGain[0][iant][250].real(), bufferGain[0][iant][250].imag(), bufferGain[1][iant][250].real(), bufferGain[1][iant][250].imag());
//  };


};




bool CalTable::getInterpolation(int iant, int ichan, std::complex<float> gain[2]){

  success = true;
  gain[0] = 1.0 ; gain[1] = 1.0 ;
  if(iant>=Nants || ichan > Nchan){success = false;} else {
    gain[0] = bufferGain[0][iant][ichan];
    gain[1] = bufferGain[1][iant][ichan];
    success = true;
  };

//  int i;
//  for (i=0;i<Nants;i++){
//  printf("\nIa: %i  PH1: %.2f  PH2: %.2f",i,std::abs(bufferGain[0][i][ichan]),std::arg(bufferGain[0][i][ichan]));
//  };
//  if(!success){printf("\n %i %i %i %i\n",iant,Nants,ichan,Nchan);};

  return success;

};


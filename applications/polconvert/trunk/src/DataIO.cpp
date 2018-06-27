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
#include "./DataIO.h"
#include "fitsio.h"


//#define   NIOBUF = 1;




DataIO::~DataIO() {

};

DataIO::DataIO() { printf("\nCreating VLBI data structure");};


// SELF-EXPLANATORY FUNCTIONS:
int DataIO::getNfreqs() {return Nfreqs;};  // NUMBER OF IFs
int DataIO::getNant() {return Nants;};  // TOTAL NUMBER OF ANTENNAS
int DataIO::getNchan(int freqid) {return Freqs[freqid].Nchan;};  // # OF CHANNEL IN freqid IF
long DataIO::getMixedNvis() {return NLinVis;};  // NUMBER OF MIXED-POLARIZATION VISIBILITIES FOUND



// GET FREQUENCIES OF CURRENT IF:
void DataIO::getFrequencies(double* output){
memcpy(output,Freqvals[currFreq],Freqs[currFreq].Nchan*sizeof(double));
};


// CHECK WHETHER SOMETHING FAILED:
bool DataIO::succeed(){return success;};


// GET FIRST DAY OF OBSERVATION (MODIFIED JULIAN DATE)
double DataIO::getDay0(){return day0;};







void DataIO::getParAng(int sidx, int Ant1, int Ant2, double*UVW, double &P1, double &P2){

double V2, Bx, By; //, Bz;
double CH, SH, CT1, CT2, HAng, H1, H2, Elev1, Elev2;
int ibas;

Elev1 = 0.0; Elev2 = 0.0;

if(sidx<Geometry->NtotSou && Ant1<Geometry->NtotAnt && Ant2<Geometry->NtotAnt){

  V2 = Geometry->SinDec[sidx]*UVW[1] - Geometry->CosDec[sidx]*UVW[2];
  ibas = Geometry->BasNum[Ant1][Ant2];


  if (ibas<0){
  ibas = -ibas;
   Bx = -Geometry->BaseLine[0][ibas];
   By = -Geometry->BaseLine[1][ibas];
//   Bz = -Geometry->BaseLine[2][ibas];
  } else {
   Bx = Geometry->BaseLine[0][ibas];
   By = Geometry->BaseLine[1][ibas];
//   Bz = Geometry->BaseLine[2][ibas];
  };

  CH = (UVW[0]*By - V2*Bx); // /(By**2. + Bx**2.);
  SH = (UVW[0]*Bx + V2*By); // /(By**2. + Bx**2.);
  CT1 = Geometry->CosDec[sidx]*tan(Geometry->Lat[Ant1]);
  CT2 = Geometry->CosDec[sidx]*tan(Geometry->Lat[Ant2]);

  HAng = atan2(SH,CH);
  H1 = HAng + Geometry->AntLon[Ant1];
  H2 = HAng + Geometry->AntLon[Ant2];

  // sin(lat)*sin(dec)+cos(lat)cos(dec)cos(H)

  if (Geometry->Mount[Ant1] > 3){
  Elev1 = asin(sin(Geometry->Lat[Ant1])*Geometry->SinDec[sidx]+cos(Geometry->Lat[Ant1])*Geometry->CosDec[sidx]*cos(H1));};

  if (Geometry->Mount[Ant2] > 3){
  Elev2 = asin(sin(Geometry->Lat[Ant1])*Geometry->SinDec[sidx]+cos(Geometry->Lat[Ant1])*Geometry->CosDec[sidx]*cos(H1));};
 

  switch (Geometry->Mount[Ant1]){
  case 0:  P1 = atan2(sin(H1), CT1 - Geometry->SinDec[sidx]*cos(H1)); break; // ALT-AZ
  case 1: P1 = 0.; break; // EQ
  case 2: P1 = 0.; break; // ORBITAL (NO WAY!)
  case 3: P1 = atan2(cos(H1), Geometry->SinDec[sidx]*sin(H1)); break; // X-Y (E-W?)
  case 4: P1 = atan2(sin(H1), CT1 - Geometry->SinDec[sidx]*cos(H1)) + Elev1; break; // NA-R
  case 5: P1 = atan2(sin(H1), CT1 - Geometry->SinDec[sidx]*cos(H1)) - Elev1; break; // NA-L
  default: P1 = 0.;

  };

  switch (Geometry->Mount[Ant2]){
  case 0:  P2 = atan2(sin(H2), CT2 - Geometry->SinDec[sidx]*cos(H2));  break; // ALT-AZ
  case 1: P2 = 0.;  break; // EQ
  case 2: P2 = 0.;  break; // ORBITAL (NO WAY!)
  case 3: P2 = atan2(cos(H2), Geometry->SinDec[sidx]*sin(H2));  break; // X-Y (E-W?)
  case 4: P2 = atan2(sin(H2), CT2 - Geometry->SinDec[sidx]*cos(H2)) + Elev2;  break; // NA-R
  case 5: P2 = atan2(sin(H2), CT2 - Geometry->SinDec[sidx]*cos(H2)) - Elev2;  break; // NA-L
  default: P2 = 0.;

  };

//  P2 = atan2(sin(H2), CT2 - Geometry->SinDec[sidx]*cos(H2));


} else {

  P1 = 0.0;
  P2 = 0.0;

};

};



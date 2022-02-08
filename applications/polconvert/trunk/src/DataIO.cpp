/* DATAIO - FITS-IDI interface to PolConvert

             Copyright (C) 2013-2020  Ivan Marti-Vidal
             Nordic Node of EU ALMA Regional Center (Onsala, Sweden)
             Max-Planck-Institut fuer Radioastronomie (Bonn, Germany)
             Observatori Astronomic, Universitat de Valencia
  
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

DataIO::DataIO() { printf("\nCreating VLBI data structure"); nautos = 0;};


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





cplx32f DataIO::getAmpRatio(int ant, int spw, int chan){
    


 //   int i; 
 //   int iX = -1;
 //   int iY = -1;
 //   double ret;
    
/*
    for (i=0; i<nautos; i++){
      if(AutoCorrs[i].AntIdx == ant && AutoCorrs[i].IF == spw){
        if (AutoCorrs[i].Pol == 1 && std::abs(JD - AutoCorrs[i].JD) < minJDX){iX = i;};
        if (AutoCorrs[i].Pol == 2 && std::abs(JD - AutoCorrs[i].JD) < minJDY){iY = i;};
      }; if (iX>=0 && iY>=0){break;};
    };
    
    ret = (iX>=0 && iY>= 0)? std::sqrt(AutoCorrs[iY].AC/AutoCorrs[iX].AC) : 1.0; 
    return cplx32f(ret,0.);
//   printf("AUTOCORRS %.2f\n",ret);

*/





    return cplx32f(averAutocorrs[ant][spw][chan],0.);
    
    
};





//z void DataIO::getParAng(int sidx, int Ant1, int Ant2, double*UVW, double &P1, double &P2){
void DataIO::getParAng(int sidx, int Ant1, int Ant2, double*UVW, double &MJD, double &P1, double &P2){

//z double V2, Bx, By; //, Bz;
//z double CH, SH, CT1, CT2, HAng, H1, H2, Elev1, Elev2;
//zz double V2, Bx, By, GMST; //, Bz;
double GMST;
//double CDec, SDec, SRA, TLat1,TLat2, Lon1,Lon2,CH, SH;
double CT1, CT2, HAng, H1, H2, Elev1, Elev2;
//zz int ibas;

Elev1 = 0.0; Elev2 = 0.0;

//z:
double days = MJD/86400.;
double t = (days-51544.0)/36525.;
double Hh = days - floor(days);
double GMsec = 24110.54841 + 8640184.812866*t + 0.093104*t*t - 0.0000062*t*t*t;
GMST = (GMsec/86400. + Hh)*2.*3.1415926535;
//z:
//CDec = Geometry->SinDec[sidx];
//SDec = Geometry->CosDec[sidx];
//TLat1 = tan(Geometry->Lat[Ant1]);
//TLat2 = tan(Geometry->Lat[Ant1]);
//Lon1 = Geometry->AntLon[Ant1];
//Lon2 = Geometry->AntLon[Ant2];
//SRA = Geometry->RA[sidx];

// Dummy value if autocorrelation:
if (Ant1==Ant2){P1 = -1.e9; P2 = -1.e9; return;};

//z more blank lines

if(sidx<Geometry->NtotSou && Ant1<Geometry->NtotAnt && Ant2<Geometry->NtotAnt){

//z  V2 = Geometry->SinDec[sidx]*UVW[1] - Geometry->CosDec[sidx]*UVW[2];
//zz   ibas = Geometry->BasNum[Ant1][Ant2];

//z  if (ibas<0){
//z  ibas = -ibas;
//z   Bx = -Geometry->BaseLine[0][ibas];
//z   By = -Geometry->BaseLine[1][ibas];
//z// Bz = -Geometry->BaseLine[2][ibas];
//z  } else {
//z   Bx = Geometry->BaseLine[0][ibas];
//z   By = Geometry->BaseLine[1][ibas];
//z// Bz = Geometry->BaseLine[2][ibas];
//z  };
//z
//z  CH = (UVW[0]*By - V2*Bx); // /(By**2. + Bx**2.);
//z  SH = (UVW[0]*Bx + V2*By); // /(By**2. + Bx**2.);

  CT1 = Geometry->CosDec[sidx]*tan(Geometry->Lat[Ant1]);
  CT2 = Geometry->CosDec[sidx]*tan(Geometry->Lat[Ant2]);

//z  HAng = atan2(SH,CH);
  HAng = GMST - Geometry->RA[sidx];
  H1 = HAng + Geometry->AntLon[Ant1];
  H2 = HAng + Geometry->AntLon[Ant2];

  // sin(lat)*sin(dec)+cos(lat)cos(dec)cos(H)

  if (Geometry->Mount[Ant1] > 3){
   Elev1 = asin(sin(Geometry->Lat[Ant1])*Geometry->SinDec[sidx]+cos(Geometry->Lat[Ant1])*Geometry->CosDec[sidx]*cos(H1));};

  if (Geometry->Mount[Ant2] > 3){
//z  Elev2 = asin(sin(Geometry->Lat[Ant1])*Geometry->SinDec[sidx]+cos(Geometry->Lat[Ant1])*Geometry->CosDec[sidx]*cos(H1));};
   Elev2 = asin(sin(Geometry->Lat[Ant2])*Geometry->SinDec[sidx]+cos(Geometry->Lat[Ant2])*Geometry->CosDec[sidx]*cos(H2));};
 

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

//z:
//if(Ant1==1){printf("%i  %i  %.16e  %.3f  %.3f  %.3f  %.3f\n",Ant1, Ant2, MJD, GMST, HAng, P1, P2);};

} else {

  P1 = 0.0;
  P2 = 0.0;

};

};



/* WEIGHTER - antenna weighting/flagging interface to PolConvert

             Copyright (C) 2015  Ivan Marti-Vidal
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
#include <complex.h>
#include "./Weighter.h"


Weighter::~Weighter(){};

Weighter::Weighter(int nPhase, long *nASDMtimes, long nASDMentries, int *ASDMant, double **ASDMtimes, int *refants, double *time0, double *time1, double* BadTimes, int NBadTimes, FILE *logF) {


 logFile = logF;

 nants = nPhase;
 nASDMEntries = nASDMentries; 
 ntimes = nASDMtimes; 
 ants = ASDMant; 
 JDtimes = ASDMtimes;
 refAnts = refants;
 Time0 = time0;
 Time1 = time1;
 currTime = 0.0;
 currRefAnt = 0;
 badTimes = BadTimes;
 NbadTimes = NBadTimes;

};


/* Returns whether ALMA was effectively phased at this time */
bool Weighter::isPhased(double JDTime){

  int i;
  bool Phased = true;

  for (i=0; i<NbadTimes; i++){
    if(JDTime>=badTimes[2*i] && JDTime<=badTimes[2*i+1]){Phased=false;break;};
  };

  return Phased;
};



/* Returns whether the antenna is in the phased sum (true) or not (false) */
bool Weighter::getWeight(int iant, double JDtime){

  long i;
  int j;

  bool inside;

  for (j=0; j<nants; j++){
    if (ants[j] == iant){break;};
  };

  inside = false;

  if(j<nants){
    for (i=0; i<ntimes[j]; i++){
      if (JDtimes[j][2*i]<=JDtime && JDtimes[j][2*i+1]>=JDtime){inside = true; break;};   
    };
  };

//  if(!inside){
//    sprintf(message,"T: %.5f ; ANT: %i ; %i\n",JDtime,iant,inside);
//    fprintf(logFile,"%s",message);fflush(logFile);
//  };

  return inside;

};



// Returns the index of the reference ALMA antenna at a given time.
// Returns -1 if no valid time range is used.
int Weighter::getRefAnt(double JDtime){

  long i;


  if (JDtime == currTime){return currRefAnt;};

  for (i=0; i<nASDMEntries; i++){
    if (Time0[i]<=JDtime && Time1[i]>=JDtime){
      currTime = JDtime; currRefAnt = refAnts[i];
      return currRefAnt;
    };   
  };

// If no refant is found, no X-Y offset will be applied:
  return -1;
};


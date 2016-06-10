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
#include "Weighter.h"


Weighter::~Weighter(){};

Weighter::Weighter(int nPhase, long *nASDMtimes, long nASDMentries, int *ASDMant, double **ASDMtimes, int *refants, double *time0, double *time1) {


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

};


/* Returns whether the antenna is in the phased sum (true) or not (false) */
bool Weighter::getWeight(int iant, double JDtime){

  long i;
  int j;

  

  for (j=0; j<nants; j++){
    if (ants[j] == iant){break;};
  };


  for (i=0; i<ntimes[j]; i++){
    if (JDtimes[j][2*i]<=JDtime && JDtimes[j][2*i+1]>=JDtime){return true;};   
  };

  return false;
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


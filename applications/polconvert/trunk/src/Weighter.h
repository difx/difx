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
#include <iostream>


/* Class to determine whether an ALMA antenna was added to the phased signal and to return the Reference Antenna at any time */
class Weighter {
  public:
    Weighter(int nPhase, long *nASDMtimes, long nASDMEntries, int *ASDMant, double **ASDMtimes, int *refants, double *time0, double *time1, double *BadTimes, int NBadTimes, FILE *logF);
    ~Weighter();

    bool isPhased(double JDtime);
    bool getWeight(int iant, double JDtime);
    int getRefAnt(double JDtime);

  private:

    FILE *logFile;
    char message[512];
    int nants;
    long *ntimes, nASDMEntries; 
    int *ants; 
    int *refAnts;
    int currRefAnt;
    double currTime;
    double **JDtimes;
    double *Time0;
    double *Time1;
    double *badTimes;
    int NbadTimes;
};


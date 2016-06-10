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
#include "DataIO.h"
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




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
#include <math.h>
#include <complex>
#include "fitsio.h"

#ifndef __DATAIO_H__
#define __DATAIO_H__


typedef std::complex<float> cplx32f;

typedef struct {
double Nu0;
int Nchan;
double BW;
bool SB;
} FreqSetup;



/* Class to read FITS-IDI or SWIN files, setup the data streams,
and iterate over all the baselines with mixed polarization. */

class DataIO {
  public:

   ~DataIO();

   DataIO(); 

   int getNfreqs();
   int getNant();

   int getNchan(int freqid);
   long getMixedNvis();
   bool succeed();

   double getDay0();

   void getFrequencies(double* Freqarray);

// Set the current IF (and reset the mixed-vis. counter):
   virtual bool setCurrentIF(int i) = 0;


/* Very important function. Finds the next combination of the 4 correlation
   products. Returns the visibilities as an array of pointers;
   It also returns the time of the visibility and the ids of the antennas in the baseline. 
   If more than one lin-pol antenna are present, this function will still work, and will 
   return the pure-linear visibilities twice, one iteration for each antenna (the same is 
   true for the auto-correlations of each lin-pol antenna). 
   Returns false if no more mixed-pol visibilities are found for the corresponding IF. */
   virtual bool getNextMixedVis(double &JDTime, int &antenna, int &otherAnt, bool &conj) = 0;


/* Complementary to the function above. Once the visibilities have been 
   corrected, this function puts them back into the right place. */
   virtual bool setCurrentMixedVis() = 0;


 // Modify the visibilities read by "getNextMixedVis" by the calibration matrix supplied
 // Saves the result in the "bufferVis" pointer  
  virtual void applyMatrix(std::complex<float> *M[2][2], bool swap, bool print, FILE *plotFile) = 0;



// Close all files.
   virtual void finish() = 0;



   static const int MAXIF = 256;
   FreqSetup *Freqs;
   double *Freqvals[MAXIF], *doRange, *JDTimes;
   int *Basels, *Freqids, *an1, *an2, *linAnts;
   int NLinAnt, NIFs, Nband, status, Nants, Nfreqs, currFreq;
   long NLinVis, Nvis, currVis, *indexes;
   bool success, currConj;
   bool *conjugate, *is1, *is2, *is1orig, *is2orig;
   int Flux;
   double day0;



};




#endif 


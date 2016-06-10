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
#include "DataIO.h"



typedef struct {
 int fileNumber;
 double Time;
 char Pol[2];
 bool notUsed;
 int Antennas[2];
 int Baseline;
 int freqIndex;
 long byteIni;
 long byteEnd;} Record;




/* Class to read FITS-IDI files, setup the data streams,
and iterate over all the baselines with mixed polarization. */

class DataIOSWIN: public DataIO {
  public:

   ~DataIOSWIN();

   DataIOSWIN(int nSWIN, std::string* outputfiles, int Nant, int *Ants, double *doRange, int nIF, int *nChan, double **Freqs, bool Overwrite, bool doTest, double jd0, FILE *logF);

   bool setCurrentIF(int i);


/* Very important function. Finds the next combination of the 4 correlation
   products. Returns the visibilities as an array of pointers;
   It also returns the time of the visibility and the ids of the antennas in the baseline. 
   If more than one lin-pol antenna are present, this function will still work, and will 
   return the pure-linear visibilities twice, one iteration for each antenna (the same is 
   true for the auto-correlations of each lin-pol antenna). 
   Returns false if no more mixed-pol visibilities are found for the corresponding IF. */
   bool getNextMixedVis(double &JDTime, int &antenna, int &otherAnt, bool &conj);


/* Complementary to the function above. Once the visibilities have been 
   corrected, this function puts them back into the right place. */
   bool setCurrentMixedVis();


 // Modify the visibilities read by "getNextMixedVis" by the calibration matrix supplied
 // Saves the result in the "bufferVis" pointer  
  void applyMatrix(std::complex<float> *M[2][2], bool swap, bool print, FILE *plotFile);



// Close all files.
   void finish();

  private:

   void openOutFiles(std::string* difxfiles);
   void readHeader(bool doTest);

////////
// Only used for SWIN files. Not used here
    static const long RECBUFFER = 1024*1024;
    static const int NFRDATA = 8;
    static const int NCFDATA = 2;
    static const long endhead = sizeof(int) + 4*sizeof(double); // Useless info at the headers end.

////////

//    char polOrder[4];
    FILE *logFile;
    char message[512];
    int nfiles;
    std::ifstream *olddifx;
    std::ofstream *newdifx;
    bool isOverWrite;
    long currEntries[MAXIF][4], nrec;
    long *filesizes;
    std::complex<float> *currentVis[4] ;
    std::complex<float> *bufferVis[4] ;
    Record *Records ;
};

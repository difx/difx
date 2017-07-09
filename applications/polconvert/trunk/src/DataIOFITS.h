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
#include "DataIO.h"




/* Class to read FITS-IDI files, setup the data streams,
and iterate over all the baselines with mixed polarization. */

class DataIOFITS: public DataIO {
  public:

   ~DataIOFITS();

   DataIOFITS(std::string outputfile, int Nant, int *Ants, double *doRange, bool Overwrite, bool doConj, bool doSolve, ArrayGeometry *Geom,  FILE *logF);

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
  void applyMatrix(std::complex<float> *M[2][2], bool swap, bool print, int thisAnt, FILE *plotFile);



// Close all files.
   void finish();

  private:


    void readInput(std::string inputfile);
    void openOutFile(std::string outputfile, bool Overwrite);   
    void saveCirculars(std::string inputfile);   

    fitsfile *fptr, *ofile; 
    FILE *logFile ;
    long *Vis2Save;
    long NVis2Save;

    int uu, vv, ww, ss;

    double *Times;
    bool doConjugate, doWriteCirc;
    char polOrder[4], message[512];
    long jump, Nentry, djump, dsize, TotSize;
    int currIF, currBand;
    std::complex<float> *currentVis ;
    std::complex<float> *bufferVis ;
    float *currentData ;
    float *bufferData ;

};

/*****************************************************************************
*    <DataSim: VLBI data simulator>                                          *
*    Copyright (C) <2015> <Zheng Meyer-Zhao>                                 *
*                                                                            *
*    This file is part of DataSim.                                           *
                                                                             *
*    DataSim is free software: you can redistribute it and/or modify         *
*    it under the terms of the GNU General Public License as published by    *
*    the Free Software Foundation, either version 3 of the License, or       *
*    (at your option) any later version.                                     *
*                                                                            *
*    DataSim is distributed in the hope that it will be useful,              *
*    but WITHOUT ANY WARRANTY; without even the implied warranty of          *
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *
*    GNU General Public License for more details.                            *
*                                                                            *
*    You should have received a copy of the GNU General Public License       *
*    along with this program.  If not, see <http://www.gnu.org/licenses/>.   *
*****************************************************************************/

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <stdint.h>
#include "architecture.h"
#include "configuration.h"
#include "vdifio.h"
#include "catvdif.h"
#include <mpi.h>

using namespace std;

void catvdif(string antname, size_t verbose, int antidx, size_t div)
{
  // each antenna combines its time-segment into one VDIF file
  ofstream outputvdif;
  // change the last character of the output vdif name to lower case for fourfit postprocessing
  //antname.back() = tolower(antname.back());
  antname.at(antname.size()-1) = tolower(antname.at(antname.size()-1));

  if(verbose >= 1)
  {
    cout << "Antenna " << antidx << ":" << endl;
  }
  ifstream tsegfile[div];

  try
  {
    // open multi-channel vdif file to write to
    outputvdif.open((antname + ".vdif").c_str(), ios::binary);

    // open vdif file of each time segment
    for(size_t  tseg = 0; tseg < div; tseg++)
    {
      stringstream ss;
      ss << tseg;
      ostringstream filename;
      filename << antname << "-" << ss.str() << ".vdif";
      tsegfile[tseg].open(filename.str().c_str(), ios::binary);
      if(verbose >= 1)
      {
        cout << " Opened input file " << filename.str() << endl;
      }
      outputvdif << tsegfile[tseg].rdbuf();

      tsegfile[tseg].close();
      if(verbose >= 1)
      {
        cout << " Closed input file  " << filename.str() << endl;
      }
	  }
    // close multi-channel vdif file after writing
    outputvdif.close();

  } catch (ofstream::failure e) {
    cerr << "Exception opening/closinging input or output vdif file" << endl;
  }
}

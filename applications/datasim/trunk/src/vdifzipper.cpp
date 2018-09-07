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
#include "vdifzipper.h"
#include <mpi.h>

using namespace std;
/**
 * @config Configuration file
 * @configindex Configuration index
 * @durus Observation time in microseconds
 */
void vdifzipper(Configuration* config, int configindex, float durus, size_t verbose, int antidx, int color)
{
  int mjd, seconds;

  cout << "Combine VDIF files of each channel into a single-thread multi-channel VDIF file ..." << endl;

  mjd = config->getStartMJD();
  seconds = config->getStartSeconds();
  seconds += durus/(float)1e6 * color;
  if(verbose >= 1)
  {
    cout << "mjd is " << mjd << ", seconds is " << seconds << endl;
  }

  // each antenna combines its subbands into one VDIF file

  size_t framebytes;                    // data frame size of the output VDIF file
  size_t numrecordedbands;              // number of channels
  size_t chvpbytes;                     // data frame size of a single channel
  size_t numsampsperframe;              // number of samples per frame of a single channel VDIF file
  float bw;                             // bandwidth
  size_t framespersec, totalnumframes;
  size_t ishift, oshift;
  uint8_t *optr, *iptr, *soptr, *siptr;
  uint8_t bits, imask, omask;
  string antname;
  ofstream outputvdif;

  if(verbose >= 1)
  {
    cout << "Antenna " << antidx << ":" << endl;
  }
  // retrieve number of subbands, total framebytes, antenna name
  // and the vdif packet bytes for each singal channel vdif file
  framebytes = (size_t)config->getFrameBytes(configindex, antidx);
  numrecordedbands = (size_t)config->getDNumRecordedBands(configindex, antidx);
  antname = config->getTelescopeName(antidx);
  // change the last character of the output vdif name to lower case for fourfit postprocessing
  //antname.back() = tolower(antname.back());
  antname.at(antname.size()-1) = tolower(antname.at(antname.size()-1));

  chvpbytes = (framebytes - VDIF_HEADER_BYTES) / numrecordedbands + VDIF_HEADER_BYTES;
  numsampsperframe = (chvpbytes - VDIF_HEADER_BYTES) * BITSPERBYTE / BITS;

  ifstream chfile[numrecordedbands];

  // retrieve bandwidth information and number of frames per second of each antenna
  bw = config->getDRecordedBandwidth(configindex, antidx, 0);
  framespersec = config->getFramesPerSecond(configindex, antidx);
  totalnumframes = durus / 1e6 * framespersec;

  if(verbose >= 1)
  {
    cout << " framebyte is " << framebytes << "bytes, number of channels is " << numrecordedbands<< "\n"
         << " bandwitdh is " << bw << "MHz, framespersec is " << framespersec<< "\n"
         << " total number of frames is " << totalnumframes << "\n"
         << " number of samples per frame is " << numsampsperframe << endl;
  }
  // allocate memory for vdif packet buffer and input file streams
  uint8_t* outputvdifbuf;
  uint8_t* inputvdifbuf;
  outputvdifbuf = vectorAlloc_u8(framebytes);
  fill_n(outputvdifbuf, framebytes, 0);
  inputvdifbuf = vectorAlloc_u8(chvpbytes);
  fill_n(inputvdifbuf, chvpbytes, 0);
  if(verbose >= 2)
  {
    cout << " Allocated memory for vdif packet buffer for antenna " << antidx << endl;
  }

  // initialize VDIF header of the output buffer
  createVDIFHeader((vdif_header *)outputvdifbuf, framebytes - VDIF_HEADER_BYTES, antidx, BITS, numrecordedbands, ISCOMPLEX, (char *)antname.c_str());
  setVDIFEpochMJD((vdif_header *)outputvdifbuf, mjd);
  setVDIFFrameMJDSec((vdif_header *)outputvdifbuf, mjd*86400 + seconds);
  if(verbose >= 2)
  {
    cout << " VDIF header initialized" << endl;
  }

  stringstream cc;
  cc << color;

  try
  {
    // open multi-channel vdif file to write to
    outputvdif.open((antname + "-" + cc.str() + ".vdif").c_str(), ios::binary);
    if(verbose >= 2)
    {
      cout << " Opened " << antname+ "-" + cc.str() << ".vdif for writing ..." << endl;
    }
    // open vdif file of each channel
    for(size_t ch = 0; ch < numrecordedbands; ch++)
    {
      stringstream ss;
      ss << ch;
      ostringstream filename;
      filename << antname << "_" << ss.str() << "-" << cc.str() << ".vdif";
      chfile[ch].open(filename.str().c_str(), ios::binary);
      if(verbose >= 2)
      {
        cout << " Opened input file " << filename.str() << endl;
      }
    }

    for(size_t idx = 0; idx < totalnumframes; idx++)
    {
      //update VDIF Header for the next packet
      if(idx != totalnumframes - 1)
        nextVDIFHeader((vdif_header *)outputvdifbuf, (int) framespersec);
      // loop through all the channels
      for(size_t ch = 0; ch < numrecordedbands; ch++)
      {
        soptr = &outputvdifbuf[VDIF_HEADER_BYTES];
        // reset the value of the input vdif buffer
        fill_n(inputvdifbuf, chvpbytes, 0);

        chfile[ch].read((char *)inputvdifbuf, chvpbytes * sizeof(uint8_t));

        // set start input pointer at the beginning of data within the current frame
        siptr = &inputvdifbuf[VDIF_HEADER_BYTES];

        // loop through number of samples within the current frame
        for(size_t samp = 0; samp < numsampsperframe; samp++)
        {
          // set the input pointer at the proper byte for the current sample
          iptr = siptr + (samp * BITS) / BITSPERBYTE;
          // shift to the proper bits and retrieve bits value
          ishift = (samp * BITS) % BITSPERBYTE;
          imask = 03;
          imask <<= ishift;
          bits = (*iptr) & imask;
          bits >>= ishift;

          // store the bits value at the proper location in the output data frame
          // samp of input data frame is equivalent to the complete sample index of output data frame
          optr = soptr + (samp * BITS * numrecordedbands) / BITSPERBYTE + (ch * BITS) / BITSPERBYTE;
          oshift = ((samp * BITS * numrecordedbands) % BITSPERBYTE + (ch * BITS) % BITSPERBYTE) % BITSPERBYTE;
          omask = 03;
          omask <<= oshift;
          bits <<= oshift;
          (*optr) &= ~omask;
          (*optr) |= bits;
        }
      }
      // write to output vdif file
      outputvdif.write((char *)outputvdifbuf, framebytes * sizeof(uint8_t));
    }
    // close input vdif files
    for(size_t ch = 0; ch < numrecordedbands; ch++)
    {
      chfile[ch].close();
      if(verbose >= 1)
      {
        cout << " Closed input file for channel " << ch << endl;
      }
    }
    // close multi-channel vdif file after writing
    outputvdif.close();
    if(verbose >= 1)
    {
      cout << " Closed " << antname << ".vdif ..." << endl;
    }
  } catch (ofstream::failure e) {
    cerr << "Exception opening/closinging input or output vdif file" << endl;
  }
  // free memory of input and output vdif buffer
  vectorFree(outputvdifbuf);
  vectorFree(inputvdifbuf);
  if(verbose >= 2)
  {
    cout << "Freed memory allocated for input and output vdif buffers" << endl;
  }
}

/*
int main(int argc, char* argv[])
{
  MPI_Init(&argc, &argv);
  int numprocs, antidx;
  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  MPI_Comm_rank(MPI_COMM_WORLD, &antidx);

  Configuration* config;
  int configindex = 0;
  float dur, durus;
  size_t verbose = 0;
  int numdatastreams;

  config = new Configuration(argv[1], configindex);
  // retrieve observation time in seconds
  // and convert it to microseconds
  dur = config->getExecuteSeconds();
  durus = dur * 1e6;
  numdatastreams = config->getNumDataStreams();
  if(antidx < numdatastreams)
    vdifzipper(config, configindex, durus, verbose, antidx);

  MPI_Finalize();
  return 0;
}
*/

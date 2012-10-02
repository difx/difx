/***************************************************************************
 *   Copyright (C) 2011 by Adam Deller                                     *
 *                                                                         *
 *   This program is free for non-commercial use: see the license file     *
 *   at http://astronomy.swin.edu.au:~adeller/software/difx/ for more      *
 *   details.                                                              *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *
 ***************************************************************************/
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL: $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include <mpi.h>
#include "visibility.h"
#include "configuration.h"
#include <unistd.h>
#include <stdio.h>
#include <sys/stat.h>
#include <signal.h>
#include <fstream>
#include <string.h>

#define BUFFER_SIZE 2000

int main(int argc, char *argv[])
{
  Configuration * config;
  Model * model;
  char * inputdifxfile;
  int * baselineindices;
  double * vistimes;
  double ** dmdelays;
  int ** dmoffsets;
  char ***** visibilities;
  char * copypointer;
  char * outputpointer;
  char * tempbuffer;
  char pol[3];
  ifstream input;
  ofstream output;
  string savedir, reldifxfile, savedifxfile;
  int lastslash;
  int fchan, lastconfigindex, maxnumintegrations;
  int mindelayoffset, maxdelayoffset, poloffset;
  int startmjd, currentmjd, currentfreq, baseline, offset=0;
  double dm, dur, inttime, chanfreq, startseconds, currentseconds, lastseconds=0;
  double snipstartmjd, snipendmjd;
  bool dotrim = false;
  pol[2] = 0;

  if(argc != 4 && argc != 6)
  {
    cerr << "Error - invoke with dedisperse_difx <inputfilename> <inputdifxfile> <dm> [startsnipmjd] [endsnipmjd]" << endl;
    return EXIT_FAILURE;
  }

  config = new Configuration(argv[1], 0);
  inputdifxfile = argv[2];
  dm = atof(argv[3]);
  if(argc == 6) {
    dotrim = true;
    snipstartmjd = atof(argv[4]);
    snipendmjd = atof(argv[5]);
  }

  //check that the config is ok
  if(!config->consistencyOK())
  {
    //There was a problem with the input file, so shut down gracefully
    cerr << "Config encountered inconsistent setup in config file - please check setup" << endl;
    return EXIT_FAILURE;
  }
  model = config->getModel();

  //check that the saved data directory does not already exist, create it if possible
  savedir = config->getOutputFilename() + "/undedispersed/";
  int flag = mkdir(savedir.c_str(), 0775);
  if(flag < 0) {
    cerr << "Error trying to create directory " << savedir << ": " << flag << ", ABORTING!" << endl;
    exit(1);
  }

  //check that the input difx file does exist
  input.open(inputdifxfile, ifstream::in);
  if(input.fail()) {
    cerr << "Error - problem opening input file " << inputdifxfile << endl;
    exit(1);
  }
  input.close();

  //bail out if more than one config
  if(config->getNumConfigs() > 1) {
    cerr << "Cannot deal with multi-config data yet - aborting!" << endl;
    exit(1);
  }

  //bail out if more than one scan
  if(model->getNumScans() > 1) {
    cerr << "Cannot deal with multi-scan data yet - aborting!" << endl;
    exit(1);
  }

  //work out the baseline indices
  baselineindices = new int[(config->getTelescopeTableLength()+1)*257];
  for(int i=0;i<config->getNumBaselines() + config->getNumDataStreams();i++) {
    if(i<config->getNumBaselines())
      baselineindices[256*(1+config->getDTelescopeIndex(0, config->getBDataStream1Index(0, i))) + 1+config->getDTelescopeIndex(0, config->getBDataStream2Index(0, i))] = i;
    else
      baselineindices[257*(1+config->getDTelescopeIndex(0, i-config->getNumBaselines()))] = i;
  }

  //move the input difx file to the saved directory
  system(("mv " + string(inputdifxfile) + " " + savedir).c_str());
  lastslash = string(inputdifxfile).find_last_of('/');
  if(lastslash == string::npos)
    lastslash = -1;
  savedifxfile = savedir + string(inputdifxfile).substr(lastslash+1);

  inttime = 0.0;
  for(int i=0;i<model->getNumScans();i++)
    inttime = config->getIntTime(config->getScanConfigIndex(i));

  //create the dmoffsets/dmdelays arrays
  mindelayoffset = 9999999;
  maxdelayoffset = 0;
  dmdelays = new double*[config->getFreqTableLength()];
  dmoffsets = new int*[config->getFreqTableLength()];
  for(int i=0;i<config->getFreqTableLength();i++) {
    fchan = config->getFNumChannels(i)/config->getFChannelsToAverage(i);
    dmdelays[i] = new double[config->getFNumChannels(i)];
    dmoffsets[i] = new int[config->getFNumChannels(i)];
    for(int j=0;j<fchan;j++) {
      chanfreq = 0.001 * (config->getFreqTableFreq(i) + ((double)j)*config->getFreqTableBandwidth(i)/fchan);
      dmdelays[i][j] = 0.00415 * dm / (chanfreq * chanfreq);
      dmoffsets[i][j] = (int)((dmdelays[i][j] + inttime/2.0) / (inttime));
      if(dmoffsets[i][j] < mindelayoffset)
        mindelayoffset = dmoffsets[i][j];
      if(dmoffsets[i][j] > maxdelayoffset)
        maxdelayoffset = dmoffsets[i][j];
    }
  }

  //work out the maximum buffer length required
  maxnumintegrations = 0;
  for(int i=0;i<model->getNumScans();i++) {
    if(model->getScanDuration(i) > config->getExecuteSeconds())
       dur = model->getScanDuration(i);
     else
       dur = config->getExecuteSeconds();
     if(dotrim)
       dur = (snipstartmjd - snipendmjd)*86400.0 + maxdelayoffset - mindelayoffset;
     inttime = config->getIntTime(config->getScanConfigIndex(i));
     if(dur/inttime > maxnumintegrations)
       maxnumintegrations = (int)(dur/inttime + 0.99999); //round up if non-integer
  }

  //bail out if its too long to fit in one big buffer
  if(maxnumintegrations > 100000) {
    cerr << "Due to lazy buffering, can't do huge files yet - aborting!" << endl;
    exit(1);
  }

  //create the visibility buffer
  lastconfigindex = -1;
  tempbuffer = new char[Visibility::HEADER_BYTES + sizeof(cf32)*config->getMaxNumChannels()];
  visibilities = new char****[maxnumintegrations];
  vistimes = new double[maxnumintegrations];
  for(int i=0;i<maxnumintegrations;i++) {
    visibilities[i] = new char***[config->getNumBaselines() + config->getNumDataStreams()];
  }

  //open the input and output files, step through each visibility and delay as appropriate
  input.open(savedifxfile.c_str(), ifstream::binary|ifstream::in);
  output.open(inputdifxfile, ios::trunc|ios::binary);

  startmjd = config->getStartMJD();
  startseconds = config->getStartSeconds() + config->getStartNS()/1.0e9;
  for(int i=0;i<model->getNumScans();i++) {
    if(config->getScanConfigIndex(i) != lastconfigindex) {
      lastconfigindex = config->getScanConfigIndex(i);
      inttime = config->getIntTime(lastconfigindex);
      for(int j=0;j<maxnumintegrations;j++) {
        for(int k=0;k<config->getNumBaselines() + config->getNumDataStreams();k++) {
          visibilities[j][k] = new char**[config->getFreqTableLength()];
          for(int l=0;l<config->getFreqTableLength();l++) {
            fchan = config->getFNumChannels(l)/config->getFChannelsToAverage(l);
            visibilities[j][k][l] = new char*[4];
            for(int m=0;m<4;m++) {
              visibilities[j][k][l][m] = new char[Visibility::HEADER_BYTES + sizeof(cf32)*fchan];
              memset(visibilities[j][k][l][m], 0, Visibility::HEADER_BYTES + sizeof(cf32)*fchan);
            }
          }
        }
      }
    }
    if(i==0) {
      input.read(tempbuffer, Visibility::HEADER_BYTES);
      currentmjd = *((int*)(tempbuffer+12));
      currentseconds  = *((double*)(tempbuffer+16));
      lastseconds = currentseconds;
      offset = int(((currentmjd-startmjd)*86400 + (currentseconds-startseconds))/inttime);
      if(dotrim)
        offset = static_cast<int>((currentmjd + currentseconds/86400.0 - snipstartmjd)/inttime - mindelayoffset);
    }
    int readcount = 0;
    while(!input.eof()) {
      readcount += 1;
      currentmjd = *((int*)(tempbuffer+12));
      currentseconds  = *((double*)(tempbuffer+16));
      currentfreq = *((int*)(tempbuffer+32));
      baseline = *((int*)(tempbuffer+8));
      pol[0] = tempbuffer[36];
      pol[1] = tempbuffer[37];
      if(currentseconds > lastseconds) {
        offset++;
        if(offset > maxnumintegrations) {
          cerr << "Trying to store data that won't fit into allocated buffer... Aborting!" << endl;
          exit(1);
        }
	lastseconds = currentseconds;
      }
      poloffset = 0;
      if(pol[0] == 'L' && pol[1] == 'L')
        poloffset = 1;
      else if(pol[0] == 'R' && pol[1] == 'L')
        poloffset = 2;
      else if(pol[0] == 'L' && pol[1] == 'R')
        poloffset = 3;
      fchan = config->getFNumChannels(currentfreq)/config->getFChannelsToAverage(currentfreq);
      if (offset >= 0) {
        memcpy(visibilities[offset][baselineindices[baseline]][currentfreq][poloffset], tempbuffer, Visibility::HEADER_BYTES);
        input.read(visibilities[offset][baselineindices[baseline]][currentfreq][poloffset] + Visibility::HEADER_BYTES, sizeof(cf32)*fchan);
      }
      else
        input.read(tempbuffer, sizeof(cf32)*fchan); //throw away data we don't want
      input.read(tempbuffer, Visibility::HEADER_BYTES);
    }
    cout << "Read " << readcount << " visibility dumps" << endl;
    cout << "Mindelayoffset is " << mindelayoffset << ", maxdelayoffset is " << maxdelayoffset << endl;
    cout << "maxnumintegrations is " << maxnumintegrations << endl;
    for(int i=0;i<maxnumintegrations-(maxdelayoffset-mindelayoffset);i++) {
      for(int j=0;j<config->getNumBaselines() + config->getNumDataStreams();j++) {
        for(int k=0;k<config->getFreqTableLength();k++) {
          fchan = config->getFNumChannels(k)/config->getFChannelsToAverage(k);
          for(int l=0;l<4;l++) {
            if(*((int*)visibilities[i][j][k][l]) != 0) {
              //fix the time header
              *((double*)(visibilities[i][j][k][l]+16)) -= mindelayoffset*inttime;
              outputpointer = visibilities[i][j][k][l] + Visibility::HEADER_BYTES;
              for(int m=0;m<fchan;m++) {
                copypointer = visibilities[i+dmoffsets[k][m]-mindelayoffset][j][k][l] + Visibility::HEADER_BYTES + m*sizeof(cf32);
		memcpy(outputpointer+m*sizeof(cf32), copypointer, sizeof(cf32));
              }
              output.write(visibilities[i][j][k][l], Visibility::HEADER_BYTES + fchan*sizeof(cf32));
            }
	    //else {
	    //  cout << "Skipping a missing visibility from offset " << i << " baseline " << j << " freq " << k << " pol " << l << endl;
	    //}
          }
        }
      }
    }
  }

  input.close();
  output.close();
}

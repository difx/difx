/***************************************************************************
 *   Copyright (C) 2009 by Adam Deller                                     *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iomanip>
#include <sys/types.h>
#include <dirent.h>
#include <errno.h>
#include <math.h>
#include "difx2profile.h"

using namespace std;

int main(int argc, char *argv[])
{
  bool readok = true;
  double doublesec;
  int configindex, sourceindex;
  char polpair[3];
  char buffer[100];
  double uvw[3];

  njobs = argc-1;
  if(njobs < 1) {
    cerr << "invoke with difx2profile <.input file 1> [.input file 2] ... [.input file n]" << endl;
    return EXIT_FAILURE;
  }

  cout << setprecision << 15 << endl;
  for(int i=1;i<=njobs;i++) {
    cout << "Processing file " << i << "/" << njobs << endl;

    config = new Configuration(argv[i], 0);
    if(config->getNumConfigs() > 1 || !config->pulsarBinOn(0) || config->scrunchOutputOn(0)) {
      cerr << "Error - must be a single config with pulsar binning on - aborting!" << endl;
      return EXIT_FAILURE;
    }

    if(i==1) {
      nbins = config->getNumPulsarBins(0);
      nchannels = config->getFNumChannels(0)/config->getFChannelsToAverage(0);
      if(nbins <= 0) {
        cerr << "Error - the first .input file had no pulsar bins - aborting!" << endl;
        return EXIT_FAILURE;
      }
      for(int j=1;j<config->getFreqTableLength();j++) {
        if(config->getFNumChannels(j)/config->getFChannelsToAverage(j) != nchannels) {
          cerr << "Error - input file " << i << " has different numbers of channels - aborting!!!" << endl;
          return EXIT_FAILURE;
        }
      }
      profile = new double[nbins];
      normprofile = new double[nbins];
      scratch = new double[nbins];
      for(int j=0;j<nbins;j++)
        profile[j] = 0.0;
      visibilities = new float[2*nchannels];
    }
    else {
      for(int j=0;j<config->getFreqTableLength();j++) {
        cerr << "Error - input file " << i << " has different numbers of channels - aborting!!!" << endl;
        return EXIT_FAILURE;
      }
    }
    if (config->getNumPulsarBins(0) != nbins) {
      cerr << "Number of bins for file " << i << " (" << config->getNumPulsarBins(0) << ") does not match initial (" << nbins << ") - aborting" << endl;
      return EXIT_FAILURE;
    }
    startmjd = config->getStartMJD();
    startsec = config->getStartSeconds();
    runsec = config->getExecuteSeconds();
    nextsec = runsec/10;
    runcount = 0;

    double maxvisibility = 0.0;
    int viscount = 0;
    double vissum = 0.0;
    for(int b=0;b<nbins;b++)
    {
      sprintf(buffer, "/DIFX_%05d_%06d.s0000.b%04d", config->getStartMJD(), config->getStartSeconds(), b);
      difxfile = config->getOutputFilename() + string(buffer);
      cout << "About to open file " << difxfile << endl;

      input = new ifstream(difxfile.c_str(), ios::in);
      if(!input->is_open()) {
        cout << "Could not open file " << difxfile << " - aborting!" << endl;
        return EXIT_FAILURE;
      }
      readok = true;
      while(readok && !(input->eof() || input->fail())) {
        readok = config->fillHeaderData(input, baseline, atmjd, doublesec, configindex, sourceindex, freqindex, polpair, bin, weight, uvw);
        atsec = int(doublesec);
        if((atmjd-startmjd)*86400 + atsec-startsec > nextsec) {
          cout << ++runcount << "0% done" << endl;
	  nextsec += runsec/10;
        }
        input->read((char*)visibilities, nchannels*8);
        for(int j=1;j<nchannels-1;j++) {
          if(visibilities[2*j] > maxvisibility)
            maxvisibility = visibilities[2*j];
          profile[bin] += visibilities[2*j];
          vissum += visibilities[2*j];
        }
        viscount++;
        input->peek();
      }
      input->clear();
      input->close();
      delete input;
    }
    cout << "Finished job " << i+1 << "/" << njobs << endl;
    cout << "Max visibility was " << maxvisibility << endl;
    cout << "Number of visibilities (not individual channels) was " << viscount << endl;
    cout << "Mean visibility was " << vissum/(viscount*(nchannels-2)) << endl;
    //delete config;
  }

  cout << "About to normalise" << endl;
  profiletotal = 0;
  //normalise
  double minprofile = profile[0];
  for(int i=0;i<nbins;i++) {
    if(profile[i] < minprofile && profile[i] > 0.0)
      minprofile = profile[i];
    profiletotal += profile[i];
  }
  int ncontributingbins = 0;
  for(int i=0;i<nbins;i++) {
    if(profile[i] > 0.0)
      ncontributingbins++;
  }
  double max = 0.0;
  int maxindex = 0;
  double mean = 1/double(ncontributingbins);
  int numexcised = 0;
  for(int i=0;i<nbins;i++) {
    if(profile[i] > 0.0) {
      normprofile[i] = (profile[i]-minprofile)/(profiletotal-ncontributingbins*minprofile);
      scratch[i] = normprofile[i];
      if(normprofile[i] > max) {
        max = normprofile[i];
	maxindex = i;
      }
    }
  }
  while(max > 2*mean) {
    numexcised++;
    scratch[maxindex] = 0.0;
    max = 0.0;
    mean = 0.0;
    for(int i=0;i<nbins;i++) {
      mean += scratch[i]/double(ncontributingbins-numexcised);
      if(scratch[i] > max) {
        max = scratch[i];
	maxindex = i;
      }
    }
  }
  for(int i=0;i<nbins;i++)
    normprofile[i] = (normprofile[i] - mean)/(1.0-double(ncontributingbins)*mean);
  
  cout << "Number of non-contibuting bins was " << nbins-ncontributingbins << endl;

  cout << "About to write out the profile file" << endl;
  //write out the profile file
  output = new ofstream("profile.out");
  *output << setprecision(15);
  *output << "Bin#       Weight" << endl;
  for(int i=0;i<nbins;i++)
    *output << i << " " << normprofile[i] << endl;
  output->close();
  output->open("unnormalised.out");
  *output << "Bin#       Weight" << endl;
  for(int i=0;i<nbins;i++)
    *output << i << " " << profile[i] << endl;
  output->close();

  return EXIT_SUCCESS;
}


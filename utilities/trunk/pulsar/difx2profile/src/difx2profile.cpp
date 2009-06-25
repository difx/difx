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
  const bool DO_BIN_COUNT = false;
  Polyco ** polycos;
  int ** bins;
  Polyco * currentpolyco;
  double doublesec;
  int numpolycos, numffts;

  njobs = argc-1;
  if(njobs < 1) {
    cerr << "invoke with difx2profile <.input file 1> [.input file 2] ... [.input file n]" << endl;
    return EXIT_FAILURE;
  }

  cout << setprecision << 15 << endl;
  for(int i=1;i<=njobs;i++) {
    cout << "Processing file " << i << "/" << njobs << endl;

    config = new Configuration(argv[i], 0);
    if(config->getNumConfigs() > 1 || !config->pulsarBinOn(0)) {
      cerr << "Error - must be a single config with pulsar binning on - aborting!" << endl;
      return EXIT_FAILURE;
    }

    if(i==1) {
      nbins = config->getNumPulsarBins(0);
      nchannels = config->getNumChannels(0);
      if(nbins <= 0) {
        cerr << "Error - the first .input file had no pulsar bins - aborting!" << endl;
        return EXIT_FAILURE;
      }
      profile = new double[nbins];
      normprofile = new double[nbins];
      scratch = new double[nbins];
      for(int j=0;j<nbins;j++)
        profile[j] = 0.0;
      visibilities = new float[2*nchannels];
    }
    else if (config->getNumPulsarBins(0) != nbins || config->getNumChannels(0) != nchannels) {
      cerr << "Number of bins/channels for file " << i << " (" << config->getNumPulsarBins(0) << "/" << config->getNumChannels(0) << ") does not match initial (" << nbins << "/" << nchannels << ") - aborting" << endl;
      return EXIT_FAILURE;
    }
    if(DO_BIN_COUNT) {
      polycos = config->getPolycos(0);
      numpolycos = config->getNumPolycos(0);
      bins = new int*[config->getFreqTableLength()];
      for(int i=0;i<config->getFreqTableLength();i++)
        bins[i] = new int[config->getNumChannels(0) + 1];
    }
    startmjd = config->getStartMJD();
    startsec = config->getStartSeconds();
    runsec = config->getExecuteSeconds();
    nextsec = runsec/10;
    runcount = 0;

    DIR *dp;
    struct dirent *dirp;
    if((dp  = opendir(config->getOutputFilename().c_str())) == NULL) {
      cout << "Error(" << errno << ") opening " << config->getOutputFilename() << endl;
      return errno;
    }
    dirp = readdir(dp);
    while(dirp != NULL && dirp->d_name[0] == '.')
      dirp = readdir(dp);
    if(dirp == NULL) {
      cerr << "Couldn't find a difx file in directory " << config->getOutputFilename() << " - aborting!!" << endl;
      return EXIT_FAILURE;
    }
    difxfile = config->getOutputFilename() + "/" + string(dirp->d_name);
    cout << "About to open file " << difxfile << endl;
    if(readdir(dp) != NULL) 
      cerr << "Warning - there was more than one file in the directory " + config->getOutputFilename() << endl;

    input = new ifstream(difxfile.c_str(), ios::in);
    if(!input->is_open()) {
      cout << "Could not open file " << difxfile << " - aborting!" << endl;
      return EXIT_FAILURE;
    }
    double maxvisibility = 0.0;
    int viscount = 0;
    double vissum = 0.0;
    int lastatsec = 0;
    while(!(input->eof() || input->fail())) {
      config->getinputline(input, &line, "BASELINE NUM");
      baseline = atoi(line.c_str());
      config->getinputline(input, &line, "MJD");
      atmjd = atoi(line.c_str());
      config->getinputline(input, &line, "SECONDS");
      atsec = atoi(line.c_str());
      doublesec = atof(line.c_str());
      if((atmjd-startmjd)*86400 + atsec-startsec > nextsec) {
        cout << ++runcount << "0% done" << endl;
	nextsec += runsec/10;
      }
      config->getinputline(input, &line, "CONFIG INDEX:");
      config->getinputline(input, &line, "SOURCE INDEX:");
      config->getinputline(input, &line, "FREQ INDEX:");
      freqindex = atoi(line.c_str());
      if(DO_BIN_COUNT) {
        if(lastatsec != atsec) {
          currentpolyco = Polyco::getCurrentPolyco(0, atmjd, (doublesec - config->getIntTime(0)/2.0)/86400.0, polycos, numpolycos, false);
	  currentpolyco->setTime(atmjd, (doublesec-config->getIntTime(0)/2.0)/86400.0);
	  lastatsec = atsec;
	}
	numffts = int(0.5+10000000*config->getIntTime(0)/(config->getNumChannels(0)/config->getDBandwidth(0,0,0)));
	for(int i=0;i<numffts;i++)
	  currentpolyco->getBins(i*config->getIntTime(0)/numffts, bins);
      }
      config->getinputline(input, &line, "POLARISATION PAIR:");
      polpair = line;
      config->getinputline(input, &line, "PULSAR BIN:");
      bin = atoi(line.c_str());
      config->getinputline(input, &line, "FLAGGED:");
      config->getinputline(input, &line, "DATA WEIGHT:");
      weight = atof(line.c_str());
      config->getinputline(input, &line, "U (METRES):");
      config->getinputline(input, &line, "V (METRES):");
      config->getinputline(input, &line, "W (METRES):");
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
    cout << "Finished file " << i << "/" << njobs << endl;
    cout << "Max visibility was " << maxvisibility << endl;
    cout << "Number of visibilities (not individual channels) was " << viscount << endl;
    cout << "Mean visibility was " << vissum/(viscount*(nchannels-2)) << endl;
    input->clear();
    input->close();
    delete input;
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
  if(DO_BIN_COUNT) {
    int *** bincounts;
    int * overallbincounts = new int[nbins];
    bincounts = currentpolyco->getBinCounts();
    for(int j=0;j<nbins;j++) {
      for(int k=0;k<config->getFreqTableLength();k++) {
        for(int l=1;l<config->getNumChannels(0)-1;l++)
          overallbincounts[j] += bincounts[j][k][l];
      }
    }
    for(int i=0;i<nbins;i++) 
      cout << "Overall bin count " << i << " was " << overallbincounts[i] << endl;
  }

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


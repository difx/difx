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

#include "difx2profile.h"
#include "math.h"

using namespace std;

int main(int argc, char *argv[])
{
  njobs = argc-1;
  if(njobs < 1) {
    cerr << "invoke with difx2profile <.input file 1> [.input file 2] ... [.input file n]" << endl;
    return EXIT_FAILURE;
  }

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
      visibilities = new float[2*nchannels];
    }
    else if (config->getNumPulsarBins(0) != nbins || config->getNumChannels(0) != nchannels) {
      cerr << "Number of bins/channels for file " << i << " (" << config->getNumPulsarBins(0) << "/" << config->getNumChannels(0) << ") does not match initial (" << nbins << "/" << nchannels << ") - aborting" << endl;
      return EXIT_FAILURE;
    }

    input = new ifstream(config->getOutputFilename().c_str());
    input->clear();
    while(!(input->eof() || input->fail())) {
      config->getinputline(input, &line, "BASELINE NUM");
      baseline = atoi(line.c_str());
      config->getinputline(input, &line, "MJD");
      config->getinputline(input, &line, "SECONDS");
      config->getinputline(input, &line, "CONFIG INDEX:");
      config->getinputline(input, &line, "SOURCE INDEX:");
      config->getinputline(input, &line, "FREQ INDEX:");
      config->getinputline(input, &line, "POLARISATION PAIR:");
      config->getinputline(input, &line, "PULSAR BIN:");
      bin = atoi(line.c_str());
      config->getinputline(input, &line, "FLAGGED:");
      config->getinputline(input, &line, "DATA WEIGHT:");
      config->getinputline(input, &line, "U (METRES):");
      config->getinputline(input, &line, "V (METRES):");
      config->getinputline(input, &line, "W (METRES):");
      if(!baseline % 257 == 0) { //not a real autocorrelation
        input->read((char*)visibilities, nchannels*8);
        for(int j=1;j<nchannels-1;j++)
          profile[bin] += visibilities[2*j];
      }
      input->peek();
    }
    input->clear();
    input->close();
    delete input;
    delete config;
  }

  profiletotal = 0;
  //normalise
  for(int i=0;i<nbins;i++)
    profiletotal += profile[i];
  for(int i=0;i<nbins;i++)
    profile[i] /= profiletotal;

  //write out the profile file
  output = new ofstream("profile.out");
  *output << "Bin#       Weight" << endl;
  for(int i=0;i<nbins;i++)
    *output << i << " " << profile[i] << endl;
  output->close();

  return EXIT_SUCCESS;
}


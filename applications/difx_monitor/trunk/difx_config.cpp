/***************************************************************************
 *   Copyright (C) 2006 by Adam Deller                                     *
 *                                                                         *
 *   This program is free for non-commercial use: see the license file     *
 *   at http://astronomy.swin.edu.au:~adeller/software/difx/ for more      *
 *   details.                                                              *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *
 ***************************************************************************/
#include "configuration.h"
#include <iomanip>
#include <sstream>
#include <iostream>

using namespace std;

int main(int argc, const char * argv[]) {
  Configuration * config;
  int freqindex, iconfig;
  char polpair[3];
  string sourcename;
  ostringstream ss;

  polpair[2] = 0;

  if(argc != 2)
  {
    cerr << "Usage: difx_config <inputfile> " << endl;
    return EXIT_FAILURE;
  }

  config = new Configuration(argv[1], 0);
  if (!config->consistencyOK()) {
    cout << "Aborting" << endl;
    exit(1);
  }

  int prod=0;
  for (iconfig=0; iconfig<config->getNumConfigs(); iconfig++) {
    if (config->getNumConfigs()>1) {
      cout << endl << "*********** CONFIG " << iconfig << " ***********" << endl << endl;;
    }

    cout << "Integration time  " << config->getIntTime(iconfig) << endl << endl;;

    int nphasecentres = config->getMaxPhaseCentres(iconfig);
    int binloop = 1;
    if(config->pulsarBinOn(iconfig) && !config->scrunchOutputOn(iconfig))
      binloop = config->getNumPulsarBins(iconfig);

    for (int i=0;i<config->getNumBaselines();i++) {
      int ds1index = config->getBDataStream1Index(iconfig, i);
      int ds2index = config->getBDataStream2Index(iconfig, i);

      for(int j=0;j<config->getBNumFreqs(iconfig,i);j++) {
	freqindex = config->getBFreqIndex(iconfig, i, j);
	int freqchannels = config->getFNumChannels(freqindex)/config->getFChannelsToAverage(freqindex);
	int npols = config->getBNumPolProducts(iconfig, i, j);
	for(int k=0;k<npols;k++) {
	  config->getBPolPair(iconfig,i,j,k,polpair);
	    
	  if (binloop*nphasecentres==1) {
	    cout << setw(3) << prod <<": ";
	  } else {
	    ostringstream oss;
	    oss << prod << "-" << prod+binloop*nphasecentres*npols-npols << " ";
	    cout << right << setw(10) << oss.str();
	  }

	  ss << config->getTelescopeName(ds1index) << "-" 
	     << config->getTelescopeName(ds2index);
	  cout << left << setw(15) << ss.str() << right;
	  ss.str("");

	  cout << " " << polpair << " ";

	  cout  << config->getFreqTableFreq(freqindex) << " MHz";
	    
	  cout << " (" << config->getFreqTableBandwidth(freqindex) << " MHz/" << freqchannels << ")";

	  if (binloop>1) {
	    cout << "  " << binloop << " bins";
	    if (nphasecentres>1) cout << "x";
	  }
	  if (nphasecentres>1) cout << "  " << nphasecentres << " Phase Centres";
	  cout << endl;
	  prod++;
	}
	prod += binloop*nphasecentres*npols-npols;
      }
    }
  

    int autocorrwidth = (config->getMaxProducts()>2)?2:1;
    
    for(int i=0;i<config->getNumDataStreams();i++) {
      for(int j=0;j<autocorrwidth;j++) {
	for(int k=0;k<config->getDNumRecordedBands(iconfig, i); k++) {
	  
	  cout << setw(7) << prod << ": " << left << setw(15) 
	       << config->getDStationName(iconfig, i) << " " << right;
	  
	  char firstpol = config->getDRecordedBandPol(iconfig, i, k);
	  char otherpol = ((firstpol == 'R')?'L':'R');
	  if (j==0)
	    cout << firstpol << firstpol;
	  else
	    cout << firstpol << otherpol;
	  
	  freqindex = config->getDRecordedFreqIndex(iconfig, i, k);
	  cout <<  " " << config->getFreqTableFreq(freqindex) << " MHz";
	  
	  cout << " (" << config->getFreqTableBandwidth(freqindex) << " MHz)";
	  
	  cout << endl;
	  
	  prod++;
	}
      }
    }
  }
}


#include <iostream>
#include <cstdlib>
#include <string>
#include <stdio.h>
#include <fstream>
#include <math.h>
#include "configuration.h"

using namespace std;

int main(int argc, char** argv)
{
  if(argc != 3)
  {
    cerr << "Usage: printDiFX <difx data file> <config file>" << endl;
    return EXIT_FAILURE;
  }

  string line;
  char pol[3];
  pol[2] = 0;
  bool badvisibility, goodsync;
  double uvw[3];
  int baseline, mjd, confindex, srcindex, freqindex, pbin, numchannels;
  double weight,sec;
  string filename = argv[1];
  int sepindex = filename.find_last_of('.');
  int numvispoints = atoi(filename.substr(sepindex+1).c_str());
  cout << "About to create the configuration - num vis points is " << numvispoints << endl;
  Configuration * config = new Configuration(argv[2], 0);
  cout << "Created the config" << endl;
  int maxnumchannels = config->getMaxNumChannels();
  float * visibilities = new float[maxnumchannels*2];

  ifstream difxin(argv[1]);

  cout << "About to start reading" << endl;
  while (!(difxin.eof())) {
    goodsync = config->fillHeaderData(&difxin, baseline, mjd, sec, confindex, srcindex, freqindex, pol, pbin, weight, uvw);
    if(!goodsync) {
      cout << "BAD SYNC! trying again" << endl;
      continue;
    }

    numchannels = config->getFNumChannels(freqindex)/config->getFChannelsToAverage(freqindex);
    difxin.read((char*)visibilities, numchannels*2*sizeof(float));
    badvisibility = false;
    for(int i=0;i<numchannels*2;i++) {
      if(visibilities[i] > 1000.0 || visibilities[i] < -1000 ||
         isinf(visibilities[i]) || isnan(visibilities[i]))
        badvisibility = true;
    }

    if(badvisibility)
    {
      cout << "NAUGHTY visibility! The full header was baseline " << baseline << ", at time " << mjd << "/" << sec << ", the source was " << srcindex << " and the config was " << confindex << ", with uvw (" << uvw[0] << "," << uvw[1] << "," << uvw[2] << "). For freq " << freqindex << ", pol " << pol << ", pulsar bin " << pbin << ", weight " << weight << ", we have:" << endl;
      cout << "NAUGHTY visibilities are: ";
      for(int i=0;i<numchannels;i++)
        cout << visibilities[2*i] << " + " << visibilities[2*i+1] << " i; ";
      cout << endl;
    }
    else;
      cout << "For baseline " << baseline << ", at time " << mjd << "/" << sec << ", the source was " << srcindex << " and the config was " << confindex << ", with uvw (" << uvw[0] << "," << uvw[1] << "," << uvw[2] << ").  For freq " << freqindex << ", pol " << pol << ", pulsar bin " << pbin << ", there are " << numchannels << " channels; visibilities[0] = " << visibilities[0] << " + " << visibilities[1] << " i.  The middle visibility is " << visibilities[numchannels] << " + " << visibilities[numchannels + 1] << " i." << ", and the weight was " << weight << endl;
  }

  return 0;
}

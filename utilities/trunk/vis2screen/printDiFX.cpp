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
  double uvw[3];
  int baseline, mjd, confindex, srcindex, freqindex, pbin, flag, numchannels;
  double u,v,w,weight,sec;
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
    config->fillHeaderData(&difxin, baseline, mjd, sec, confindex, srcindex, freqindex, pol, pbin, weight, uvw);

    numchannels = config->getFNumChannels(freqindex)/config->getFChannelsToAverage(freqindex);
    difxin.read((char*)visibilities, numchannels*2*sizeof(float));

    cout << "For baseline " << baseline << ", at time " << mjd << "/" << sec << ", the source was " << srcindex << " and the config was " << confindex << ", with uvw (" << u << "," << v << "," << w << ").  For freq " << freqindex << ", pol " << pol << ", pulsar bin " << pbin << ", there are " << numchannels << " channels; visibilities[0] = " << visibilities[0] << " + " << visibilities[1] << " i.  The middle visibility is " << visibilities[numchannels] << " + " << visibilities[numchannels + 1] << " i." << ", and the weight was " << weight << endl;
  }
}

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

  string line, pol;
  int baseline, mjd, sec, confindex, srcindex, freqindex, pbin, flag, numchannels;
  double u,v,w,weight;
  string filename = argv[1];
  int sepindex = filename.find_last_of('.');
  int numvispoints = atoi(filename.substr(sepindex+1).c_str());
  cout << "About to create the configuration - num vis points is " << numvispoints << endl;
  Configuration * config = new Configuration(argv[2], 0);
  cout << "Created the config" << endl;
  int maxnumchannels = 0;
  for(int i=0;i<config->getNumConfigs();i++) {
    if(config->getNumChannels(i) > maxnumchannels)
      maxnumchannels = config->getNumChannels(i);
  }
  float * visibilities = new float[maxnumchannels*2];

  ifstream difxin(argv[1]);

  cout << "About to start reading" << endl;
  while (!(difxin.eof())) {
    getline(difxin, line);
    cout << "Read line " << line << endl;
    baseline = atoi(line.substr(20).c_str());
    getline(difxin, line);
    mjd = atoi(line.substr(20).c_str());
    getline(difxin, line);
    sec = atoi(line.substr(20).c_str());
    getline(difxin, line);
    confindex = atoi(line.substr(20).c_str());
    getline(difxin, line);
    srcindex = atoi(line.substr(20).c_str());
    getline(difxin, line);
    freqindex = atoi(line.substr(20).c_str());
    getline(difxin, line);
    pol = line.substr(20);
    getline(difxin, line);
    pbin = atoi(line.substr(20).c_str());
    getline(difxin, line);
    flag = atoi(line.substr(20).c_str());
    getline(difxin, line);
    weight = atof(line.substr(20).c_str());
    getline(difxin, line);
    u = atof(line.substr(20).c_str());
    getline(difxin, line);
    v = atof(line.substr(20).c_str());
    getline(difxin, line);
    w = atof(line.substr(20).c_str());

    numchannels = config->getNumChannels(confindex);
    cout << "W line was " << line << ", numchannels is " << numchannels << endl;
    difxin.read((char*)visibilities, numchannels*2*sizeof(float));

    cout << "For baseline " << baseline << ", at time " << mjd << "/" << sec << ", the source was " << srcindex << " and the config was " << confindex << ", with uvw (" << u << "," << v << "," << w << ").  For freq " << freqindex << ", pol " << pol << ", pulsar bin " << pbin << ", we have visibilities[0] = " << visibilities[0] << " + " << visibilities[1] << " i.  The middle visibility is " << visibilities[numchannels] << " + " << visibilities[numchannels + 1] << " i." << ", and the weight was " << weight << endl;
    //ofstream asciiout("ascii.out", ios::trunc);
    //for(int j=0;j<numchannels;j++)
    //  asciiout << j << " " << sqrt(visibilities[j*2]*visibilities[j*2] + visibilities[j*2+1]*visibilities[j*2+1]) << " " << atan2(visibilities[2*j+1], visibilities[2*j]) << endl;
    //return EXIT_SUCCESS;
  }
}

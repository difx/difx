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
  if(argc != 4)
  {
    cerr << "Usage: difx2fb <difx data file> <config file> <fb output file>" << endl;
    return EXIT_FAILURE;
  }

  string line;
  char pol[3];
  pol[2] = 0;
  bool badvisibility, goodsync;
  double uvw[3];
  int baseline, mjd, confindex, srcindex, freqindex, pbin, numchannels;
  double weight,sec;
  Configuration * config = new Configuration(argv[2], 0);
  cout << "Created the config" << endl;
  Model * model = config->getModel();
  int dsindex, scansec, scanns, bandindex, lastscan = 0;
  int maxnumchannels = config->getMaxNumChannels();
  float *visibilities;
  int data[10];
  char autocorrs[9000];

  //test that the config conforms to the limited scope of this program
  if(config->getNumDataStreams() != config->getTelescopeTableLength()) {
    cerr << "Num datastreams != telescope table length!" << endl;
    return EXIT_FAILURE;
  }
  if(config->getNumConfigs()>1) {
    cerr << "Num configs > 1 (=" << config->getNumConfigs() << ")!" << endl;
    return EXIT_FAILURE;
  }
  for(int i=0;i<config->getNumDataStreams();i++) {
    if(config->getDTelescopeIndex(0, i) != i) {
      cerr << "Datastream " << i << " does not map to telescope " << i << endl;
      return EXIT_FAILURE;
    }
  }

  ifstream difxin(argv[1]);
  ofstream fbout(argv[3], ios::trunc);
  visibilities = new float[maxnumchannels*2];

  cout << "About to start reading" << endl;
  while (!(difxin.eof())) {
    goodsync = config->fillHeaderData(&difxin, baseline, mjd, sec, confindex, srcindex, freqindex, pol, pbin, weight, uvw);
    if(!goodsync) {
      cout << "BAD SYNC! trying again" << endl;
      continue;
    }
    numchannels = config->getFNumChannels(freqindex)/config->getFChannelsToAverage(freqindex);
    difxin.read(autocorrs, numchannels*2*sizeof(float));
    if(baseline % 257 != 0) {
      continue; //skip all cross correlations
    }
    dsindex = baseline/257 - 1;
    while(model->getScanEndMJD(lastscan) < mjd + sec/86400.0)
      lastscan++;
    scansec = int((mjd + sec/86400.0 - model->getScanStartMJD(lastscan))*86400.0);
    scanns = int(((mjd + sec/86400.0 - model->getScanStartMJD(lastscan))*86400.0 - scansec)*1000000000.0);
    bandindex = -1;
    for(int i=0;i<config->getDNumRecordedBands(0,dsindex);i++) {
      if(config->getDRecordedFreqIndex(0,dsindex,i) == freqindex && config->getDRecordedBandPol(0,dsindex,i) == pol[0] && pol[0] == pol[1])
        bandindex = i;
    }
    if (bandindex == -1) {
      cerr << "Couldn't find appropriate band!" << endl;
      delete [] visibilities;
      return EXIT_FAILURE;
    }
    data[0] = MAX_U32;
    data[1] = dsindex; //only right in simple cases! But we checked this was true.
    data[2] = lastscan;
    data[3] = scansec;
    data[4] = scanns; 
    data[5] = int(config->getIntTime(0)*1e9); //nswidth
    data[6] = bandindex;
    data[7] = numchannels;
    data[8] = 0; //coreindex
    data[9] = 0; //threadindex
    for(int i=1;i<numchannels;i++)
      autocorrs[i] = autocorrs[2*i];
    fbout.write((char *)data, sizeof(data));
    fbout.write(autocorrs, numchannels*sizeof(float));
    /*badvisibility = false;
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
    */
  }

  delete [] visibilities;

  difxin.close();
  fbout.close();

  return EXIT_SUCCESS;
}

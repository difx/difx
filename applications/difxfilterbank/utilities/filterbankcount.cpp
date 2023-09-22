#include <fstream>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "architecture.h"
#include "configuration.h"

using namespace std;

//variables
const int MAX_CHANNELS=4096;
static const u32 SYNC = MAX_U32;
double dumptime, freq, maxy, inttimems;
int dsindex, nchan, bandindex, threadindex, coreindex, nthreads, configindex;
string pol, antennaname;
bool verbose;
float data[MAX_CHANNELS];
double ** timecount;
ifstream * input;
Configuration * config;
Model * model;

//prototypes
bool getData(ifstream * input, bool verbose);

int main(int argc, char *argv[])
{
  //check for correct invocation
  if(argc != 3 && argc != 4) {
    cout << "Error - invoke with filterbankcount <dump file> <correlator config file> [verbose]" << endl;
    return EXIT_FAILURE;
  }
  verbose = false;
  if(argc == 4)
    verbose = true;

  cout << "Warning - sets up assuming first config only!" << endl;

  //open the file containing the filterbank data
  input = new ifstream(argv[1], ios::binary);
  config = new Configuration(argv[2], -1);
  model = config->getModel();

  //create the array to hold the timecount
  timecount = new double*[config->getNumDataStreams()];
  for(int i=0;i<config->getNumDataStreams();i++) {
    timecount[i] = new double[config->getDNumRecordedBands(0, i)];
    for(int j=0;j<config->getDNumRecordedBands(0, i);j++)
      timecount[i][j] = 0.0;
  }

  cout << "About to start counting" << endl;

  //loop through counting the data
  while (!input->eof()) {
    if(getData(input, verbose)) {
      timecount[dsindex][bandindex] += inttimems/1000.0;
    }
  }

  //print out the results
  for(int i=0;i<config->getNumDataStreams();i++) {
    for(int j=0;j<config->getDNumRecordedBands(0, i);j++) {
      cout << "Completeness for dsindex " << i << ", bandindex " << j << " was " << 100*timecount[i][j]/config->getExecuteSeconds() << "%" << endl;
    }
  }
}

bool getData(ifstream * input, bool verbose)
{
  unsigned int sync;
  int configindex, scan, sec, ns, nswidth, extrachan = 0;

  input->read((char*)&sync, sizeof(int));
  while(sync != SYNC && !input->eof()) {
    cout << "Searching for lost SYNC!" << endl;
    input->read((char*)&sync, sizeof(int));
  }
  if (input->eof())
    return false;
  input->read((char*)&dsindex, sizeof(int));
  input->read((char*)&scan, sizeof(int));
  input->read((char*)&sec, sizeof(int));
  input->read((char*)&ns, sizeof(int));
  input->read((char*)&nswidth, sizeof(int));
  input->read((char*)&bandindex, sizeof(int));
  input->read((char*)&nchan, sizeof(int));
  input->read((char*)&coreindex, sizeof(int));
  input->read((char*)&threadindex, sizeof(int));

  //figure out the MJD (including fractional component) for this dump and the freq/pol/antname
  dumptime = model->getScanStartMJD(scan) + ((double)sec)/86400.0 + ((double)ns)/86400000000000.0;
  inttimems = ((double)nswidth)/1000000.0;
  configindex = config->getScanConfigIndex(scan);
  freq = config->getFreqTableFreq(config->getDRecordedFreqIndex(configindex, dsindex, bandindex));
  pol = config->getDRecordedBandPol(configindex, dsindex, bandindex);
  antennaname = config->getDStationName(configindex, dsindex);

  if(verbose) {
    cout << "Data from core " << coreindex << ", thread " << threadindex << ", MJD " << int(dumptime);
    cout << ", sec " << static_cast<int>(86400*(dumptime-int(dumptime)));
    cout << ", ns " << ns << endl;
  }

  if(nchan > MAX_CHANNELS) {
    cout << "nchan=" << nchan << " is greater than MAX_CHANNELS=" << MAX_CHANNELS;
    cout << "Will only read the first MAX_CHANNELS channels" << endl;
    extrachan = nchan-MAX_CHANNELS;
    nchan = MAX_CHANNELS;
  }
  input->read((char*)data, sizeof(int)*nchan);
  if(extrachan > 0)
    input->ignore(extrachan);
  return true;
}

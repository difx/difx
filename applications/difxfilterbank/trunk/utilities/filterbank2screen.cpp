#include <fstream>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <unistd.h>
#include <plplot/plplot.h>
#include "architecture.h"
#include "configuration.h"

using namespace std;

//variables
const int MAX_CHANNELS=4096;
static const u32 SYNC = MAX_U32;
double dumptime, freq, inttimems;
float miny, maxy;
int dsindex, nchan, bandindex, threadindex, coreindex, nthreads, configindex;
string pol, antennaname;
float data[MAX_CHANNELS];
PLFLT ddata[MAX_CHANNELS];	// because plplot may want to operate on doubles
PLFLT xaxis[MAX_CHANNELS];
char title[256];
char junk[256];
ifstream * input;
Configuration * config;
Model * model;

//prototypes
bool getData(ifstream * input);
void getMinMax(float * data, int length, float * min, float * max);

int main(int argc, char *argv[])
{
  //check for correct invocation
  if(argc != 3) {
    cout << "Error - invoke with difxfilterbank <dump file> <correlator config file>" << endl;
    return EXIT_FAILURE;
  }

  //open the file containing the filterbank data
  input = new ifstream(argv[1], ios::binary);
  config = new Configuration(argv[2], -1);
  model = config->getModel();

  //set up plplot and the xaxis data
  plstart("xwin", 1, 1);
  plspause(0);
  for(int i=0;i<MAX_CHANNELS;i++)
    xaxis[i] = i;

  //loop through inspecting the data
  while (!input->eof()) {
    if(getData(input)) {
      getMinMax(data, nchan, &miny, &maxy);
      if(maxy != 0.0 || miny != 0.0) {
        sprintf(title, "Antenna=%s, Freq=%8.2f, Pol=%s, Time=%f, Int time (ms)=%5.2f, CoreID=%i, ThreadID=%i", antennaname.c_str(), freq, pol.c_str(), dumptime, inttimems, coreindex, threadindex);
        //cpgpage();
        plenv(0.0, static_cast<float>(nchan), miny, maxy, 0, 1);
        pllab("Channel number", "Unnormalised amplitude", title);
	for(int q = 0; q < nchan; ++q) {
	  ddata[q] = data[q];
	}
        plline(nchan, xaxis, ddata);
        usleep(100000);
      }
      else {
        cout << "Skipping data since its all empty..." << endl;
      }
    }
  }
}

void getMinMax(float * data, int length, float * min, float * max)
{
  *min = data[0];
  *max = data[0];
  for (int i=1;i<length;i++) {
    if(data[i] > *max)
      *max = data[i];
    if(data[i] < *min)
      *min = data[i];
  }
}

bool getData(ifstream * input)
{
  u32 sync;
  int configindex, scan, sec, ns, nswidth, extrachan = 0;

  cout << "About to start another block" << endl;
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
  cout << "NSwidth is " << nswidth << endl; 

  //figure out the MJD (including fractional component) for this dump and the freq/pol/antname
  dumptime = model->getScanStartMJD(scan) + ((double)sec)/86400.0 + ((double)ns)/86400000000000.0;
  inttimems = ((double)nswidth)/1000000.0;
  configindex = config->getScanConfigIndex(scan);
  freq = config->getFreqTableFreq(config->getDRecordedFreqIndex(configindex, dsindex, bandindex));
  pol = config->getDRecordedBandPol(configindex, dsindex, bandindex);
  antennaname = config->getDStationName(configindex, dsindex);

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

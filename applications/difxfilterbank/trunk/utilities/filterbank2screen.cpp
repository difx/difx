#include "cpgplot.h"
#include <fstream>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

using namespace std;

//variables
const int MAX_CHANNELS=4096;
const int SYNC=4294967295;
float maxy, dumptime;
int antID, nchan, bandID, threadID;
float data[MAX_CHANNELS];
float xaxis[MAX_CHANNELS];
char title[256];
char junk[256];
ifstream * input;

//prototypes
bool getData(ifstream * input);
float getMax(float * data, int length);

int main(int argc, char *argv[])
{
  //check for correct invocation
  if(argc != 2) {
    cout << "Error - invoke with difxfilterbank <dump file>" << endl;
    return EXIT_FAILURE;
  }

  //open the file containing the filterbank data
  input = new ifstream(argv[1], ios::binary);

  //set up pgplot and the xaxis data
  if(cpgbeg(0, "?", 1, 1) != 1)
    exit(EXIT_FAILURE);
  cpgask(0);
  for(int i=0;i<MAX_CHANNELS;i++)
    xaxis[i] = i;

  //loop through inspecting the data
  while (!input->eof()) {
    if(getData(input)) {
      maxy = getMax(data, nchan);
      if(maxy > 0.0) {
        sprintf(title, "Antenna ID=%i, Band ID=%i, Thread ID=%i, Time=%f", antID, bandID, threadID, dumptime);
        cpgpage();
        cpgenv(0.0, (float)nchan, 0.0, maxy, 0, 1);
        cpglab("Channel number", "Unnormalised amplitude", title);
        cpgline(nchan, xaxis, data);
        //cout << "Enter \"y\" to continue" << endl;
        //cin >> junk;
        usleep(100000);
      }
      else {
        cout << "Skipping data since its all empty..." << endl;
      }
    }
  }
}

float getMax(float * data, int length)
{
  float max = data[0];
  for (int i=1;i<length;i++) {
    if(data[i] > max)
      max = data[i];
  }
  return max;
}

bool getData(ifstream * input)
{
  int sync, s, ns, extrachan = 0;

  cout << "About to start another block" << endl;
  input->read((char*)&sync, sizeof(int));
  while(sync != SYNC && !input->eof()) {
    cout << "Searching for lost SYNC!" << endl;
    input->read((char*)&sync, sizeof(int));
  }
  if (input->eof())
    return false;
  input->read((char*)&antID, sizeof(int));
  input->read((char*)&s, sizeof(int));
  input->read((char*)&ns, sizeof(int));
  input->read((char*)&threadID, sizeof(int));
  input->read((char*)&bandID, sizeof(int));
  input->read((char*)&nchan, sizeof(int));
  //cout << "Seconds was " << s << ", ns was " << ns << endl;
  dumptime = (float)s + ((float)ns)/1000000000.0;
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

#include <iostream>
#include <fstream>
#include <cstdlib>
#include <string>
#include <stdio.h>
#include <stdlib.h>
#include <iomanip>
#include "vlba_stream.h"

using namespace std;

int main(int argc, char ** argv)
{
  if(argc != 4 && argc != 5)
  {
    cerr << "Error - invoke with mk5check <mk5filename> <numbits> <fanout> [offset bytes]" << endl;
    return EXIT_FAILURE; //note EXIT from program
  }
  long long offset = 0;
  if(argc == 5)
    offset = atol(argv[4]);

  VLBA_stream * vs = VLBA_stream_open(argv[1],
        atoi(argv[2]), atoi(argv[3]), offset);

  if(vs == 0)
    cout << "Error - could not open file " << argv[1] << endl;
  else
  {
    cout << "For file " << argv[1] << ", the following parameters were found:" << endl;
    cout << "Sampling rate was " << vs->samprate << endl;
    cout << "MJD was " << vs->mjd << endl;
    cout << "Seconds were " << int(vs->sec) << endl;
    cout << "Milliseconds were " << vs->sec - int(vs->sec) << endl;
    cout << "Tracks was " << vs->tracks << endl;
    cout << "Numbers of channels is " << vs->nchan << endl;
    cout << "Format was " << vs->format << endl;
  }
}

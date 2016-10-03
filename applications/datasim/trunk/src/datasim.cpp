/*****************************************************************************
*    <DataSim: VLBI data simulator>                                          * 
*    Copyright (C) <2015> <Zheng Meyer-Zhao>                                 *
*                                                                            *
*    This file is part of DataSim.                                           *
                                                                             *
*    DataSim is free software: you can redistribute it and/or modify         *
*    it under the terms of the GNU General Public License as published by    *
*    the Free Software Foundation, either version 3 of the License, or       *
*    (at your option) any later version.                                     *
*                                                                            *
*    DataSim is distributed in the hope that it will be useful,              *
*    but WITHOUT ANY WARRANTY; without even the implied warranty of          *
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *
*    GNU General Public License for more details.                            *
*                                                                            *
*    You should have received a copy of the GNU General Public License       *
*    along with this program.  If not, see <http://www.gnu.org/licenses/>.   *
*****************************************************************************/

#include <iostream>
#include <cstdlib>
#include <unistd.h>
#include <vector>
#include <string>
#include <gsl/gsl_randist.h>
#include <getopt.h>
#include "configuration.h"
#include "datasim.h"
#include "util.h"
#include "subband.h"
#include "vdifio.h"
#include "model.h"
#include "vdifzipper.h"

using namespace std;

static void usage(int argc, char **argv)
{
  cout << endl;
  cout << "Usage:  " << argv[0] << " [<options>] <input file>" << endl;
  cout << endl;
  cout << "  options can include:" << endl;
  cout << "     -h" << endl;
  cout << "     --help        display this information and quit." << endl;
  cout << endl;
  cout << "     -f" << endl;
  cout << "     --flux        source flux density in Jansky (default is 1 Jy)." << endl;
  cout << endl;
  cout << "     -s" << endl;
  cout << "     --sefd        antenna SEFDs in a comma-seperated list in Jansky.\n"
       << "                   If there are more antennas than provided SEFDs,\n"
       << "                   SEFD of the remaining antennas is set to 1000 Jansky." << endl;
  cout << endl;
  cout << "     -d" << endl;
  cout << "     --seed        random number generator seed." << endl;
  cout << endl;
  cout << "     -v" << endl;
  cout << "     --verbose     increase the verbosity of the output; -v -v for more." << endl;
  cout << endl;
  cout << "     -t" << endl;
  cout << "     --test        run in test mode, generate 1 second data for each station,\n"
       << "                   no matter what's given in the configuration file." << endl;
  cout << endl;
}

static void cmdparser(int argc, char* argv[], setup &setupinfo)
{
  char tmp;
  static struct option long_options[] = {
    {"help",    no_argument,        0,  'h'},
    {"flux",    required_argument,  0,  'f'},
    {"sefd",    required_argument,  0,  's'},
    {"seed",    required_argument,  0,  'd'},
    {"verbose", no_argument,        0,  'v'},
    {"test",    no_argument,        0,  't'},
    {0,         0,                  0,   0 }
  };
  int long_index = 0;
  while((tmp=getopt_long(argc,argv,"hf:s:d:vt",
              long_options, &long_index)) != -1)
  {
    switch(tmp)
    {
      case 'h':
        usage(argc, argv);
        exit (EXIT_SUCCESS);
        break;
      case 'f':
        if(*optarg == '-' || atof(optarg) == 0.0)
        {
          cerr << "Option -f requires a non-zero floating-point number as argument." << endl;
          exit (EXIT_FAILURE);
        }
        else
          setupinfo.sfluxdensity = atof(optarg);
        break;
      case 's':
        if(*optarg == '-' || *optarg == ' ')
        {
          cerr << "Option -s requires a comma-separated list of unsigned integer as argument." << endl;
          exit (EXIT_FAILURE);
        }
        else
        {
          istringstream ss(optarg);
          string token;
          while(getline(ss, token, ','))
            setupinfo.antSEFDs.push_back(atoi(token.c_str()));
        }
        break;
      case 'd':
        if(*optarg == '-' || *optarg == ' ')
        {
          cerr << "Option -d requires an unsigned integer as argument." << endl;
          exit (EXIT_FAILURE);
        }
        else
        {
          setupinfo.seed = atoi(optarg);
        }
        break;
      case 'v':
        setupinfo.verbose++;
        break;
      case 't':
      setupinfo.test = 1;
      break;
      default:
        usage(argc, argv);
        exit (EXIT_FAILURE);
        break;
    }
  }
  // check if input VDIF file name is given
  if(optind > argc - 1)
  {
    cerr << "No .input file provided, nothing to do ..." << endl;
    usage(argc, argv);
    exit (EXIT_FAILURE);
  }
  else if(optind < argc - 1)
  {
    cerr << "Multiple input files provided, only one is expected." << endl;
    usage(argc, argv);
    exit (EXIT_FAILURE);
  }
  else
  {
    setupinfo.inputfilename = argv[optind];
  }
}

int main(int argc, char* argv[])
{
  setup setupinfo;
  setupinfo.verbose = 0;
  setupinfo.test = 0;
  setupinfo.seed = SEED;
  setupinfo.sfluxdensity = 1;     // source flux density in Jansky
  setupinfo.inputfilename = "";   // .input file name

  // parse command line argument
  cmdparser(argc, argv, setupinfo);

  float tdur = 0.5 * 1e6;
  float testdur = 1; // time duration of test mode
  Configuration* config;
  Model* model;
  float dur;                            // duration of the simulated data in seconds
  float durus;                          // duration of the simulated data in microseconds
  float specRes, minStartFreq;
  int numdatastreams, numrecordedbands, freqindex;
  int numSamps;                         // number of samples to be generated per time block for the common signal
  int stime;                            // step time in microsecond == time block
  int configindex = 0;
  cf32* commFreqSig;                    // 0.5 seconds common frequency domain signal
  vector<Subband*> subbands;            // vector of subband arrays

  double timer = 0.0, tt;               
  double vptime;                        // vptime is the time duration of the samples within the packet
  size_t framespersec;

  // initialize random number generator
  gsl_rng *rng_inst;
  gsl_rng_env_setup();
  rng_inst = gsl_rng_alloc(gsl_rng_ranlux389);
  gsl_rng_set(rng_inst, setupinfo.seed);

  config = new Configuration(setupinfo.inputfilename.c_str(), 0);
  model = config->getModel();

  // retrieve general information from config
  // if in test mode, only generate data for 1 second
  dur = setupinfo.test ? testdur : config->getExecuteSeconds();

  numdatastreams = config->getNumDataStreams();
  cout << "Generate " << dur << " seconds data for " << numdatastreams << " stations ..." << "\n"
       << "source flux density is " << setupinfo.sfluxdensity << ".\n"
       << "random number generator seed is " << setupinfo.seed << endl;

  size_t len = setupinfo.antSEFDs.size();
  for(int i = len; i < numdatastreams; i++)
    setupinfo.antSEFDs.push_back(SEFD);

  // use framespersec of the first antenna to calculate vptime
  // use this vptime as time reference
  framespersec = config->getFramesPerSecond(configindex, 0);
  vptime = 1.0 * 1e6 / framespersec;
  if(!is_integer(vptime))
  {
    cout << "VDIF packet time in microsecond is not an integer!! Something is wrong here ..." << endl;
    return EXIT_FAILURE;
  }

  for(int i = 0; i < numdatastreams; i++)
  {
    framespersec = config->getFramesPerSecond(configindex, i);
      
    numrecordedbands = config->getDNumRecordedBands(configindex, i);

    cout << "Telescope " << config->getTelescopeName(i) << "\n"
         << " Number of recorded band(s) is " << numrecordedbands << "\n"
         << " Antenna SEFD is " << setupinfo.antSEFDs.at(i) << "\n"
         << " Number of frames per second is " << framespersec << endl;

    for(int j = 0; j < numrecordedbands; j++)
    {
      freqindex = config->getDRecordedFreqIndex(configindex, i, j); 
      cout << "Subband " << j << ":" << "\n"
           << "  Frequency " << config->getFreqTableFreq(freqindex) << "\n"
           << "  Bandwidth " << config->getFreqTableBandwidth(freqindex) << endl; 
    }
  }

  // general information from model
  if(setupinfo.verbose >= 1)
  {
    cout << "Number of scans is " << model->getNumScans() << endl;
    cout << "Scan start second is " << model->getScanStartSec(0, config->getStartMJD(), config->getStartSeconds()) << endl;
    cout << "Scan end second is " << model->getScanEndSec(0, config->getStartMJD(), config->getStartSeconds()) << endl;
    cout << "vptime in seconds is " << vptime/1e6 << endl;
  }

  // calculate specRes, number of samples per time block, step time
  if(getSpecRes(config, configindex, specRes, setupinfo.verbose) != EXIT_SUCCESS)
  {
    cout << "Failed to calculate spectral resolution ..." << endl;
    return EXIT_FAILURE;
  };
  numSamps = getNumSamps(config, configindex, specRes, setupinfo.verbose);
  stime = static_cast<int>(1 / specRes); // step time in microsecond
  if(setupinfo.verbose >= 1)
  {
    cout << "SpecRes is " << specRes << " MHz" << endl;
    cout << "number of samples per time block is " << numSamps << "\n"
         << "step time is " << stime << " us" << endl;
    cout << "tdur is " << tdur << endl;
  }
  minStartFreq = getMinStartFreq(config, configindex, setupinfo.verbose);

  // create and initialize subband of each antenna
  if(initSubbands(config, configindex, specRes, minStartFreq, subbands, model, tdur, setupinfo)
      != EXIT_SUCCESS)
  {
    cout << "Failed to create and initialize Subband ..." << endl;
    return EXIT_FAILURE;
  };
  // allocate memory for the common frequency domain signal
  commFreqSig = vectorAlloc_cf32(numSamps);

  cout << "Start generating data ...\n"
          "The process may take a while, please be patient!" << endl;

  // generate tdur time (e.g. 500000 microseconds) common frequency domain signal
  // and copy them to the right subband of each antenna
  genSignal(tdur/stime, commFreqSig, subbands, numSamps, rng_inst, tdur, setupinfo.sfluxdensity, setupinfo.verbose);

  // loop though the simulation time dur
  do
  {
    // move data in each array from the second half to the first half
    // and set the process pointer to the proper location
    // i.e. data is moved half array ahead, therefore process pointer 
    // is moved half array ahead
    movedata(subbands, setupinfo.verbose);
    // fill in the second half of each array, and
    // reset the current pointer of each array after data is generated
    genSignal(tdur/stime, commFreqSig, subbands, numSamps, rng_inst, tdur, setupinfo.sfluxdensity, setupinfo.verbose);

    // set tt to the lowest process pointer time among all subbands
    tt = getMinProcPtrTime(subbands, setupinfo.verbose);
    if(setupinfo.verbose >= 2) cout << "the lowest process pointer is at time " << tt << " us" << endl;

    if(tt >= tdur)
    {
      cout << "**the lowest process pointer time is larger than tdur!!!\n"
              "**Something is wrong here!!!" << endl;
      return (EXIT_FAILURE);
    }

    // dur is in second
    // durus is in microsecond
    durus = dur * 1e6;
    while((tt < tdur) && (timer < durus))
    {
      // process and packetize one vdif packet for each subband array
      if(processAndPacketize(framespersec, subbands, model, setupinfo.verbose))
        return(EXIT_FAILURE);
      tt += vptime;
      timer += vptime; 
      if(setupinfo.verbose >= 2) cout << "tt is " << tt << ", timer is " << timer << endl;
    }

  }while(timer < durus);
  
  freeSubbands(subbands);  
  if(setupinfo.verbose >= 2) cout << "free memory for commFreq" << endl;
  vectorFree(commFreqSig); 

  // combine VDIF files
  vdifzipper(config, configindex, durus, setupinfo.verbose);

  cout << "All data has been generated successfully, bye!" << endl;

  // free random number generator
  gsl_rng_free(rng_inst);

  return(EXIT_SUCCESS);
}

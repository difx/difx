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
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <cpgplot.h>
#include <string.h>
#include <sstream>
#include "architecture.h"
#include "configuration.h"
#include "monserver.h"
#include <iomanip>

using namespace std;

//prototypes
void plot_results();
void change_config();
int readnetwork(int sock, char* ptr, int bytestoread, int* nread);

Configuration * config;
int socketnumber, currentconfigindex, maxresultlength, nav, atseconds;
int resultlength, numchannels, nprod=0;
double intseconds = 1;
//IppsFFTSpec_C_32fc* fftspec;
IppsFFTSpec_R_32f* fftspec=NULL;
cf32 ** products=NULL;
f32  *phase=NULL, *amplitude=NULL;
f32 *lags=NULL;
f32 *xval=NULL;

int main(int argc, const char * argv[])
{
  int timestampsec, readbytes, i, status = 1;
  int numchan, prod, nvis, startsec;
  cf32 *vis;
  struct monclient monserver;

  if(argc < 3 || argc > 4)
  {
    cerr << "Error - invoke with difx_monitor <inputfile> <host> [int time (s)]" << endl;
    return EXIT_FAILURE;
  }

  //work out the config stuff
  config = new Configuration(argv[1], 0);
  config->loaduvwinfo(true);
  startsec = config->getStartSeconds();

  if(argc == 4)
    intseconds = atof(argv[3]);


  status  = monserver_connect(&monserver, (char*)argv[2],  Configuration::MONITOR_TCP_WINDOWBYTES);
  if (status) exit(1);

  status = monserver_requestall(monserver);
  if (status) exit(1);

  //get into the loop of receiving, processing and plotting!
  currentconfigindex = -1;
  atseconds = 0;

  umask(002);

  nvis=0;
  while (1) {

    status = monserver_readvis(&monserver);
    if (status) break;
    nvis++;

    cout << "Got visibility # " << monserver.timestamp << endl;

    if (monserver.timestamp==-1) continue;

    atseconds = monserver.timestamp-startsec;
    if(config->getConfigIndex(atseconds) != currentconfigindex) 
      change_config();

    while (!monserver_nextvis(&monserver, &prod, &vis)) {
      if (prod>=nprod) {
	cerr << "Got product larger than expected - aborting" << endl;
	exit(1);
      }

      status = vectorAdd_cf32_I(vis, products[prod], numchannels);

      if(status != vecNoErr) {
	cerr << "Error trying to add to accumulation buffer - aborting!" << endl;
	exit(1);
      }
    }

    //plot
    if(nvis==nav) {
      plot_results();
      nvis=0;
      for (i=0; i<nprod; i++) {
	status = vectorZero_cf32(products[i], numchannels);
	if(status != vecNoErr) {
	  cerr << "Error trying to zero visibility buffer - aborting!" << endl;
	  exit(1);
	}
      }
    }
  }

  //close the socket
  monserver_close(monserver);
}

void plot_results()
{
  char pgplotname[256];
  char polpair[3];
  char timestr[10];
  string sourcename;
  ostringstream ss;
  f32 temp;
  cf32 div;
  int status;

  polpair[2] = 0;

  div.re = 1.0/nav;
  div.im = 0;
  for(int i=0;i<nprod;i++) {

    status = vectorMulC_cf32_I(div, products[i], numchannels);
    if(status != vecNoErr) {
          cerr << "Error trying to add to accumulation buffer - aborting!" << endl;
	  exit(1);
    }
  }

  int binloop = 1;
  if(config->pulsarBinOn(currentconfigindex) && !config->scrunchOutputOn(currentconfigindex))
    binloop = config->getNumPulsarBins(currentconfigindex);

  int at = 0;
  //int sourceindex = config->getSourceIndex
  for(int i=0;i<config->getNumBaselines();i++)
  {

    int ds1index = config->getBDataStream1Index(currentconfigindex, i);
    int ds2index = config->getBDataStream2Index(currentconfigindex, i);

    //cout << "Baseline name is " << config->getDStationName(currentconfigindex, ds1index) << "-" <<  config->getDStationName(currentconfigindex, ds2index) << endl; 
    for(int j=0;j<config->getBNumFreqs(currentconfigindex,i);j++)
    {
      int freqindex = config->getBFreqIndex(currentconfigindex, i, j);
      //cout << "Frequency is " << config->getFreqTableFreq(freqindex) << endl;
      for(int b=0;b<binloop;b++)
      {
        for(int k=0;k<config->getBNumPolProducts(currentconfigindex, i, j);k++)
        {
          config->getBPolPair(currentconfigindex,i,j,k,polpair);
          //cout << "Polarisation is " << polpair << endl;
          //get the lagspace as well
	  
	  // Calculate amplitude, phase and lags

	  status = vectorPhase_cf32(products[at], phase, numchannels);
	  if(status != vecNoErr) {
	    cerr << "Error trying to calculate phase - aborting!" << endl;
	    exit(1);
	  }
	  vectorMulC_f32_I(180/M_PI, phase, numchannels);

	  status = vectorMagnitude_cf32(products[at], amplitude, numchannels);
	  if(status != vecNoErr) {
	    cerr << "Error trying to calculate amplitude - aborting!" << endl;
	    exit(1);
	  }

	  ippsFFTInv_CCSToR_32f((Ipp32f*)products[at], lags, fftspec, 0);
	  //rearrange the lags into order
	  for(int l=0;l<numchannels;l++) {
            temp = lags[l];
            lags[l] = lags[l+numchannels];
	    lags[l+numchannels] = temp;
          }

          //plot something - data is from resultbuffer[at] to resultbuffer[at+numchannels+1]
          //cout << "Plotting baseline " << i << ", freq " << j << ", bin " << b << ", polproduct " << k << endl;

	  sprintf(pgplotname, "lba-%d-f%d-p%d-b%d.png/png",
		  i, j, k, b);

	  status = cpgbeg(0,pgplotname,1,3);
	  if (status != 1) {
	    cout << "Error opening pgplot device: " << pgplotname << endl;
	  } else {
	    float max, min, delta;

	    cpgscr(0,1,1,1);
	    cpgscr(1,0,0,0);

            // Plot lags
	    ippsMax_32f(lags, numchannels*2, &max);
	    ippsMin_32f(lags, numchannels*2, &min);
	    delta = (max-min)*0.05;
	    if (delta==0) delta = 1;
	    min -= delta;
	    max += delta;

	    cpgsch(1.5);
	    cpgsci(1);
            cpgenv(0,numchannels*2,min,max,0,0);
            cpglab("Channel", "Correlation coefficient", "");
            cpgsci(2);
            cpgline(numchannels*2, xval, lags);

	    // Annotate

	    config->getUVW()->getSourceName(config->getStartMJD(), 
		      atseconds+config->getStartSeconds(),sourcename);

	    cpgsci(4);
	    cpgsch(2.5);

	    //cout << "INDEX" << ds1index << "  " << ds2index << endl;
	    //cout << config->getDStationName(currentconfigindex, ds1index)  << endl;
	    //cout <<  config->getDStationName(currentconfigindex, ds2index) << endl;
	    //cout << sourcename << endl; 
	    //ss << config->getDStationName(currentconfigindex, ds1index) 
	    //   << "-" 
	    //   <<  config->getDStationName(currentconfigindex, ds2index)
	    //   << "  " << sourcename; 
	    ss << config->getTelescopeName(ds1index) 
	       << "-" 
	       <<  config->getTelescopeName(ds2index)
	       << "  " << sourcename; 

	    cpgmtxt("T", -1.5,0.02, 0, ss.str().c_str());	    
	    ss.str("");

	    cpgmtxt("T", -1.5,0.97,1,polpair);

	    ss << config->getFreqTableFreq(freqindex) << " MHz";
	    cpgmtxt("T", -2.6,0.97,1,ss.str().c_str());	    
	    ss.str("");

	    int seconds = atseconds+config->getStartSeconds();
	    int hours = seconds/3600;
	    seconds -= hours*3600;
	    int minutes = seconds/60;
	    seconds %= 60;
	    sprintf(timestr, "%02d:%02d:%02d", hours, minutes, seconds);
	    cpgmtxt("T", -1.5,0.90,1,timestr);

	    //ss << hours << ":" << minutes << ":" << seconds;
	    //cpgmtxt("T", -1.5,0.90,1,ss.str().c_str());
	    //ss.str("");

	    cpgsch(1.5);
	    cpgsci(1);

	    // Plot Amplitude
	    ippsMax_32f(amplitude, numchannels, &max);
	    ippsMin_32f(amplitude, numchannels, &min);
	    delta = (max-min)*0.05;
	    if (delta==0) delta = 1;
	    min -= delta;
	    max += delta;

	    cpgsci(1);
	    cpgenv(0,numchannels,min,max,0,0);
	    cpglab("Channel", "Amplitude (Jy)", "");
	    cpgsci(2);
	    cpgline(numchannels, xval, amplitude);

	    // Plot Phase
	    cpgsci(1);
	    cpgenv(0,numchannels,-180,180,0,0);
	    cpglab("Channel", "Phase (deg)", "");
	    cpgsci(2);
	    cpgsch(2);
	    cpgpt(numchannels, xval, phase, 17);
	    cpgsch(1);


	    cpgend();

	  }
          at++;
        }
      }
    }
  }

  int autocorrwidth = (config->getMaxProducts()>1)?2:1;
  for(int i=0;i<config->getNumDataStreams();i++)
  {
    //cout << "Doing autocorrelation for " << config->getDStationName(currentconfigindex, i) << endl;
    for(int j=0;j<autocorrwidth;j++)
    {
      for(int k=0;k<config->getDNumOutputBands(currentconfigindex, i); k++)
      {
        //keep plotting...
        //cout << "Frequency is " <<  config->getFreqTableFreq(config->getDFreqIndex(currentconfigindex, i, k)) << endl;
        //char firstpol = config->getDBandPol(currentconfigindex, i, k);

	if (j==0) {
	  sprintf(pgplotname, "lba-auto%d-b%d.png/png",
		  i, k);
	  status = cpgbeg(0,pgplotname,1,1);
	} else {
	  sprintf(pgplotname, "lba-autocross%d-b%d.png/png",
		  i, k);
	  status = cpgbeg(0,pgplotname,1,2);
	}
	if (status != 1) {
	  cout << "Error opening pgplot device: " << pgplotname << endl;
	} else {
	  float max, min;

	  cpgscr(0,1,1,1);
	  cpgscr(1,0,0,0);

	  // Plot Amplitude

	  status = vectorMagnitude_cf32(products[at], amplitude, numchannels);
	  if(status != vecNoErr) {
	    cerr << "Error trying to calculate amplitude - aborting!" << endl;
	    exit(1);
	  }

	  max = amplitude[0];
	  min = max;
	  for (int n=1; n<numchannels; n++) {
	    if (amplitude[n] > max) max = amplitude[n];
	    if (amplitude[n] < min) min = amplitude[n];
	  }
	  if (min==max) {
	    min-=1;
	    max+=1;
	  }

	  cpgsci(1);
	  cpgenv(0,numchannels+1,min,max,0,0);
	  cpglab("Channel", "Amplitude (Jy)", "");
	  cpgsci(2);
	  cpgline(numchannels, xval, amplitude);

	  if (j!=0) {
	    status = vectorPhase_cf32(products[at], phase, numchannels);
	    if(status != vecNoErr) {
	      cerr << "Error trying to calculate phase - aborting!" << endl;
	      exit(1);
	    }
	    vectorMulC_f32_I(180/M_PI, phase, numchannels);

	    // Plot Phase
	    cpgsci(1);
	    cpgenv(0,numchannels,-180,180,0,0);
	    cpglab("Channel", "Phase (deg)", "");
	    cpgsci(2);
	    cpgline(numchannels, xval, phase);
	  }
	  cpgend();
	}
	at++;
      }
    }
  }
}

void change_config()
{
  int status, i;

  currentconfigindex = config->getConfigIndex(atseconds);
  numchannels = config->getNumChannels(currentconfigindex);

  cout << "New config " << currentconfigindex << " at " << atseconds << endl;

  if(xval) vectorFree(xval);
  if(lags) vectorFree(lags);
  if(fftspec) ippsFFTFree_R_32f(fftspec);
  if(phase) vectorFree(phase);
  if(amplitude) vectorFree(amplitude);
  
  if (products) {
    for (i=0; i<nprod; i++) {
      if (products[i]) vectorFree(products[i]);
    }
    delete [] products;
  }

  xval = vectorAlloc_f32(numchannels*2);
  for(int i=0;i<numchannels*2;i++)
    xval[i] = i;
  lags = vectorAlloc_f32(numchannels*2);

  phase = vectorAlloc_f32(numchannels);
  amplitude = vectorAlloc_f32(numchannels);

  int order = 0;
  while(((numchannels*2) >> order) > 1)
    order++;
  ippsFFTInitAlloc_R_32f(&fftspec, order, IPP_FFT_NODIV_BY_ANY, ippAlgHintFast);

  nav = ceil(intseconds/config->getIntTime(currentconfigindex));
  cout << "#Integrations to average = " << nav << endl;

  resultlength = config->getResultLength(currentconfigindex);

  nprod = resultlength/(numchannels+1);
  cout << "Got " << nprod << " products" << endl;

  products = new cf32*[nprod];
  for (i=0; i<nprod; i++) {
    products[i] = vectorAlloc_cf32(numchannels);

    status = vectorZero_cf32(products[i], numchannels);
    if(status != vecNoErr) {
      cerr << "Error trying to zero visibility buffer - aborting!" << endl;
      exit(1);
    }
  }
}




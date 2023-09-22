/***************************************************************************
 *   Copyright (C) 2009-2016 by Chris Phillips                             *
 *                                                                         *
 *   This program is free software: you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation, either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program.  If not, see <http://www.gnu.org/licenses/>. *
 ***************************************************************************/
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <string.h>
#include <sstream>
#include "architecture.h"
#include "configuration.h"
#include "monserver.h"
#include <iomanip>

using namespace std;

//prototypes
vector<DIFX_ProdConfig> change_config(Configuration *config, int configindex, int refant, struct monclient client);
int calcdelay(cf32 *vis, int nchan, IppsFFTSpec_R_32f* fftspec, double *delay, 
	      float *snr);

int main(int argc, const char * argv[]) {
  int i, status, nchan, atseconds, count, prod, numchan;
  int startsec, nsamples, currentconfig, refant;
  float snr;
  cf32 *vis=NULL;
  double delay;
  char polpair[3];
  struct monclient monserver;
  ostringstream ss;

  Configuration * config;
  Model * model;
  IppsFFTSpec_R_32f* fftspec=NULL;
  vector<double> delaysum;
  vector<int> ndel;
  vector<struct monclient> visibilities;
  vector<struct monclient>::iterator it;
  vector<DIFX_ProdConfig> prodconfig;

  if(argc < 3 || argc > 4)
  {
    cerr << "Error - Usage: difx_monitor <inputfile> <host> [refant [# samples]]" << endl;
    return EXIT_FAILURE;
  }

  //work out the config stuff
  config = new Configuration(argv[1], 0);
  if (!config->consistencyOK()) {
    cerr << "Aborting" << endl;
    exit(1);
  }
  model = config->getModel();
  startsec = config->getStartSeconds();

  nsamples = 1;
  refant = 0;
  
  if(argc > 3)
    refant = atoi(argv[3]);

  if(argc > 4)
    nsamples = atoi(argv[4]);

  // Connect to monitor server
  status  = monserver_connect(&monserver, (char*)argv[2],  Configuration::MONITOR_TCP_WINDOWBYTES);
  if (status) exit(1);

  currentconfig = 0;
  prodconfig = change_config(config, currentconfig, refant, monserver);
  if (prodconfig.size()==0) exit(1);

  atseconds = 0;

  count = 0;
  while (count<nsamples) {

    status = monserver_readvis(&monserver);
    if (status) break;
    cout << "Got visibility # " << monserver.timestamp << endl;

    if (monserver.timestamp==-1) continue;

    atseconds = monserver.timestamp-startsec;
    int thisconfig = sec2config(config, atseconds);
    if(thisconfig != currentconfig) {
      currentconfig = thisconfig;
      nsamples = 0;
      prodconfig = change_config(config, currentconfig, refant, monserver);
      for (i=0; i<count; i++) {
	monserver_clear(&visibilities[i]);
      }
      count = 0;
      visibilities.resize(0);
      continue;
    }
    count++;
    visibilities.resize(count);
    monserver_dupclient(monserver,&visibilities[count-1]);
  }
  monserver_close(&monserver);


  nchan = 0;

  cout << string(36, ' ') << "Delay" << endl;
  cout << string(37, ' ') << "usec   SNR" << endl;


  for (i=0; i<(int)visibilities.size(); i++) {
    while (!monserver_nextvis(&visibilities[i], &prod, &numchan, &vis)) {
      streamsize prec;

      if (nchan==0) {
	int order = 0;
	nchan = numchan;
	while(((nchan*2) >> order) > 1) order++;

#ifdef IPP9
	int sizeFFTSpec, sizeFFTInitBuf, wbufsize;
	u8 *fftInitBuf, *fftSpecBuf;
  
	ippsFFTGetSize_R_32f(order, IPP_FFT_NODIV_BY_ANY, ippAlgHintFast,  &sizeFFTSpec, &sizeFFTInitBuf, &wbufsize);
	fftSpecBuf = ippsMalloc_8u(sizeFFTSpec);
	if (sizeFFTInitBuf>0)
	  fftInitBuf = ippsMalloc_8u(sizeFFTInitBuf);
	else
	  fftInitBuf=NULL;

	// Initialize FFT
	ippsFFTInit_R_32f(&fftspec, order, IPP_FFT_NODIV_BY_ANY, ippAlgHintFast, fftSpecBuf, fftInitBuf);
	if (fftInitBuf) ippFree(fftInitBuf);
	
#else
	ippsFFTInitAlloc_R_32f(&fftspec, order, IPP_FFT_NODIV_BY_ANY, ippAlgHintFast);
#endif	
      } else if (nchan!=numchan) {
	cerr << "Do not support differing number of channels. Abort" << endl;
	exit(1);
      }

      status = calcdelay(vis, nchan, fftspec, &delay, &snr);

      cout << setw(3) << prod << ": ";

      ss << prodconfig[prod].getTelescopeName1() << "-" 
	 << prodconfig[prod].getTelescopeName2();
      
      cout << left << setw(15) << ss.str() << right;
      ss.str("");

      prodconfig[prod].getPolPair(polpair);
      cout << " " <<  polpair << " ";

      cout << prodconfig[prod].getFreq() << " MHz  ";

      delay *= 1/(2*prodconfig[prod].getBandwidth()); // Really should use sampling rate
      if (prodconfig[prod].getTelescopeIndex2()==refant) delay *= -1;

      prec = cout.precision();
      cout << setprecision(3) << fixed << setw(7) << delay;
      cout << setprecision(1) << "  (" << snr << ")" << endl;
      cout << setprecision(prec);
      cout.unsetf(ios_base::fixed);

      if (polpair[0]==polpair[1] && snr > 10) {
	int antid;
	if (prodconfig[prod].getTelescopeIndex1()==refant)
	  antid = prodconfig[prod].getTelescopeIndex2();
	else
	  antid = prodconfig[prod].getTelescopeIndex1();
	if (antid>(int)delaysum.size()-1) {
	  delaysum.resize(antid+1,0);
	  ndel.resize(antid+1,0);
	}
	delaysum[antid] += delay;
	ndel[antid]++;
      }
    }
    if (nsamples>1) cout << endl;
  }

  cout << endl;
  cout << "Average delays to " << config->getTelescopeName(refant) << endl;
  cout << "These delays must be ADDED to the CLOCK DELAY value (including sign)" << endl;
  cout << endl;

  for (i=0; i<(int)delaysum.size(); i++) {
    if (ndel[i]>0) {
      delay = delaysum[i]/ndel[i];
      cout << left << setw(7) << config->getTelescopeName(i) 
	   << right << " " << setprecision(3) << setw(7) << fixed
	   << delay << endl;
    }
  }
}


vector<DIFX_ProdConfig> change_config(Configuration *config, int configindex, int refant, struct monclient client) {
  int status;
  unsigned int i, nprod;
  vector<int> iproducts;
  vector<DIFX_ProdConfig> allproducts, selectedproducts;


  if (refant >= config->getTelescopeTableLength()) {
    cerr << "Refant " << refant << " too large. Aborting" << endl;
    exit(1);
  } else {
    cout << "Using reference telescope " << config->getTelescopeName(refant) << endl;
  }

  allproducts = monserver_productconfig(config, configindex);

  for (i=0; i<allproducts.size(); i++) {
    if ((allproducts[i].getTelescopeIndex1()==refant || allproducts[i].getTelescopeIndex2()==refant) && allproducts[i].getTelescopeIndex1()!=allproducts[i].getTelescopeIndex2()) {
      iproducts.push_back(i);
      selectedproducts.push_back(allproducts[i]);
    }
  }

  nprod = iproducts.size();
  if (nprod==0) {
    cout << "No baselines selected" << endl;
    allproducts.clear();
    return allproducts;
  }

  struct product_offset *offsets = new struct product_offset [nprod];
  set_productoffsets(nprod, &iproducts[0], offsets, allproducts);

  status = monserver_requestproducts_byoffset(client, offsets, nprod);
  if (status) exit(1);

  delete [] offsets;

  return allproducts;
}

int calcdelay(cf32 *vis, int nchan, IppsFFTSpec_R_32f* fftspec, double *delay, 
	      float *snr) {
  static Ipp32f *lags=NULL;
  Ipp32f max, stddev;
  int imax, i1, i2;

  if (lags==NULL) lags = vectorAlloc_f32(nchan*2);

  ippsFFTInv_CCSToR_32f((Ipp32f*)vis, lags, fftspec, 0); 
  ippsAbs_32f_I(lags, nchan*2);

  ippsMaxIndx_32f(lags, nchan*2, &max, &imax);

  // Ignore actual fringe 
  i1 = (imax-10+nchan*2)%(nchan*2);
  i2 = (imax+10)%(nchan*2);

  if (i1<i2) {
    ippsMove_32f(&lags[i2], &lags[i1], nchan*2-i2);
  } else {
    ippsMove_32f(&lags[i2], lags, nchan*2-20);    
  }
  ippsStdDev_32f(lags, 2*nchan-20, &stddev, ippAlgHintFast);

  if (imax>nchan) imax -= 2*nchan;
  
  *delay = imax;
  *snr = max/stddev;

  return(0);
}

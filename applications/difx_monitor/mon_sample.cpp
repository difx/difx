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

#include <stdio.h>
#include <stdlib.h>

#include <complex>

#define PL_DOUBLE
#include <plplot/plplot.h>
#include "architecture.h"
#include "configuration.h"

#include "monserver.h"

#define MAX_PROD 4

PLFLT arraymax(PLFLT *array[], int nchan, int nprod);
PLFLT arraymin(PLFLT *a[], int nchan, int nprod);
void mjd2cal(int mjd, int *day, int *month, int *year);

int main(int argc, const char * argv[]) {
  int status, i, ivis, nchan=0, nprod, cols[MAX_PROD] = {1,3,9,11};
  int mjd, startsec, currentconfig, currentscan, numchan;
  int iprod[MAX_PROD];
  char polpair[3], timestr[20];
  struct monclient monserver;
  float delta, min, max, temp;
  PLFLT *xval=NULL, *phase[MAX_PROD], *amp[MAX_PROD], *lags[MAX_PROD], *lagx=NULL;
  cf32 *vis32;
  Ipp8u *wbuf=NULL;
  Ipp64fc *vis64;
  string sourcename;
  ostringstream ss;
  IppsFFTSpec_R_64f* fftspec=NULL;
  Configuration *config;
  Model * model;
  vector<DIFX_ProdConfig> products;
  struct product_offset plotprod[MAX_PROD];

  if (sizeof(PLFLT)!=sizeof(f64)) {
    fprintf(stderr, "PLFLT is not the size of a float (%d/%d) - aborting\n", 
	    (int)sizeof(PLFLT), (int)sizeof(f64));
    return(EXIT_FAILURE);
  }

  if(argc < 4)  {
    cerr << "Error - invoke with mon_sample <host> <inputfile> <product#> [<product#> ...]" << endl;
    return(EXIT_FAILURE);
  }

  if (argc-3>MAX_PROD) {
    cerr << "Error - Too many products requested" << endl;
    return(EXIT_FAILURE);
  }

  config = new Configuration(argv[2], 0);
  if (!config->consistencyOK()) {
    cerr << "Aborting" << endl;
    exit(1);
  }
  model = config->getModel();
  startsec = config->getStartSeconds();
  mjd = config->getStartMJD();
  currentscan = 0;
  currentconfig = config->getScanConfigIndex(currentscan);
  products = monserver_productconfig(config, currentconfig);

  for (i=3; i<argc; i++) iprod[i-3] = atoi(argv[i]);
  nprod = argc-3;

  set_productoffsets(nprod, iprod, plotprod, products);

  for (i=0; i<nprod; i++) {
    amp[i] = NULL;
    phase[i] = NULL;
    lags[i] = NULL;
  }

  char  ver[80];
  plgver( ver );
  fprintf( stdout, "PLplot library version: %s\n", ver );

  // Open PGPLOT first, as pgplot server seems to inherit monitor socket
  plstart("xwin", 1,3);  //??RETURN
  plspause(0);

  status  = monserver_connect(&monserver, (char*)argv[1], -1);
  if (status) exit(1);

  cout << "Opened connection to monitor server" << endl;

  status = monserver_requestproducts_byoffset(monserver, plotprod, nprod);
  if (status) exit(1);

  cout << "Sent product request" << endl;

  status = 0;
  while (!status) {
    cout << "Waiting for visibilities" << endl;
    status = monserver_readvis(&monserver);
    if (!status) {
      cout << "Got visibility # " << monserver.timestamp << endl;

      int atseconds = monserver.timestamp-startsec;
      if (atseconds < model->getScanStartSec(currentscan, mjd, startsec)
	  || atseconds > model->getScanStartSec(currentscan, mjd, startsec)) {

	int scan = 0;
	while (scan<model->getNumScans()) {
	  if (atseconds>model->getScanEndSec(scan, mjd, startsec)) {
	    scan++;
	  } else {
	    break;
	  }
	}
	currentscan = scan;
	int newconfig = config->getScanConfigIndex(currentscan);

	if(newconfig != currentconfig) {
	  currentconfig = newconfig;
	  products = monserver_productconfig(config, currentconfig);
	  set_productoffsets(nprod, iprod, plotprod, products);
	  status = monserver_requestproducts_byoffset(monserver, plotprod, nprod);
	  if (status) exit(1);
	  cout << "Config changes - skipping this integration" << endl;
	  continue;
	}
      }

      ivis = 0;
      while (!monserver_nextvis(&monserver, (int*)&iprod[ivis], &numchan, &vis32)) {
	if (nchan!=numchan) {
	  if (ivis==0) {
	    // (Re)allocate arrays if number of channels changes, including first time
	    nchan = numchan;
	    cout << "Number of channels = " << nchan << endl;
	    if (xval!=NULL) vectorFree(xval);
	    if (lagx!=NULL) vectorFree(lagx);
	    if (vis64!=NULL) vectorFree(vis64);
	    if (fftspec!=NULL) ippFree(fftspec);
	    for (i=0; i<nprod; i++) {
	      if (amp[i]!=NULL) vectorFree(amp[i]);
	      if (phase[i]!=NULL) vectorFree(phase[i]);
	      if (lags[i]!=NULL) vectorFree(lags[i]);
	    }

	    xval = vectorAlloc_f64(nchan);
	    lagx = vectorAlloc_f64(nchan*2);
	    vis64 = vectorAlloc_cf64(nchan);
	    for (i=0; i<nprod; i++) {
	      amp[i] = vectorAlloc_f64(nchan);
	      phase[i] = vectorAlloc_f64(nchan);
	      lags[i] = vectorAlloc_f64(nchan*2);
	      if (amp[i]==NULL || phase[i]==NULL || lags[i]==NULL) {
		cerr << "Failed to allocate memory for plotting arrays" << endl;
		exit(1);
	      }
	    }
	    if (xval==NULL || lagx==NULL) {
	      cerr << "Failed to allocate memory for plotting arrays" << endl;
	      exit(1);
	    }
	    for (i=0; i<nchan; i++) {
	      xval[i] = i;
	    }
	    for (i=-nchan; i<nchan; i++) {
	      lagx[i+nchan] = i;
	    }
	    
	    int order = 0;
	    while(((nchan*2) >> order) > 1)
	      order++;
#ifdef IPP9
	int sizeFFTSpec, sizeFFTInitBuf, wbufsize;
	u8 *fftInitBuf, *fftSpecBuf;
  
	ippsFFTGetSize_R_64f(order, IPP_FFT_NODIV_BY_ANY, ippAlgHintFast,  &sizeFFTSpec, &sizeFFTInitBuf, &wbufsize);
	fftSpecBuf = ippsMalloc_8u(sizeFFTSpec);
	if (sizeFFTInitBuf>0)
	  fftInitBuf = ippsMalloc_8u(sizeFFTInitBuf);
	else
	  fftInitBuf=NULL;
	if (wbufsize>0)
	  wbuf = ippsMalloc_8u(wbufsize);
	else
	  wbuf=NULL;

	// Initialize FFT
	ippsFFTInit_R_64f(&fftspec, order, IPP_FFT_NODIV_BY_ANY, ippAlgHintFast, fftSpecBuf, fftInitBuf);
	if (fftInitBuf) ippFree(fftInitBuf);
#else
	    ippsFFTInitAlloc_R_64f(&fftspec, order, IPP_FFT_NODIV_BY_ANY, ippAlgHintFast);
	    wbuf = NULL;
#endif
	  } else  {
	    cerr << "Does not support mixed number of channels between products" << endl;
	    exit(1); // Should do a cleaner exit
	  }
	}

	for (i=0;i<nchan;i++) {
	  vis64[i].re = vis32[i].re;
	  vis64[i].im = vis32[i].im;
	}
	ippsMagnitude_64fc(vis64, amp[ivis], nchan);
	ippsPhase_64fc(vis64, phase[ivis], nchan);
	vectorMulC_f64_I(180/M_PI, phase[ivis], nchan);

	ippsFFTInv_CCSToR_64f((Ipp64f*)vis64, lags[ivis], fftspec, wbuf);
	//rearrange the lags into order
	for(i=0;i<nchan;i++) {
	  temp = lags[ivis][i];
	  lags[ivis][i] = lags[ivis][i+nchan];
	  lags[ivis][i+nchan] = temp;
	}
	ivis++;
      }

      // Plot all the data
      //cpgbbuf();

      max = arraymax(amp, nchan, nprod);
      min = arraymin(amp, nchan, nprod);
      delta = (max-min)*0.05;
      if (delta==0.0) delta = 0.5;
      min -= delta;
      max += delta;

      plschr(0,0.8);
      plcol0(15);
      plenv(0,nchan,min,max,0,0);
      pllab("Channel", "Amplitude", "");

      for (i=0; i<nprod; i++) {
	plcol0(cols[i]);
	plline(nchan, xval, amp[i]);
      }

      // Source name
      sourcename = model->getScanPointingCentreSource(currentscan)->name;
      plcol0(1);
      plschr(0,1);
      plmtex("t", -1.5, 0.02, 0, sourcename.c_str());	    

      // Time 
      int seconds = atseconds+config->getStartSeconds();
      int hours = seconds/3600;
      seconds -= hours*3600;
      int minutes = seconds/60;
      seconds %= 60;
      
      // BUG: Fails for scans which wrap day boundary.
      int day, month, year;
      mjd2cal(mjd, &day, &month, &year);

      sprintf(timestr, "%d/%d %02d:%02d:%02d", day, month, hours, minutes, seconds);
      plmtex("t", -1.5,0.98,1,timestr);

      plschr(0,1);

      max = arraymax(phase, nchan, nprod);
      min = arraymin(phase, nchan, nprod);
      delta = (max-min)*0.1;
      if (delta==0.0) delta = 0.5;
      min -= delta;
      max += delta;
      plcol0(15);
      plschr(0,0.8);
      plenv(0,nchan,min,max,0,0);
      pllab("Channel", "Phase", "");


      for (i=0; i<nprod; i++) {
	plcol0(cols[i]);
	plsym(nchan, xval, phase[i], 17);
      }

      max = arraymax(lags, nchan*2, nprod);
      min = arraymin(lags, nchan*2, nprod);
      delta = (max-min)*0.1;
      if (delta<1e-10) delta = 0.5;
      min -= delta;
      max += delta;
      plcol0(15);
      plschr(0,0.8);
      plenv(-nchan,nchan,min,max,0,0);
      pllab("Channel", "Delay", "");

      for (i=0; i<nprod; i++) {
	plcol0(cols[i]);
	plline(nchan*2, lagx, lags[i]);
      }

      for (i=0; i<nprod; i++) {
	products[iprod[i]].getPolPair(polpair);
	ss << iprod[i] << ": "
	   << products[iprod[i]].getTelescopeName1();
	if (products[iprod[i]].getTelescopeIndex1() 
	    != products[iprod[i]].getTelescopeIndex2()) {
	  ss << "-"  << products[iprod[i]].getTelescopeName2();
	}
	ss << " " <<  polpair << " "
	   << products[iprod[i]].getFreq() << " MHz  ";
	if (products[iprod[i]].getLSB()) 
	  ss << "LSB";
	else 
	  ss << "USB";

	plschr(0,1);
	plcol0(cols[i]);
	if (i==0) {
	  plmtex("t", -1.5, 0.02, 0, ss.str().c_str());	    
	} else if (i==1) { 
	  plmtex("t", -1.5, 0.98, 1, ss.str().c_str());	    
	} else if (i==2) { 
	  plmtex("b", -1.5, 0.02, 0, ss.str().c_str());	    
	} else if (i==3) { 
	  plmtex("b", -1.5, 0.98, 1, ss.str().c_str());	    
	}
	ss.str("");
      }
      plschr(0,1);
      //cpgebuf();

    }
  }

  plend();
  monserver_close(&monserver);
}


PLFLT arraymax(PLFLT *a[], int nchan, int nprod) {
  PLFLT max, thismax;
  int i;

  max = a[0][0];
  for (i=0; i<nprod; i++)  {
    ippsMax_64f(a[i], nchan, &thismax);
    if (thismax>max) max = thismax;
  }
  return max;
}

PLFLT arraymin(PLFLT *a[], int nchan, int nprod) {
  PLFLT min, thismin;
  int i;

  min = a[0][0];
  for (i=0; i<nprod; i++)  {
    ippsMin_64f(a[i], nchan, &thismin);
    if (thismin<min) min = thismin;
  }
  return min;
}

void mjd2cal(int mjd, int *day, int *month, int *year) {
  //  Julian Day number
  int jd = mjd + 2400001;

  // Do some rather cryptic calculations
  int temp1 = 4*(jd+((6*(((4*jd-17918)/146097)))/4+1)/2-37);
  int temp2 = 10*(((temp1-237)%1461)/4)+5;

  *year = temp1/1461-4712;
  *month =((temp2/306+2)%12)+1;
  *day = (temp2%306)/10+1;

  return;
}


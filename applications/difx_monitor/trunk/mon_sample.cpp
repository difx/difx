/***************************************************************************
 *   Copyright (C) 2009 by Chris Phillips                                  *
 *                                                                         *
 *  This program is free software; you can redistribute it and/or          *
 *  modify it under the terms of the GNU General Public License as         *
 *  published by the Free Software Foundation; version 2 of the License    *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.   See the        *
 *   GNU General Public License for more details.                          *
 ***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <cpgplot.h>
#include <complex>

#include "architecture.h"
#include "configuration.h"

#include "monserver.h"

#define MAX_PROD 4

float arraymax(float *array[], int nchan, int nprod);
float arraymin(float *a[], int nchan, int nprod);
void mjd2cal(int mjd, int *day, int *month, int *year);

int main(int argc, const char * argv[]) {
  int status, i, ivis, nchan=0, nprod, cols[MAX_PROD] = {2,3,4,5};
  int mjd, startsec, currentconfig, currentscan, numchan;
  int iprod[MAX_PROD];
  char polpair[3], timestr[20];
  struct monclient monserver;
  float *xval=NULL, *amp[MAX_PROD], *phase[MAX_PROD], *lags[MAX_PROD], *lagx=NULL;
  float delta, min, max, temp;
  cf32 *vis;
  string sourcename;
  ostringstream ss;
  IppsFFTSpec_R_32f* fftspec=NULL;
  Configuration *config;
  Model * model;
  vector<DIFX_ProdConfig> products;
  struct product_offset plotprod[MAX_PROD];

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

  // Open PGPLOT first, as pgplot server seems to inherit monitor socket
  status = cpgbeg(0,"/xs",1,3);
  if (status != 1) {
    cerr << "Error opening pgplot device" << endl;
  }
  cpgask(0);

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
      while (!monserver_nextvis(&monserver, (int*)&iprod[ivis], &numchan, &vis)) {
	if (nchan!=numchan) {
	  if (ivis==0) {
	    // (Re)allocate arrays if number of channels changes, including first time
	    nchan = numchan;
	    cout << "Number of channels = " << nchan << endl;
	    if (xval!=NULL) vectorFree(xval);
	    if (lagx!=NULL) vectorFree(lagx);
	    if (fftspec!=NULL) ippsFFTFree_R_32f(fftspec);
	    for (i=0; i<nprod; i++) {
	      if (amp[i]!=NULL) vectorFree(amp[i]);
	      if (phase[i]!=NULL) vectorFree(phase[i]);
	      if (lags[i]!=NULL) vectorFree(lags[i]);
	    }

	    xval = vectorAlloc_f32(nchan);
	    lagx = vectorAlloc_f32(nchan*2);
	    for (i=0; i<nprod; i++) {
	      amp[i] = vectorAlloc_f32(nchan);
	      phase[i] = vectorAlloc_f32(nchan);
	      lags[i] = vectorAlloc_f32(nchan*2);
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
	    ippsFFTInitAlloc_R_32f(&fftspec, order, IPP_FFT_NODIV_BY_ANY, ippAlgHintFast);
	  } else  {
	    cerr << "Does not support mixed number of channels between products" << endl;
	    exit(1); // Should do a cleaner exit
	  }
	}

	ippsMagnitude_32fc(vis, amp[ivis], nchan);
	ippsPhase_32fc(vis, phase[ivis], nchan);
	vectorMulC_f32_I(180/M_PI, phase[ivis], nchan);

	ippsFFTInv_CCSToR_32f((Ipp32f*)vis, lags[ivis], fftspec, 0);
	//rearrange the lags into order
	for(i=0;i<nchan;i++) {
	  temp = lags[ivis][i];
	  lags[ivis][i] = lags[ivis][i+nchan];
	  lags[ivis][i+nchan] = temp;
	}
	ivis++;
      }

      // Plot all the data
      cpgbbuf();

      max = arraymax(amp, nchan, nprod);
      min = arraymin(amp, nchan, nprod);
      delta = (max-min)*0.05;
      if (delta==0.0) delta = 0.5;
      min -= delta;
      max += delta;

      cpgsci(1);
      cpgenv(0,nchan,min,max,0,0);
      cpglab("Channel", "Amplitude", "");

      for (i=0; i<nprod; i++) {
	cpgsci(cols[i]);
	cpgline(nchan, xval, amp[i]);
      }

      // Source name
      //sourcename = model->getScanIdentifier(currentscan);
      sourcename = model->getScanPointingCentreSource(currentscan)->name;
      cpgsci(2);
      cpgsch(4);
      cpgmtxt("T", -1.5, 0.02, 0, sourcename.c_str());	    

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
      cpgmtxt("T", -1.5,0.98,1,timestr);

      cpgsch(1);

      max = arraymax(phase, nchan, nprod);
      min = arraymin(phase, nchan, nprod);
      delta = (max-min)*0.1;
      if (delta==0.0) delta = 0.5;
      min -= delta;
      max += delta;
      cpgsci(1);
      cpgenv(0,nchan,min,max,0,0);
      cpglab("Channel", "Phase", "");


      for (i=0; i<nprod; i++) {
	cpgsci(cols[i]);
	cpgpt(nchan, xval, phase[i], 17);
      }

      max = arraymax(lags, nchan*2, nprod);
      min = arraymin(lags, nchan*2, nprod);
      delta = (max-min)*0.1;
      if (delta<1e-10) delta = 0.5;
      min -= delta;
      max += delta;
      cpgsci(1);
      cpgenv(-nchan,nchan,min,max,0,0);
      cpglab("Channel", "Delay", "");

      for (i=0; i<nprod; i++) {
	cpgsci(cols[i]);
	cpgline(nchan*2, lagx, lags[i]);
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

	cpgsch(3);
	cpgsci(cols[i]);
	if (i==0) {
	  cpgmtxt("T", -1.5, 0.02, 0, ss.str().c_str());	    
	} else if (i==1) { 
	  cpgmtxt("T", -1.5, 0.98, 1, ss.str().c_str());	    
	} else if (i==2) { 
	  cpgmtxt("B", -1.5, 0.02, 0, ss.str().c_str());	    
	} else if (i==3) { 
	  cpgmtxt("B", -1.5, 0.98, 1, ss.str().c_str());	    
	}
	ss.str("");
      }
      cpgsch(1);
      cpgebuf();

    }
  }

  monserver_close(&monserver);
}


float arraymax(float *a[], int nchan, int nprod) {
  float max, thismax;
  int i;

  max = a[0][0];
  for (i=0; i<nprod; i++)  {
    ippsMax_32f(a[i], nchan, &thismax);
    if (thismax>max) max = thismax;
  }
  return max;
}

float arraymin(float *a[], int nchan, int nprod) {
  float min, thismin;
  int i;

  min = a[0][0];
  for (i=0; i<nprod; i++)  {
    ippsMin_32f(a[i], nchan, &thismin);
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


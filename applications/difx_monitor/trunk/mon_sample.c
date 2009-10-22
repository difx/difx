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
#include "monserver.h"
#define MAX_PROD 4

int main(int argc, const char * argv[]) {
  int status, prod, i, nchan=0, nprod;
  unsigned int iprod[MAX_PROD];
  struct monclient monserver;
  float *xval=NULL, *amp=NULL, *phase=NULL, *lags=NULL, *lagx=NULL;
  float delta, min, max, temp;
  cf32 *vis;
  IppsFFTSpec_R_32f* fftspec=NULL;

  if(argc < 3)  {
    fprintf(stderr, "Error - invoke with mon_sample <host> <product#>\n");
    return(EXIT_FAILURE);
  }

  if (argc-2>MAX_PROD) {
    fprintf(stderr, "Error - Too many producst requested\n");
    return(EXIT_FAILURE);
  }

  for (i=2; i<argc; i++) iprod[i-2] = atoi(argv[i]);
  nprod = argc-2;

  status  = monserver_connect(&monserver, (char*)argv[1], -1);
  if (status) exit(1);

  printf("Opened connection to monitor server\n");

  status = monserver_requestproducts(monserver, iprod, nprod);
  if (status) exit(1);

  status = cpgbeg(0,"/xs",1,3);
  if (status != 1) {
    printf("Error opening pgplot device\n");
  }
  
  cpgask(0);

  printf("Sent product request\n");

  status = 0;
  while (!status) {
    status = monserver_readvis(&monserver);
    if (!status) {
      printf("Got visibility # %d\n", monserver.timestamp);

      while (!monserver_nextvis(&monserver, &prod, &vis)) {
	printf("Got visibility for product %d\n", prod);

	if (nchan!=monserver.numchannels) {
	  nchan = monserver.numchannels;
	  if (xval!=NULL) vectorFree(xval);
	  if (amp!=NULL) vectorFree(amp);
	  if (phase!=NULL) vectorFree(phase);
	  if (lags!=NULL) vectorFree(lags);
	  if (lagx!=NULL) vectorFree(lagx);
	  if (fftspec!=NULL) ippsFFTFree_R_32f(fftspec);

	  xval = vectorAlloc_f32(nchan);
	  amp = vectorAlloc_f32(nchan);
	  phase = vectorAlloc_f32(nchan);
	  lags = vectorAlloc_f32(nchan*2);
	  lagx = vectorAlloc_f32(nchan*2);

	  if (xval==NULL || amp==NULL || phase==NULL || lags==NULL || lagx==NULL) {
	    fprintf(stderr, "Failed to allocate memory for plotting arrays\n");
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
	}

	ippsMagnitude_32fc(vis, amp, nchan);
	ippsPhase_32fc(vis, phase, nchan);
	vectorMulC_f32_I(180/M_PI, phase, nchan);

	ippsFFTInv_CCSToR_32f((Ipp32f*)vis, lags, fftspec, 0);
	//rearrange the lags into order
	for(i=0;i<nchan;i++) {
	  temp = lags[i];
	  lags[i] = lags[i+nchan];
	  lags[i+nchan] = temp;
	}

	ippsMax_32f(amp, nchan, &max);
	ippsMin_32f(amp, nchan, &min);
	delta = (max-min)*0.05;
	if (delta==0.0) delta = 0.5;
	min -= delta;
	max += delta;

	cpgbbuf();
	cpgsci(1);
	cpgenv(0,nchan,min,max,0,0);
	cpglab("Channel", "Amplitude", "");
	cpgsci(2);
	cpgline(nchan, xval, amp);

	ippsMax_32f(phase, nchan, &max);
	ippsMin_32f(phase, nchan, &min);
	delta = (max-min)*0.1;
	if (delta==0.0) delta = 0.5;
	min -= delta;
	max += delta;
	cpgsci(1);
	cpgenv(0,nchan,min,max,0,0);
	cpglab("Channel", "Phase", "");
	cpgsci(2);
	cpgpt(nchan, xval, phase, 17);


	ippsMax_32f(lags, nchan*2, &max);
	ippsMin_32f(lags, nchan*2, &min);
	delta = (max-min)*0.1;
	if (delta==0.0) delta = 0.5;
	min -= delta;
	max += delta;
	cpgsci(1);
	cpgenv(-nchan,nchan,min,max,0,0);
	cpglab("Channel", "Delay", "");
	cpgsci(2);
	cpgline(nchan*2, lagx, lags);
	cpgebuf();

      }
      printf("\n");
    }
  }

  monserver_close(monserver);
}



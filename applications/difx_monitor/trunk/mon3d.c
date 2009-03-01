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
#include <pthread.h>
#include <unistd.h>

#define NTIMES 32

#include "architecture.h"
#include "monserver.h"

void *plotit (void *arg);

pthread_mutex_t vismutex = PTHREAD_MUTEX_INITIALIZER;
int32_t nchan;
int32_t timestamp=-1;
cf32 *buf=NULL;
cf32 *plotbuf=NULL;

int main(int argc, const char * argv[]) {
  int status, prod, i, nchan=0;
  char monhostname[] = "localhost";
  struct monclient monserver;
  float *xval=NULL, *amp=NULL, *phase=NULL, *lags=NULL, *lagx=NULL;
  float delta, min, max, temp;
  IppsFFTSpec_R_32f* fftspec=NULL;
  cf32 *vis;
  pthread_t plotthread;

  status  = monserver_connect(&monserver, monhostname, -1);
  if (status) exit(1);

  printf("Opened connection to monitor server\n");

  status = monserver_requestproduct(monserver, 0);
  if (status) exit(1);

  printf("Sent product request\n");


  /* Start plotting thread */
  status = pthread_create( &plotthread, NULL, plotit, NULL);
  if (status) {
    printf("Failed to start net read thread: %d\n", status);
    exit(1);    
  }

  status = 0;
  while (!status) {
    status = monserver_readvis(&monserver);
    if (!status) {
      if (monserver.timestamp==-1) continue;
      printf("Got visibility # %d\n", monserver.timestamp);

      pthread_mutex_lock(&vismutex);	
      timestamp=monserver.timestamp;
      if (nchan!=monserver.numchannels) {
	nchan = monserver.numchannels;
	if (buf!=NULL) vectorFree(buf);
	buf = vectorAlloc_cf32(nchan);
      }

      if (monserver_nextvis(&monserver, &prod, &vis)) {
	fprintf(stderr, "Error getting visibility. Aborting\n");
	monserver_close(monserver);
	pthread_mutex_unlock(&vismutex);	
	exit(1);
      }
      ippsCopy_32fc(vis, buf, nchan);

      pthread_mutex_unlock(&vismutex);	
      
      printf("Got visibility for product %d\n", prod);
    } 
  }

  monserver_close(monserver);
}


void *plotit (void *arg) {
  int32_t lasttime=-1;
  int32_t lastchans=-1;
  int reinit, i;
  float **allvis, *visbuf=NULL;

  allvis = new float* [NTIMES];

  while (1) {
    pthread_mutex_lock(&vismutex);
    if (timestamp==lasttime) {
      pthread_mutex_unlock(&vismutex);
      printf("THREAD: sleep\n");
      usleep(10000000); // 1 sec
      continue;
    }

    reinit=0;
    if (lastchans!=nchan) {
      if (plotbuf!=NULL) vectorFree(plotbuf);
      plotbuf = vectorAlloc_cf32(nchan);
      lastchans = nchan;
      reinit=1;
    }

    ippsCopy_32fc(buf, plotbuf, nchan);
    lasttime = timestamp;
    pthread_mutex_unlock(&vismutex);
    printf("THREAD: plot %d\n", timestamp);

    if (reinit) { 
      if (visbuf!=NULL) vectorFree(visbuf);
      visbuf = vectorAlloc_f32(nchan*NTIMES);
      vectorZero_f32(visbuf, nchan*NTIMES);      

      for (i=0; i<NTIMES; i++) {
	allvis[i] = &visbuf[i*nchan];
      }
    }
    
    // Shuffle the values up one
    ippsMove_32f(visbuf, visbuf+nchan, nchan*(NTIMES-1));

    // Get the amp
    ippsMagnitude_32fc(plotbuf, visbuf, nchan);
  }
}


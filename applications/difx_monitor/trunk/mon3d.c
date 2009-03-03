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
#include "s2plot.h"
#include <signal.h>
#include <sys/time.h>

#include "monserver.h"

#define NTIMES 32

#define X1LIMIT 0
#define X2LIMIT NTIMES
#define Y1LIMIT 0
#define Y2LIMIT 256
#define Z1LIMIT 0
#define Z2LIMIT 2

void *plotit(void *arg);
void cb(double *t, int *kc);
void alarm_signal(int sig);

pthread_mutex_t vismutex = PTHREAD_MUTEX_INITIALIZER;
int32_t nchan=0;
int32_t timestamp=-1;
int32_t lasttime=-1;
Ipp32fc *buf=NULL;
Ipp32fc *plotbuf=NULL;
float **allvis;

int main(int argc, const char * argv[]) {
  int status, iprod;
  struct monclient monserver;
  pthread_t plotthread;
  struct itimerval timeval;
  sigset_t set;

  if(argc != 3)  {
    fprintf(stderr, "Error - invoke with mon_sample <host> <product#>\n");
    return(EXIT_FAILURE);
  }
  iprod = atoi(argv[2]);

  status  = monserver_connect(&monserver, (char*)argv[1], -1);
  if (status) exit(1);

  printf("Opened connection to monitor server\n");

  status = monserver_requestproduct(monserver, iprod);
  if (status) exit(1);
  printf("Sent product request\n");

  allvis = malloc(sizeof(float*)*NTIMES);
  if (allvis==NULL) {
    fprintf(stderr, "Failed to allocate memory for image array\n");
    exit(1);
  }

  /* Start plotting thread */
  status = pthread_create( &plotthread, NULL, plotit, &monserver);
  if (status) {
    printf("Failed to start net read thread: %d\n", status);
    exit(1);    
  }

  s2opendo("/s2mono");                     /* Open the display */
  s2swin(X1LIMIT,X2LIMIT,Y1LIMIT,Y2LIMIT,Z1LIMIT,Z2LIMIT);   /* Set the window coordinates */
  s2box("BCDET",0,0,"BCDET",0,0,"BCDET",0,0);  /* Draw coordinate box */

  s2icm("rainbow", 1000, 1500);                /* Install colour map */
  s2scir(1000, 1500);                          /* Set colour range */

  cs2scb(&cb);                                  /* Install the callback */
  cs2dcb();

  signal(SIGALRM, alarm_signal);
  sigemptyset(&set);
  sigaddset(&set, SIGALRM);

  timeval.it_interval.tv_sec = 0; 
  timeval.it_interval.tv_usec = 5e5;
  timeval.it_value.tv_sec = 0;
  timeval.it_value.tv_usec = 5e5;
  setitimer(ITIMER_REAL, &timeval, NULL);

  s2show(1);					/* Show the S2PLOT window */

  return(0);
}

void alarm_signal(int sig) {
  pthread_mutex_lock(&vismutex);
  if (timestamp!=lasttime) {
    cs2ecb();
  }
  pthread_mutex_unlock(&vismutex);
}

void cb(double *t, int *kc) {
  int i, reinit;
  float tr[8];
  static int32_t lastchans=-1;
  static  float *visbuf=NULL;

  pthread_mutex_lock(&vismutex);
  lasttime=timestamp;
  reinit=0;
  if (lastchans!=nchan) {
    if (plotbuf!=NULL) ippsFree(plotbuf);
    plotbuf = ippsMalloc_32fc(nchan);
    lastchans = nchan;
    reinit=1;
  }

  ippsCopy_32fc(buf, plotbuf, nchan);
  pthread_mutex_unlock(&vismutex);
  printf("THREAD: plot %d\n", timestamp);
  
  if (reinit) { 
    if (visbuf!=NULL) ippsFree(visbuf);
    visbuf = ippsMalloc_32f(nchan*NTIMES);
    ippsZero_32f(visbuf, nchan*NTIMES);      
      
    for (i=0; i<NTIMES; i++) {
      allvis[i] = &visbuf[i*nchan];
    }
    printf("..Done\n");
  } else {

    // Shuffle the values up one
    ippsMove_32f(visbuf, visbuf+nchan, nchan*(NTIMES-1));
  }

  // Get the amp
  ippsMagnitude_32fc(plotbuf, visbuf, nchan);

  if (reinit) {
    for (i=1; i<NTIMES; i++) {
      ippsCopy_32f(visbuf, visbuf+(nchan)*i, nchan);
    }
  }

  /* set up a unity transformation matrix */
  tr[0] = 0;
  tr[1] = 1; 
  tr[2] = 0.0;
  tr[3] = 0;
  tr[4] = 0.0;
  tr[5] = 1;
  tr[6] = 0;
  tr[7] = 1;

  /* plot surface */

  printf("Surface\n");
  s2surp(allvis, NTIMES, nchan, 0, NTIMES-1, 0, nchan-1, 0, Z2LIMIT, tr);

  //XYZ pos, up, vdir;
  
  //ss2qc(&pos, &up, &vdir, 0);

  //printf("Camera @ Pos: %5.2f %5.2f %5.2f\n",pos.x,pos.y,pos.z);
  //printf("          Up: %5.2f %5.2f %5.2f\n",up.x,up.y,up.z);
  //printf("        Vdir: %5.2f %5.2f %5.2f\n",vdir.x,vdir.y,vdir.z);

  cs2dcb();
  
}

void *plotit (void *arg) {
  int status, prod;
  struct monclient *monserver;
  Ipp32fc *vis;

  monserver = arg;

  status = 0;
  while (!status) {
    status = monserver_readvis(monserver);
    if (!status) {
      if (monserver->timestamp==-1) continue;
      printf("Got visibility # %d\n", monserver->timestamp);

      pthread_mutex_lock(&vismutex);	
      timestamp=monserver->timestamp;
      if (nchan!=monserver->numchannels) {
	nchan = monserver->numchannels;
	if (buf!=NULL) ippsFree(buf);
	buf = ippsMalloc_32fc(nchan);
      }

      if (monserver_nextvis(monserver, &prod, &vis)) {
	fprintf(stderr, "Error getting visibility. Aborting\n");
	monserver_close(*monserver);
	pthread_mutex_unlock(&vismutex);	
	exit(1);
      }
      ippsCopy_32fc(vis, buf, nchan);

      pthread_mutex_unlock(&vismutex);	
      
      printf("Got visibility for product %d\n", prod);
    } 




  }

  monserver_close(*monserver);

  return(0);
}



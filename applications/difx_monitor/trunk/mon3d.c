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

#ifndef NOIPP
#define USEIPP
#endif

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include "s2plot.h"
#include <signal.h>
#include <sys/time.h>
#include <math.h>

#include "configuration.h"

#define USEIPP

#ifdef USEIPP
#include <ippi.h>
#define FCMALLOC(x) ippsMalloc_32fc(x)
#define FMALLOC(x) ippsMalloc_32f(x)
#define FREE(x) ippsFree(x)
#else
//#include <complex.h>
#include <fftw3.h>
typedef  fftwf_complex Ipp32fc;
typedef float Ipp32f;
#define FCMALLOC(x) (float(*)[2])fftwf_malloc(sizeof(fftwf_complex)*x)
#define FMALLOC(x) (float*)fftwf_malloc(sizeof(float)*x)
#define FREE(x) fftwf_free(x)
#endif
#include "monserver.h"

#define NTIMES 16
#define DISPLAYCHAN 64

#define X1LIMIT 0
#define X2LIMIT 100
#define Y1LIMIT 0
#define Y2LIMIT 100
#define Z1LIMIT 0
#define Z2LIMIT 12

void *plotit(void *arg);
void cb(double *t, int *kc);
void alarm_signal(int sig);

pthread_mutex_t vismutex = PTHREAD_MUTEX_INITIALIZER;
int32_t nchan=0;
int32_t timestamp=-1;
Ipp32fc *buf=NULL;
float **lagdisplay;
int sequence, lastseq;

int main(int argc, const char * argv[]) {
  int status, iprod, currentscan, currentconfig;
  struct monclient monserver;
  pthread_t plotthread;
  struct itimerval timeval;
  sigset_t set;
  vector<DIFX_ProdConfig> products;
  struct product_offset plotprod;
  Configuration *config;

  if(argc != 4 )  {
    fprintf(stderr, "Error - invoke with mon_sample <host> <input> <product#>\n");
    return(EXIT_FAILURE);
  }
  iprod = atoi(argv[3]);

  config = new Configuration(argv[2], 0);
  if (!config->consistencyOK()) {
    cerr << "Aborting" << endl;
    exit(1);
  }

  status  = monserver_connect(&monserver, (char*)argv[1], -1);
  if (status) exit(1);

  printf("Opened connection to monitor server\n");

  currentscan = 0;
  currentconfig = config->getScanConfigIndex(currentscan);
  products = monserver_productconfig(config, currentconfig);
  set_productoffsets(1, &iprod, &plotprod, products);

  status = monserver_requestproducts_byoffset(monserver, &plotprod, 1);
  if (status) exit(1);
  printf("Sent product request\n");

  lagdisplay = (float**)malloc(sizeof(float*)*NTIMES);
  if (lagdisplay==NULL) {
    fprintf(stderr, "Failed to allocate memory for image array\n");
    exit(1);
  }

  sequence = -1;
  lastseq = -1;

  /* Start plotting thread */
  status = pthread_create( &plotthread, NULL, plotit, &monserver);
  if (status) {
    printf("Failed to start net read thread: %d\n", status);
    exit(1);    
  }

  s2opendo((char*)"/s2mono");                     /* Open the display */

  s2icm((char*)"rainbow", 1000, 1500);                /* Install colour map */
  s2scir(1000, 1500);                          /* Set colour range */

  s2swin(X1LIMIT,X2LIMIT,Y1LIMIT,Y2LIMIT,Z1LIMIT,Z2LIMIT);   /* Set the window coordinates */
  s2box((char*)"BCDET",0,0,(char*)"BCDET",0,0,(char*)"BCDET",0,0);  /* Draw coordinate box */

  cs2scb((void*)&cb);                                  /* Install the callback */
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
  if (sequence!=lastseq) {
    cs2ecb();
  }
  pthread_mutex_unlock(&vismutex);
}

void cb(double *t, int *kc) {
  int i, reinit;
#ifdef USEIPP
  IppStatus status;
  static IppiFFTSpec_C_32fc *spec;
#else
  fftw_plan plan;
#endif
  static Ipp32f max;
  static int32_t lastchans=-1;
  static  Ipp32fc *visbuf=NULL, *lags=NULL;
  static Ipp32f *lagamp=NULL;
  static float tr[8];
  static int nav;
  static Ipp32fc navc;

  pthread_mutex_lock(&vismutex);
  lastseq=sequence;
  reinit=0;
  if (lastchans!=nchan) {
    reinit=1;
    int orderx, ordery;

    lastchans = nchan;
    nav = nchan/DISPLAYCHAN;
    navc.re  = nav;
    navc.im = 0;

    if (visbuf!=NULL) FREE(visbuf);
    visbuf = FCMALLOC(DISPLAYCHAN*NTIMES);

    if (lags!=NULL) FREE(lags);
    lags = FCMALLOC(DISPLAYCHAN*NTIMES);

    if (lagamp!=NULL) FREE(lagamp);
    lagamp = FMALLOC(DISPLAYCHAN*NTIMES);

#ifdef USEIPP
    if (spec !=NULL) ippiFFTFree_C_32fc(spec);
    orderx = round(log(NTIMES)/log(2));
    ordery = round(log(DISPLAYCHAN)/log(2));
    printf("FFT: %dx%d\n", orderx, ordery);
    ippiFFTInitAlloc_C_32fc( &spec, ordery, orderx, IPP_FFT_DIV_INV_BY_N, ippAlgHintFast);
#else
    
#endif

    for (i=0; i<NTIMES; i++) {
     lagdisplay[i] = &lagamp[i*DISPLAYCHAN];
    }
  } else {

    // Shuffle the values up one
    ippsMove_32fc(visbuf, visbuf+DISPLAYCHAN, DISPLAYCHAN*(NTIMES-1));
  }

  // Copy the newest value
  for (i=0; i<DISPLAYCHAN; i++) {
    ippsSum_32fc(&buf[i*nav], nav, &visbuf[i], ippAlgHintFast);
  }
  ippsDivC_32fc_I(navc, visbuf, DISPLAYCHAN);
  //ippsCopy_32fc(buf, visbuf, nchan);

  pthread_mutex_unlock(&vismutex);
  printf("THREAD: plot %d\n", timestamp);

  if (reinit) { // Duplicate the first vis is starting from scratch
    for (i=1; i<NTIMES; i++) {
      ippsCopy_32fc(visbuf, visbuf+(DISPLAYCHAN)*i, DISPLAYCHAN);
    }

    /* set up a unity transformation matrix */
    tr[0] = 0;
    tr[1] = 1.0/NTIMES*100; 
    tr[2] = 0.0;
    tr[3] = 0;
    tr[4] = 0;
    tr[5] = 1.0/DISPLAYCHAN*100;
    tr[6] = 0;
    tr[7] = 1;
  }

  //Do the fft 
#ifdef USEIPP
  status = ippiFFTFwd_CToC_32fc_C1R(visbuf, DISPLAYCHAN*sizeof(Ipp32fc), lags, DISPLAYCHAN*sizeof(Ipp32fc),
				     spec, 0 );
  if (status!=ippStsNoErr) {
    fprintf(stderr, "Error calling ippiFFT. Aborting\n");
    exit(1);
  }
#else
  fftwf_execute(plan);
#endif


  // Get the Lag amplitude. Also translate the origin to the middle of the
  // image, assuming symmetry of the data

  for (i=0;i<NTIMES/2;i++) {
    status = ippsMagnitude_32fc(lags+i*DISPLAYCHAN, lagamp+i*DISPLAYCHAN+DISPLAYCHAN/2+NTIMES/2*DISPLAYCHAN, DISPLAYCHAN/2);
    if (status!=ippStsNoErr) {
      fprintf(stderr, "Error calling ippsMagnitude. Aborting\n");
      exit(1);
    }
    status = ippsMagnitude_32fc(lags+i*DISPLAYCHAN+DISPLAYCHAN/2, lagamp+i*DISPLAYCHAN+NTIMES/2*DISPLAYCHAN, DISPLAYCHAN/2);
    if (status!=ippStsNoErr) {
      fprintf(stderr, "Error calling ippsMagnitude. Aborting\n");
      exit(1);
    }
    
    status = ippsMagnitude_32fc(lags+i*DISPLAYCHAN+NTIMES/2*DISPLAYCHAN,  lagamp+i*DISPLAYCHAN+DISPLAYCHAN/2, DISPLAYCHAN/2);
    if (status!=ippStsNoErr) {
      fprintf(stderr, "Error calling ippsMagnitude. Aborting\n");
      exit(1);
    }
    status = ippsMagnitude_32fc(lags+i*DISPLAYCHAN+DISPLAYCHAN/2+NTIMES/2*DISPLAYCHAN,  lagamp+i*DISPLAYCHAN, DISPLAYCHAN/2);
    if (status!=ippStsNoErr) {
      fprintf(stderr, "Error calling ippsMagnitude. Aborting\n");
      exit(1);
    }
  }

  /*
  status = ippsMagnitude_32fc(lags, lagamp, DISPLAYCHAN*NTIMES);
  if (status!=ippStsNoErr) {
    fprintf(stderr, "Error calling ippsMagnitude. Aborting\n");
    exit(1);
   }
  */

  // Normalize
  ippsDivC_32f_I(DISPLAYCHAN*NTIMES/100, lagamp, DISPLAYCHAN*NTIMES);
  //ippsAddC_32f_I(DISPLAYCHAN*NTIMES, lagamp, 1);

  ippsSqrt_32f_I(lagamp, DISPLAYCHAN*NTIMES);

  /*
  for (i=0; i<DISPLAYCHAN; i++) {
    for (j=0; j<NTIMES; j++) {
      printf(" %.2f", lagamp[j*NTIMES+i]);
    }
    printf("\n");
  }
  */

  if (reinit) {
    ippsMax_32f(lagamp, DISPLAYCHAN*NTIMES, &max);
    s2swin(X1LIMIT,X2LIMIT,Y1LIMIT,Y2LIMIT,0,max*1.1);   /* Set the window coordinates */
    //s2box("BCDET",0,0,"BCDET",0,0,"BCDET",0,0);  /* Draw coordinate box */

  }
  
  /* plot surface */

  s2surp(lagdisplay, NTIMES, DISPLAYCHAN, 0, NTIMES-1, 0, DISPLAYCHAN-1, 0, max/2, tr);

  //XYZ pos, up, vdir;
  
  //ss2qc(&pos, &up, &vdir, 0);

  //printf("Camera @ Pos: %5.2f %5.2f %5.2f\n",pos.x,pos.y,pos.z);
  //printf("          Up: %5.2f %5.2f %5.2f\n",up.x,up.y,up.z);
  //printf("        Vdir: %5.2f %5.2f %5.2f\n",vdir.x,vdir.y,vdir.z);

  cs2dcb();
  
}

void *plotit (void *arg) {
  int status, prod, numchan;
  struct monclient *monserver;
  Ipp32fc *vis;

  monserver = (struct monclient *)arg;

  status = 0;
  while (!status) {
    status = monserver_readvis(monserver);
    if (!status) {
      if (monserver->timestamp==-1) continue;
      printf("Got visibility # %d\n", monserver->timestamp);

      pthread_mutex_lock(&vismutex);	
      timestamp=monserver->timestamp;
      sequence++;

      if (monserver_nextvis(monserver, &prod, &numchan, &vis)) {
	fprintf(stderr, "Error getting visibility. Aborting\n");
	monserver_close(monserver);
	pthread_mutex_unlock(&vismutex);	
	exit(1);
      }
      if (nchan!=numchan) {
	nchan = numchan;
	if (buf!=NULL) FREE(buf);
	buf = FCMALLOC(nchan);
      }

      ippsCopy_32fc(vis, buf, nchan);

      pthread_mutex_unlock(&vismutex);	
      
      printf("Got visibility for product %d\n", prod);
    } 
  }

  monserver_close(monserver);

  return(0);
}



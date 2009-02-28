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


int main(int argc, const char * argv[]) {
  int status, prod, i, nchan=0;
  char monhostname[] = "localhost";
  struct monclient monserver;
  float *xval=NULL, *yval=NULL, min, max;
  cf32 *vis;

  status  = monserver_connect(&monserver, monhostname, -1);
  if (status) exit(1);

  printf("Opened connection to monitor server\n");

  status = monserver_requestproduct(monserver, 0);
  if (status) exit(1);

  status = cpgbeg(0,"/xs",1,1);
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
	  if (xval!=NULL) delete [] xval;
	  if (yval!=NULL) delete [] yval;
	  xval = new float [nchan];
	  yval =  new float [nchan];
	  if (xval==NULL || yval==NULL) {
	    fprintf(stderr, "Failed to allocate memory for plotting arrays\n");
	    exit(1);
	  }
	  for (i=0; i<nchan; i++) {
	    xval[i] = i;
	  }
	}

	status = ippsMagnitude_32fc(vis, yval, nchan);
	if (status != vecNoErr) {
	  printf("Error calculating vector magnitude\n");
	}
	ippsMax_32f(yval, nchan, &max);
	ippsMin_32f(yval, nchan, &min);

	cpgsci(1);
	cpgenv(0,nchan,min,max,0,0);
	cpglab("Channel", "Amplitude", "");
	cpgsci(2);
	cpgline(nchan, xval, yval);

      }
      printf("\n");
    }
  }

  monserver_close(monserver);
}



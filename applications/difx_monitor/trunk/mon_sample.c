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

#include "monserver.h"

int main(int argc, const char * argv[]) {
  int status, prod;
  char monhostname[] = "localhost";
  struct monclient monserver;
  complex float *vis;

  status  = monserver_connect(&monserver, monhostname, -1);
  if (status) exit(1);

  printf("Opened connection to monitor server\n");

  status = monserver_requestproduct(monserver, 0);
  if (status) exit(1);

  printf("Sent product request\n");

  status = 0;
  while (!status) {
    status = monserver_readvis(&monserver);
    if (!status) {
      while (!monserver_nextvis(&monserver, &prod, &vis)) {
	printf("Got visibility for product %d\n", prod);
      }
      printf("\n");
    }


  }


  


  monserver_close(monserver);


}



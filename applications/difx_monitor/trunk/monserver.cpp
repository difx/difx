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
#include <unistd.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <string.h>
#include <errno.h>

#include "monserver.h"

#ifndef MSG_NOSIGNAL
#define MSG_NOSIGNAL SO_NOSIGPIPE 
#endif

int readnetwork(int sock, char* ptr, int bytestoread) {
  int nr;

  while (bytestoread>0)
  {
    nr = recv(sock,ptr,bytestoread,0);

    if (nr==-1) { // Error reading socket
      if (errno == EINTR) continue;
      fprintf(stderr, "Error reading socket\n");
      return(-1);
    } else if (nr==0) {  // Socket closed remotely
      fprintf(stderr, "Socket closed remotely\n");
      return(-1);
    } else {
      ptr += nr;
      bytestoread -= nr;
    }
  }
  return(0);
}

int writenetwork(int sock, char* ptr, int bytestowrite) {
  int nwrote;

  while (bytestowrite>0) {

    //nwrote = send(sock, ptr, bytestowrite, MSG_NOSIGNAL|MSG_DONTWAIT);
    nwrote = send(sock, ptr, bytestowrite, MSG_NOSIGNAL);
    if (nwrote==-1) {
      if (errno == EINTR) continue;
      perror("\nError writing to network");

      return(1);
    } else if (nwrote==0) {
      fprintf(stderr, "Warning: Did not write any bytes!\n");
      return(1);
    } else {
      bytestowrite -= nwrote;
      ptr += nwrote;
    }
  }
  return(0);
}

void sendint(int sock, int32_t val, int *status) {
  if (*status) return;
  *status = writenetwork(sock, (char*)&val, sizeof(int32_t)); 
  return;
}

void readint(int sock, int32_t *val, int *status) {
  if (*status) return;
  *status = readnetwork(sock, (char*)val, sizeof(int32_t)); 
  return;
}

int monserver_connect(struct monclient *monclient, char *monhostname, int window_size) {
  int sock, status;
  int32_t status32;
  unsigned long ip_addr;
  struct hostent     *hostptr;
  struct sockaddr_in server;

  memset(monclient, 0, sizeof(struct monclient));

  // Setup network structures etc
  hostptr = gethostbyname(monhostname);
  if (hostptr==NULL) {
    fprintf(stderr,"Failed to look up hostname %s\n", monhostname);
    return(1);
  }

  memset((char *) &server, 0, sizeof(server));
  server.sin_family = AF_INET;
  server.sin_port = htons(MONITOR_PORT); /* Which port number to use */
  memcpy(&ip_addr, (char *)hostptr->h_addr, sizeof(ip_addr));
  server.sin_addr.s_addr = ip_addr;
  
  printf("Connecting to %s\n",inet_ntoa(server.sin_addr));

  /* Create the initial socket */
  sock = socket(AF_INET,SOCK_STREAM,0); 
  if (sock==-1) {
    perror("Error creating socket");
    return(1);
  }

  if (window_size>0) {
    status = setsockopt(sock, SOL_SOCKET, SO_SNDBUF,
			(char *) &window_size, sizeof(window_size));
    if (status!=0) {
      perror("Error setting socket send buffer");
      close(sock);
      return(1);
    } 

    status = setsockopt(sock, SOL_SOCKET, SO_RCVBUF,
			(char *) &window_size, sizeof(window_size));
    
    if (status!=0) {
      perror("Error setting socket receive buffer");
      close(sock);
      return(1);
    }
  }

  // Actually connect to remote host

  status = connect(sock, (struct sockaddr *) &server, sizeof(server));
  if (status!=0) {
    perror("Failed to connect to server");
    close(sock);
    return(1);
  }

  // Receive a status message from the server - 4 byte code

  status32 = 0;
  status = readnetwork(sock, (char*)&status32, sizeof(status32));
  if (status) return(status);

  if (status32!=DIFXMON_NOERROR) {
    fprintf(stderr, "Monitor server refused connection with Error %d\n", status32);
    close(sock);
    return(1);
  }

  monclient->fd = sock;

  return(0);
}

int monserver_close(struct monclient *client) {
  if (client->nvis>0) delete [] client->vis; // Probably should zero, but that means passing a pointer
  if (client->bufsize>0) delete [] client->visbuf;
  client->nvis = 0;
  client->bufsize = 0;
  client->nretvis = 0;

  return(close(client->fd));
}

int monserver_sendstatus(int sock, int32_t status32) {
  return(writenetwork(sock, (char*)&status32, sizeof(status32)));
}

//int monserver_requestproduct(struct monclient client, unsigned int product) {
//  return monserver_requestproducts(client, &product, 1);
//}

int monserver_requestproducts_byoffset(struct monclient client, struct product_offset offset[], int nprod) {
  int status, i;
  int32_t  status32;
  
  status = DIFXMON_NOERROR;

  if (status) return(status);

  status32 = 0;

  sendint(client.fd, nprod, &status);
  for (i=0; i<nprod; i++) {
    sendint(client.fd, offset[i].offset, &status);
    sendint(client.fd, offset[i].npoints, &status);
    sendint(client.fd, offset[i].product, &status);
  }
  readint(client.fd, &status32, &status);
  if (status) return(status);

  if (status32!=DIFXMON_NOERROR) {
    fprintf(stderr, "Error %d requesting products from monitor\n", status32);
    return(1);
  }

  return(0);
}

//int monserver_requestall(struct monclient client) {
//  unsigned int product=-1;
//  return monserver_requestproducts(client, &product, 1);
//}

int monserver_readvis(struct monclient *client) {
  int status;
  int32_t headbuf[3], bufsize;

  status = readnetwork(client->fd, (char*)&headbuf, sizeof(headbuf)); 
  if (status) return(status);

  client->timestamp = headbuf[0];
  client->ivis = 0;
  client->ioffset = 0;
  client->nretvis = headbuf[1];
  bufsize = headbuf[2];
  if (client->bufsize != bufsize) {
    if (client->bufsize!=0) {
      delete [] client->visbuf;
      client->bufsize = 0;
    }
    client->visbuf = new char [bufsize];
    if (client->visbuf==NULL) {
      fprintf(stderr, "Error: Could not allocate memory for visibility buffer\n");
      return(1);
    }
    client->bufsize = bufsize;
  }
  status = readnetwork(client->fd, (char*)client->visbuf, bufsize);

  return(status);
}

int monserver_nextvis(struct monclient *client, int *product, int *nchan,
		      Ipp32fc **vis) {
  int ivis = client->ivis;

  if (ivis >= client->nretvis)  return(1);

  *nchan = *(int32_t*)(client->visbuf+client->ioffset);
  client->ioffset += sizeof(int32_t);
  *product = *(int32_t*)(client->visbuf+client->ioffset);
  client->ioffset += sizeof(int32_t);
  *vis = (Ipp32fc*)(client->visbuf+client->ioffset);
  client->ioffset += *nchan*sizeof(cf32);
  client->ivis++;
  return(0);
}

void monserver_resetvis(struct monclient *client) {
  client->ivis=0;
}

int monserver_dupclient(struct monclient client, struct monclient *copy) {

  if (client.visbuf==NULL) {
    copy->visbuf=NULL;
  } else {
    copy->visbuf = new char [client.bufsize];
    if (copy->visbuf==NULL) {
      return(DIFXMON_MALLOCERROR);
    }
    memcpy(copy->visbuf,client.visbuf,client.bufsize);
  }
  if (client.vis==NULL) {
    copy->vis=NULL;
  } else {
    copy->vis = new struct product_offset [client.nvis];
    if (copy->vis==NULL) {
      delete [] copy->visbuf;
      return(DIFXMON_MALLOCERROR);
    }
    memcpy(copy->vis,client.vis,client.nvis*sizeof(struct product_offset));
  }

  copy->fd = 0; // Don't copy file handle
  copy->nvis = client.nvis;
  copy->ivis = 0; // Reset counter
  copy->timestamp = client.timestamp;
  //copy->numchannels = client.numchannels;
  copy->nretvis = client.nretvis;
  copy->bufsize = client.bufsize;
  
  return(0);
}

// Copy all of the elements from on struct to another
void monserver_copyclient(struct monclient client, struct monclient *copy) {
  copy->fd = client.fd;
  copy->nvis = client.nvis;
  copy->ivis = client.ivis;
  copy->timestamp = client.timestamp;
  //copy->numchannels = client.numchannels;
  copy->nretvis = client.nretvis;
  copy->bufsize = client.bufsize;
  copy->visbuf = client.visbuf;
  copy->vis = client.vis;
}

// Destroy the elements. Time to move to a full C++ object, I think
void monserver_clear(struct monclient *client) {
  client->fd = 0;
  client->nvis = 0;
  client->ivis = 0;
  client->timestamp = -1;
  //client->numchannels = 0;
  client->nretvis = 0;
  client->bufsize = 0;
  delete [] client->visbuf;
  delete [] client->vis;
  client->visbuf = NULL;
  client->vis = NULL;
}

vector<DIFX_ProdConfig> monserver_productconfig(Configuration *config, int configindex) {
  char polpair[3];
  vector<DIFX_ProdConfig> products;
  int binloop = 1;

  polpair[2] = 0;
  
  if(config->pulsarBinOn(configindex) && !config->scrunchOutputOn(configindex))
    binloop = config->getNumPulsarBins(configindex);

  for (int i=0;i<config->getNumBaselines();i++) {
    int ds1index = config->getBDataStream1Index(configindex, i);
    int ds2index = config->getBDataStream2Index(configindex, i);

    for(int j=0;j<config->getBNumFreqs(configindex,i);j++) {
      int freqindex = config->getBFreqIndex(configindex, i, j);
      int resultindex = config->getCoreResultBaselineOffset(configindex, freqindex, i);
      int freqchannels = config->getFNumChannels(freqindex)/config->getFChannelsToAverage(freqindex);

      for(int s=0;s<config->getMaxPhaseCentres(configindex);s++) {
	for(int b=0;b<binloop;b++) {
	  for(int k=0;k<config->getBNumPolProducts(configindex, i, j);k++) {
	    config->getBPolPair(configindex,i,j,k,polpair);




	    products.push_back(DIFX_ProdConfig(ds1index,
					       ds2index,
					       config->getTelescopeName(ds1index), 
					       config->getTelescopeName(ds2index),
					       config->getFreqTableFreq(freqindex),
					       config->getFreqTableBandwidth(freqindex),
					       polpair,
					       s,
					       b,
					       resultindex,
					       freqchannels,
					       config->getFreqTableLowerSideband(freqindex)));
	    resultindex += freqchannels;
	  }
	}
      }
    }
  }

  // Autocorrelations

  int autocorrwidth = (config->getMaxProducts()>2)?2:1;

  for(int i=0;i<config->getNumDataStreams();i++) {
    int offset = config->getCoreResultAutocorrOffset(configindex, i);

    for(int j=0;j<autocorrwidth;j++) {
      for(int k=0;k<config->getDNumRecordedBands(configindex, i); k++) {

	polpair[0] = config->getDRecordedBandPol(configindex, i, k);
	if (j==0)
	  polpair[1] = polpair[0];
	else
	  polpair[1] = ((polpair[0] == 'R')?'L':'R');
	int freqindex = config->getDRecordedFreqIndex(configindex, i, k);
	int nchan = config->getFNumChannels(freqindex)/config->getFChannelsToAverage(freqindex);

	products.push_back(DIFX_ProdConfig(i,
					   i,
					   config->getDStationName(configindex, i), 
					   "",
					   config->getFreqTableFreq(freqindex),
					   config->getFreqTableBandwidth(freqindex),
					   polpair,
					   0,
					   0,
					   offset,
					   nchan,
					   config->getFreqTableLowerSideband(freqindex)));
	offset += nchan;
      }
    }
  }
  return products;
}


DIFX_ProdConfig::DIFX_ProdConfig(int TelIndex1, int TelIndex2, string TelName1, 
				 string TelName2, double freq, double bandwidth, 
				 char polpair[3], int nbin, int nphasecentre, int offset,
				 int nchan, bool lsbval) {
  TelescopeIndex1 = TelIndex1;
  TelescopeIndex2 = TelIndex2;
  TelescopeName1 = TelName1;
  TelescopeName2 = TelName2;
  Bandwidth = bandwidth;
  Freq = freq;
  PolPair[0] = polpair[0];
  PolPair[1] = polpair[1];
  PolPair[2] = 0;
  Nbin = nbin;
  Nphasecentre = nphasecentre;
  Offset = offset;
  Nchan = nchan;
  lsb = lsbval;
}

DIFX_ProdConfig::~DIFX_ProdConfig() {

}

int sec2config (Configuration *config, int sec) {
  Model * model = config->getModel();
  int startsec = config->getStartSeconds();
  int mjd = config->getStartMJD();
  int scan = 0;

  while (scan<model->getNumScans()) {
    if (sec>model->getScanEndSec(scan, mjd, startsec)) {
      scan++;
    } else {
      break;
    }
  }
  return config->getScanConfigIndex(scan);
}

int set_productoffsets(int nprod, int iprod[], struct product_offset offsets[], vector<DIFX_ProdConfig> products) {
  int i;
  int maxprod = products.size();

  for (i=0; i<nprod; i++) {
    if (iprod[i]>maxprod) return(DIFXMON_BADPRODUCTS);
    offsets[i].offset = products[iprod[i]].getOffset();
    offsets[i].npoints = products[iprod[i]].getNFreqChannels();
    offsets[i].product = iprod[i];
  }
  return(DIFXMON_NOERROR);
}

int set_productoffsets_all(int *nprod, struct product_offset **offsets, vector<DIFX_ProdConfig> products) {
  int i;
  *nprod = products.size();

  if (*offsets!=NULL) delete [] (*offsets);
  *offsets = new struct product_offset[*nprod];
  if (*offsets==NULL) return(DIFXMON_MALLOCERROR);

  for (i=0; i<*nprod; i++) {
    (*offsets)[i].offset = products[i].getOffset();
    (*offsets)[i].npoints = products[i].getNFreqChannels();
    (*offsets)[i].product = i;
  }
  return(DIFXMON_NOERROR);
}

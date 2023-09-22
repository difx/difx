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
#ifndef MONSERVER_H
#define MONSERVER_H

#include <string>
#include <stdint.h>

#include <configuration.h>
#include <ipps.h>

using std::string;

#define MONITOR_PORT 52300

#define DIFXMON_NOERROR         0
#define DIFXMON_TOOMANYCLIENTS  1
#define DIFXMON_BADPRODUCTS     2
#define DIFXMON_MALLOCERROR     3

struct product_offset {
  int32_t offset;
  int32_t npoints;
  int32_t product;
};

struct monclient {
  int     fd;       /* file descriptor */
  int     nvis;     /* Number of visibilities to return */
  struct product_offset *vis;     /* Offset/#points of Visibilities to return */
  int32_t timestamp; /* Correlator time field */
  //  int32_t numchannels; /* Number of spectral points/visibility */
  int32_t bufsize;    /* Size contained in visbuf */
  int32_t nretvis;    /* Number of visibilities actually returned */
  char *visbuf; /* buffer of returned visibilities, int32_t followed by numchannel complexfloat */
  int ivis;
  int ioffset;
};

class DIFX_ProdConfig {
 public:

  DIFX_ProdConfig(int TelIndex1, int TelIndex2, string TelName1, string TelName2, double freq, double bandwidth, char polpair[3], int nbin, int nphasecentre, int offset, int nchan, bool lsb);
  ~DIFX_ProdConfig();

  inline int getTelescopeIndex1 () { return TelescopeIndex1; }
  inline int getTelescopeIndex2 () { return TelescopeIndex2; }
  inline double getFreq () { return Freq; }
  inline double getBandwidth () { return Bandwidth; }
  inline string getTelescopeName1 () { return TelescopeName1; }
  inline string getTelescopeName2 () { return TelescopeName2; }
  inline void getPolPair(char polpair[3]) {polpair[0]=PolPair[0]; polpair[1]=PolPair[1]; polpair[2]=0;}
  inline int getOffset () { return Offset; }
  inline int getNFreqChannels () { return Nchan; }
  inline int getLSB () { return lsb; }

 private:

  int TelescopeIndex1;
  int TelescopeIndex2;
  string TelescopeName1;
  string TelescopeName2;
  char PolPair[3];
  bool lsb;
  double Freq;
  double Bandwidth;
  int Nbin;
  int Nphasecentre;
  int Offset;
  int Nchan;

};

int readnetwork(int sock, char* ptr, int bytestoread);
int writenetwork(int sock, char* ptr, int bytestowrite);
void sendint(int sock, int32_t val, int *status);
void readint(int sock, int32_t *val, int *status);

int monserver_connect(struct monclient *monserver, char *monhostname, int window_size);
int monserver_sendstatus(int sock, int32_t status32);
//int monserver_requestproduct(struct monclient client, unsigned int product);
int monserver_requestproducts_byoffset(struct monclient client, struct product_offset offset[], int nprod);
int monserver_requestall(struct monclient client);
int monserver_readvis(struct monclient *client);
int monserver_close(struct monclient *monserver);
int monserver_nextvis(struct monclient *client, int *product, int *nchan,
		      Ipp32fc **vis);
void monserver_resetvis(struct monclient *client);
void monserver_copyclient(struct monclient client, struct monclient *copy);
int monserver_dupclient(struct monclient client, struct monclient *copy);
void monserver_clear(struct monclient *client);
int sec2config (Configuration *config, int sec);
vector<DIFX_ProdConfig> monserver_productconfig(Configuration *config, int configindex);
int set_productoffsets(int nprod, int iprod[], struct product_offset offsets[], vector<DIFX_ProdConfig> products);
int set_productoffsets_all(int *nprod, struct product_offset **offsets, vector<DIFX_ProdConfig> products);

#endif

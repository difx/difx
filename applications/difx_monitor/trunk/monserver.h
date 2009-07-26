#ifndef MONSERVER_H
#define MONSERVER_H

#include <stdint.h>
#include <ipps.h>

#define MONITOR_PORT 52100

#define DIFXMON_NOERROR         0
#define DIFXMON_TOOMANYCLIENTS  1
#define DIFXMON_BADPRODUCTS     2

struct monclient {
  int     fd;       /* file descriptor */
  int     nvis;     /* Number of visibilities to return */
  int32_t *vis;     /* Visibilities numbers to  return*/
  int32_t timestamp; /* Correlator time field */
  int32_t numchannels; /* Number of spectral points/visibility */
  int32_t nretvis;    /* Number of visibilities actually returned */
  char *visbuf; /* buffer of returned visibilities, int32_t followed by numchannel complexfloat */
  int ivis;
};

int readnetwork(int sock, char* ptr, int bytestoread);
int writenetwork(int sock, char* ptr, int bytestowrite);
void sendint(int sock, int32_t val, int *status);

int monserver_connect(struct monclient *monserver, char *monhostname, int window_size);
int monserver_sendstatus(int sock, int32_t status32);
int monserver_requestproduct(struct monclient client, unsigned int product);
int monserver_requestproducts(struct monclient client, unsigned int product[], int nprod);
int monserver_readvis(struct monclient *client);
int monserver_close(struct monclient monserver);
int monserver_nextvis(struct monclient *client, int *product, Ipp32fc **vis);
void monserver_resetvis(struct monclient *client);

#endif

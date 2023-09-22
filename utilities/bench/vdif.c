#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include "vdif.h"

#define YEAR2000_UNIX 946684800
#define UNIXZERO_MJD 40587

#if 0
int main(void) {
  int status;
  vdif_header header;

  vdif_createheader(&header, 8000, 1000, 0, 2, 2, "At");

  vdif_nextheader(&header);

  return(0);
}
#endif

#define VDIF_VERSION 0

int vdif_createheader(vdif_header *header, int framelength, int framepersec,
		      int threadid,  int bits, int nchan,  int iscomplex,
		      char stationid[3]) {
  int lognchan;

  header->epoch = 0;

  if (VDIF_VERSION>7) return(VDIF_ERROR);
  if (bits>32 || bits<1) return(VDIF_ERROR);
  if (framelength%8!=0 || framelength<0) return(VDIF_ERROR);
  if (threadid>1023 || threadid<0) return(VDIF_ERROR);
  if (framepersec<0) return(VDIF_ERROR);

  // Number of channels encoded as power of 2
  if (nchan<1) return(VDIF_ERROR);
  lognchan = 0;
  while (nchan>1) {
    if (nchan%2==1) return(VDIF_ERROR);
    lognchan++;
    nchan /=2;
  }
  if (lognchan>31) return(VDIF_ERROR);

  memset(header, 0, VDIFHEADERSIZE);

  header->version = VDIF_VERSION;
  header->nchan = lognchan;
  header->framelength = framelength/8;
  if (iscomplex)
    header->complex = 1;
  else
    header->complex = 0;
  header->nbits = bits-1;
  header->threadid = threadid;
  header->stationid = stationid[0]<<8 | stationid[1];

  header->frame=0;
  header->framepersec=framepersec;

  return(VDIF_NOERROR);
}

int vdif_nextheader(vdif_header *header) {
  header->frame++;
  if (header->frame>header->framepersec) {
    return(VDIF_ERROR);
  } else if (header->frame==header->framepersec) {
    header->seconds++;
    header->frame = 0;
  }
  return(VDIF_NOERROR);
}

int vdif_settime(vdif_header *header, time_t time) {
  int epoch;
  struct tm t;

  gmtime_r(&time, &t);

  t.tm_sec = 0;
  t.tm_min = 0;
  t.tm_hour = 0;
  t.tm_mday = 1;

  epoch = (t.tm_year-100)*2;

  if (epoch<0)     // Year is year since 1900
    return(VDIF_ERROR);
  if (t.tm_mon<6) {
    t.tm_mon = 0;
  } else {
    t.tm_mon = 6;
    epoch++;
  }

  epoch %= 32;

  setenv("TZ", "", 1); /* Force mktime to return gmt not local time */
  tzset();
  header->unix_epoch = mktime(&t);

  header->epoch = epoch;
  header->seconds = time-header->unix_epoch;
  header->frame = 0;

  return(VDIF_NOERROR);
}

int vdif_setmjd(vdif_header *header, double mjd) {
  time_t time;

  time = (mjd-UNIXZERO_MJD)*60*60*24+0.5;

  return vdif_settime(header, time);
}

double vdif_mjd(vdif_header *header) {
  return (double)header->unix_epoch/(60.0*60.0*24.0)+UNIXZERO_MJD+(double)header->seconds/(60.0*60.0*24.0);
}

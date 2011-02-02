#ifndef VDIFLIB_H
#define VDIFLIB_H

#define VDIFHEADERSIZE  32  // Bytes
#define VDIF_MAXSTR   256
#define MAXVDIFTHREADS 64

#define VDIF_NOERROR 0
#define VDIF_ERROR 1

typedef struct vdif_header {
  u_int32_t seconds: 30;
  u_int32_t legacy : 1;
  u_int32_t invalid : 1;
  u_int32_t frame : 24;
  u_int32_t epoch : 6;
  u_int32_t unassigned : 2;
  u_int32_t framelength : 24;
  u_int32_t nchan : 5;
  u_int32_t version : 3;
  u_int32_t stationid : 16;
  u_int32_t threadid : 10;
  u_int32_t nbits : 5;
  u_int32_t complex : 1;
  u_int32_t eversion : 8;
  u_int32_t extra1 : 24;
  u_int32_t extra2;
  u_int32_t extra3;
  u_int32_t extra4;
 
  int framepersec;
  time_t unix_epoch;
} vdif_header;
  
int vdif_createheader (vdif_header *header, int framelength, int framepersec,
		       int threadid,  int bits, int nchan, int iscomplex,
		       char stationid[3]);
int vdif_settime (vdif_header *header, time_t time);
int vdif_nextheader(vdif_header *header);
double vdif_mjd(vdif_header *header);
int vdif_setmjd(vdif_header *header, double mjd);

#endif

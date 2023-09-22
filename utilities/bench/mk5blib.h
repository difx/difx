#ifndef MK5BLIB_H
#define MK5BLIB_H

#define MK5BFRAMESIZE (2500*4)  // bytes
#define MK5BHEADSIZE  4         // 32bit words
#define MAXSTR        200 

typedef struct mk5bheader {
  u_int32_t header[MK5BHEADSIZE];
  int framepersec;
  int nsec;
  double startmjd;
  unsigned char crcdata[6];
  unsigned short *nframe;
  unsigned short *crc;
} mk5bheader;

void initialise_mark5bheader (mk5bheader *header, int rate, double mjd);
void setmjd_mark5bheader(mk5bheader *header, double startmjd);
void next_mark5bheader(mk5bheader *header);
double mark5b_mjd(mk5bheader *header);
void init_bitreversal ();

#endif

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "mk5blib.h"

static unsigned char bytetable[256];

unsigned int crcc (unsigned char *idata, int len, int mask, int cycl);
unsigned short reversebits16 (unsigned short s);

#if 0
int main(void) {
  int rate;
  double mjd;
  mk5bheader header;

  /* Mjd of start time */
  mjd = 54599;

  init_bitreversal();

  initialise_mark5bheader(&header, rate, mjd);

  next_mark5bheader(&header);

  return(0);
}
#endif

void settime_mark5bheader(mk5bheader *header) {
  unsigned char j1, j2, j3, s1, s2, s3, s4, s5;
  int intmjd, secmjd, fracsec;
  double mjd;

  if (*header->nframe==0) { // Start of second, set first word of VLBA time code 
    if (header->nsec>=60*60*24) {
      header->nsec = 0;
      header->startmjd += 1.0;
    }

    mjd = header->startmjd + header->nsec*(1.0/(60.*60.*24.));
    intmjd = (int)floor(mjd);
    secmjd = (int)floor((mjd-intmjd)*60*60*24+5e-7);  // Avoid round off errors

    // Word 2 VLBA BCD time code 'JJJSSSSS'. Only needs to be updated
    // once a second

    j1 = (intmjd%1000/100);
    j2 = ((intmjd%100)/10);
    j3 = (intmjd%10);
	
    s1 = (secmjd/10000)&0xF;
    s2 = ((secmjd%10000)/1000)&0xF;
    s3 = ((secmjd%1000)/100)&0xF;
    s4 = ((secmjd%100)/10)&0xF;
    s5 = (secmjd%10)&0xF;

    header->header[2]  = j1<<28 | j2<<24 | j3<<20 | 
      s1<<16 | s2<<12 | s3<<8 | s4<<4 | s5;

    header->crcdata[0] = j1<<4|j2;
    header->crcdata[1] = j3<<4|s1;
    header->crcdata[2] = s2<<4|s3;
    header->crcdata[3] = s4<<4|s5;
  }

  // MJD sec fraction
  fracsec = (*header->nframe/(double)header->framepersec)*10000;

  s1 = fracsec/1000;
  s2 = (fracsec%1000)/100;
  s3 = (fracsec%100)/10;
  s4 = fracsec%10;
  header->header[3] = s1<<28 | s2<<24 | s3<<20 | s4<<16;

  // CRC
  header->crcdata[4] = s1<<4|s2;
  header->crcdata[5] = s3<<4|s4;
  *header->crc = reversebits16(crcc(header->crcdata, 48, 040003, 16));
}

void setmjd_mark5bheader(mk5bheader *header, double startmjd) {
  *header->nframe = 0;
  header->startmjd = startmjd;
  header->nsec = 0;
  settime_mark5bheader(header);
}

void initialise_mark5bheader (mk5bheader *header, int rate, double startmjd) {
  // Mark5b Sync word is constant. Blank user bits
  
  header->header[0] = 0xABADDEED;
  header->header[1] = 0xF00F0000;
  header->header[2] = 0x0;
  header->header[3] = 0x0;
  //  Return pointer into header for frame number and crc
  header->nframe = (unsigned short*)&header->header[1];
  header->crc = (unsigned short*)&header->header[3];
  header->framepersec = rate*1e6/8 / MK5BFRAMESIZE;

  setmjd_mark5bheader(header, startmjd);
}

void next_mark5bheader(mk5bheader *header) {
  *header->nframe = (*header->nframe+1) % header->framepersec;
  if (*header->nframe==0) header->nsec++;
  settime_mark5bheader(header);
}

double mark5b_mjd(mk5bheader *header) {
  return(header->startmjd + 
  	 (header->nsec+*header->nframe/(double)header->framepersec)
	 *(1.0/(60.*60.*24.)));
}

// crc subroutine - converts input into a 16 bit CRC code
unsigned int crcc (unsigned char *idata, int len, int mask, int cycl) {
    
  unsigned int istate = 0;
  int idbit;
  int q, ich, icb;

  for (idbit = 1; idbit <= len; idbit++) {
    q = istate & 1;
    ich = (idbit - 1) / 8;
    icb = 7 - (idbit - 1) % 8;
    if ((((idata[ich] >> icb) & 1) ^ q)  ==  0)
      istate &= -2;
    else {
      istate ^=  mask;
      istate |= 1;
    }
    istate = (istate >> 1) | (istate & 1) << (cycl -1);
  } 
  return (istate);
}

unsigned short reversebits16 (unsigned short s) {
  unsigned char *c1, *c2;
  unsigned short ss;
  
  c1 = (unsigned char*)&s;
  c2 = (unsigned char*)&ss;
  
  // Flip bytes and bits
  c2[0] = bytetable[c1[1]]; 
  c2[1] = bytetable[c1[0]];
  return ss;
}

void init_bitreversal () {
  unsigned int i;
  for (i=0; i<256; i++) {
    bytetable[i] = ((i&0x80)>>7
                    |(i&0x40)>>5
                    |(i&0x20)>>3
                    |(i&0x10)>>1
                    |(i&0x08)<<1
                    |(i&0x04)<<3
                    |(i&0x02)<<5
                    |(i&0x01)<<7);
  }
}

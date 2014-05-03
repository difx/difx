/***************************************************************************
 *  Copyright (C) 2009-2011 by Adam Deller/Walter Brisken/Chris Phillips   *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include <string.h>
#include <stdio.h>
#include "vdifio.h"


#define VDIF_VERSION 0

#define UNIXZERO_MJD 40587

void mjd2ymd(int mjd, int *year, int *month, int *day)
{
	int jd, temp1, temp2;

	jd = mjd + 2400001;

	// Do some rather cryptic calculations

	temp1 = 4*(jd+((6*(((4*jd-17918)/146097)))/4+1)/2-37);
	temp2 = 10*(((temp1-237)%1461)/4)+5;

	*year = temp1/1461-4712;
	*month =((temp2/306+2)%12)+1;
	*day = (temp2%306)/10+1;
}

int ymd2doy(int yr, int mo, int day)
{
        int monstart1[] = {0,31,59,90,120,151,181,212,243,273,304,334};
        int monstart2[] = {0,31,60,91,121,152,182,213,244,274,305,335};
        int L2;

        L2 = yr/4-(yr+7)/4-yr/100+(yr+99)/100+yr/400-(yr+399)/400;
        if(L2 == -1)
        {
                return day + monstart2[mo-1];
        }
        else
        {
                return day + monstart1[mo-1];
        }
}

int ymd2mjd(int yr, int mo, int day)
{
        int doy;
        int yr1 = yr - 1;

        doy = ymd2doy(yr, mo, day);

        return doy-678576+365*yr1+yr1/4-yr1/100+yr1/400;
}

//int epoch2mjd(int epoch) {
//  return ymd2mjd(2000 + epoch/2, (epoch%2)*6+1, 1); // Year and Jan/July
//}

int createVDIFHeader(vdif_header *header, int framelength, int threadid, int bits, int nchan,
		      int iscomplex, char stationid[3]) {
  int lognchan;

  header->epoch = 0;

  if (VDIF_VERSION>7) return(VDIF_ERROR);
  if (bits>32 || bits<1) return(VDIF_ERROR);
  if (framelength%8!=0 || framelength<0) return(VDIF_ERROR);
  if (threadid>1023 || threadid<0) return(VDIF_ERROR);

  // Number of channels encoded as power of 2
  if (nchan<1) return(VDIF_ERROR);
  lognchan = 0;
  while (nchan>1) {
    if (nchan%2==1) return(VDIF_ERROR);
    lognchan++;
    nchan /=2;
  }
  if (lognchan>31) return(VDIF_ERROR);

  memset(header, 0, VDIF_HEADER_BYTES);

  header->version = VDIF_VERSION;
  header->nchan = lognchan;
  header->framelength8 = framelength/8;
  if (iscomplex)
    header->iscomplex = 1;
  else
    header->iscomplex = 0;
  header->nbits = bits-1;
  header->threadid = threadid;
  header->stationid = stationid[0]<<8 | stationid[1];

  header->frame=0;
  //header->framepersec=framepersec;

  return(VDIF_NOERROR);
}


void setVDIFThreadID(vdif_header *header, int threadid)
{
  // Should check bounds
  header->threadid = threadid;
}

void setVDIFFrameBytes(vdif_header *header, int bytes)
{
  // Should check modulo8 and not too big
  header->framelength8 = bytes/8;
}

int getVDIFEpochMJD(const vdif_header *header)
{
  int epoch = (int)header->epoch;
  return ymd2mjd(2000 + epoch/2, (epoch%2)*6+1, 1);
}

void setVDIFNumChannels(vdif_header *header, int numchannels)
{
  unsigned int logchans = 0;
  while(numchannels > 1)
    {
      numchannels /= 2;
      logchans++;
    }
  header->nchan = logchans;
}

int getVDIFNumChannels(const vdif_header *header)
{
  int logchans = header->nchan;
  int numchannels = 1;
  while(logchans > 0)
  {
    numchannels *= 2;
    logchans--;
  }
  return numchannels;
}

int getVDIFFrameMJD(const vdif_header *header)
{
  int mjd = getVDIFEpochMJD(header);

  return mjd + header->seconds/86400; // Seconds may be greater than one day
}

double getVDIFDMJD(const vdif_header *header, int framepersec) 
{
  int mjd = getVDIFFrameMJD(header);
  int sec = getVDIFFrameSecond(header);
  return (double)mjd+(sec+(double)header->frame/(double)framepersec)/(24*60*60);
}

// Note assumes the Epoch is already set
void setVDIFFrameMJD(vdif_header *header, int framemjd)
{
  int emjd = getVDIFEpochMJD(header);
  int seconds = (int)header->seconds;
  int mjd = emjd + seconds/86400;    // BUG? I think this step is wrong CJP
  if(emjd == framemjd) return; //its already right
  header->seconds = (framemjd-mjd)*86400;
}

void setVDIFMJDSec(vdif_header *header, uint64_t mjdsec)
{
  int epoch = (int)header->epoch;
  int emjd = ymd2mjd(2000 + epoch/2, (epoch%2)*6+1, 1);
  header->seconds = (int)(mjdsec - ((uint64_t)emjd)*86400);
}

void setVDIFEpoch(vdif_header *header, int mjd) {
  int year, month, day;
  mjd2ymd(mjd, &year, &month, &day);
  header->epoch = (year-2000)*2;
  if (month>6) header->epoch++;
}

int nextVDIFHeader(vdif_header *header, int framepersec) {
  header->frame++;
  if (header->frame>framepersec) {
    return(VDIF_ERROR);
  } else if (header->frame==framepersec) {
    header->seconds++;
    header->frame = 0;
  }
  return(VDIF_NOERROR);
}

uint64_t time2mjdsec(time_t time) {
  return ((uint64_t)UNIXZERO_MJD*24*60*60 + (uint64_t)time);

}
int setVDIFTime(vdif_header *header, time_t time) {
  int epoch;
  struct tm t;

  gmtime_r(&time, &t);
  epoch = (t.tm_year-100)*2;
  if (epoch<0)     // Year is year since 2000
    return(VDIF_ERROR);
  if (t.tm_mon>=6) {
    epoch++;
  }
  epoch %= 32;
  header->epoch = epoch;

  uint64_t mjdsec = time2mjdsec(time);
  setVDIFMJDSec(header, mjdsec);

  return(VDIF_NOERROR);
}

static void fprintVDIFHeaderHex(FILE *out, const vdif_header *header)
{
	const uint32_t *data = (const uint32_t *)header;
	
	fprintf(out, "%08x %08x %08x %08x", data[0], data[1], data[2], data[3]);
	if(header->legacymode == 0)
	{
		fprintf(out, "  %08x %08x %08x %08x", data[4], data[5], data[6], data[7]);
	}
	fprintf(out, "\n");
}

static void fprintVDIFHeaderLong(FILE *out, const vdif_header *header)
{
	fprintf(out, "VDIF Header\n");
	fprintf(out, "  epoch = %d\n", header->epoch);
	fprintf(out, "  seconds = %d\n", header->seconds);
	fprintf(out, "  frame = %d\n", header->frame);
	fprintf(out, "  threadid = %d\n", header->threadid);
	fprintf(out, "  framelength8 = %d -> frame length = %d\n", header->framelength8, header->framelength8*8);
	fprintf(out, "  ln2 nchan = %d -> nchan = %d\n", header->nchan, 1 << header->nchan);
	fprintf(out, "  nbits-1 = %d -> nbits = %d\n", header->nbits, header->nbits + 1);
	fprintf(out, "  legacymode = %d\n", header->legacymode);
	fprintf(out, "  invalid = %d\n", header->invalid);
	fprintf(out, "  version = %d\n", header->version);
	fprintf(out, "  stationid = %d\n", header->stationid);
	fprintf(out, "  iscomplex = %d\n", header->iscomplex);
	fprintf(out, "  eversion = %d\n", header->eversion);
	if(header->eversion == 1)
	{
		const vdif_edv1_header *edv1 = (const vdif_edv1_header *)header;
		
		fprintf(out, "  samprate = 0x%06x = %d %s\n", edv1->samprate, edv1->samprate, edv1->samprateunits ? "MHz" : "kHz");
		fprintf(out, "  syncword = %08x\n", edv1->syncword);
		fprintf(out, "  name = %8s", edv1->name);
	}
	else if(header->eversion == 3)
	{
		const vdif_edv3_header *edv3 = (const vdif_edv3_header *)header;
		
		fprintf(out, "  samprate = 0x%06X = %d %s\n", edv3->samprate, edv3->samprate, edv3->samprateunits ? "MHz" : "kHz");
		fprintf(out, "  syncword = 0x%08X\n", edv3->syncword);
		fprintf(out, "  tuning = 0x%08X = %8.6f MHz\n", edv3->tuning, edv3->tuning/16777216.0);
		fprintf(out, "  dbeunit = %d\n", edv3->dbeunit);
		fprintf(out, "  ifnumber = %d\n", edv3->ifnumber);
		fprintf(out, "  subband = %d\n", edv3->subband);
		fprintf(out, "  sideband = %d -> %s\n", edv3->sideband, edv3->sideband ? "U" : "L");
		fprintf(out, "  rev = %d.%d\n", edv3->majorrev, edv3->minorrev);
		fprintf(out, "  personalitytype = 0x%2X\n", edv3->personalitytype);
	}
	else
	{
		fprintf(out, "  extended1 = %06X\n", header->extended1);
		fprintf(out, "  extended2 = %08X\n", header->extended2);
		fprintf(out, "  extended3 = %08X\n", header->extended3);
		fprintf(out, "  extended4 = %08X\n", header->extended4);
	}
}

static void fprintVDIFHeaderShort(FILE *out, const vdif_header *header)
{
	fprintf(out, "%5d %8d %5d %6d %6d %5d %4d %d %d %d %3d", header->epoch, header->seconds, header->frame, header->threadid, header->framelength8*8, 1 << header->nchan, header->nbits+1, header->legacymode, header->invalid, header->iscomplex, header->eversion);
	if(header->eversion == 1)
	{
		const vdif_edv1_header *edv1 = (const vdif_edv1_header *)header;
		long long int samprate;

		samprate = edv1->samprate * (edv1->samprateunits ? 1000000LL : 1000LL);
		fprintf(out, " %10Ld 0x%08X %8s", samprate, edv1->syncword, edv1->name);
	}
	else if(header->eversion ==3)
	{
		const vdif_edv3_header *edv3 = (const vdif_edv3_header *)header;
		long long int samprate;

		samprate = edv3->samprate * (edv3->samprateunits ? 1000000LL : 1000LL);
		fprintf(out, " %10Ld 0x%08X %3d %2d %2d %10.6f    %c %d.%d 0x%2X", samprate, edv3->syncword, edv3->dbeunit, edv3->ifnumber, edv3->subband, edv3->tuning/16777216.0, edv3->sideband ? 'U' : 'L', edv3->majorrev, edv3->minorrev, edv3->personalitytype);
	}
	fprintf(out, "\n");
}

static void fprintVDIFHeaderColumns(FILE *out, const vdif_header *header)
{
	fprintf(out, "Epoch  Seconds Frame Thread Length Chans Bits L I C EDV");
	if(header->eversion == 1)
	{
		fprintf(out, " SampleRate   SyncWord Name");
	}
	if(header->eversion == 3)
	{
		fprintf(out, " SampleRate   SyncWord DBE IF Sub Tuning(MHz) Side Rev Pers");
	}
	fprintf(out, "\n");
}

void fprintVDIFHeader(FILE *out, const vdif_header *header, enum VDIFHeaderPrintLevel printLevel)
{
	switch(printLevel)
	{
	case VDIFHeaderPrintLevelHex:
		fprintVDIFHeaderHex(out, header);
		break;
        case VDIFHeaderPrintLevelLong:
		fprintVDIFHeaderLong(out, header);
		break;
        case VDIFHeaderPrintLevelColumns:
		fprintVDIFHeaderColumns(out, header);
		break;
        case VDIFHeaderPrintLevelShort:
		fprintVDIFHeaderShort(out, header);
		break;
	}
}

void printVDIFHeader(const vdif_header *header, enum VDIFHeaderPrintLevel printLevel)
{
	switch(printLevel)
	{
	case VDIFHeaderPrintLevelHex:
		fprintVDIFHeaderHex(stdout, header);
		break;
        case VDIFHeaderPrintLevelLong:
		fprintVDIFHeaderLong(stdout, header);
		break;
        case VDIFHeaderPrintLevelColumns:
		fprintVDIFHeaderColumns(stdout, header);
		break;
        case VDIFHeaderPrintLevelShort:
		fprintVDIFHeaderShort(stdout, header);
		break;
	}
}

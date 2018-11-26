/***************************************************************************
 *  Copyright (C) 2009-2015 by Adam Deller/Walter Brisken/Chris Phillips   *
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <inttypes.h>
#include "dateutils.h"
#include "vdifio.h"


#define VDIF_VERSION 0


int createVDIFHeader(vdif_header *header, int dataarraylength, int threadid, int bits, int nchan,
		      int iscomplex, char stationid[3]) {
  int lognchan;

  header->epoch = 0;

  if (VDIF_VERSION>7) return(VDIF_ERROR);
  if (bits>32 || bits<1) return(VDIF_ERROR);
  if (dataarraylength%8!=0 || dataarraylength<0) return(VDIF_ERROR);
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
  header->legacymode = 0;
  header->invalid = 0;
  header->nchan = lognchan;
  header->framelength8 = (dataarraylength+VDIF_HEADER_BYTES)/8;
  if (iscomplex)
    header->iscomplex = 1;
  else
    header->iscomplex = 0;
  header->nbits = bits-1;
  header->threadid = threadid;
  header->stationid = stationid[0]<<8 | stationid[1];

  header->frame=0;

  return(VDIF_NOERROR);
}


int setVDIFThreadID(vdif_header *header, int threadid)
{
  // Should check bounds
  header->threadid = threadid;
  return(VDIF_NOERROR);
}

int setVDIFFrameBytes(vdif_header *header, int bytes)
{
  // Should check modulo8 and not too big
  header->framelength8 = bytes/8;
  return(VDIF_NOERROR);
}

int getVDIFEpochMJD(const vdif_header *header)
{
  int epoch = (int)header->epoch;
  return ymd2mjd(2000 + epoch/2, (epoch%2)*6+1, 1);
}

int setVDIFNumChannels(vdif_header *header, int numchannels)
{
  unsigned int logchans = 0;
  while(numchannels > 1)
    {
      numchannels /= 2;
      logchans++;
    }
  header->nchan = logchans;
  return(VDIF_NOERROR);
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

  return mjd + header->seconds/86400; // Seconds will usually be greater than one day
}

double getVDIFFrameDMJD(const vdif_header *header, int framepersec) 
{
  int mjd = getVDIFFrameMJD(header);
  int sec = getVDIFFrameSecond(header);
  return (double)mjd+(sec+(double)header->frame/(double)framepersec)/(24*60*60);
}

// Note assumes the Epoch is already set
int setVDIFFrameMJD(vdif_header *header, int mjd)
{
  int emjd = getVDIFEpochMJD(header);
  int sec = header->seconds % 86400;  // Remember fraction of a day
  header->seconds = (mjd-emjd)*86400 + sec;
  return(VDIF_NOERROR);
} 

int setVDIFFrameMJDSec(vdif_header *header, uint64_t mjdsec)
{
  int emjd = getVDIFEpochMJD(header);
  header->seconds = (int)(mjdsec - ((uint64_t)emjd)*86400);
  return(VDIF_NOERROR);
}

uint64_t getVDIFFrameMJDSec(vdif_header *header)
{
  uint64_t emjd = getVDIFEpochMJD(header);
  return emjd*86400 + header->seconds;
}



// Assumes Epoch is already set and frame time is set to desired MJD. 
// Does not attempt to deal with wraps of time
int setVDIFFrameSecond(vdif_header *header, int seconds)
{
  uint64_t mjdsec = getVDIFFrameMJDSec(header);
  mjdsec -= (mjdsec % 86400); // Remove fraction of a day

  return setVDIFFrameMJDSec(header, mjdsec + seconds);
}

int setVDIFEpochMJD(vdif_header *header, int mjd) {
  int year, month, day;
  mjd2ymd(mjd, &year, &month, &day);
  header->epoch = (year-2000)*2;
  if (month>6) header->epoch++;
  return(VDIF_NOERROR);
}

int nextVDIFHeader(vdif_header *header, int framepersec) {
  // This would fail if there were 16777216 frames/sec (ie 2^24) due to overflow so at least fail in this case
  assert(framepersec!=16777216);
  header->frame++; 
  if (header->frame>framepersec) {
    return(VDIF_ERROR);
  } else if (header->frame==framepersec) {
    header->seconds++;
    header->frame = 0;
  }
  return(VDIF_NOERROR);
}

int incrementVDIFHeader(vdif_header *header, int framepersec, int64_t inc) {
  int secinc=0;
  int64_t frame = header->frame;
  frame += inc;
  if (frame>framepersec) {
    secinc = frame/framepersec;
    frame -= secinc*framepersec;
    header->seconds += secinc;
  } else { // Negative - could optimise this but risk of off by one moderately high
    while (frame<0) {
      secinc++;
      frame += framepersec;
    }
    assert(abs(secinc)<header->seconds); // Otherwise before epoch started
    header->seconds -= secinc;  
  }
  header->frame = frame;
  return(VDIF_NOERROR);
}

uint64_t time2mjdsec(time_t time) {
  return ((uint64_t)UNIXZERO_MJD*24*60*60 + (uint64_t)time);

}

int setVDIFEpochTime(vdif_header *header, time_t time) {
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

  return(VDIF_NOERROR);
}

int setVDIFFrameTime(vdif_header *header, time_t time) {
  uint64_t mjdsec = time2mjdsec(time);
  return setVDIFFrameMJDSec(header, mjdsec);
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
	const char *stnCode;

	stnCode = ((const char *)header) + 12;

	fprintf(out, "VDIF Header\n");
	fprintf(out, "  epoch = 0x%X = %d\n", header->epoch, header->epoch);
	fprintf(out, "  seconds = 0x%X = %d\n", header->seconds, header->seconds);
	fprintf(out, "  frame = 0x%X = %d\n", header->frame, header->frame);
	fprintf(out, "  threadid = 0x%X = %d\n", header->threadid, header->threadid);
	fprintf(out, "  framelength8 = 0x%X -> frame length = %d\n", header->framelength8, header->framelength8*8);
	fprintf(out, "  ln2 nchan = 0x%X -> nchan = %d\n", header->nchan, 1 << header->nchan);
	fprintf(out, "  nbits-1 = 0x%X -> nbits = %d\n", header->nbits, header->nbits + 1);
	fprintf(out, "  legacymode = %d\n", header->legacymode);
	fprintf(out, "  invalid = %d\n", header->invalid);
	fprintf(out, "  version = %d\n", header->version);
	if(isprint(stnCode[0]) && isprint(stnCode[1]))
	{
		fprintf(out, "  stationid = 0x%X = %d = '%c%c'\n", header->stationid, header->stationid, stnCode[1], stnCode[0]);
	}
	else
	{
		fprintf(out, "  stationid = 0x%X = %d\n", header->stationid, header->stationid);
	}
	fprintf(out, "  iscomplex = %d\n", header->iscomplex);
	if(header->legacymode == 0)
	{
		fprintf(out, "  eversion = 0x%X = %d\n", header->eversion, header->eversion);
		if(header->eversion == 1)
		{
			const vdif_edv1_header *edv1 = (const vdif_edv1_header *)header;

			fprintf(out, "  samprate = 0x%06X = %d %s\n", edv1->samprate, edv1->samprate, edv1->samprateunits ? "MHz" : "kHz");
			fprintf(out, "  syncword = 0x%08X\n", edv1->syncword);
			fprintf(out, "  name = %8s", edv1->name);
		}
		else if(header->eversion == 2)
		{
			const vdif_edv2_header_generic *edv2 = (const vdif_edv2_header_generic *)header;
			if (edv2->subsubversion == VDIF_EDV2_SUBVER_ALMA)
			{
				const vdif_edv2_header_alma *edv2a = (const vdif_edv2_header_alma *)header;
				fprintf(out, "  polblock = %d\n", edv2a->polblock);
				fprintf(out, "  quadrant-1 = %d\n", edv2a->quadrantminus1);
				fprintf(out, "  correlator = %d\n", edv2a->correlator);
				fprintf(out, "  PIC status = %" PRIu32 "\n", edv2a->picstatus);
				fprintf(out, "  VTP PSN = %" PRIu64 "\n", edv2a->psn);
			}
			else if (edv2->subsubversion == VDIF_EDV2_SUBVER_R2DBE)
			{
				const vdif_edv2_header_r2dbe *edv2r2 = (const vdif_edv2_header_r2dbe *)header;
				fprintf(out, "  polblock = %d\n", edv2r2->polblock);
				fprintf(out, "  BDC sideband = %d\n", edv2r2->bdcsideband);
				fprintf(out, "  rx sideband = %d\n", edv2r2->rxsideband);
				fprintf(out, "  1PPS offset = %+" PRId32 " = %+.4f usec\n", edv2r2->ppsdiff, edv2r2->ppsdiff/256.0f);
				fprintf(out, "  VTP PSN = %" PRIu64 "\n", edv2r2->psn);
			}
			else
			{
				fprintf(out, "  unknown EDV2 subversion = %" PRIu32 "\n", edv2->subsubversion);
				fprintf(out, "  word5 = %" PRIu32 "\n", edv2->word5);
				fprintf(out, "  word6 = %" PRIu32 "\n", edv2->word6);
				fprintf(out, "  word7 = %" PRIu32 "\n", edv2->word7);
			}
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
		else if(header->eversion == 4)
		{
			const vdif_edv4_header *edv4 = (const vdif_edv4_header *)header;
			int64_t i;

			fprintf(out, "  masklength = %d\n", edv4->masklength);
			fprintf(out, "  syncword = 0x%08X\n", edv4->syncword);
			fprintf(out, "  validity mask = 0x");
			for(i = edv4->masklength - 1; i >= 0; --i)
			{
				fprintf(out, "%c", ((edv4->validitymask) & (1LL << i)) ? '1' : '0');
			}
			fprintf(out, "\n");
		}
		else
		{
			fprintf(out, "  extended1 = %06X\n", header->extended1);
			fprintf(out, "  extended2 = %08X\n", header->extended2);
			fprintf(out, "  extended3 = %08X\n", header->extended3);
			fprintf(out, "  extended4 = %08X\n", header->extended4);
		}
	}
}

static void fprintVDIFHeaderShort(FILE *out, const vdif_header *header)
{
	if(header->legacymode == 0)
	{
		fprintf(out, "%5d %8d %6d %6d %6d %5d %4d %d %d %d %3d", header->epoch, header->seconds, header->frame, header->threadid, header->framelength8*8, 1 << header->nchan, header->nbits+1, header->legacymode, header->invalid, header->iscomplex, header->eversion);
		if(header->eversion == 1)
		{
			const vdif_edv1_header *edv1 = (const vdif_edv1_header *)header;
			long long int samprate;

			samprate = edv1->samprate * (edv1->samprateunits ? 1000000LL : 1000LL);
			fprintf(out, " %10lld 0x%08X %8s", samprate, edv1->syncword, edv1->name);
		}
		else if(header->eversion)
		{
			const vdif_edv2_header_generic *edv2 = (const vdif_edv2_header_generic *)header;
			if (edv2->subsubversion == VDIF_EDV2_SUBVER_ALMA)
			{
				const vdif_edv2_header_alma *edv2a = (const vdif_edv2_header_alma *)header;
				fprintf(out, " PIC-%s-Q%d%c %13" PRIu32 " %" PRIu64,  // e.g. PIC-BL-Q1Y
					edv2a->correlator == 0 ? "2A" : "BL",
					edv2a->quadrantminus1+1,
					edv2a->polblock == 0 ? 'X' : 'Y',
					edv2a->picstatus, edv2a->psn);
			}
			else if (edv2->subsubversion == VDIF_EDV2_SUBVER_R2DBE)
			{
				const vdif_edv2_header_r2dbe *edv2r2 = (const vdif_edv2_header_r2dbe *)header;
				fprintf(out, " %3d %3d %2d %+13d %" PRIu64, edv2r2->polblock, edv2r2->bdcsideband, edv2r2->rxsideband, edv2r2->ppsdiff, edv2r2->psn);
			}
			else
			{
				fprintf(out, " %" PRIu64 " %" PRIu64 " %" PRIu64, edv2->word5, edv2->word6, edv2->word7);
			}
		}
		else if(header->eversion == 3)
		{
			const vdif_edv3_header *edv3 = (const vdif_edv3_header *)header;
			long long int samprate;

			samprate = edv3->samprate * (edv3->samprateunits ? 1000000LL : 1000LL);
			fprintf(out, " %10lld 0x%08X %3d %2d %2d %10.6f    %c %d.%d 0x%2X", samprate, edv3->syncword, edv3->dbeunit, edv3->ifnumber, edv3->subband, edv3->tuning/16777216.0, edv3->sideband ? 'U' : 'L', edv3->majorrev, edv3->minorrev, edv3->personalitytype);
		}
		else if(header->eversion == 4)
		{
			const vdif_edv4_header *edv4 = (const vdif_edv4_header *)header;
			int64_t i;

			fprintf(out, "  %2d   0x%08X 0x", edv4->masklength, edv4->syncword);
			for(i = edv4->masklength - 1; i >= 0; --i)
			{
				fprintf(out, "%c", ((edv4->validitymask) & (1LL << i)) ? '1' : '0');
			}
		}
	}
	else
	{
		fprintf(out, "%5d %8d %5d %6d %6d %5d %4d %d %d %d NA", header->epoch, header->seconds, header->frame, header->threadid, header->framelength8*8, 1 << header->nchan, header->nbits+1, header->legacymode, header->invalid, header->iscomplex);
	}
	fprintf(out, "\n");
}

static void fprintVDIFHeaderColumns(FILE *out, const vdif_header *header)
{
	fprintf(out, "Epoch  Seconds  Frame Thread Length Chans Bits L I C EDV");
	if(header->eversion == 1)
	{
		fprintf(out, " SampleRate   SyncWord Name");
	}
	else if(header->eversion == 2)
	{
		const vdif_edv2_header_generic *edv2 = (const vdif_edv2_header_generic *)header;
		if (edv2->subsubversion == VDIF_EDV2_SUBVER_ALMA)
		{
			fprintf(out, "     Origin    PIC_Status PSN");
		}
		else if (edv2->subsubversion == VDIF_EDV2_SUBVER_R2DBE)
		{
			fprintf(out, " Pol BDC RX FPGA_PPS_diff PSN");
		}
		else
		{
			fprintf(out, " PolBlock FPGA_PPS_diff PSN");
		}
	}
	else if(header->eversion == 3)
	{
		fprintf(out, " SampleRate   SyncWord DBE IF Sub Tuning(MHz) Side Rev Pers");
	}
	else if(header->eversion == 4)
	{
		fprintf(out, " MgdThds SyncWord ValidityMask");
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

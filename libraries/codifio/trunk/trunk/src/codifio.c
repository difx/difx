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
// $Id: codifio.c 7056 2015-10-30 16:08:10Z WalterBrisken $
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/codifio/trunk/src/codifio.c $
// $LastChangedRevision: 7056 $
// $Author: WalterBrisken $
// $LastChangedDate: 2015-10-31 03:08:10 +1100 (Sat, 31 Oct 2015) $
//
//============================================================================

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <inttypes.h>
#include <limits.h>
#include "dateutils.h"
#include "codifio.h"


#define CODIF_VERSION 1


int createCODIFHeader(codif_header *header, int dataarraylength, uint16_t threadid, uint16_t groupid, int bits, uint16_t nchan,
		      int sampleblocklength, int period, uint64_t totalsamples, int iscomplex, char stationid[3]) {
  
  if (CODIF_VERSION>7) return(CODIF_ERROR);
  if (bits>32 || bits<1) return(CODIF_ERROR);
  if (dataarraylength%8!=0 || dataarraylength<0) return(CODIF_ERROR);
  if (threadid>USHRT_MAX || threadid<0) return(CODIF_ERROR);
  if (groupid>USHRT_MAX || groupid<0) return(CODIF_ERROR);

  if (nchan<1 || nchan>USHRT_MAX) return(CODIF_ERROR);

  memset(header, 0, CODIF_HEADER_BYTES);

  header->version = CODIF_VERSION;
  header->invalid = 0;
  setCODIFNumChannels(header, nchan);
  setCODIFFrameBytes(header, dataarraylength);
  if (iscomplex)
    header->iscomplex = 1;
  else
    header->iscomplex = 0;
  setCODIFBitsPerSample(header, bits);
  setCODIFVersion(header, 1);
  setCODIFRepresentation(header, 0);
  header->threadid = threadid;
  header->groupid = groupid;
  header->stationid = stationid[0]<<8 | stationid[1];
  setCODIFPeriod(header, period);
  header->totalsamples = totalsamples;
  setCODIFSampleblockLength(header, sampleblocklength);
  header->frame=0;
  header->sync = 0xADEADBEE;
  

  return(CODIF_NOERROR);
}


int setCODIFFrameBytes(codif_header *header, int bytes)
{
  // Should check modulo8 and not too big
  header->framelength8 = bytes/8;
  return(CODIF_NOERROR);
}

int getCODIFEpochMJD(const codif_header *header)
{
  int epoch = (int)header->epoch;
  return ymd2mjd(2000 + epoch/2, (epoch%2)*6+1, 1);
}

int setCODIFNumChannels(codif_header *header, int numchannels)
{
  header->nchan = numchannels-1;
  return(CODIF_NOERROR);
}

int getCODIFFrameMJD(const codif_header *header)
{
  int mjd = getCODIFEpochMJD(header);

  return mjd + header->seconds/86400; // Seconds will usually be greater than one day
}

double getCODIFFrameDMJD(const codif_header *header, double framepersec) 
{
  int mjd = getCODIFFrameMJD(header);
  int sec = getCODIFFrameSecond(header);
  return (double)mjd+(sec+(double)header->frame/(double)framepersec)/(24*60*60);
}

double CODIFframe2mjd(const codif_header *header, int sec, int frame, int framepersec) 
{
  double mjd = getCODIFEpochMJD(header);
  mjd += (sec + frame/(double)framepersec)/86400.0;
  return mjd;
}

// Note assumes the Epoch is already set
int setCODIFFrameMJD(codif_header *header, int mjd)
{
  int emjd = getCODIFEpochMJD(header);
  int sec = header->seconds % 86400;  // Remember fraction of a day
  header->seconds = (mjd-emjd)*86400 + sec;
  return(CODIF_NOERROR);
} 

int setCODIFFrameMJDSec(codif_header *header, uint64_t mjdsec)
{
  int emjd = getCODIFEpochMJD(header);
  header->seconds = (int)(mjdsec - ((uint64_t)emjd)*86400);
  return(CODIF_NOERROR);
}

uint64_t getCODIFFrameMJDSec(codif_header *header)
{
  uint64_t emjd = getCODIFEpochMJD(header);
  return emjd*86400 + header->seconds;
}



// Assumes Epoch is already set and frame time is set to desired MJD. 
// Does not attempt to deal with wraps of time
int setCODIFFrameSecond(codif_header *header, int seconds)
{
  uint64_t mjdsec = getCODIFFrameMJDSec(header);
  mjdsec -= (mjdsec % 86400); // Remove fraction of a day

  return setCODIFFrameMJDSec(header, mjdsec + seconds);
}

int setCODIFEpochMJD(codif_header *header, int mjd) {
  int year, month, day;
  mjd2ymd(mjd, &year, &month, &day);
  header->epoch = (year-2000)*2;
  if (month>6) header->epoch++;
  return(CODIF_NOERROR);
}

int nextCODIFHeader(codif_header *header, int framepersec) {
  // This would fail if there were 16777216 frames/sec (ie 2^24) due to overflow so at least fail in this case
  assert(framepersec!=16777216);
  header->frame++; 
  if (header->frame>framepersec) {
    return(CODIF_ERROR);
  } else if (header->frame==framepersec) {
    header->seconds += header->period;
    header->frame = 0;
  }
  return(CODIF_NOERROR);
}

int incrementCODIFHeader(codif_header *header, int framepersec, int64_t inc) {
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
  return(CODIF_NOERROR);
}

uint64_t time2mjdsec(time_t time) {
  return ((uint64_t)UNIXZERO_MJD*24*60*60 + (uint64_t)time);

}

int setCODIFEpochTime(codif_header *header, time_t time) {
  int epoch;
  struct tm t;

  gmtime_r(&time, &t);
  epoch = (t.tm_year-100)*2;
  if (epoch<0)     // Year is year since 2000
    return(CODIF_ERROR);
  if (t.tm_mon>=6) {
    epoch++;
  }
  epoch %= 32;
  header->epoch = epoch;

  return(CODIF_NOERROR);
}

int setCODIFFrameTime(codif_header *header, time_t time) {
  uint64_t mjdsec = time2mjdsec(time);
  return setCODIFFrameMJDSec(header, mjdsec);
}

#if 0
static void fprintCODIFHeaderHex(FILE *out, const codif_header *header)
{
	const uint32_t *data = (const uint32_t *)header;
	
	fprintf(out, "%08x %08x %08x %08x", data[0], data[1], data[2], data[3]);
	if(header->legacymode == 0)
	{
		fprintf(out, "  %08x %08x %08x %08x", data[4], data[5], data[6], data[7]);
	}
	fprintf(out, "\n");
}

static void fprintCODIFHeaderLong(FILE *out, const codif_header *header)
{
	const char *stnCode;

	stnCode = ((const char *)header) + 12;

	fprintf(out, "CODIF Header\n");
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
	if(stnCode[0] >= ' ' && stnCode[0] <= 127 && (stnCode[1] >= ' ' || stnCode[1] == 0) && stnCode[1] <= 127)
	{
		fprintf(out, "  stationid = 0x%X = %d = '%c%c'\n", header->stationid, header->stationid, stnCode[0], stnCode[1]);
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
			const codif_edv1_header *edv1 = (const codif_edv1_header *)header;
			
			fprintf(out, "  samprate = 0x%06X = %d %s\n", edv1->samprate, edv1->samprate, edv1->samprateunits ? "MHz" : "kHz");
			fprintf(out, "  syncword = 0x%08X\n", edv1->syncword);
			fprintf(out, "  name = %8s", edv1->name);
		}
		else if(header->eversion == 2)
		{
			const codif_edv2_header *edv2 = (const codif_edv2_header *)header;

			fprintf(out, "  polblock = %d\n", edv2->polblock);
			fprintf(out, "  quadrant-1 = %d\n", edv2->quadrantminus1);
			fprintf(out, "  correlator = %d\n", edv2->correlator);
			fprintf(out, "  sync/magic = %x\n", edv2->sync);
			fprintf(out, "  PIC status = %" PRIu32, edv2->status);
			fprintf(out, "  VTP PSN = %" PRIu64, edv2->psn);
		}
		else if(header->eversion == 3)
		{
			const codif_edv3_header *edv3 = (const codif_edv3_header *)header;
			
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
			const codif_edv4_header *edv4 = (const codif_edv4_header *)header;
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

static void fprintCODIFHeaderShort(FILE *out, const codif_header *header)
{
	if(header->legacymode == 0)
	{
		fprintf(out, "%5d %8d %6d %6d %6d %5d %4d %d %d %d %3d", header->epoch, header->seconds, header->frame, header->threadid, header->framelength8*8, 1 << header->nchan, header->nbits+1, header->legacymode, header->invalid, header->iscomplex, header->eversion);
		if(header->eversion == 1)
		{
			const codif_edv1_header *edv1 = (const codif_edv1_header *)header;
			long long int samprate;

			samprate = edv1->samprate * (edv1->samprateunits ? 1000000LL : 1000LL);
			fprintf(out, " %10lld 0x%08X %8s", samprate, edv1->syncword, edv1->name);
		}
		else if(header->eversion == 2)
		{
			const codif_edv2_header *edv2 = (const codif_edv2_header *)header;
			fprintf(out, " %8d %13d %" PRIu64, edv2->polblock, edv2->status, edv2->psn);
		}
		else if(header->eversion == 3)
		{
			const codif_edv3_header *edv3 = (const codif_edv3_header *)header;
			long long int samprate;

			samprate = edv3->samprate * (edv3->samprateunits ? 1000000LL : 1000LL);
			fprintf(out, " %10lld 0x%08X %3d %2d %2d %10.6f    %c %d.%d 0x%2X", samprate, edv3->syncword, edv3->dbeunit, edv3->ifnumber, edv3->subband, edv3->tuning/16777216.0, edv3->sideband ? 'U' : 'L', edv3->majorrev, edv3->minorrev, edv3->personalitytype);
		}
		else if(header->eversion == 4)
		{
			const codif_edv4_header *edv4 = (const codif_edv4_header *)header;
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

static void fprintCODIFHeaderColumns(FILE *out, const codif_header *header)
{
	fprintf(out, "Epoch  Seconds  Frame Thread Length Chans Bits L I C EDV");
	if(header->eversion == 1)
	{
		fprintf(out, " SampleRate   SyncWord Name");
	}
	else if(header->eversion == 2)
	{
		fprintf(out, " PolBlock FPGA_PPS_diff PSN");
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

void fprintCODIFHeader(FILE *out, const codif_header *header, enum CODIFHeaderPrintLevel printLevel)
{
	switch(printLevel)
	{
	case CODIFHeaderPrintLevelHex:
		fprintCODIFHeaderHex(out, header);
		break;
        case CODIFHeaderPrintLevelLong:
		fprintCODIFHeaderLong(out, header);
		break;
        case CODIFHeaderPrintLevelColumns:
		fprintCODIFHeaderColumns(out, header);
		break;
        case CODIFHeaderPrintLevelShort:
		fprintCODIFHeaderShort(out, header);
		break;
	}
}

void printCODIFHeader(const codif_header *header, enum CODIFHeaderPrintLevel printLevel)
{
	switch(printLevel)
	{
	case CODIFHeaderPrintLevelHex:
		fprintCODIFHeaderHex(stdout, header);
		break;
        case CODIFHeaderPrintLevelLong:
		fprintCODIFHeaderLong(stdout, header);
		break;
        case CODIFHeaderPrintLevelColumns:
		fprintCODIFHeaderColumns(stdout, header);
		break;
        case CODIFHeaderPrintLevelShort:
		fprintCODIFHeaderShort(stdout, header);
		break;
	}
}

#endif

uint32_t getCODIFFramesPerPeriod(const codif_header *header)
{
    uint64_t databytes = (uint64_t)getCODIFFrameBytes(header);
    uint64_t samplesperframe = (databytes*8) / (getCODIFNumChannels(header) * getCODIFBitsPerSample(header));
    if (getCODIFComplex(header))
    {
        samplesperframe /= 2;
    }
    return (uint32_t)(getCODIFTotalSamples(header) / samplesperframe);
}



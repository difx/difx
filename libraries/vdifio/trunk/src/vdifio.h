/***************************************************************************
 *   Copyright (C) 2009-2010 by Adam Deller / Walter Brisken               *
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
	
#ifndef __VDIFIO_H__
#define __VDIFIO_H__

#ifdef __cplusplus
extern "C" {
#endif

#define VDIF_HEADER_BYTES		32
#define VDIF_LEGACY_HEADER_BYTES	16
#define MAX_VDIF_FRAME_BYTES		9032

typedef struct {
	int invalid; /* 1 is invalid, 0 is valid (normal) */
	int legacymode; /* 1 is legacy mode, 0 otherwise (normal) */
	int mjd;
	int seconds;
	int frameno;
	int version;
	int nchan;
	int framelength;
	int complexdata; /* 1 is complex data, 0 is real data (normal) */
	int bitspersample;
	int threadid;
	int stationid;
	char stationcode[3];
	int extendedformat;
	int extendeddata[4];
} vdif_header;

/* Date manipulation functions */
int ymd2doy(int yr, int mo, int day);
int ymd2mjd(int yr, int mo, int day);

/* Function to completely fill header struct, returns non-zero on error */
int parse_vdif_header(char * rawheader, vdif_header * parsedheader);

/* Functions to grab just one value from the raw header */
int getVDIFThreadID(char * rawheader);
int getVDIFFrameBytes(char * rawheader);
int getVDIFFrameMJD(char * rawheader);
int getVDIFFrameSecond(char * rawheader);
int getVDIFFrameNumber(char * rawheader);
int getVDIFStationID(char * rawheader);
int getVDIFBitsPerSample(char * rawheader);
int getVDIFNumChannels(char * rawheader);
int getVDIFFrameInvalid(char * rawheader);

/* Functions to set just one value from a raw header */
void setVDIFFrameMJD(char * rawheader, int framemjd);
void setVDIFFrameSecond(char * rawheader, int framesecond);
void setVDIFFrameNumber(char * rawheader, int framenumber);
void setVDIFFrameInvalid(char * rawheader, unsigned int invalid);

#ifdef __cplusplus
}
#endif

#endif

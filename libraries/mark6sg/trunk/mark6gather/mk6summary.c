/***************************************************************************
 *   Copyright (C) 2015-2018 by Walter Brisken                             *
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
// $Id: mk6summary.c 7590 2016-12-07 00:41:57Z ChrisPhillips $
// $HeadURL: $
// $LastChangedRevision: 7590 $
// $Author: ChrisPhillips $
// $LastChangedDate: 2016-12-06 18:41:57 -0600 (Tue, 06 Dec 2016) $
//
//============================================================================

#include <stdio.h>
#include <stdlib.h>
#include "mark6gather.h"

/* the VDIF structure is defined in vdifio.h, but is included here as vdifio is not available to this library */
typedef struct vdif_header {
   uint32_t seconds : 30;
   uint32_t legacymode : 1;
   uint32_t invalid : 1;

   uint32_t frame : 24;
   uint32_t epoch : 6;
   uint32_t unassigned : 2;

   uint32_t framelength8 : 24;  // Frame length (including header) divided by 8 
   uint32_t nchan : 5;
   uint32_t version : 3;

   uint32_t stationid : 16;
   uint32_t threadid : 10;
   uint32_t nbits : 5;
   uint32_t iscomplex : 1;

   uint32_t extended1 : 24;
   uint32_t eversion : 8;

   uint32_t extended2;
   uint32_t extended3;
   uint32_t extended4;
} vdif_header;
static inline int getVDIFFrameEpochSecOffset(const vdif_header *header) { return (int)header->seconds; }
static inline int getVDIFFrameNumber(const vdif_header *header) { return (int)header->frame; }
static inline int getVDIFThreadID(const vdif_header *header) { return (int)header->threadid; }
static inline int getVDIFFrameBytes(const vdif_header *header) { return (int)(header->framelength8)* 8; }

void summarize(const char *fileName)
{
	FILE *in;
	Mark6Header H;
	char *buffer;
	char *data;
	unsigned char *udata, *lastdata;
	int i;
	Mark6BlockHeader_ver2 *h;
	int s;
	vdif_header *v1;
	size_t v;

	in = fopen(fileName, "r");
	if(in == 0)
	{
		fprintf(stderr, "Cannot open %s for read\n", fileName);

		return;
	}

	v = fread(&H, sizeof(H), 1, in); // read mark6 file header
	if(v<1)
	{
		fprintf(stderr, "Error reading mark6 file header for %s\n", fileName);
		fclose(in);
		
		return;
	}

	printf("File: %s\n", fileName);
	printMark6Header(&H);

	if(H.version < 2)
	{
		fprintf(stderr, "Mark6 file header less than version 2 for %s, exiting.\n", fileName);
		fclose(in);

		return;
	}

	s = sizeof(Mark6BlockHeader_ver2);
	buffer = (char *)malloc(H.block_size);
	h = (Mark6BlockHeader_ver2 *)buffer;
	data = buffer + s;

	// VDIF first packet
	v1 = (vdif_header *)data;

	for(i = 0; ; ++i)
	{
		int j, n;
		int nSize;

		nSize = 0;
		v = fread(buffer, s, 1, in); // read mark6 block header
		if(v < 1)
		{
			break;
		}
		n = (h->wb_size-s)/H.packet_size;
		v = fread(buffer+s, 1, h->wb_size-s, in); // read block
		if(v < s)
		{
			printf("Early EOF: only %lu bytes read.  %d bytes expected.\n", v, h->wb_size-s);
		}
		for(j = 0; j < n; ++j)
		{
			// Mark5B first packet
			if(H.packet_format == 1 && j == 0)
			{
				udata = (unsigned char*)(data);
				printf("%d %d ", h->blocknum, n);
				printf("%d:", (udata[10] & 0x0F)*10000 + (udata[9] >> 4)*1000 + (udata[9] & 0x0F)*100 + (udata[8] >> 4)*10 +  (udata[8] & 0x0F));
				printf("%05d - ", (udata[5] * 256) + udata[4]);
			}

			vdif_header *v2;

			v2 = (vdif_header *)(data + j*H.packet_size);
			if(getVDIFFrameBytes(v2) == H.packet_size)
			{
				++nSize;
			}
			if(j == n-1) // print summary for last one
			{
				// Mark5B last packet
				if(H.packet_format == 1)
				{
					lastdata = (unsigned char*)(data + j*H.packet_size);
					printf("%d:", (lastdata[10] & 0x0F)*10000 + (lastdata[9] >> 4)*1000 + (lastdata[9] & 0x0F)*100 + (lastdata[8] >> 4)*10 + (lastdata[8] & 0x0F));
					printf("%05d\n", (lastdata[5] * 256) + lastdata[4]);
				}
				else // VDIF last packet
				{
					printf("%d %d/%d  %d:%05d:%d - %d:%05d:%d\n", h->blocknum, nSize, n,
						getVDIFFrameEpochSecOffset(v1), getVDIFFrameNumber(v1), getVDIFThreadID(v1),
						getVDIFFrameEpochSecOffset(v2), getVDIFFrameNumber(v2), getVDIFThreadID(v2));
				}
			}
		}
	}

	fclose(in);
	free(buffer);
}

int main(int argc, char **argv)
{
	int a;

	if(argc < 2)
	{
		printf("Usage: %s <mk6file>\n", argv[0]);

		exit(EXIT_SUCCESS);
	}

	for(a = 1; a < argc; ++a)
	{
		summarize(argv[a]);
	}

	return 0;
}

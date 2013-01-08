/***************************************************************************
 *   Copyright (C) 2008-2012 by Walter Brisken & Adam Deller               *
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
//
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#define _FILE_OFFSET_BITS 64

static unsigned char bytetable[256];

int main(int argc, char *argv[])
{
	char filenamein[256];
	char filenameout[256];
	FILE *file1, *file2;
	int gotit=0,i,iret=1,iw;
	unsigned char ccc;
	u_int32_t ddin[22500];
	u_int32_t xtra;
	unsigned char last4[4];
	u_int32_t last = 0x00000000;
	u_int32_t syncPattern = 0xffffffff;
	u_int32_t badPattern;
	unsigned short nframe=0;
	unsigned short badTrack = 0;   // Tracks range from 0-31
	unsigned short copyTrack = 0;  // Tracks range from 0-31
	long fileSize = 0;
	int byteCount = 0;

	if(argc < 4) 
	{ 
		printf("A program to replace one data track with another in a MarkIV dataset.\n\n");
		printf("USAGE: m5fixtrack {filein}  {fileout} {badtrack}\n\n");
		printf("filein:\t\t input data file\n");
		printf("fileout:\t output data file\n");
		printf("badtrack:\t the track to be replaced (0-32)\n\n");
		printf("Note: the bad track will be replaced by the next higher track\n");
		return 0;
	}

	sscanf(argv[1], "%255s", filenamein);
	sscanf(argv[2], "%255s", filenameout);
	sscanf(argv[3], "%d", &badTrack);

	if ((file1 = fopen(filenamein, "r")) == NULL)
	{ 
		fprintf(stderr, "Can't open input filename: %s\n", filenamein);
		return 0;
	}	
	if ((file2 = fopen(filenameout, "w")) == NULL)
	{ 
		fprintf(stderr, "Can't open output filename: %s\n", filenameout);
		return 0;
	}	
	
	// determine file size of input file
	fseek(file1, 0L, SEEK_END);
	fileSize = ftell(file1);
	fseek(file1, 0L, SEEK_SET);

	// replace the bad track by the upper adjacent one
	copyTrack = badTrack +1;
	if (badTrack > 31)
		copyTrack = 0;
	
	badPattern = ~((0x80000000 >> (31 - badTrack)) & syncPattern);

	// read through file until defective sync word has been found
	while(gotit == 0)
	{
		ccc = (char)fgetc(file1);
		byteCount ++;

		last >>= 8;
		last |= (ccc << 24);
		if(last == badPattern)
		{
			gotit=1;
		}
	}
	
	if (gotit == 0)
	{
		fprintf (stderr, "Did not find a sync pattern with defective track %d \n", badTrack);
		return(0);
	}

	printf("Found first defective sync pattern at byte position: %d \n", byteCount);
	printf("Now replacing track %d by track %d \n", badTrack, copyTrack);

	// now we have skipped until a defective ffffff word, continue reading 32-bit words
	
	int blockCount = 0;
	while(iret !=0) 
	{	
		iret = fread(ddin, sizeof(ddin), 1, file1);
		nframe++;
		for (i=1; i < 22500; i+=1)
		{
			xtra = (ddin[i] & (1 << copyTrack)) >> copyTrack; //steal good bit from the copy track

			//printf ("before %x ", ddin[i]);
			ddin[i]= ddin[i] |  xtra << badTrack  ; //up 30 bits  nd or it in
			//printf (" %x %x %x\n", ddin[i], (1 << copyTrack), xtra);
			
		}
		iw = fwrite(ddin, sizeof(ddin), 1, file2); //========write data block
		blockCount ++;
		printf ("\r Done: %.1f%%", (float)(blockCount * 22500) / fileSize * 100.0);	
	}

	fclose(file1);
	fclose(file2);
}

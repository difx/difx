/***************************************************************************
 *   Copyright (C) 2015 by Walter Brisken                                  *
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
// $HeadURL: $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <vdifio.h>

const char program[] = "filterVDIF";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.1";
const char verdate[] = "20150709";

static void usage(const char *pgm)
{
	printf("%s ver. %s  %s  %s\n\n", program, version, author, verdate);
	printf("A utility to extract specfied threads from a VDIF file.\n\n");
	printf("Usage: %s <VDIF input file> <VDIF output file> <threadids>\n\n", pgm);
	printf("<VDIF input file> is the name of the VDIF file to read\n\n");
	printf("<VDIF output file> is the name of the VDIF file to write\n\n");
	printf("<threadids> is a comma separated list of thread ids to copy\n\n");
	printf("Note: this currently assumes no interloper bytes and that all frames are the same size\n\n");
}

int parseThreads(int *threadMap, const char *threadList)
{
	int i, t, n;
	char c;

	for(t = 0; t <= VDIF_MAX_THREAD_ID; ++t)
	{
		threadMap[t] = 0;
	}

	t = -1;
	n = 0;
	i = 0;
	do
	{
		c = threadList[i];
		if(c >= '0' && c <= '9')
		{
			if(t < 0)
			{
				t = 0;
			}
			t *= 10;
			t += (c - '0');
		}
		else if(c == ',' || c == 0)
		{
			if(t < 0)
			{
				return -1;
			}
			if(t > VDIF_MAX_THREAD_ID)
			{
				return -2;
			}
			if(threadMap[t])
			{
				printf("Warning: thread %d listed more than once!\n", t);
			}
			else
			{
				++n;
			}
			threadMap[t] = 1;
			t = -1;
		}
		else
		{
			return -3;
		}
		++i;
	}
	while(c > 0);

	return n;
}

int main(int argc, char **argv)
{
	FILE *in;
	FILE *out;
	vdif_header H;
	const vdif_header *V;
	char *data;
	int frameSize;
	int nThread;
	int threadMap[VDIF_MAX_THREAD_ID+1];	// set to 1 if the thread is to be saved, zero otherwise
	int threadCount[VDIF_MAX_THREAD_ID+1];	// count of threads found in the file (for end report)
	int t;
	size_t s;

	if(argc != 4)
	{
		usage(argv[0]);

		return EXIT_FAILURE;
	}

	nThread = parseThreads(threadMap, argv[3]);
	if(nThread < 0)
	{
		fprintf(stderr, "Error: cannot properly parse thread list : %s .   Return code was %d .\n", argv[3], nThread);

		return EXIT_FAILURE;
	}

	printf("Extracting %d threads\n", nThread);

	for(t = 0; t <= VDIF_MAX_THREAD_ID; ++t)
	{
		threadCount[t] = 0;
	}

	in = fopen(argv[1], "r");
	if(!in)
	{
		fclose(in);
		fprintf(stderr, "Error: cannot open %s for read\n", argv[1]);

		return EXIT_FAILURE;
	}

	s = fread(&H, sizeof(H), 1, in);
	if(!s)
	{
		fclose(in);
		fprintf(stderr, "Error: first header read from %s failed\n", argv[1]);

		return EXIT_FAILURE;
	}

	frameSize = getVDIFFrameBytes(&H);
	if(frameSize < 16 || frameSize > 5000000)
	{
		fclose(in);
		fprintf(stderr, "Error: Likely corruption of VDIF data as frame size is %d bytes\n", frameSize);

		return EXIT_FAILURE;
	}

	data = malloc(frameSize);
	V = (const vdif_header *)data;
	if(!data)
	{
		fclose(in);
		fprintf(stderr, "Error: cannot allocate %d bytes\n", frameSize);

		return EXIT_FAILURE;
	}
	memcpy(data, &H, sizeof(H));
	s = fread(data+sizeof(H), 1, frameSize-sizeof(H), in);
	if(s != frameSize-sizeof(H))
	{
		free(data);
		fclose(in);
		fprintf(stderr, "Error: first frame read from %s failed\n", argv[1]);

		return EXIT_FAILURE;
	}

	out = fopen(argv[2], "w");
	if(!out)
	{
		free(data);
		fclose(in);
		fprintf(stderr, "Error: cannot open %s for write\n", argv[2]);

		return EXIT_FAILURE;
	}

	do
	{
		t = getVDIFThreadID(V);
		if(threadMap[t])
		{
			s = fwrite(data, 1, frameSize, out);
			if(s != frameSize)
			{
				free(data);
				fclose(in);
				fclose(out);
				fprintf(stderr, "Error: failed to write to %s.  Disk full?\n", argv[2]);

				return EXIT_FAILURE;
			}
		}
		++threadCount[t];

		s = fread(data, 1, frameSize, in);
	}
	while(s == frameSize);
	
	fclose(in);
	fclose(out);
	free(data);

	printf("Summary of threads\n");

	for(t = 0; t <= VDIF_MAX_THREAD_ID; ++t)
	{
		if(threadMap[t] || threadCount[t])
		{
			printf("  Thread %4d : %d packets %s\n", t, threadCount[t], threadMap[t] ? "retained" : "filtered out");
		}
	}

	return EXIT_SUCCESS;
}

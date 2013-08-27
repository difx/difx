/***************************************************************************
 *   Copyright (C) 2006-2011 by Walter Brisken                             *
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

#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../mark5access/mark5_stream.h"

const char program[] = "m5states";
const char author[]  = "Walter Brisken";
const char version[] = "0.2";
const char verdate[] = "2011 Mar 15";

int usage(const char *pgm)
{
	printf("\n");

	printf("%s ver. %s   %s  %s\n\n", program, version, author, verdate);
	printf("A Mark5 switched power generator for VLBA, Mark3/4, and Mark5B "
		"formats using the\nmark5access library.\n\n");
	printf("Usage : %s <file> <dataformat> [<n>] [<offset>]\n\n", pgm);
	printf("  <file> is the name of the input file\n\n");
	printf("  <dataformat> should be of the form: "
		"<FORMAT>-<Mbps>-<nchan>-<nbit>, e.g.:\n");
	printf("    VLBA1_2-256-8-2\n");
	printf("    MKIV1_4-128-2-1\n");
	printf("    Mark5B-512-16-2\n");
	printf("    VDIF_1000-64-1-2 (here 1000 is payload size in bytes)\n\n");
	printf("  <n> is the number of samples per channel to count\n\n");
	printf("  <offset> is number of bytes into file to start count\n\n");
	printf("If <n> is not provided or is < 0, then the whole file is processed.\n\n");

	return 0;
}

void printpower(int sec, int nchan, unsigned int *pOn, unsigned int *pOff, int nOn, int nOff)
{
	int k;
	double fOn = 0.0, fOff = 0.0;
	double powerOn, powerOff;

	printf("%5d %7d %7d", sec, nOn, nOff);
	for(k = 0; k < nchan; k++)
	{
		if(nOn > 0)
		{
			fOn = pOn[k]/(double)nOn;
		}
		if(nOff > 0)
		{
			fOff = pOff[k]/(double)nOff;
		}
		
		powerOn = high_state_fraction_to_power(fOn);
		powerOff = high_state_fraction_to_power(fOff);

		printf(" %7.5f %7.5f", powerOn, powerOff);
	}
	printf("\n");
}

int calcpower(const char *filename, const char *formatname, long long offset, long long n)
{
	struct mark5_stream *ms;
	unsigned int *pOn, *pOff;
	int k, status;
	long long chunk = 2000;
	long long nOn, nOff;
	int mjd, sec, lastsec = -1;
	double ns;
	int phase, on;

	if(n < 0)
	{
		n = 1LL << 60;	/* in otherwords, the entire file */
	}

	nOn = 0;
	nOff = 0;

	ms = new_mark5_stream_absorb(
		new_mark5_stream_file(filename, offset),
		new_mark5_format_generic_from_string(formatname) );

	if(!ms)
	{
		printf("Error: problem opening %s\n", filename);

		return 0;
	}

	pOn = (unsigned int *)calloc(ms->nchan, sizeof(unsigned int));
	pOff = (unsigned int *)calloc(ms->nchan, sizeof(unsigned int));

	mark5_stream_print(ms);

	if(n % (long long)(ms->samplegranularity) > 0LL)
	{
		printf("Warning: reducing read size from %Ld", n);
		n -= (n % (long long)(ms->samplegranularity));
		printf(" to %Ld\n", n);
	}

	mark5_stream_get_frame_time(ms, &mjd, &sec, &ns);

	for(; n > 0; n -= chunk)
	{
		if(n < chunk)
		{
			chunk = n;
		}

		phase = (int)(ns * 160.0e-9);
		on = (phase % 2 == 0);

		if(sec != lastsec && nOn + nOff > 0)
		{
			printpower(lastsec, ms->nchan, pOn, pOff, nOn, nOff);
			for(k = 0; k < ms->nchan; k++)
			{
				pOn[k] = pOff[k] = 0.0;
			}
			nOn = nOff = 0;
		}

		lastsec = sec;

		if(on)
		{
			status = mark5_stream_count_high_states(ms, chunk, pOn);
		}
		else
		{
			status = mark5_stream_count_high_states(ms, chunk, pOff);
		}
		
		if(status < 0)
		{
			break;
		}
		else
		{
			if(on)
			{
				nOn += status;
			}
			else
			{
				nOff += status;
			}
		}

		ns += chunk*1.0e9/ms->samprate;
		if(ns > 1.0e9)
		{
			ns -= 1.0e9;
			sec++;
		}
	}

	if(nOn + nOff > 0)
	{
		printpower(lastsec, ms->nchan, pOn, pOff, nOn, nOff);
	}

	free(pOn);
	free(pOff);

	delete_mark5_stream(ms);

	return 0;
}

int main(int argc, char **argv)
{
	long long offset = 0;
	long long n = -1;
	int r;

	if(argc == 2)
	{
		struct mark5_format *mf;
		int bufferlen = 1<<11;
		char *buffer;
		FILE *in;

		if(strcmp(argv[1], "-h") == 0 ||
		   strcmp(argv[1], "--help") == 0)
		{
			return usage(argv[0]);
		}

		in = fopen(argv[1], "r");
		if(!in)
		{
			fprintf(stderr, "Error: cannot open file '%s'\n", argv[1]);
			return 0;
		}

		buffer = malloc(bufferlen);

		r = fread(buffer, bufferlen, 1, in);
		if(r < 1)
		{
			fprintf(stderr, "Error: cannot read %d bytes from file\n", bufferlen);
		}
		else
		{
			mf = new_mark5_format_from_stream(new_mark5_stream_memory(buffer, bufferlen/2));

			print_mark5_format(mf);
			delete_mark5_format(mf);

			mf = new_mark5_format_from_stream(new_mark5_stream_memory(buffer, bufferlen/2));

			print_mark5_format(mf);
			delete_mark5_format(mf);
		}

		fclose(in);
		free(buffer);

		return 0;
	}

	else if(argc < 3)
	{
		return usage(argv[0]);
	}

	if(argc > 3)
	{
		n = atoll(argv[3]);
	}

	if(argc > 4)
	{
		offset = atoll(argv[4]);
	}

	calcpower(argv[1], argv[2], offset, n);

	return 0;
}


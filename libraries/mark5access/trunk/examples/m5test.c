/***************************************************************************
 *   Copyright (C) 2007-2019 by Walter Brisken                             *
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
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/mark5access/trunk/mark5access/mark5_stream.c $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include "../mark5access/mark5_stream.h"
#include "config.h"

const char program[] = "m5test";
const char author[]  = "Walter Brisken";
const char version[] = "1.4";
const char verdate[] = "20191114";

const int ChunkSize = 10000;

volatile int die = 0;

typedef void (*sighandler_t)(int);

struct sigaction old_sigint_action;

void siginthand(int j)
{
	fprintf(stderr, "\nBeing killed.\n\n");
	die = 1;

	sigaction(SIGINT, &old_sigint_action, 0);
}

int usage(const char *pgm)
{
	printf("\n");

	printf("%s ver. %s   %s  %s\n\n", program, version, author, verdate);
	printf("A Mark5 tester.  Can verify VLBA, Mark3/4, and Mark5B "
		"formats using the\nmark5access library.\n\n");
	printf("Usage : %s <file> <dataformat> [<offset>] [<report>]\n\n", pgm);
	printf("  <file> is the name of the input file\n\n");
	printf("  <dataformat> should be of the form: "
		"<FORMAT>-<Mbps>-<nchan>-<nbit>, e.g.:\n");
	printf("    VLBA1_2-256-8-2\n");
	printf("    MKIV1_4-128-2-1\n");
	printf("    Mark5B-512-16-2\n");
	printf("    VDIF_1000-64-1-2 (here 1000 is payload size in bytes)\n");
	printf("  alternatively for VDIF and CODIF, Mbps can be replaced by <FramesPerPeriod>m<AlignmentSeconds>, e.g.\n");
	printf("    VDIF_1000-64000m1-1-2 (8000 frames per 1 second, x1000 bytes x 8 bits= 64 Mbps)\n");
	printf("    CODIFC_5000-51200m27-8-1 (51200 frames every 27 seconds, x5000 bytes x 8 bits / 27  ~= 76 Mbps\n");
	printf("    This allows you to specify rates that are not an integer Mbps value, such as 32/27 CODIF oversampling\n\n");
	printf("  <offset> is number of bytes into file to start decoding\n\n");
	printf("  <report> use 0 to report all timestamps, 1 to report once a second\n\n");

	return EXIT_SUCCESS;
}

int verify(const char *filename, const char *formatname, long long offset, int report_interval)
{
	struct mark5_stream *ms;
	float **data;
	int i, status;
	long long total, unpacked, nvalidatepass = 0, nvalidatefail = 0;
	int osec = 0, omjd = 0;

	total = unpacked = 0;

	ms = new_mark5_stream(
		new_mark5_stream_file(filename, offset),
		new_mark5_format_generic_from_string(formatname) );

	if(!ms)
	{
		fprintf(stderr, "problem opening %s\n", filename);

		return 0;
	}

	data = (float **)malloc(ms->nchan*sizeof(float *));
	for(i = 0; i < ms->nchan; ++i)
	{
		data[i] = (float *)malloc(ChunkSize*sizeof(float));
	}

	mark5_stream_print(ms);

	for(i = 0;; ++i)
	{
		if(die)
		{
			break;
		}

		if(!ms->iscomplex) 
		{
			status = mark5_stream_decode(ms, ChunkSize, data);
		}
		else
		{
			status = mark5_stream_decode_complex(ms, ChunkSize/2, (mark5_float_complex **)data);
		}
		
		if(status < 0)
		{
			fprintf(stderr, "<EOF> status=%d\n", status);
			break;
		}
		else
		{
			total += ChunkSize;
			unpacked += status;
		}
		if(i%1 == 0)
		{
			int mjd, sec;
			double ns;

			mark5_stream_get_frame_time(ms, &mjd, &sec, &ns);

			if(omjd <= 0) 
			{
				omjd = mjd;
				osec = sec;
			}

			if((mjd - omjd) >= 2 || (mjd < omjd))
			{
				printf("Jump in MJD day (%d/%d.xxxxs -> %d/%.4fs), trying to resync\n", omjd, osec, mjd, sec+ns*1e-9);

				mark5_stream_resync(ms);

				continue;
			}

                        if(report_interval == 0)
			{
				printf("frame_num=%lld mjd=%d sec=%d ns=%011.1f n_valid=%d n_invalid=%d %llu\n",
					ms->framenum, mjd, sec, ns,
					ms->nvalidatepass, ms->nvalidatefail, offset);
                        } 
			else 
			{
				if(sec != osec) 
				{
					printf("frame_num=%lld mjd=%d sec=%d ns=%011.1f n_valid=%lld n_invalid=%lld total=%llu unp=%llu\n",
						ms->framenum, mjd, sec, ns,
						nvalidatepass, nvalidatefail, total, unpacked);
					nvalidatepass = 0;
					nvalidatefail = 0;
					osec = sec;
				} 
				else 
				{
					nvalidatepass += ms->nvalidatepass;
					nvalidatefail += ms->nvalidatefail;
				}
			}
		}

		if(ms->nvalidatefail > 20)
		{
			// break;
		}
	}

	printf("%lld / %lld samples unpacked\n", unpacked, total);

	for(i = 0; i < ms->nchan; ++i)
	{
		free(data[i]);
	}
	free(data);

	delete_mark5_stream(ms);

	return 0;
}

int main(int argc, char **argv)
{
	long long offset = 0;
	int report_interval = 0;
	int r;
	struct sigaction new_sigint_action;

	if(argc > 1 && (strcmp(argv[1], "-h") == 0 || strcmp(argv[1], "--help") == 0))
	{
		return usage(argv[0]);
	}

	if(argc == 2)
	{
		struct mark5_format *mf;
		int bufferlen = 5e4;
		char *buffer;
		FILE *in;

		buffer = malloc(bufferlen);

		in = fopen(argv[1], "r");
		if(!in)
		{
			fprintf(stderr, "Error: cannot open %s for read\n", argv[1]);

			exit(EXIT_FAILURE);
		}
		r = fread(buffer, bufferlen, 1, in);
		if(r < 1)
		{
			fprintf(stderr, "Error: cannot read %d bytes from file\n", bufferlen);
		}
		else
		{
			mf = new_mark5_format_from_stream(
				new_mark5_stream_memory(buffer, bufferlen/2));

			print_mark5_format(mf);
			delete_mark5_format(mf);
		}

		fclose(in);
		free(buffer);

		return EXIT_SUCCESS;
	}

	else if(argc < 3)
	{
		return usage(argv[0]);
	}

	if(argc > 3)
	{
		offset=atoll(argv[3]);
	}

	if(argc > 4)
	{
		report_interval=atoi(argv[4]);
	}

	new_sigint_action.sa_handler = siginthand;
	sigemptyset(&new_sigint_action.sa_mask);
	new_sigint_action.sa_flags = 0;
	sigaction(SIGINT, &new_sigint_action, &old_sigint_action);

	verify(argv[1], argv[2], offset, report_interval);

	return EXIT_SUCCESS;
}


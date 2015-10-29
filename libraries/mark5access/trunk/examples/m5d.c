/***************************************************************************
 *   Copyright (C) 2006-2012 by Walter Brisken                             *
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

#include "config.h"
#include <complex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "../mark5access/mark5_stream.h"

const char program[] = "m5d";
const char author[]  = "Walter Brisken";
const char version[] = "1.4";
const char verdate[] = "20151029";

static void usage(const char *pgm)
{
	printf("\n");

	printf("%s ver. %s   %s  %s\n\n", program, version, author, verdate);
	printf("A Mark5 decoder.  Can decode VLBA, Mark3/4, and Mark5B formats using the\nmark5access library.\n\n");
	printf("Usage : %s <file> <dataformat> <n> [<offset>]\n\n", pgm);
	printf("  <file> is the name of the input file\n\n");
	printf("  <dataformat> should be of the form: <FORMAT>-<Mbps>-<nchan>-<nbit>, e.g.:\n");
	printf("    VLBA1_2-256-8-2\n");
	printf("    MKIV1_4-128-2-1\n");
	printf("    Mark5B-512-16-2\n");
	printf("    VDIF_1000-64-1-2 (here 1000 is payload size in bytes)\n\n");
	printf("  <n> is the number of samples per channel to decode\n\n");
	printf("  <offset> is number of bytes into file to start decoding\n\n");
	printf("The following options are supported\n\n");
	printf("    --double    Double sideband (complex) data\n");
	printf("                If using VDIF, specify VDIFC (complex VDIF) under dataformat\n\n");
	printf("    --format=%%f Format specifier for sample printout (default: %%2.0f)\n\n");
	printf("    --help      This list\n\n");
}


typedef struct vdif_edv4_header {	/* proposed extension extensions: (WFB email to VDIF committee 2015/10/09) */
   uint32_t seconds : 30;
   uint32_t legacymode : 1;
   uint32_t invalid : 1;
   
   uint32_t frame : 24;
   uint32_t epoch : 6;
   uint32_t unassigned : 2;
   
   uint32_t framelength8 : 24;	// Frame length (including header) divided by 8 
   uint32_t nchan : 5;
   uint32_t version : 3;
   
   uint32_t stationid : 16;
   uint32_t threadid : 10;
   uint32_t nbits : 5;
   uint32_t iscomplex : 1;

   uint32_t dummy : 8;
   uint32_t oldEDV : 8;		// EDV of pre-merged streams
   uint32_t mergedthreads : 8;	// Number of threads merged to get to this VDIF frame */
   uint32_t eversion : 8;	// Should be set to 4
   
   uint32_t syncword;		// 0xACABFEED
   
   uint64_t validitymask;	// bits set if data is present
 } vdif_edv4_header;


static int decode_short(const char *filename, const char *formatname, const char *f, long long offset, int n)
{
	struct mark5_stream *ms;
	float **data;
	int i, j, k, status;
	int chunk = 20000;
	int total, unpacked;
	int *invalid;
	int *validsum;
	int rawsize;
	int start;
	unsigned char *raw;
	FILE *in;
	int edv4 = 0;

	total = unpacked = 0;

	in = fopen(filename, "r");
	if(!in)
	{
		fprintf(stderr, "Error: could not open %s for read\n", filename);

		return EXIT_FAILURE;
	}
	if(offset != lseek(fileno(in), offset, SEEK_SET))
	{
		fprintf(stderr, "Error: can't seek %lld bytes into %s\n", offset, filename);
		fclose(in);

		return EXIT_FAILURE;
	}

	ms = new_mark5_stream( new_mark5_stream_unpacker(0), new_mark5_format_generic_from_string(formatname) );
	if(!ms)
	{
		fprintf(stderr, "Error: problem opening or decoding %s\n", filename);
		fclose(in);

		return EXIT_FAILURE;
	}

	data = (float **)malloc(ms->nchan*sizeof(float *));
	for(i = 0; i < ms->nchan; ++i)
	{
		data[i] = (float *)malloc(chunk*sizeof(float));
	}
	rawsize = ms->nchan*ms->nbit*ms->framebytes/(8LL*ms->databytes) + 2*ms->framebytes;
	raw = (unsigned char *)malloc(rawsize);
	invalid = (int *)calloc(ms->nchan, sizeof(int));
	validsum = (int *)calloc(ms->nchan, sizeof(int));

	fread(raw, 1, rawsize, in);
	fclose(in);

	mark5_stream_print(ms);

	if(n % (long long)(ms->samplegranularity) > 0LL)
	{
		printf("EOF reached; reducing read size from %d", n);
		n -= (n % (long long)(ms->samplegranularity));
		printf(" to %d\n", n);
	}

	start = 0;
	for(; n > 0; n -= chunk)
	{
		if(n < chunk)
		{
			chunk = n;
		}
		status = mark5_unpack_with_offset(ms, raw, start, data, chunk);
		
		if(status < 0)
		{
			printf("<EOF> status=%d\n", status);
			
			break;
		}
		else
		{
			total += chunk;
			unpacked += status;
		}

		for(j = 0; j < ms->nchan; ++j)
		{
			validsum[j] += status;
		}

		if(ms->format == MK5_FORMAT_VDIF)
		{
			const vdif_edv4_header *V;

			V = (const vdif_edv4_header *)raw;

			if(V->eversion == 4)
			{
				++edv4;
				blank_vdif_EDV4(raw, start, data, chunk, invalid);
				
				for(j = 0; j < ms->nchan; ++j)
				{
					validsum[j] -= invalid[j];
				}
			}
		}

		for(j = 0; j < chunk; j++)
		{
			for(k = 0; k < ms->nchan; k++)
			{
				printf(f, data[k][j]);
			}
			printf("\n");
		}

		start += chunk;
	}

	if(edv4)
	{
		for(j = 0; j < ms->nchan; ++j)
		{
			fprintf(stderr, "chan %d : %d / %d samples unpacked\n", j, validsum[j], total);
		}
	}
	else
	{
		fprintf(stderr, "%d / %d samples unpacked\n", unpacked, total);
	}

	for(i = 0; i < ms->nchan; i++)
	{
		free(data[i]);
	}
	free(data);

	delete_mark5_stream(ms);

	free(raw);
	free(invalid);
	free(validsum);

	return EXIT_SUCCESS;
}

static int decode(const char *filename, const char *formatname, const char *f, long long offset, long long n)
{
	struct mark5_stream *ms;
	float **data;
	int i, j, k, status;
	long long chunk = 20000;
	long long total, unpacked;

	total = unpacked = 0;

	ms = new_mark5_stream_absorb(
		new_mark5_stream_file(filename, offset),
		new_mark5_format_generic_from_string(formatname));
	if(!ms)
	{
		fprintf(stderr, "Error: problem opening or decoding %s\n", filename);

		return EXIT_FAILURE;
	}

	data = (float **)malloc(ms->nchan*sizeof(float *));
	for(i = 0; i < ms->nchan; ++i)
	{
		data[i] = (float *)malloc(chunk*sizeof(float));
	}

	mark5_stream_print(ms);

	if(n % (long long)(ms->samplegranularity) > 0LL)
	{
		printf("EOF reached; reducing read size from %lld", n);
		n -= (n % (long long)(ms->samplegranularity));
		printf(" to %lld\n", n);
	}

	for(; n > 0; n -= chunk)
	{
		if(n < chunk)
		{
			chunk = n;
		}
		status = mark5_stream_decode(ms, chunk, data);
		
		if(status < 0)
		{
			printf("<EOF> status=%d\n", status);
			
			break;
		}
		else
		{
			total += chunk;
			unpacked += status;
		}

		for(j = 0; j < chunk; j++)
		{
			for(k = 0; k < ms->nchan; k++)
			{
				printf(f, data[k][j]);
			}
			printf("\n");
		}
	}

	fprintf(stderr, "%lld / %lld samples unpacked\n", unpacked, total);

	for(i = 0; i < ms->nchan; i++)
	{
		free(data[i]);
	}
	free(data);

	delete_mark5_stream(ms);

	return EXIT_SUCCESS;
}

static int decode_complex(const char *filename, const char *formatname, const char *f, long long offset, long long n)
{
	struct mark5_stream *ms;
	float complex **data;
	int i, j, k, status;
	long long chunk = 20000;
	long long total, unpacked;

	total = unpacked = 0;

	ms = new_mark5_stream_absorb(
		new_mark5_stream_file(filename, offset),
		new_mark5_format_generic_from_string(formatname));
	if(!ms)
	{
		fprintf(stderr, "Error: problem opening or decoding %s\n", filename);

		return EXIT_FAILURE;
	}

	data = (float complex **)malloc(ms->nchan*sizeof(float *));
	for(i = 0; i < ms->nchan; i++)
	{
		data[i] = (float complex *)malloc(chunk*sizeof(float complex));
	}

	mark5_stream_print(ms);

	if(n % (long long)(ms->samplegranularity) > 0LL)
	{
		printf("EOF reached; reducing read size from %lld", n);
		n -= (n % (long long)(ms->samplegranularity));
		printf(" to %lld\n", n);
	}

	for(; n > 0; n -= chunk)
	{
		if(n < chunk)
		{
			chunk = n;
		}
		status = mark5_stream_decode_complex(ms, chunk, data);
		
		if(status < 0)
		{
			printf("<EOF> status=%d\n", status);
			
			break;
		}
		else
		{
			total += chunk;
			unpacked += status;
		}

		for(j = 0; j < chunk; j++)
		{
			for(k = 0; k < ms->nchan; k++)
			{
				printf(f, creal(data[k][j]), cimag(data[k][j]));
			}
			printf("\n");
		}
	}

	fprintf(stderr, "%lld / %lld complex samples unpacked\n", unpacked, total);

	for(i = 0; i < ms->nchan; i++)
	{
		free(data[i]);
	}
	free(data);

	delete_mark5_stream(ms);

	return EXIT_SUCCESS;
}

int main(int argc, char **argv)
{
	long long offset = 0;
	long long n;
	int r;
	int retval;
	int doublesideband = 0;
	int optind = 1, optc = 0;
	char *format = "%2.0f";

	while(optind < argc)
	{
		if(strcmp(argv[optind], "-h") == 0 ||
		   strcmp(argv[optind], "--help") == 0)
		{
			usage(argv[0]);

			return EXIT_SUCCESS;
		}
		else if(strcmp(argv[optind], "-d") == 0 ||
		   strcmp(argv[optind], "--double") == 0)
		{
			doublesideband = 1;
			printf("Assuming double sideband data\n");
		}
		else if(strncmp(argv[optind], "--format=", 9) == 0)
		{
			format = argv[optind]+9;
		}
		else
		{
			break;
		}
		optind++;
	}
	optc = argc - optind;

	if(optc == 1)
	{
		struct mark5_format *mf;
		int bufferlen = 1<<19;	/* should not be less than twice the largest data frame that could be encountered */
		char *buffer;
		FILE *in;

		in = fopen(argv[optind], "r");
		if(!in)
		{
			fprintf(stderr, "Error: cannot open file '%s'\n", argv[optind]);

			return EXIT_FAILURE;
		}

		buffer = malloc(bufferlen);

		r = fread(buffer, bufferlen, 1, in);
		if(r < 1)
		{
			fprintf(stderr, "Error: cannot read %d bytes from file\n", bufferlen);

			fclose(in);
			free(buffer);

			return EXIT_FAILURE;
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

	else if((optc < 3) || (optc > 4))
	{
		usage(argv[0]);

		return EXIT_FAILURE;
	}

	n = atoll(argv[optind+2]);

	if(optc == 4)
	{
		offset=atoll(argv[optind+3]);
	}

	if(doublesideband)
	{
		char* f = (char*)malloc(2*strlen(format)+10);
		sprintf(f, "%s %si  ", format, format);
		retval = decode_complex(argv[optind], argv[optind+1], f, offset, n);
	}
	else
	{
		char* f = (char*)malloc(strlen(format)+10);
		sprintf(f, "%s ", format);
		if(n < 100000)
		{
			retval = decode_short(argv[optind], argv[optind+1], f, offset, n);
		}
		else
		{
			retval = decode(argv[optind], argv[optind+1], f, offset, n);
		}
	}

	return retval;
}


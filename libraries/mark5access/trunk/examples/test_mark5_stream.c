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
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/mark5access/trunk/mark5access/mark5_stream.c $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include "../mark5access/mark5_stream.h"

#define NREAD 1

#if 0

int test0(const char *filename, int nbit, int ntrack, 
	int fanout, int offset,  int n)
{
	struct mark5_stream *ms;
	double **data;
	int i, j;

	ms = new_mark5_stream_absorb(
		new_mark5_stream_file(filename, offset),
		new_mark5_format_vlba(nbit, ntrack, fanout) );

	if(!ms)
	{
		printf("problem opening %s\n", filename);
		return 0;
	}

	data = (double **)malloc(ms->nchan*sizeof(double *));
	for(i = 0; i < ms->nchan; i++)
	{
		data[i] = (double *)malloc(n*sizeof(double));
	}

	for(i = 0; i < NREAD; i++)
		mark5_stream_decode_double(ms, n, data);

	mark5_stream_print(ms);

	for(i = 0; i < 10; i++)
	{
		for(j = 0; j < ms->nchan; j++)
		{
			printf("%+4.1f ", data[j][i]);
		}
		printf("\n");
	}
	
	for(i = 0; i < ms->nchan; i++)
	{
		free(data[i]);
	}
	free(data);

	delete_mark5_stream(ms);
}

int test1(const char *filename, int nbit, int ntrack, 
	int fanout, int offset,  int n)
{
	struct mark5_stream *ms;
	double **data;
	unsigned char  *buffer;
	FILE *in;
	int i, j;

	buffer = (unsigned char  *)malloc(1<<19);

	in = fopen(filename, "r");
	if(!in)
	{
		printf("problem opening %s\n", filename);
		return 0;
	}

	fread(buffer, 1, 1<<19, in);

	fclose(in);

	ms = new_mark5_stream_absorb(
		new_mark5_stream_memory(buffer, 1<<19),
		new_mark5_format_vlba(nbit, ntrack, fanout) );

	printf("%x %x\n", ms->frame[0], ms->frame[192]);


	data = (double **)malloc(ms->nchan*sizeof(double *));
	for(i = 0; i < ms->nchan; i++)
	{
		data[i] = (double *)malloc(n*sizeof(double));
	}

	for(i = 0; i < NREAD; i++)
		mark5_stream_decode_double(ms, n, data);

	mark5_stream_print(ms);

	for(i = 0; i < 10; i++)
	{
		for(j = 0; j < ms->nchan; j++)
		{
			printf("%+4.1f ", data[j][i]);
		}
		printf("\n");
	}
	
	for(i = 0; i < ms->nchan; i++)
	{
		free(data[i]);
	}
	free(data);

	delete_mark5_stream(ms);
}

int test2(const char *filename, int nbit, int fanout, int offset,  int n)
{
	struct mark5_stream *ms;
	float **data;
	int i, j;

	ms = mark5_stream_open(filename, nbit, fanout, offset);

	if(!ms)
	{
		printf("problem opening %s\n", filename);
		return 0;
	}

	data = (float **)malloc(ms->nchan*sizeof(float *));
	for(i = 0; i < ms->nchan; i++)
	{
		data[i] = (float *)malloc(n*sizeof(float));
	}

	for(i = 0; i < NREAD; i++)
		mark5_stream_decode(ms, n, data);

	mark5_stream_print(ms);

	for(i = 0; i < 10; i++)
	{
		for(j = 0; j < ms->nchan; j++)
		{
			printf("%+4.1f ", data[j][i]);
		}
		printf("\n");
	}
	
	for(i = 0; i < ms->nchan; i++)
	{
		free(data[i]);
	}
	free(data);

	delete_mark5_stream(ms);
}

int test3(const char *filename, int nbit, int ntrack, 
	int fanout, int offset, int n)
{
	struct mark5_stream *ms1, *ms2;
	float **data;
	unsigned char  *buffer;
	int i, j;

	buffer = (unsigned char  *)malloc(1<<20);

	ms1 = new_mark5_stream_absorb(
		new_mark5_stream_file(filename, offset),
		new_mark5_format_vlba(nbit, ntrack, fanout) );

	ms2 = new_mark5_stream_absorb(
		new_mark5_stream_unpacker(1),
		new_mark5_format_vlba(nbit, ntrack, fanout) );

	if(!ms1)
	{
		printf("problem opening %s\n", filename);
		return 0;
	}

	if(!ms2)
	{
		printf("problem making unpacker\n");
		return 0;
	}

	for(i = 0; i < 200; i++) printf("%x ", ms1->frame[i]); printf("\n");

	mark5_stream_print(ms1);
	mark5_stream_print(ms2);

	data = (float **)malloc(ms1->nchan*sizeof(float *));
	for(i = 0; i < ms1->nchan; i++)
	{
		data[i] = (float *)malloc(n*sizeof(float));
	}

	mark5_stream_copy(ms1, 1<<18, buffer);
	mark5_unpack(ms2, buffer, data, n);
	for(i = 0; i < 200; i++) printf("%x ", buffer[i]); printf("\n");

	for(i = 0; i < 10; i++)
	{
		for(j = 0; j < ms1->nchan; j++)
		{
			printf("%+4.1f ", data[j][i]);
		}
		printf("\n");
	}
	
	for(i = 0; i < ms1->nchan; i++)
	{
		free(data[i]);
	}
	free(data);

	free(buffer);

	delete_mark5_stream(ms1);
	delete_mark5_stream(ms2);
}

int test4(const char *filename, int nbit, int ntrack, 
	int fanout, int offset,  int n)
{
	struct mark5_stream *ms;
	float **data;
	unsigned char  *buffer;
	FILE *in;
	int i, j;

	buffer = (unsigned char  *)malloc(1<<19);

	in = fopen(filename, "r");
	if(!in)
	{
		printf("problem opening %s\n", filename);
		return 0;
	}

	fread(buffer, 1, offset, in);
	fread(buffer, 1, 1<<19, in);

	fclose(in);

	ms = new_mark5_stream(
		new_mark5_stream_unpacker(0),
		new_mark5_format_vlba(nbit, ntrack, fanout) );

	data = (float **)malloc(ms->nchan*sizeof(float *));
	for(i = 0; i < ms->nchan; i++)
	{
		data[i] = (float *)malloc(n*sizeof(float ));
	}

	mark5_unpack(ms, buffer, data, n);

	mark5_stream_print(ms);

	for(i = 0; i < 10; i++)
	{
		for(j = 0; j < ms->nchan; j++)
		{
			printf("%+4.1f ", data[j][i]);
		}
		printf("\n");
	}
	
	for(i = 0; i < ms->nchan; i++)
	{
		free(data[i]);
	}
	free(data);

	delete_mark5_stream(ms);
}

#endif

int test5(const char *filename, const char *formatname, int offset,  int n)
{
	struct mark5_stream *ms;
	float **data;
	int i, j, k;
	FILE *out;
	char fn[MARK5_STREAM_ID_LENGTH];

	ms = new_mark5_stream_absorb(
		new_mark5_stream_file(filename, offset),
		new_mark5_format_generic_from_string(formatname) );

	if(!ms)
	{
		printf("Problem opening %s\n", filename);
		
		return EXIT_FAILURE;
	}

	data = (float **)malloc(ms->nchan*sizeof(float *));
	for(i = 0; i < ms->nchan; i++)
	{
		data[i] = (float *)malloc(n*sizeof(float ));
	}

	mark5_stream_print(ms);

	snprintf(fn, MARK5_STREAM_ID_LENGTH,"ms.%s", ms->formatname);

	out = fopen(fn, "w");

	for(i = 0; i < 100; i++)
	{
		mark5_stream_decode(ms, n, data);
		
		for(j = 0; j < n; j++)
		{
			for(k = 0; k < ms->nchan; k++)
			{
				fprintf(out, "%+2.0f ", data[k][j]);
			}
			fprintf(out, "\n");
		}
	}
	
	fclose(out);

	mark5_stream_fix_mjd(ms, 54000);

	mark5_stream_print(ms);
	
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
	int retval;

	if(argc < 3)
	{
		printf("Usage : %s <infile> <formatname> [<offset>]\n", argv[0]);
		printf("\n  <formatname> should be of the form: <FORMAT>-<Mbps>-<nchan>-<nbit>, e.g.:\n");
		printf("    VLBA1_2-256-8-2\n");
		printf("    MKIV1_4-128-2-1\n");
		printf("    Mark5B-512-16-2\n");
		printf("    VDIF_1000-64-1-2 (here 1000 is payload size in bytes)\n");
		printf("  alternatively for VDIF and CODIF, Mbps can be replaced by <FramesPerPeriod>m<AlignmentSeconds>, e.g.\n");
		printf("    VDIF_1000-64000m1-1-2 (8000 frames per 1 second, x1000 bytes x 8 bits= 64 Mbps)\n");
		printf("    CODIFC_5000-51200m27-8-1 (51200 frames every 27 seconds, x5000 bytes x 8 bits / 27  ~= 76 Mbps\n");
		printf("    This allows you to specify rates that are not an integer Mbps value, such as 32/27 CODIF oversampling\n\n");

		return EXIT_FAILURE;
	}

	if(argc > 3)
	{
		offset=atoll(argv[3]);
	}

#if 0
	test0(argv[1], nbit, ntrack, fanout, offset, 100);
	test1(argv[1], nbit, ntrack, fanout, offset, 100);
	test2(argv[1], nbit, fanout, offset, 100);
	test3(argv[1], nbit, ntrack, fanout, offset, 100);
	test4(argv[1], nbit, ntrack, fanout, offset, 100);
#endif
	retval = test5(argv[1], argv[2], offset, 1000);

	return retval;
}


/***************************************************************************
 *   Copyright (C) 2008-2011 by Walter Brisken                             *
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
#include "../mark5access/mark5_stream.h"

static void usage(const char *pgm)
{
	printf("Usage : %s <format> [<n> [<offset>] ]\n", pgm);
	printf("\n  <format> is something like VLBA1_4-128-8-2\n");
	printf("             or for VDIF: VDIF_1000-64-1-2\n");
	printf("             (where 1000 is payload size in bytes)\n");
	printf("\n  <n>      is samples to look at [default 32]\n");
	printf("\n  <offset> is samples to slip [default 0]\n\n");
}

static int conf(float ***data, struct mark5_stream **ms, const char *format, int samples, int os)
{
	char fmt[MARK5_STREAM_ID_LENGTH];
	int i, j;

	if(os == 1)
	{
		snprintf(fmt, MARK5_STREAM_ID_LENGTH, "%s", format);
	}
	else
	{
		snprintf(fmt, MARK5_STREAM_ID_LENGTH, "%s/%d", format, os);
	}
	printf("Initialize %s\n", fmt);

	*ms = new_mark5_stream(
		new_mark5_stream_unpacker(1),
		new_mark5_format_generic_from_string(fmt) );
	
	if(!*ms)
	{
		return -1;
	}
	
	mark5_stream_print(*ms);
	
	*data = (float **)malloc((*ms)->nchan*sizeof(float *));
	for(i = 0; i < (*ms)->nchan; i++)
	{
		(*data)[i] = (float *)malloc(samples*sizeof(float));
		for(j = 0; j < samples; j++)
		{
			(*data)[i][j] = j;
		}
	}

	return 0;
}

int main(int argc, char **argv)
{
	int n = 32, N = 10000000;
	int offsetsamples = 0;
	float **os1, **os2, **os4, **os8;
	struct mark5_stream *ms1, *ms2, *ms4, *ms8;
	char *data;
	char line[2000], str[2000];
	int i, c;
	char v;

	if(argc < 2)
	{
		usage(argv[0]);

		return EXIT_FAILURE;
	}

	if(argc > 2)
	{
		sscanf(argv[2], "%d", &n);
	}
	if(argc > 3)
	{
		sscanf(argv[3], "%d", &offsetsamples);
	}

	fprintf(stderr, "format = %s  n = %d  o = %d\n", argv[1], n, offsetsamples);

	conf(&os1, &ms1, argv[1], n, 1); 
	conf(&os2, &ms2, argv[1], n, 2); 
	conf(&os4, &ms4, argv[1], n, 4); 
	conf(&os8, &ms8, argv[1], n, 8); 

	data = (char *)malloc(N);
	for(i = 0; i < N; i++)
	{
		data[i] = (i % 11) + 2*(i % 121);
	}

	printf("0\n");
	mark5_unpack_with_offset(ms1, data, offsetsamples,   os1, n);
	printf("1\n");
	mark5_unpack_with_offset(ms2, data, offsetsamples/2, os2, (n+1)/2);
	printf("2\n");
	mark5_unpack_with_offset(ms4, data, offsetsamples/4, os4, (n+3)/4);
	printf("3\n");
	mark5_unpack_with_offset(ms8, data, offsetsamples/8, os8, (n+7)/8);
	printf("4\n");

	for(i = 0; i < n; i++)
	{
		line[0] = 0;
		for(c = 0; c < ms1->nchan; c++)
		{
			sprintf(str, "%3.0f ", os1[c][i]);
			strcat(line, str);
		}
		if(i % 2 == 0)
		{
			strcat(line, "  . ");
			for(c = 0; c < ms2->nchan; c++)
			{
				v = os2[c][i/2] == os1[c][i] ? ' ' : '!';
				sprintf(str, "%2.0f%c", os2[c][i/2], v);
				strcat(line, str);
			}
		}
		if(i % 4 == 0)
		{
			strcat(line, " . ");
			for(c = 0; c < ms4->nchan; c++)
			{
				v = os4[c][i/4] == os1[c][i] ? ' ' : '!';
				sprintf(str, "%2.0f%c", os4[c][i/4], v);
				strcat(line, str);
			}
		}
		if(i % 8 == 0)
		{
			strcat(line, " . ");
			for(c = 0; c < ms8->nchan; c++)
			{
				v = os8[c][i/8] == os1[c][i] ? ' ' : '!';
				sprintf(str, "%2.0f%c", os8[c][i/8], v);
				strcat(line, str);
			}
		}
		printf("%s\n", line);
	}

	return EXIT_SUCCESS;
}

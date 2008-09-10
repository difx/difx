/***************************************************************************
 *   Copyright (C) 2008 by Walter Brisken                                  *
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

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "config.h"
#include "mark5access/mark5_stream.h"

/* the high mag value for 2-bit reconstruction */
static const float HiMag = 3.3359;

struct mark5_format_mark5c
{
	int8_t **payload;
	int *blanked;
};

struct mark5_format_mark5c *new_mark5_format_mark5c(int nchan)
{
	struct mark5_format_mark5c *m;

	m = (struct mark5_format_mark5c *)calloc(1,
		sizeof(struct mark5_format_mark5c));

	m->payload = (int8_t **)calloc(nchan, sizeof(int8_t *));
	m->blanked = (int *)calloc(nchan, sizeof(int));

	return m;
}

void delete_mark5_format_mark5c(struct mark5_format_mark5c *m)
{
	if(m)
	{
		if(m->payload)
		{
			free(m->payload);
		}
		if(m->blanked)
		{
			free(m->blanked);
		}
		free(m);
	}
}

static int findfirstframe(const uint8_t *data, int bytes)
{
	return 0;
}

static int mark5_format_mark5c_init(struct mark5_stream *ms)
{
	struct mark5_format_mark5c *f;

	if(!ms)
	{
		fprintf(stderr, "mark5_format_mark5c_init: ms = 0\n");
		return -1;
	}
	
	f = (struct mark5_format_mark5c *)(ms->formatdata);

	

	return 0;
}

static int mark5_format_mark5c_final(struct mark5_stream *ms)
{
	if(!ms)
	{
		return -1;
	}

	delete_mark5_format_mark5c(
		(struct mark5_format_mark5c *m)(ms->formatdata));
	
	return 0;
}

static int mark5_format_mark5c_validate(const struct mark5_stream *ms)
{
	return 1;
}

struct mark5_format_generic *new_mark5_format_mark5c(int Mbps, int nchan,
	int nbit, int packetsize, int decimation)
{
	struct mark5_format_generic *f;
	struct mark5_format_mark5c *m;
	int decoderindex=0;



	m = new_mark5_format_mark5c(nchan);

	f = (struct mark5_format_generic *)calloc(1,
		sizeof(struct mark5_format_generic));

	f->Mbps = Mbps;
	f->nchan = nchan;
	f->nbit = nbit;
	f->decimation = decimation;
	f->formatdata = m;
	f->gettime = mark5_stream_frame_time_mark5c;
	f->init_format = mark5_format_mark5c_init;
	f->final_format = mark5_format_mark5c_final;
	f->fixmjd = 0;
	f->validate = mark5_format_mark5c_validate;

	/* Fixme -- expand parameter space */
	switch(nbit)
	{
		case 1 : f->decode = mark5c_decode_1bit_decimation1; break;
		case 2 : f->decode = mark5c_decode_1bit_decimation1; break;
	}

	if(f->decode == 0)
	{
		fprintf(stderr, "Illegal Mark5C mode\n");
		return 0;
	}

	return f;
}

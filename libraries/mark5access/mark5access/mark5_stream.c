/***************************************************************************
 *   Copyright (C) 2006-2024 by Walter Brisken, Chris Phillips,            *
 *                              Jan Wagner, Adam Deller,                   *
 *                              Richard Dodson, ...                        * 
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

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <errno.h>

#include "config.h"

#include "mark5access/mark5_stream.h"

FILE* m5stderr = (FILE*)NULL;
FILE* m5stdout = (FILE*)NULL;

#ifdef __GNUC__
void __attribute__ ((constructor)) autocall_mark5_library_init(void)
{
	mark5_library_init();
}
#endif

#ifdef HAVE_CODIFIO
#include <codifio.h>
#endif

void mark5_library_init(void)
{
	// Apply all defaults
	// Note: Apple OS X: during auto-((constructor)) the C-lib stdout, stderr may still be uninitialized/null
	if(stdout != NULL)
	{
		m5stdout = stdout;
	}
	if(stderr != NULL)
	{
		m5stderr = stderr;
	}
}

static void mark5_library_consistent(void)
{
	// Re-apply individual defaults if necessary
	if(m5stdout == NULL)
	{
		m5stdout = stdout;
	}
	if(m5stderr == NULL)
	{
		m5stderr = stderr;
	}
}

int mark5_library_getoption(const int mk5option, void *result)
{
	if (!result)
	{
		return -1;
	}

	mark5_library_consistent();

	switch (mk5option)
	{
		case M5A_OPT_STDOUTFD:
			*((FILE**)result) = m5stdout;
			return sizeof(FILE*);
		case M5A_OPT_STDERRFD:
			*((FILE**)result) = m5stderr;
			return sizeof(FILE*);
		default:
			break;
	}

	return -1;
}

int mark5_library_setoption(const int mk5option, void *value)
{
	int rc = -1;

	mark5_library_consistent();

	if (!value)
	{
		return rc;
	}

	switch (mk5option)
	{
		case M5A_OPT_STDOUTFD:
			m5stdout = (FILE*)value;
			rc = sizeof(FILE*);
			break;
		case M5A_OPT_STDERRFD:
			m5stderr = (FILE*)value;
			rc = sizeof(FILE*);
			break;
		default:
			rc = -1;
			break;		
	}

	mark5_library_consistent();

	return rc;
}

static void mark5_stream_blank_frame(struct mark5_stream *ms)
{
	int z;

	for(z = 0; z < MAXBLANKZONES; ++z)
	{
		ms->blankzonestartvalid[z] = 1<<30;
		ms->blankzoneendvalid[z] = 0;
	}
}

int mark5_stream_next_frame(struct mark5_stream *ms)
{
	int n;
	int v = 0;

	/* call specialized function to ready next frame */
	n = ms->next(ms);

	/* are we at end of file(s)? */
	if(n < 0)
	{
		ms->payload = 0;
		
		return -1;
	}
	
	if(ms->frame)
	{
		/* validate frame */
		v = ms->validate(ms);
		if(!v)
		{
			++ms->nvalidatefail;
			++ms->consecutivefails;
		}
		else
		{
			++ms->nvalidatepass;
			ms->consecutivefails = 0;
		}
	}

	/* set payload pointer to point to start of actual data */
	if(ms->frame)
	{
		ms->payload = ms->frame + ms->payloadoffset;
	}
	
	/* blank bad data if any */
	if(v)
	{
		ms->blanker(ms);
	}
	else /* blank entire frame if validity check fails */
	{
		mark5_stream_blank_frame(ms);
	}

	return 0;
}

static int set_stream(struct mark5_stream *ms, const struct mark5_stream_generic *s)
{
	if(s && ms)
	{
		ms->init_stream = s->init_stream;
		ms->final_stream = s->final_stream;
		ms->next = s->next;
		ms->seek = s->seek;
		if(s->inputdatasize > 0)
		{
			ms->inputdata = malloc(s->inputdatasize);
			memcpy(ms->inputdata, s->inputdata, s->inputdatasize);
		}
		if(!s->init_stream || !s->next)
		{
			return -1;
		}

		return 0;
	}

	return -1;
}

static int set_format(struct mark5_stream *ms, const struct mark5_format_generic *f)
{
	if(f && ms)
	{
		ms->init_format = f->init_format;
		ms->final_format = f->final_format;
		ms->decode = f->decode;
		ms->count = f->count;
		ms->iscomplex = f->iscomplex;
		ms->complex_decode = f->complex_decode;
		ms->validate = f->validate;
		ms->resync = f->resync;
		ms->genheaders = f->genheaders;
		ms->gettime = f->gettime;
		ms->fixmjd = f->fixmjd;
		if(f->formatdatasize > 0)
		{
			ms->formatdata = malloc(f->formatdatasize);
			memcpy(ms->formatdata, f->formatdata, f->formatdatasize);
		}
		ms->Mbps = f->Mbps;
		ms->framesperperiod = f->framesperperiod;
		ms->alignmentseconds = f->alignmentseconds;
		ms->nchan = f->nchan;
		ms->nbit = f->nbit;
		ms->decimation = f->decimation;

		if(!f->init_format)
		{
			return -2;
		}
		if(!(f->decode || f->complex_decode))
		{
			return -3;
		}
		if(!f->gettime)
		{
			return -4;
		}
		
		return 0;
	}

	return -1;
}

static int copy_format(const struct mark5_stream *ms, struct mark5_format *mf)
{
	mf->frameoffset     = ms->frameoffset;
	mf->framebytes      = ms->framebytes;
	mf->databytes       = ms->databytes;
	mf->framens         = ms->framens;
	mf->mjd             = ms->mjd;
	mf->sec             = ms->sec;
	mf->ns              = ms->ns;
	mf->Mbps            = ms->Mbps;
	mf->framesperperiod = ms->framesperperiod;
	mf->alignmentseconds= ms->alignmentseconds;
	mf->nchan           = ms->nchan;
	mf->nbit            = ms->nbit;
	mf->decimation      = ms->decimation;

	return 0;
}

static int mark5_format_init(struct mark5_stream *ms)
{
        ms->framenum = 0;
	ms->readposition = 0;
	ms->frame = 0;
	ms->payload = 0;
	ms->framens = 0.0;
	ms->samprate = 0;
	ms->mjd = 0;
	ms->sec = 0;
	ms->ns = 0;

	return ms->init_format(ms);
}

/* Compatibility function */
struct mark5_stream *mark5_stream_open(const char *filename, int nbit, int fanout, long long offset)
{
	struct mark5_stream_generic *s;
	struct mark5_format_generic *f;
	struct mark5_stream *ms;
	int status, ntrack;

	s = new_mark5_stream_file(filename, offset);
	if(!s)
	{
		return 0;
	}
	
	ms = (struct mark5_stream *)calloc(1, sizeof(struct mark5_stream));
	
	if(set_stream(ms, s) < 0)
	{
		delete_mark5_stream(ms);
		fprintf(m5stderr, "mark5_stream_open: Incomplete stream.\n");
		
		return 0;
	}

	status = s->init_stream(ms);
	if(status < 0)
	{
		delete_mark5_stream(ms);
		fprintf(m5stderr, "mark5_open_stream: init_stream() failed\n");
		
		return 0;
	}

	/* Now go through known formats, looking for a match with the data */
	
	/* VLBA modes */
	for(ntrack = 8; ntrack <= 64; ntrack*=2)
	{
		f = new_mark5_format_vlba(0, ntrack/(nbit*fanout), nbit, fanout, 1);
		set_format(ms, f);
		status = mark5_format_init(ms);
		if(status < 0)
		{
			if(f->final_format)
			{
				f->final_format(ms);
			}
			free(f);
		}
		else
		{
			strcat(ms->formatname, "-Auto");
			return ms;
		}
	}
	
	/* Mark4 modes */
	for(ntrack = 8; ntrack <= 64; ntrack*=2)
	{
		f = new_mark5_format_mark4(0, ntrack/(nbit*fanout), nbit, fanout, 1);
		set_format(ms, f);
		status = mark5_format_init(ms);
		if(status < 0)
		{
			if(f->final_format)
			{
				f->final_format(ms);
			}
			free(f);
		}
		else
		{
			strcat(ms->formatname, "-Auto");
			return ms;
		}
	}
	
	/* No match found */
	delete_mark5_stream(ms);

	return 0;
}

struct mark5_format_generic *new_mark5_format_generic_from_string( const char *formatname)
{
	int a, b, c, d, e, r, s;
	char newformatseparator = 'm'; 	//If present in VDIF or CODIF format string, indicates new-style file name
					//with "<FRAMESPERPERIOD>m<ALIGNEMENTSECONDS> rather than <Mbps>"

	mark5_library_consistent();

	if(strncasecmp(formatname, "VLBA1_", 6) == 0)
	{
		r = sscanf(formatname+6, "%d-%d-%d-%d/%d", &a, &b, &c, &d, &e);
		if(r < 4)
		{
			return 0;
		}
		if(r < 5)
		{
			e = 1;
		}

		return new_mark5_format_vlba(b, c, d, a, e);
	}
	else if(strncasecmp(formatname, "MKIV1_", 6) == 0)
	{
		r = sscanf(formatname+6, "%d-%d-%d-%d/%d", &a, &b, &c, &d, &e);
		if(r < 4)
		{
			return 0;
		}
		if(r < 5)
		{
			e = 1;
		}

		return new_mark5_format_mark4(b, c, d, a, e);
	}
	else if(strncasecmp(formatname, "Mark5B-", 7) == 0)
	{
		r = sscanf(formatname+7, "%d-%d-%d/%d", &a, &b, &c, &e);
		if(r < 3)
		{
			return 0;
		}
		if(r < 4)
		{
			e = 1;
		}

		return new_mark5_format_mark5b(a, b, c, e);
	}
	else if(strncasecmp(formatname, "KVN5B-", 6) == 0)
	{
		r = sscanf(formatname+6, "%d-%d-%d/%d", &a, &b, &c, &e);
		if(r < 3)
		{
			return 0;
		}
		if(r < 4)
		{
			e = 1;
		}
		return new_mark5_format_kvn5b(a, b, c, e);
	}
	else if(strncasecmp(formatname, "D2K-", 6) == 0)
	{
		r = sscanf(formatname+6, "%d-%d-%d/%d", &a, &b, &c, &e);
		if(r < 3)
		{
			return 0;
		}
		if(r < 4)
		{
			e = 1;
		}
		return new_mark5_format_d2k(a, b, c, e);
	}
	else if(strncasecmp(formatname, "VDIF_", 5) == 0)
	{
		if (strchr(formatname, newformatseparator) != NULL)
		{
			r = sscanf(formatname+5, "%d-%dm%d-%d-%d/%d", &e, &a, &s, &b, &c, &d);
			if(r < 5)
			{
				return 0;
			}
			if(r < 6)
			{
				d = 1;
			}
			return new_mark5_format_generalized_vdif(a, s, b, c, d, e, 32, 0);
		}
		else
		{
			r = sscanf(formatname+5, "%d-%d-%d-%d/%d", &e, &a, &b, &c, &d);
			if(r < 4)
			{
				return 0;
			}
			if(r < 5)
			{
				d = 1;
			}
			return new_mark5_format_vdif(a, b, c, d, e, 32, 0);
		}
	}
	else if(strncasecmp(formatname, "VDIFL_", 6) == 0)
	{
		if (strchr(formatname, newformatseparator) != NULL)
		{
			r = sscanf(formatname+6, "%d-%dm%d-%d-%d/%d", &e, &a, &s, &b, &c, &d);
			if(r < 5)
			{
			        return 0;
			}
			if(r < 6)
			{
			        d = 1;
			}
			
			return new_mark5_format_generalized_vdif(a, s, b, c, d, e, 16, 0);
		}
		else
		{
			r = sscanf(formatname+6, "%d-%d-%d-%d/%d", &e, &a, &b, &c, &d);
			if(r < 4)
			{
				return 0;
			}
			if(r < 5)
			{
				d = 1;
			}
	
			return new_mark5_format_vdif(a, b, c, d, e, 16, 0);
		}
	}
	else if(strncasecmp(formatname, "VDIFC_", 6) == 0)
	{
		if (strchr(formatname, newformatseparator) != NULL)
		{
			r = sscanf(formatname+6, "%d-%dm%d-%d-%d/%d", &e, &a, &s, &b, &c, &d);
			if(r < 5)
			{
			        return 0;
			}
			if(r < 6)
			{
			        d = 1;
			}

			return new_mark5_format_generalized_vdif(a, s, b, c, d, e, 32, 1);
		}
		else
		{
			r = sscanf(formatname+6, "%d-%d-%d-%d/%d", &e, &a, &b, &c, &d);
			if(r < 4)
			{
				return 0;
			}
			if(r < 5)
			{
				d = 1;
			}
	
			return new_mark5_format_vdif(a, b, c, d, e, 32, 1);
		}
	}
	else if(strncasecmp(formatname, "VDIFCL_", 7) == 0)
	{
		if (strchr(formatname, newformatseparator) != NULL)
		{
			r = sscanf(formatname+7, "%d-%dm%d-%d-%d/%d", &e, &a, &s, &b, &c, &d);
			if(r < 5)
			{
				return 0;
			}
			if(r < 6)
			{
				d = 1;
			}

			return new_mark5_format_generalized_vdif(a, s, b, c, d, e, 16, 1);
		}
		else
		{
			r = sscanf(formatname+7, "%d-%d-%d-%d/%d", &e, &a, &b, &c, &d);
			if(r < 4)
			{
				return 0;
			}
			if(r < 5)
			{
				d = 1;
			}
	
			return new_mark5_format_vdif(a, b, c, d, e, 16, 1);
		}
	}
#ifdef HAVE_CODIFIO
	else if(strncasecmp(formatname, "CODIF_", 6) == 0)
	{
		r = sscanf(formatname+6, "%d-%dm%d-%d-%d/%d", &e, &a, &s, &b, &c, &d);
		if(r < 5)
		{
			return 0;
		}
		if(r < 6)
		{
			d = 1;
		}

		return new_mark5_format_codif(a,s,b,c,d,e,64,0);
	}
	else if(strncasecmp(formatname, "CODIFC_", 7) == 0)
	{
		r = sscanf(formatname+7, "%d-%dm%d-%d-%d/%d", &e, &a, &s, &b, &c, &d);
		if(r < 5)
		{
			return 0;
		}
		if(r < 6)
		{
			d = 1;
		}

		return new_mark5_format_codif(a,s,b,c,d,e,64,1);
	}
#endif
	else if(strncasecmp(formatname, "VLBN1_", 6) == 0)
	{
		r = sscanf(formatname+6, "%d-%d-%d-%d/%d", &a, &b, &c, &d, &e);
		if(r < 4)
		{
			return 0;
		}
		if(r < 5)
		{
			e = 1;
		}

		return new_mark5_format_vlba_nomod(b, c, d, a, e);
	}
	else
	{
		fprintf(m5stderr, "Unknown format : %s\n", formatname);

		return 0;
	}
}

/* a string containing a list of supported formats */
const char *mark5_stream_list_formats()
{
#ifdef HAVE_CODIFIO
	return "VLBA1_*-*-*-*[/*], MKIV1_*-*-*-*[/*], MARK5B-*-*-*[/*], VDIF_*-*-*-*[/*], VDIFC_*-*-*-*[/*], VLBN1_*-*-*-*[/*], VDIFL_*-*-*-*[/*], VDIFCL_*-*-*-*[/*], KVN5B-*-*-*[/*], D2K-*-*-*[/*], CODIF_*-*m*-*-*[/*], CODIFC_*-*m*-*-*[/*]";
#else
	return "VLBA1_*-*-*-*[/*], MKIV1_*-*-*-*[/*], MARK5B-*-*-*[/*], VDIF_*-*-*-*[/*], VDIFC_*-*-*-*[/*], VLBN1_*-*-*-*[/*], VDIFL_*-*-*-*[/*], VDIFCL_*-*-*-*[/*], KVN5B-*-*-*[/*], D2K-*-*-*[/*]";
#endif
}

/* given a format string, populate a structure with info about format */
struct mark5_format *new_mark5_format_from_name(const char *formatname)
{
	int a=1, b=0, c=0, d=0, e=0, s=0, ntrack=0;
	int databytes, framebytes;
	double framens;
	struct mark5_format *f;
	enum Mark5Format F;
	int decimation = 1;
	int r;
	int fanout = 1;
	int framesperperiod;
	int alignmentseconds = 1;
	double mbps;
	char newformatseparator = 'm';

	mark5_library_consistent();

	if(strncasecmp(formatname, "VLBA1_", 6) == 0)
	{
		r = sscanf(formatname+6, "%d-%d-%d-%d/%d", &a, &b, &c, &d, &e);
		if(r < 4)
		{
			return 0;
		}
		F = MK5_FORMAT_VLBA;
		mbps = (double)b;
		databytes = 2500*a*c*d;
		framebytes = 2520*a*c*d;
		framens = 1000*((20000*a*c*d)/b);
		framesperperiod = (50*b)/(a*c*d);
		if(r > 4)
		{
			decimation = e;
		}
		fanout = a;
		ntrack = a*c*d;
	}
	else if(strncasecmp(formatname, "MKIV1_", 6) == 0)
	{
		r = sscanf(formatname+6, "%d-%d-%d-%d/%d", &a, &b, &c, &d, &e);
		if(r < 4)
		{
			return 0;
		}
		F = MK5_FORMAT_MARK4;
		mbps = (double)b;
		databytes = 2500*a*c*d;
		framebytes = databytes;
		framens = 1000*((20000*a*c*d)/b);
		framesperperiod = (50*b)/(a*c*d);
		if(r > 4)
		{
			decimation = e;
		}
		fanout = a;
		ntrack = a*c*d;
	}
	else if(strncasecmp(formatname, "Mark5B-", 7) == 0)
	{
		r = sscanf(formatname+7, "%d-%d-%d/%d", &b, &c, &d, &e);
		if(r < 3)
		{
			return 0;
		}
		F = MK5_FORMAT_MARK5B;
		mbps = (double)b;
		databytes = 10000;
		framebytes = databytes+16;
		framens = 1000.0*(8.0*databytes/(double)b);
		framesperperiod = (b*1000000)/(8*databytes);
		if(r > 3)
		{
			decimation = e;
		}
	}
	else if(strncasecmp(formatname, "KVN5B-", 6) == 0)
	{
		r = sscanf(formatname+6, "%d-%d-%d/%d", &b, &c, &d, &e);
		if(r < 3)
		{
			return 0;
		}
		F = MK5_FORMAT_KVN5B;
		mbps = (double)b;
		databytes = 10000;
		framebytes = databytes+16;
		framens = 1000.0*(8.0*databytes/(double)b);
		framesperperiod = (b*1000000)/(8*databytes);
		if(r > 3)
		{
			decimation = e;
		}
	}
	else if(strncasecmp(formatname, "D2K-", 6) == 0)
	{
		r = sscanf(formatname+6, "%d-%d-%d/%d", &b, &c, &d, &e);
		if(r < 3)
		{
			return 0;
		}
		F = MK5_FORMAT_D2K;
		mbps = (double)b;
		databytes = 10000;
		framebytes = databytes+16;
		framens = 1000.0*(8.0*databytes/(double)b);
		framesperperiod = (b*1000000)/(8*databytes);
		if(r > 3)
		{
			decimation = e;
		}
	}
	/* for VDIF, the datasize per packet must be supplied as first numeric element:
	 * e.g., VDIF_4000-2048-4-2
	 */
	else if(strncasecmp(formatname, "VDIF_", 5) == 0)
	{
		if (strchr(formatname, newformatseparator) != NULL)
		{
			r = sscanf(formatname+5, "%d-%dm%d-%d-%d/%d", &a, &b, &s, &c, &d, &e);	
			if(r < 5)
			{
				return 0;
			}
			if(r > 5)
			{
				decimation = e;
			}
			F = MK5_FORMAT_VDIF;
			mbps = ((double)a*b*8)/s;
			databytes = a;
			framebytes = databytes + 32;
			framens = 1000.0*(8.0*databytes/(double)b);
			framesperperiod = b;
			alignmentseconds = s;
		}
		else
		{
			r = sscanf(formatname+5, "%d-%d-%d-%d/%d", &a, &b, &c, &d, &e);
			if(r < 4)
			{
				return 0;
			}
			F = MK5_FORMAT_VDIF;
			mbps = (double)b;
			databytes = a;
			framebytes = databytes + 32;
			framens = 1000.0*(8.0*databytes/(double)b);
			framesperperiod = (b*1000000)/(8*databytes);
			if(r > 4)
			{
				decimation = e;
			}
		}
	}
	/* for VDIF with legacy (16 byte) headers, a different name is used: */
	else if(strncasecmp(formatname, "VDIFL_", 6) == 0)
	{
		if (strchr(formatname, newformatseparator) != NULL)
		{
			r = sscanf(formatname+6, "%d-%dm%d-%d-%d/%d", &a, &b, &s, &c, &d, &e);
			if(r < 5)
			{
				return 0;
			}
			if(r > 5)
			{
				decimation = e;
			}
			F = MK5_FORMAT_VDIF;
			mbps = ((double)a*b*8)/s;
			databytes = a;
			framebytes = databytes + 16;
			framens = 1000.0*(8.0*databytes/(double)b);
			framesperperiod = b;
			alignmentseconds = s;
		}
		else
		{
			r = sscanf(formatname+6, "%d-%d-%d-%d/%d", &a, &b, &c, &d, &e);
			if(r < 4)
			{
				return 0;
			}
			F = MK5_FORMAT_VDIF;
			mbps = (double)b;
			databytes = a;
			framebytes = databytes + 16;
			framens = 1000.0*(8.0*databytes/(double)b);
			framesperperiod = (b*1000000)/(8*databytes);
			if(r > 4)
			{
				decimation = e;
			}
		}
	}
	/* for VDIF with complex sampling, a different name is used: */
	else if(strncasecmp(formatname, "VDIFC_", 6) == 0)
	{
		if (strchr(formatname, newformatseparator) != NULL)
		{
			r = sscanf(formatname+6, "%d-%dm%d-%d-%d/%d", &a, &b, &s, &c, &d, &e);
			if(r < 5)
			{
				return 0;
			}
			if(r > 5)
			{
				decimation = e;
			}
			F = MK5_FORMAT_VDIF;
			mbps = ((double)a*b*8)/s;
			databytes = a;
			framebytes = databytes + 32;
			framens = 1000.0*(8.0*databytes/(double)b);
			framesperperiod = b;
			alignmentseconds = s;
		}
		else
		{
			r = sscanf(formatname+6, "%d-%d-%d-%d/%d", &a, &b, &c, &d, &e);
			if(r < 4)
			{
				return 0;
			}
			F = MK5_FORMAT_VDIF;
			mbps = (double)b;
			databytes = a;
			framebytes = databytes + 32;
			framens = 1000.0*(8.0*databytes/(double)b);
			framesperperiod = (b*1000000)/(8*databytes);
			if(r > 4)
			{
				decimation = e;
			}
		}
	}
	else if(strncasecmp(formatname, "VDIF-", 5) == 0)
	{
		r = sscanf(formatname+5, "%d-%d-%d/%d", &b, &c, &d, &e);
		if(r < 4)
		{
			return 0;
		}
		F = MK5_FORMAT_VDIF;
		mbps = (double)b;
		databytes = 0;
		framebytes = databytes + 32;
		framens = 1000.0*(8.0*databytes/(double)b);
		framesperperiod = 0;
		if(r > 4)
		{
			decimation = e;
		}
	}
	/* for VDIF with legacy (16 byte) headers, a different name is used: */
	else if(strncasecmp(formatname, "VDIFL-", 6) == 0)
	{
		r = sscanf(formatname+6, "%d-%d-%d/%d", &b, &c, &d, &e);
		if(r < 4)
		{
			return 0;
		}
		F = MK5_FORMAT_VDIF;
		mbps = (double)b;
		databytes = 0;
		framebytes = databytes + 16;
		framens = 1000.0*(8.0*databytes/(double)b);
		framesperperiod = 0;
		if(r > 4)
		{
			decimation = e;
		}
	}
	/* for CODIF */
	else if(strncasecmp(formatname, "CODIF_", 6) == 0)
	{
		r = sscanf(formatname+6, "%d-%dm%d-%d-%d/%d", &a, &b, &s, &c, &d, &e);
		if(r < 5)
		{
			return 0;
		}
		if(r>5)
		{
			decimation = e;
		}
		F = MK5_FORMAT_CODIF;
		mbps = ((double)a*b*8)/s;
		databytes = a;
		framebytes = a+64;
		framens = 0;
		framesperperiod = b;
		alignmentseconds = s;
	}
	else if(strncasecmp(formatname, "CODIFC_", 7) == 0)
	{
		r = sscanf(formatname+7, "%d-%dm%d-%d-%d/%d", &a, &b, &s, &c, &d, &e);
		if(r < 5)
		{
			return 0;
		}
		if(r>5)
		{
			decimation = e;
		}
		F = MK5_FORMAT_CODIF;
		mbps = ((double)a*b*8)/s;
		databytes = a;
		framebytes = a+64;
		framens = 0;
		framesperperiod = b;
		alignmentseconds = s;
	}
	else if(strncasecmp(formatname, "VLBN1_", 6) == 0)
	{
		r = sscanf(formatname+6, "%d-%d-%d-%d/%d", &a, &b, &c, &d, &e);
		if(r < 4)
		{
			return 0;
		}
		F = MK5_FORMAT_VLBN;
		mbps = (double)b;
		databytes = 2500*a*c*d;
		framebytes = 2520*a*c*d;
		framens = 1000*((20000*a*c*d)/b);
		if(r > 4)
		{
			decimation = e;
		}
		fanout = a;
		ntrack = a*c*d;
		framesperperiod = (50*b)/(a*c*d);
	}
	else
	{
		return 0;
	}

	f = (struct mark5_format *)calloc(1, sizeof(struct mark5_format));
	f->format = F;
	f->fanout = fanout;
	f->Mbps = mbps;
	f->nchan = c;
	f->nbit = d;
	f->framebytes = framebytes;
	f->databytes = databytes;
	f->ntrack = ntrack;
	f->framens = framens;
	f->decimation = decimation;
	f->framesperperiod = framesperperiod;
	f->alignmentseconds = alignmentseconds;

	return f;
}

struct mark5_format *new_mark5_format_from_stream(struct mark5_stream_generic *s)
{
	struct mark5_stream *ms;
	struct mark5_format_generic *f;
	struct mark5_format *mf;
	int status, ntrack;
	size_t offset;
	int framesize, headersize;

	mark5_library_consistent();
	
	if(!s)
	{
		return 0;
	}
	
	mf = (struct mark5_format *)calloc(1, sizeof(struct mark5_format));
	
	ms = (struct mark5_stream *)calloc(1, sizeof(struct mark5_stream));
	
	if(set_stream(ms, s) < 0)
	{
		free(mf);
		free(ms);

		fprintf(m5stderr, "new_mark5_format_from_stream: Incomplete stream.\n");
		
		return 0;
	}

	status = s->init_stream(ms);
	if(status < 0)
	{
		free(mf);
		delete_mark5_stream(ms);
		fprintf(m5stderr, "new_mark5_format_from_stream: init_stream() failed\n");
		
		return 0;
	}

	/* Now go through known formats, looking for a match with the data */
	
	/* Mark5b */
	f = new_mark5_format_mark5b(0, 16, 2, 1);
	set_format(ms, f);
	status = mark5_format_init(ms);
	if(status < 0)
	{
		if(f->final_format)
		{
			f->final_format(ms);
		}
		free(f);
	}
	else
	{
		copy_format(ms, mf);
		mf->format = MK5_FORMAT_MARK5B;
		mf->ntrack = 0;
		delete_mark5_stream(ms);
		
		return mf;
	}
	/* Warning: there is no way to know if data is KVN5B vs. Mark5B format.  Don't search for KVN5B as that is less standard. */

	/* VLBA modes */
	for(ntrack = 8; ntrack <= 64; ntrack*=2)
	{
		f = new_mark5_format_vlba(0, ntrack, 1, 1, 1);
		set_format(ms, f);
		status = mark5_format_init(ms);
		if(status < 0)
		{
			if(f->final_format)
			{
				f->final_format(ms);
			}
			free(f);
		}
		else
		{
			copy_format(ms, mf);
			mf->format = MK5_FORMAT_VLBA;
			mf->ntrack = ntrack;
			mf->fanout = ntrack/(ms->nbit*ms->nchan);
			delete_mark5_stream(ms);
			
			return mf;
		}
	}
	
	/* Mark4 modes */
	for(ntrack = 8; ntrack <= 64; ntrack*=2)
	{
		f = new_mark5_format_mark4(0, ntrack, 1, 1, 1);
		set_format(ms, f);
		status = mark5_format_init(ms);
		if(status < 0)
		{
			if(f->final_format)
			{
				f->final_format(ms);
			}
			free(f);
		}
		else
		{
			copy_format(ms, mf);
			mf->format = MK5_FORMAT_MARK4;
			mf->ntrack = ntrack;
			mf->fanout = ntrack/(ms->nbit*ms->nchan);
			delete_mark5_stream(ms);
			
			return mf;
		}
	}

	/* VDIF */
	framesize = 0;
	if(find_vdif_frame(ms->datawindow, ms->datawindowsize, &offset, &framesize) >= 0)
	{
		ms->frameoffset = offset;
		f = new_mark5_format_vdif(
			1024, 	// Need to give it something.  This will be wrong in general and will have to be fixed by downstream software.
			get_vdif_chans_per_thread(ms->datawindow+offset),
			get_vdif_quantization_bits(ms->datawindow+offset),
			1,
			framesize-32,
			32,
			get_vdif_complex(ms->datawindow+offset) );

		set_format(ms, f);
		status = mark5_format_init(ms);
		if(status < 0)
		{
			if(f->final_format)
			{
				f->final_format(ms);
			}
			free(f);
		}
		else
		{
			copy_format(ms, mf);
			mf->format = MK5_FORMAT_VDIF;
			mf->ntrack = get_vdif_threads(ms->datawindow+offset, ms->datawindowsize-offset, framesize);
			delete_mark5_stream(ms);
			
			return mf;
		}
	}

	/* VDIFL */
	framesize = 0;
	if(find_vdifl_frame(ms->datawindow, ms->datawindowsize, &offset, &framesize) >= 0)
	{
		ms->frameoffset = offset;
		f = new_mark5_format_vdif(
			1024, 	// Need to give it something.  This will be wrong in general and will have to be fixed by downstream software.
			get_vdif_chans_per_thread(ms->datawindow+offset),
			get_vdif_quantization_bits(ms->datawindow+offset),
			1,
			framesize-16,
			16,
			get_vdif_complex(ms->datawindow+offset) );

		set_format(ms, f);
		status = mark5_format_init(ms);
		if(status < 0)
		{
			if(f->final_format)
			{
				f->final_format(ms);
			}
			free(f);
		}
		else
		{
			copy_format(ms, mf);
			mf->format = MK5_FORMAT_VDIF;
			mf->ntrack = get_vdif_threads(ms->datawindow+offset, ms->datawindowsize-offset, framesize);
			delete_mark5_stream(ms);
			
			return mf;
		}
	}

#ifdef HAVE_CODIFIO
	/* CODIF */
	framesize = 0;
	headersize = 0;
	if (find_codif_frame(ms->datawindow, ms->datawindowsize, &offset, &framesize, &headersize) >= 0)
	{
	    void * header;
	    header = (void*)ms->datawindow+offset;

	    ms->frameoffset = offset;
	    f = new_mark5_format_codif(
		  getCODIFFramesPerPeriod(header),
		  getCODIFPeriod(header),
		  getCODIFNumChannels(header),
		  getCODIFBitsPerSample(header),
		  1, /* decimation */
		  framesize-headersize,
		  headersize,
		  getCODIFComplex(header));

	    set_format(ms, f);
	    status = mark5_format_init(ms);
	    if(status < 0)
	      {
		  if(f->final_format)
		    {
			f->final_format(ms);
		    }
		  free(f);
	      }
	    else
	      {
		  copy_format(ms, mf);
		  mf->format = MK5_FORMAT_CODIF;
		  mf->ntrack = get_codif_threads(ms->datawindow+offset, ms->datawindowsize-offset, framesize);
		  delete_mark5_stream(ms);
		  return mf;
	      }
	}
#endif
	
	/* No match found */
	free(mf);
	free(ms);

	return 0;
}

void print_mark5_format(const struct mark5_format *mf)
{
	fprintf(m5stdout, "mark5_format : %p\n", mf);
	if(!mf)
	{
		return;
	}
	fprintf(m5stdout, "  format ID = %d\n", mf->format);
	fprintf(m5stdout, "  Mbps = %f\n", mf->Mbps);
	fprintf(m5stdout, "  Frames per period = %d\n", mf->framesperperiod);
	fprintf(m5stdout, "  Alignment seconds = %d\n", mf->alignmentseconds);
	fprintf(m5stdout, "  nchan = %d\n", mf->nchan);
	fprintf(m5stdout, "  nbit = %d\n", mf->nbit);
	fprintf(m5stdout, "  frameoffset = %d\n", mf->frameoffset);
	fprintf(m5stdout, "  framebytes = %d\n", mf->framebytes);
	fprintf(m5stdout, "  framens = %f\n", mf->framens);
	fprintf(m5stdout, "  mjd = %d sec = %d ns = %d\n", mf->mjd, mf->sec, mf->ns);
	if(mf->format == MK5_FORMAT_VDIF || mf->format == MK5_FORMAT_VDIFL || mf->format == MK5_FORMAT_CODIF)
	{
		fprintf(m5stdout, "  nthread = %d\n", mf->ntrack);
	}
	else
	{
		fprintf(m5stdout, "  ntrack = %d\n", mf->ntrack);
	}
	if(mf->format == MK5_FORMAT_VLBA || mf->format == MK5_FORMAT_MARK4 || mf->format == MK5_FORMAT_VLBN)
	{
		fprintf(m5stdout, "  fanout = %d\n", mf->fanout);
	}
	fprintf(m5stdout, "  decimation = %d\n", mf->decimation);
}

void delete_mark5_format(struct mark5_format *mf)
{
	if(mf)
	{
		free(mf);
	}
}

struct mark5_stream *new_mark5_stream(const struct mark5_stream_generic *s, const struct mark5_format_generic *f)
{
	struct mark5_stream *ms;
	int status;

	mark5_library_consistent();

	ms = (struct mark5_stream *)calloc(1, sizeof(struct mark5_stream));
       	if(!ms)
	{
		fprintf(m5stderr, "Error allocating memory for mark5_stream\n");
		
		return 0;
	}

	ms->format = MK5_FORMAT_UNKNOWN;
	
	if(set_stream(ms, s) < 0)
	{
		fprintf(m5stderr, "new_mark5_stream: Incomplete stream.\n");
		free(ms);
		
		return 0;
	}
	if(set_format(ms, f) < 0)
	{
		fprintf(m5stderr, "new_mark5_stream: Incomplete format.\n");
		free(ms);
		
		return 0;
	}

	ms->log2blankzonesize = 30;
	ms->blanker = blanker_none;

	status = s->init_stream(ms);
	if(status < 0)
	{
		fprintf(m5stderr, "new_mark5_format: init_stream(%s) failed\n", ms->formatname);
		delete_mark5_stream(ms);
		
		return 0;
	}

	status = mark5_format_init(ms);
	if(status < 0)
	{
		fprintf(m5stderr, "new_mark5_stream: init_format(%s) failed\n", ms->formatname);
		delete_mark5_stream(ms);
		
		return 0;
	}

	ms->blanker(ms);

	return ms;
}

struct mark5_stream *new_mark5_stream_absorb(struct mark5_stream_generic *s, struct mark5_format_generic *f)
{
	struct mark5_stream *ms;
	int failed = 0;

	mark5_library_consistent();

	if(!s)
	{
		failed = 1;
	}
	if(!f)
	{
		failed = 1;
	}
	if(failed)
	{
		ms = 0;
	}
	else
	{
		ms = new_mark5_stream(s, f);
	}
	if(s)
	{
		delete_mark5_stream_generic(s);
	}
	if(f)
	{
		delete_mark5_format_generic(f);
	}

	return ms;
}

void delete_mark5_stream(struct mark5_stream *ms)
{
	if(ms)
	{
#if 0
		/* Note: folks calling delete_mark5_stream might want to first check to see
		 * how many validations were reported and react accordingly
		 */
		if(ms->nvalidatefail > 0)
		{
			fprintf(m5stderr, "Warning: %d validation failures on %s framenum=%lld -> bytepos=%lld\n",
				ms->nvalidatefail, ms->streamname, ms->framenum, ms->framenum*ms->framebytes);
		}
#endif
		if(ms->final_stream)
		{
			ms->final_stream(ms);
		}
		if(ms->final_format)
		{
			ms->final_format(ms);
		}
		free(ms);
	}
}

int mark5_stream_resync(struct mark5_stream *ms)
{
	if(!ms)
	{
		return -1;
	}

	return ms->resync(ms);
}

int mark5_stream_get_frame_time(struct mark5_stream *ms, 
	int *mjd, int *sec, double *ns)
{
	if(!ms)
	{
		return -1;
	}

	return ms->gettime(ms, mjd, sec, ns);
}

int mark5_stream_get_sample_time(struct mark5_stream *ms, 
	int *mjd, int *sec, double *ns)
{
	int status;

	if(!ms)
	{
		return -1;
	}
	status = ms->gettime(ms, mjd, sec, ns);

	if(status < 0)
	{
		return status;
	}

	if(ns)
	{
		*ns += (ms->framens/ms->databytes)*ms->readposition;
	}

	return 0;
}

int mark5_stream_snprint(char *str, int maxLength, const struct mark5_stream *ms)
{
      if (!ms)
      {
        return -1;
      }
      
        //snprintf(str, maxLength, "Mark5 stream: %p stream=%s format=%s = %d startMJD=%d startSecond=%05d.%09d " , ms->streamname, ms->formatname,ms->format, ms->mjd, ms->sec, ms->ns);
        snprintf(str, maxLength, "Mark5 stream: %p stream=%s format=%s = %d ", ms, ms->streamname,ms->formatname, ms->format );

        if(ms->mjd >= 0)
        {
            snprintf(str +strlen(str), maxLength, "start mjd/sec = %d %05d.%09d frame duration = %8.2f ns framenum = %lld", ms->mjd, ms->sec, ms->ns,ms->framens,  ms->framenum);
        }
        if(ms->samprate > 0)
        {
            snprintf(str +strlen(str), maxLength," sample rate = %lld Hz", (long long)ms->samprate );
        }
        snprintf(str +strlen(str), maxLength, " offset = %d framebytes = %d bytes", ms->frameoffset,ms->framebytes);
        snprintf(str +strlen(str), maxLength, " datasize = %d bytes", ms->databytes);
        snprintf(str +strlen(str), maxLength, " frames per period = %d", ms->framesperperiod);
        snprintf(str +strlen(str), maxLength, " bits = %d", ms->nbit);
        snprintf(str +strlen(str), maxLength, " alignment seconds = %d", ms->alignmentseconds);
        
        snprintf(str +strlen(str), maxLength, " sample granularity = %d", ms->samplegranularity);
        snprintf(str +strlen(str), maxLength, " frame granularity = %d", ms->framegranularity);
        snprintf(str +strlen(str), maxLength, "  gframens = %d", ms->gframens);
        snprintf(str +strlen(str), maxLength, " payload offset = %d", ms->payloadoffset);
        snprintf(str +strlen(str), maxLength, " read position = %d", ms->readposition);
	if(ms->datawindow)
	{
            snprintf(str +strlen(str), maxLength, "  data window size = %lld bytes", ms->datawindowsize);
	}
      return 0;
}

/**
 * Deprecated. Use mark5_stream_snprint and report messages outside of the mark5 access library
 **/
int mark5_stream_print(const struct mark5_stream *ms)
{
	fprintf(m5stdout, "Mark5 stream: %p\n", ms);
	if(!ms)
	{
		return -1;
	}
	fprintf(m5stdout, "  stream = %s\n", ms->streamname);
	fprintf(m5stdout, "  format = %s = %d\n", ms->formatname, ms->format);
	if(ms->mjd >= 0)
	{
		fprintf(m5stdout, "  start mjd/sec = %d %05d.%09d\n", 
			ms->mjd, ms->sec, ms->ns);
		fprintf(m5stdout, "  frame duration = %8.2f ns\n", ms->framens);
		fprintf(m5stdout, "  framenum = %lld\n", ms->framenum);
	}
	if(ms->samprate > 0)
	{
		fprintf(m5stdout, "  sample rate = %lld Hz\n", (long long)ms->samprate);
	}
	fprintf(m5stdout, "  offset = %d\n", ms->frameoffset);
	fprintf(m5stdout, "  framebytes = %d bytes\n", ms->framebytes);
	fprintf(m5stdout, "  datasize = %d bytes\n", ms->databytes);
	fprintf(m5stdout, "  frames per period = %d\n", ms->framesperperiod);
	fprintf(m5stdout, "  bits = %d\n", ms->nbit);
	fprintf(m5stdout, "  alignment seconds = %d\n", ms->alignmentseconds);
	fprintf(m5stdout, "  sample granularity = %d\n", ms->samplegranularity);
	fprintf(m5stdout, "  frame granularity = %d\n", ms->framegranularity);
	fprintf(m5stdout, "  gframens = %d\n", ms->gframens);
	fprintf(m5stdout, "  payload offset = %d\n", ms->payloadoffset);
	fprintf(m5stdout, "  read position = %d\n", ms->readposition);
	if(ms->datawindow)
	{
		fprintf(m5stdout, "  data window size = %lld bytes\n", ms->datawindowsize);
	}

	return 0;
}

int mark5_stream_seek(struct mark5_stream *ms, int mjd, int sec, double ns)
{
	int status;
	double jumpns;
	long long n;

	if(!ms)
	{
		return -1;
	}
	if(ms->seek)
	{
		jumpns = 86400000000000LL*(mjd - ms->mjd) + 1000000000LL*(sec - ms->sec) + (ns - ms->ns);

		if(jumpns < 0) /* before start of stream */
		{
			return -1;
		}
		n = jumpns / ms->framens - 1;

		//status = ms->seek(ms, n + ms->framenum);
		status = ms->seek(ms, n);

		if(status < 0)
		{
			return -1;
		}

		ms->framenum = n;

		mark5_stream_next_frame(ms);

		/* FIXME: validate here? */

		return 0;
	}
	else
	{
		return -1;
	}
}

int mark5_stream_copy(struct mark5_stream *ms, int nbytes, char *data)
{
	int nleft, q;

	if(!ms)
	{
		return -1;
	}
	if(ms->readposition < 0)
	{
		return -1;
	}

	int bitspersample = ms->nbit;
	if (ms->iscomplex) bitspersample *= 2;

	q = ms->samplegranularity*ms->nchan*bitspersample*ms->decimation/8; 
	if (q==0) q=1;  // WALTER IS THIS RIGHT???
	if(nbytes % q != 0)
	{
		return -1;
	}

	while(1)
	{
		nleft = ms->databytes - ms->readposition;
		if(nleft > nbytes)
		{
			memcpy(data, ms->payload+ms->readposition, nbytes);
			return 0;
		}
		memcpy(data, ms->payload+ms->readposition, nleft);
		if(ms->next(ms) < 0)
		{
			if(nbytes == nleft)
			{
				return 0;
			}
			else
			{
				return -1;
			}
		}
		nbytes -= nleft;
		data += nleft;
	}

	return 0;
}

int mark5_stream_set_blanker(struct mark5_stream *ms, 
	enum Mark5Blanker blanker)
{
	switch(blanker)
	{
	case MK5_BLANKER_NONE:
		ms->blanker = blanker_none;
		break;
	case MK5_BLANKER_MARK5:
		ms->blanker = blanker_mark5;
		break;
	default:
		return -1;
	}

	return ms->blanker(ms);
}

/*********************** data decode routines **********************/


int mark5_stream_decode(struct mark5_stream *ms, int nsamp, float **data)
{
	if(!ms)
	{
		return -1;
	}
	if(ms->readposition < 0)
	{
		return -1;
	}

	/* In order to ensure expected behavior, must unpack multiples of
	 * ms->samplegranularity
	 */
	if(nsamp % ms->samplegranularity != 0)
	{
		return -1;
	}
	return ms->decode(ms, nsamp, data);
}

int mark5_stream_decode_double(struct mark5_stream *ms, int nsamp, double **data)
{
	double *d;
	const float *f;
	int c;
	int i;
	int r;
	
	r = mark5_stream_decode(ms, nsamp, (float **)data);
	if(r < 0) 
	{
		return r;
	}

	/* convert in place */
	for(c = 0; c < ms->nchan; ++c)
	{
		d = data[c]+nsamp;
		f = ((const float *)(data[c]))+nsamp;
		for(i = 0; i < nsamp; ++i)
		{
			--d;
			--f;
			*d = *f;
		}
	}

	return r;
}

int mark5_stream_decode_complex(struct mark5_stream *ms, int nsamp, mark5_float_complex **data)
{
	mark5_float_complex *fc;
	const float *f;
	int c, i, r;
	
	if(!ms) 
	{
		return -1;
	} 

	if(ms->iscomplex) 
	{
		if(ms->readposition<0)
		{
			return -1 ;
		}
		
		return ms->complex_decode(ms, nsamp, data);
	} 
	else
	{
		r = mark5_stream_decode(ms, nsamp, (float **)data);
		if(r >= 0) 
		{

			/* convert in place */
			for(c = 0; c < ms->nchan; ++c)
			{
				fc = data[c]+nsamp;
				f = ((const float *)(data[c]))+nsamp;
				for(i = 0; i < nsamp; ++i)
				{
					--fc;
					--f;
					*fc= *f;
				}
			}
		}
		
		return r;
	}
}

int mark5_stream_decode_double_complex(struct mark5_stream *ms, int nsamp, mark5_double_complex **data)
{
	mark5_double_complex *dc;
	const float *f;
	const mark5_float_complex *fc;
	int c, i, r;

	if (ms->iscomplex) 
	{
		r = mark5_stream_decode_complex(ms, nsamp, (mark5_float_complex**)data);
		if(r < 0) 
		{
			return r;
		}

		/* convert in place */
		for(c = 0; c < ms->nchan; ++c)
		{
			dc = data[c]+nsamp;
			fc = ((const mark5_float_complex *)data[c])+nsamp;
			for(i = 0; i < nsamp; ++i)
			{
				--dc;
				--fc;
				*dc = *fc;
			}
		}

	}
	else
	{
		r = mark5_stream_decode(ms, nsamp, (float **)data);
		if(r < 0) 
		{
			return r;
		}

		/* convert in place */
		for(c = 0; c < ms->nchan; ++c)
		{
			dc = data[c]+nsamp;
			f = ((const float *)(data[c]))+nsamp;
			for(i = 0; i < nsamp; ++i)
			{
				--dc;
				--f;
				*dc = *f;
			}
		}
	}

	return r;
}


void delete_mark5_stream_generic(struct mark5_stream_generic *s)
{
	if(s)
	{
		if(s->inputdata)
		{
			free(s->inputdata);
			s->inputdata = 0;
		}
		free(s);
	}
}

void delete_mark5_format_generic(struct mark5_format_generic *f)
{
	if(f)
	{
		if(f->formatdata)
		{
			free(f->formatdata);
			f->formatdata = 0;
		}
		free(f);
	}
}

void mark5_format_generic_print(const struct mark5_format_generic *f)
{
	printf("mark5_format_generic [%p]\n", f);
	if(f)
	{
		printf("  formatdatasize = %d\n", f->formatdatasize);
		printf("  Mbps = %f\n", f->Mbps);
		printf("  Frames per period = %d\n", f->framesperperiod);
		printf("  Alignment seconds = %d\n", f->alignmentseconds);
		printf("  nchan = %d\n", f->nchan);
		printf("  nbit = %d\n", f->nbit);
		printf("  decimation = %d\n", f->decimation);
	}
}

int mark5_stream_count_high_states(struct mark5_stream *ms, int nsamp,
	unsigned int *highstates)
{
	if(!ms)
	{
		return -1;
	}
	if(ms->readposition < 0)
	{
		return -1;
	}

	if(!ms->count)
	{
		return 0;
	}

	/* In order to ensure expected behavior, must unpack multiples of
	 * ms->samplegranularity
	 */
	if(nsamp % ms->samplegranularity != 0)
	{
		return -1;
	}

	return ms->count(ms, nsamp, highstates);
}

/* Returns: -1 on error
 *           0 on success with no change
 *          >0 on success with change
 */
int mark5_stream_fix_mjd(struct mark5_stream *ms, int refmjd)
{
	if(!ms)
	{
		return -1;
	}
	if(ms->fixmjd)
	{
		return ms->fixmjd(ms, refmjd);
	}
	else
	{
		return 0;
	}
}



#if 0
/* series expansion from wikipedia */
static double inverseerfbyseries(double x)
{
	return (1.0/M_2_SQRTPI)*
	(
		x +
		(M_PI/12.0)*x*x*x +
		(7.0*M_PI*M_PI/480.0)*x*x*x*x*x +
		(127.0*M_PI*M_PI*M_PI/40320.0)*x*x*x*x*x*x*x +
		(4369.0*M_PI*M_PI*M_PI*M_PI/5806080.0)*x*x*x*x*x*x*x*x*x +
		(34807.0*M_PI*M_PI*M_PI*M_PI*M_PI/182476800.0)*x*x*x*x*x*x*x*x*x*x*x  /* + ... */
	);
}
#endif

/* Approximation by Sergei Winitzki: 
 * http://homepages.physik.uni-muenchen.de/~Winitzki/erf-approx.pdf*/
static double inverseerf(double x)
{
	const double a = 8.0/(3.0*M_PI) * (M_PI-3.0)/(4.0-M_PI);
	double b, c;

	if(x == 0.0)
	{
		return 0.0;
	}
	else if(x < 0.0)
	{
		return -inverseerf(-x);
	}
	else
	{
		c = log(1.0-x*x);
		b = 2.0/(M_PI*a) + 0.5*c;

		return sqrt(-b + sqrt(b*b - c/a) );
	}
}

/* send into this function <v^2>, where |v| = 1 or OPTIMAL_2BIT_HIGH */
double correct_2bit_power(double x)
{
	const double a = OPTIMAL_2BIT_HIGH*OPTIMAL_2BIT_HIGH;

	if(x >= 1.0 && x <= a)
	{
		double f;
		
		f = inverseerf( (a-x)/(a-1.0) );

		return 0.5/(f*f);
	}
	else
	{
		return -1.0;
	}
}

/* input:  fraction of counts in a high state.
   output: total power, corrected for 2bit quantization, normalized to about 1 for optimal state counts */
double high_state_fraction_to_power(double x)
{
	if(x > 0.0 && x <= 1.0)
	{
		double f;
		
		f = inverseerf(1.0-x);

		return 0.5/(f*f);
	}
	else
	{
		return -1.0;
	}
}

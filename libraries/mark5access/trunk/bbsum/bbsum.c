/***************************************************************************
 *   Copyright (C) 2007-2011 by Walter Brisken                             *
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

#if defined(__linux__)
#define _FILE_OFFSET_BITS 64
#endif

#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <glob.h>
#include <sys/time.h>
#include "../mark5access/mark5_stream.h"

#define SEC_DAY         86400.0             /* seconds in a mean solar day */
#define MUSEC_DAY       86400000000.0       /* mus in a mean solar day */
#define MJD_UNIX0       40587.0             /* MJD at beginning of unix time */



const char program[] = "bbsum";
const char author[]  = "Walter Brisken";
const char version[] = "1.0";
const char verdate[] = "20111008";

int usage(const char *pgm)
{
	printf("\n");

	printf("%s ver. %s   %s  %s\n\n", program, version, author, verdate);
	printf("A program to summarize the contents of a baseband file.\n\n");
	printf("Usage : %s <file> <dataformat> <n> [<offset>]\n\n", pgm);
	printf("  <file> is the name of the input file\n\n");
	printf("  <dataformat> should be of the form: "
		"<FORMAT>-<Mbps>-<nchan>-<nbit>, e.g.:\n");
	printf("    VLBA1_2-256-8-2\n");
	printf("    MKIV1_4-128-2-1\n");
	printf("    Mark5B-512-16-2\n\n");
	printf("  <n> is the number of samples per channel to decode\n\n");
	printf("  <offset> is number of bytes into file to start decoding\n\n");

	return 0;
}

int summarize(const char *filename, int refmjd)
{
	struct mark5_format *mf;
	int n;
	int err;
	struct stat fileStatus;
	int in;
	double size, dur;
	double start;
	
	in = open(filename, O_RDONLY);
	if(!in)
	{
		printf("%s cannot be opened.\n", filename);
		
		return 0;
	}
	
	err = fstat(in, &fileStatus);
	if(err < 0)
	{
		fprintf(stderr, "fstat() failed.\n");
		close(in);
		
		return 0;
	}
	close(in);

	size = fileStatus.st_size;
	
	mf = new_mark5_format_from_stream(
	 	new_mark5_stream_file(filename, 0) );

	if(!mf)
	{
		printf("%s unknown\n", filename);

		return 0;
	}

	/* fix mjd */
	if(refmjd > 0)
	{
		switch(mf->format)
		{
		case MK5_FORMAT_VLBA:
		case MK5_FORMAT_MARK5B:
			n = (refmjd - mf->mjd + 500) / 1000;
			mf->mjd += n*1000;
			break;
		case MK5_FORMAT_MARK4:
#if 0
			/* for now, assume Mark4 goes away before 2010 */
			mjd = mf->mjd;
			decade = (refmjd - mf->mjd + 1826)/3652.5;
			mf->mjd += 3652.5*decade;
			if( (decade % 2) && ((mjd-51910) % 1461 > 730) )
			{
				mf->mjd++;
			}
#endif
			break;
		case MK5_FORMAT_K5:
			mf->mjd = refmjd;
			break;
		default:
			break;
		}
	}

	dur = size*mf->framens/(mf->framebytes*1.0e9);

	start = mf->mjd + (mf->sec + mf->ns*1.0e-9)/86400.0;

	delete_mark5_format(mf);

	printf("%s %14.8f %7.3f\n", filename, start, dur);

	return 0;
}

int main(int argc, char **argv)
{
	int mjd, f;
	glob_t files;
	char pattern[256] = "";
	struct timeval t;

	if(argc < 2)
	{
		return usage(argv[0]);
	}
	if(strcmp(argv[1], "-h") == 0 || strcmp(argv[1], "--help") == 0)
	{
		return usage(argv[0]);
	}

	if(argc > 2)
	{
		mjd = atof(argv[2]);
	}
	else
	{
		gettimeofday(&t, 0);
		mjd = MJD_UNIX0 + t.tv_sec/SEC_DAY + t.tv_usec/MUSEC_DAY;
	}

	sprintf(pattern, "%s/*", argv[1]);

	glob(pattern, 0, 0, &files);
	if(files.gl_pathc == 0)
	{
		fprintf(stderr, "No files found\n");
		return 0;
	}

	for(f = 0; f < files.gl_pathc; f++)
	{
		summarize(files.gl_pathv[f], (int)mjd);
	}
	
	return 0;
}


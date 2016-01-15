/***************************************************************************
 *   Copyright (C) 2011-2016 by Helge Rottmann                             *
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

#define _LARGEFILE64_SOURCE 1
#define _FILE_OFFSET_BITS 64

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>
#include <signal.h>
#include <dirent.h>
#include <string.h>
#include <time.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "../mark5access/mark5_stream.h"

const char program[] = "directory2filelist";
const char author[]  = "Helge Rottmann";
const char version[] = "1.4";
const char verdate[] = "2015 May 21";

const int MJD_UNIX0 = 40587;	// MJD at beginning of unix time

static int64_t eofReadLength = (80000+40000);

int die = 0;

typedef void (*sighandler_t)(int);

sighandler_t oldsiginthand;

void siginthand(int j)
{
	fprintf(stderr, "\nBeing killed.\n\n");
	die = 1;

	signal(SIGINT, oldsiginthand);
}

int usage(const char *pgm, int defaultMJD)
{
	printf("\n");

	printf("%s ver. %s   %s  %s\n\n", program, version, author, verdate);
	printf("Creates a filelist to be used by vex2difx using all the files present in the given directory\n");
	printf("Can handle VLBA, Mark3/4, and Mark5B formats using the\nmark5access library.\n\n");
	printf("Usage : %s <directory> <dataformat> [<refMJD>]\n\n", pgm);
	printf("  <directory> is the name of the input directory\n\n");
	printf("  <dataformat> should be of the form: "
		"<FORMAT>-<Mbps>-<nchan>-<nbit>, e.g.:\n");
	printf("    VLBA1_2-256-8-2\n");
	printf("    MKIV1_4-128-2-1\n");
	printf("    Mark5B-512-16-2\n\n");
	printf("  [<refMJD>]  changes the reference MJD (default is %d)\n\n", defaultMJD);

	return 0;
}

int is_reasonable_timediff(double startmjd, double stopmjd)
{
	int startday = (int)startmjd;
	int stopday = (int)stopmjd;
	return ( (startmjd <= stopmjd) && ((stopday-startday) <= 1) );
}

struct mark5_stream *openmk5(const char *filename, const char *formatname, int64_t *offset)
{
	struct mark5_stream *ms;
	int64_t offset0 = *offset;
	char did_fail = 0;
	while (1) {
                ms = new_mark5_stream_absorb(
                        new_mark5_stream_file(filename, *offset),
                        new_mark5_format_generic_from_string(formatname) );

                if(!ms)
                {
			if (*offset < (offset0 + 32*43500L))
			{
                        	fprintf(stderr, "problem at initial decode of %s at offset %"PRId64", trying new offset\n", filename, *offset);
				*offset += 43500;
				did_fail = 1;
				continue;
			}
			else
			{
                        	fprintf(stderr, "problem opening %s\n", filename);
	                        break;
			}
                }
                if(0 == (ms->samprate % 1000) && ms->samprate>0)
                {
			if (did_fail)
			{
				fprintf(stderr, "decode %s at offset %"PRId64" succeeded\n\n", filename, *offset);
			}
                        break;
                }
                fprintf(stderr, "File offset %"PRId64": decoded suspect sample rate %"PRId64", trying new offset\n", *offset, ms->samprate);
                delete_mark5_stream(ms);
                (*offset) += 43500;
	}
	return ms;
}

int verify(const char *filename, const char *formatname, int refMJD)
{
	struct mark5_stream *ms;
	int i;
	int status = 0, corrupt = 0;
	int mjd, sec;
        double ns, startmjd, stopmjd = 0.0, eofmjd = 0.0;
	int64_t validoffset = 0;

	// open with seeking to first valid-looking frame pair
	ms = openmk5(filename, formatname, &validoffset);
	if(!ms)
	{
		fprintf(stderr, "problem opening %s\n", filename);
		return 0;
	}

	// resolve any day ambiguities
        mark5_stream_fix_mjd(ms, refMJD);

	//mark5_stream_print(ms);

	mark5_stream_get_frame_time(ms, &mjd, &sec, &ns);
	startmjd = mjd + (sec + ns/1e9) / 86400.0;
	stopmjd = startmjd;
	eofReadLength = ms->datawindowsize;

	struct stat st;
	stat(filename, &st);
	int64_t length = st.st_size;

	int64_t numFrames = length / ms->framebytes;
	double jumpNs = numFrames * ms->framens;

	double skipNs = ns + jumpNs;


	long endSec = skipNs / 1e9;
	/* double endNs = skipNs - endSec*1e9; */	/* FIXME: variable set but not used */

	status = mark5_stream_seek(ms, mjd, sec+endSec-1, ns);

	for(i = 0; ; i++)
	{
		if(die)
		{
			break;
		}
	
		if(status < 0)
		{
			break;
		}

		if(ms->nvalidatefail > 1024)
		{
			fprintf(stderr, "Warning: too many frame validation failures sequentially scanning file %s\n", filename);
			corrupt = 1;
			break;
		}

		if(i%1 == 0)
		{
			int mjd, sec;
			double ns;

			mark5_stream_get_frame_time(ms, &mjd, &sec, &ns);

			if((mjd - (int)stopmjd) >= 2 || (mjd < (int)stopmjd))
			{
#ifdef DEBUG
				int gday = (int)stopmjd;
				double gsec = (stopmjd - (double)gday) * 24.0*3600.0;

				fprintf(stderr, "Jump in MJD day (%d/%.4fs (mjd=%.6f) -> %d/%.4fs), "
						"trying to resync\n", 
						gday, gsec, stopmjd, mjd, sec+ns*1e-9);
#endif

				status = mark5_stream_resync(ms);
				status = mark5_stream_next_frame(ms);
				corrupt = 1;

				continue;
			}
			else
			{
				stopmjd = mjd + (sec + ns/1e9) / 86400.0;
			}
		}

		status = mark5_stream_next_frame(ms);

	}

	delete_mark5_stream(ms);

	// open short before EOF, with seeking to first valid-looking frame pair
        // note: this may generate many "Shortening datawindowsize" warnings
	validoffset = length - eofReadLength;
	ms = openmk5(filename, formatname, &validoffset);
	if(!ms)
	{
		fprintf(stderr, "problem opening at tail of %s\n", filename);
		return 0;
	}

	// resolve any day ambiguities
        mark5_stream_fix_mjd(ms, refMJD);

	mark5_stream_get_frame_time(ms, &mjd, &sec, &ns);
	eofmjd = mjd + (sec + ns/1e9) / 86400.0;

	delete_mark5_stream(ms);

#ifdef DEBUG
	fprintf(stderr, "Timing: MJD start=%lf stop=%lf eof=%lf in %s\n", startmjd, stopmjd, eofmjd, filename);
#endif

	// choose most plausible scan data stop time in presence of frame or sync errors

	// both Stop and EOF MJD corrupt?
	if(!is_reasonable_timediff(startmjd, stopmjd) && !is_reasonable_timediff(startmjd, eofmjd))
	{
		stopmjd = startmjd;
		corrupt = 1;
	}
	// both Stop and EOF MJD look good?
	else if(is_reasonable_timediff(startmjd, stopmjd) && is_reasonable_timediff(startmjd, eofmjd))
	{
		stopmjd = fmax(stopmjd, eofmjd);
	}
	// either Stop or EOF MJD is corrupt
	else
	{
		if(is_reasonable_timediff(startmjd,eofmjd))
		{
			stopmjd = eofmjd;
		}
		corrupt = 1;
	}

	if(corrupt)
	{
		fprintf(stderr, "Warning: found corrupt data frames in file %s\n", filename);
	}

	fprintf(stdout, "%s %lf %lf\n", filename, startmjd, stopmjd);

	return 0;
}

int main(int argc, char **argv)
{
	const int MaxFilenameLength = 2048;
	struct dirent *ep;
	char filename[MaxFilenameLength];
	int refMJD = 57000;
	char *dir;
	char *fmt;
	int defaultMJD;

	oldsiginthand = signal(SIGINT, siginthand);

	defaultMJD = time(0)/86400 + MJD_UNIX0;

	// redirect mark5access STDOUT->STDERR
	mark5_library_setoption(M5A_OPT_STDOUTFD, (void*)stderr);

	if(argc != 3 && argc != 4)
	{
		usage(argv[0], defaultMJD);
	
		return EXIT_FAILURE;
	}

	dir = argv[1];
	fmt = argv[2];
	refMJD = (argc==4) ? atoi(argv[3]) : defaultMJD;

	DIR *dp = opendir(dir);
	if (dp != NULL)
	{
		while ( (ep = readdir (dp)) && !die )
		{
			if ((strcmp(ep->d_name, ".") != 0) && (strcmp(ep->d_name, "..") != 0))
			{
				int p;

				p = snprintf(filename, MaxFilenameLength, "%s/%s", dir, ep->d_name);
				if(p >= MaxFilenameLength)
				{
					fprintf(stderr, "ERROR: file name is too long: %s\n", ep->d_name);
				}
				verify(filename, fmt, refMJD);
			}
		}
	}
	else
	{
		fprintf (stderr,"ERROR: Directory %s does not exist\n", dir);
		exit(1);
	}
	(void) closedir (dp);

	return EXIT_SUCCESS;
}


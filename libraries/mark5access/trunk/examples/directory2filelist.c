/***************************************************************************
 *   Copyright (C) 2011 by Helge Rottmann                                  *
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

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <dirent.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "../mark5access/mark5_stream.h"

const char program[] = "directory2filelist";
const char author[]  = "Helge Rottmann";
const char version[] = "1.2";
const char verdate[] = "2011 Apr 06";

const static int DEFAULT_MJD = 57000;
static long DEFAULT_EOF_READLEN = (80000+40000);

int die = 0;

typedef void (*sighandler_t)(int);

sighandler_t oldsiginthand;

void siginthand(int j)
{
	fprintf(stderr, "\nBeing killed.\n\n");
	die = 1;

	signal(SIGINT, oldsiginthand);
}

int usage(const char *pgm)
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
	printf("  [<refMJD>]  changes the reference MJD (default is %d)\n\n", DEFAULT_MJD);

	return 0;
}

int is_reasonable_timediff(double startmjd, double stopmjd)
{
	int startday = (int)startmjd;
	int stopday = (int)stopmjd;
	return ( (startmjd <= stopmjd) && ((stopday == startday) || (stopday == (startday+1))) );
}

struct mark5_stream *openmk5(const char *filename, const char *formatname, long *offset)
{
	struct mark5_stream *ms;
	while (1) {
                ms = new_mark5_stream_absorb(
                        new_mark5_stream_file(filename, *offset),
                        new_mark5_format_generic_from_string(formatname) );

                if(!ms)
                {
                        fprintf(stderr, "problem opening %s\n", filename);
                        break;
                }
                if (0 == (ms->samprate % 1000) && ms->samprate>0)
                {
                        break;
                }
                fprintf(stderr, "File offset %ld: decoded suspect sample rate %d, trying new offset\n", *offset, ms->samprate);
                delete_mark5_stream(ms);
                (*offset) += 43500;
	}
	return ms;
}

int verify(const char *filename, const char *formatname, int refMJD)
{
	struct mark5_stream *ms;
	int i;
	int  status = 0;
	int mjd, sec;
        double ns, startmjd, stopmjd=0.0, eofmjd=0.0;
	long validoffset = 0;

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
	DEFAULT_EOF_READLEN = ms->datawindowsize;

	FILE *fp = fopen(filename, "rb");

	fseek (fp,0L,SEEK_END);
        long length = ftell(fp);
        fclose (fp);


	long numFrames = length / ms->framebytes;
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
		if(i%1 == 0)
		{
			int mjd, sec;
			double ns;
			mark5_stream_get_frame_time(ms, &mjd, &sec, &ns);
			stopmjd = mjd + (sec + ns/1e9) / 86400.0;

		}
		status = mark5_stream_next_frame(ms); 

		if(ms->nvalidatefail > 20)
		{
			break;
		}
	}

	delete_mark5_stream(ms);

	// open short before EOF, with seeking to first valid-looking frame pair
        // note: this may generate many "Shortening datawindowsize" warnings
	validoffset = length - DEFAULT_EOF_READLEN;
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

	// check that start and stop time are quite valid
	if (!is_reasonable_timediff(startmjd, stopmjd))
	{
		fprintf (stderr, "Error: timestamps suspicious (either stop(%lf)<=start(%lf) or stop>>start) in file %s\n", stopmjd, startmjd, filename);
	}
	if ((int)eofmjd != (int)stopmjd)
	{
		fprintf (stderr, "Error: eof day (%d) != stop day (%d) in file %s\n", (int)eofmjd, (int)stopmjd, filename);
	}

	// output for debug/verbose
	//fprintf (stderr, "%s %lf %lf %lf\n", filename, startmjd, stopmjd, eofmjd);

	// choose most plausible ending time
	if (is_reasonable_timediff(startmjd, stopmjd))
	{
		fprintf (stdout, "%s %lf %lf\n", filename, startmjd, stopmjd);
	}
	else if (is_reasonable_timediff(startmjd, eofmjd))
	{
		fprintf (stdout, "%s %lf %lf\n", filename, startmjd, eofmjd);
	}
	else
	{
		if (eofmjd>startmjd)
		{
			fprintf (stdout, "%s %lf %lf\n", filename, startmjd, eofmjd);
		}
		else if(stopmjd>startmjd)
		{
			fprintf (stdout, "%s %lf %lf\n", filename, startmjd, stopmjd);
		}
		else
		{
			fprintf (stdout, "%s\n", filename);
		}
	}

	return 0;
}

int main(int argc, char **argv)
{
	struct dirent *ep;
	char filename[2048];
	int refMJD = 57000;
	char *dir;
	char *fmt;

	oldsiginthand = signal(SIGINT, siginthand);

	#if 0 // redirect mark5access STDOUT->STDERR; default disabled, function does not exist in older libraries
	mark5_library_setoption(M5A_OPT_STDOUTFD, (void*)stderr);
	#endif

	if(argc != 3 && argc != 4)
	{
		usage(argv[0]);
	
		return EXIT_FAILURE;
	}

	dir = argv[1];
	fmt = argv[2];
	refMJD = (argc==4) ? atoi(argv[3]) : DEFAULT_MJD;

	DIR *dp = opendir(dir);
	if (dp != NULL)
	{
		while ( (ep = readdir (dp)) && !die )
		{
			if ((strcmp(ep->d_name, ".") != 0) && (strcmp(ep->d_name, "..") != 0))
			{
				strcpy(filename, dir);
				strcat(filename, ep->d_name);	
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


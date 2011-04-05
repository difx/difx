/***************************************************************************
 *   Copyright (C) 2007-2010 by Walter Brisken                             *
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
// $Id: m5test.c 2403 2010-08-18 20:58:21Z WalterBrisken $
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/mark5access/trunk/mark5access/mark5_stream.c $
// $LastChangedRevision: 2403 $
// $Author: WalterBrisken $
// $LastChangedDate: 2010-08-18 22:58:21 +0200 (Mi, 18 Aug 2010) $
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
const char version[] = "1.1";
const char verdate[] = "2011 Apr 04";


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
	printf("Usage : %s <directory> <dataformat> \n\n", pgm);
	printf("  <directory> is the name of the input directory\n\n");
	printf("  <dataformat> should be of the form: "
		"<FORMAT>-<Mbps>-<nchan>-<nbit>, e.g.:\n");
	printf("    VLBA1_2-256-8-2\n");
	printf("    MKIV1_4-128-2-1\n");
	printf("    Mark5B-512-16-2\n\n");

	return 0;
}

int verify(const char *filename, const char *formatname, int refMJD)
{
	struct mark5_stream *ms;
	float **data;
	int i;
	int  status = 0;
	long long total, unpacked;
	int mjd, sec;
        double ns, startmjd, stopmjd;

	ms = new_mark5_stream(
		new_mark5_stream_file(filename, 0),
		new_mark5_format_generic_from_string(formatname) );

	if(!ms)
	{
		fprintf(stderr, "problem opening %s\n", filename);
		return 0;
	}

	// resolve any day ambiguities
        mark5_stream_fix_mjd(ms, refMJD);

	//mark5_stream_print(ms);

	mark5_stream_get_frame_time(ms, &mjd, &sec, &ns);
	startmjd = mjd + (sec + ns/1e9) / 86400;
	

	FILE *fp = fopen(filename, "rb");

	fseek (fp,0L,SEEK_END);
        long length = ftell(fp);
        fclose (fp);


	long numFrames = length / ms->framebytes;
	double jumpNs = numFrames * ms->framens;

	double skipNs = ns + jumpNs;


	long endSec = skipNs / 1e9;
	double endNs = skipNs - endSec*1e9;

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

	if ((int)stopmjd != (int)startmjd)
	{
		fprintf (stderr, "Error: startMJD (%d) != stopMJD (%d) on file %s\n", (int)startmjd, (int)stopmjd, filename);
	}
	
	printf ("%s %lf %lf\n", filename, startmjd, stopmjd);

	delete_mark5_stream(ms);

	return 0;
}

int main(int argc, char **argv)
{
	long long offset = 0;
	int r;
	struct dirent *ep;
	char filename[2048];
	int refMJD = 57000;
	

	oldsiginthand = signal(SIGINT, siginthand);

	if(argc != 3)
	{
		return usage(argv[0]);
	}

	DIR *dp = opendir(argv[1]);
	if (dp != NULL)
	{
		while (ep = readdir (dp))
		{
			if ((strcmp(ep->d_name, ".") != 0) && (strcmp(ep->d_name, "..") != 0))
			{
				strcat(filename, argv[1]);
				strcat(filename, ep->d_name);	
				verify(filename, argv[2], refMJD);
				strcpy(filename, "");
			}
		}
	}
	else
	{
		fprintf (stderr,"ERROR: Directory %s does not exist\n", argv[1]);
		exit(1);
	}
	(void) closedir (dp);


	return 0;
}


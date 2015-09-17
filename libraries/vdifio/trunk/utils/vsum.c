/***************************************************************************
 *   Copyright (C) 2013-2015 by Walter Brisken                             *
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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <vdifio.h>
#include "config.h"
#ifdef HAVE_MARK6SG
#include <mark6sg/mark6_sg_utils.h>
#include <vdifmark6sg.h>
#endif

const char program[] = "vsum";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.4";
const char verdate[] = "20150917";

static void usage(const char *pgm)
{
	printf("%s ver. %s  %s  %s\n\n", program, version, author, verdate);
	printf("A utility to summarize the contents of VDIF data files\n\n");
	printf("Usage: %s [<options>] <file1> [<file2> [ ... ] ]\n\n", pgm);
	printf("  <fileX> is the name of a VDIF data file\n\n");
	printf("  <options> can include:\n");
	printf("    -h or --help      print this usage information and quit\n");
	printf("    -s or --shortsum  print a short summary, also usable for input to vex2difx\n");
	printf("    -6 or --mark6     operate directly on Mark6 module data\n");
#ifndef HAVE_MARK6SG
	printf("        NOTE: mark6sg library not compiled in so this option is not available\n");
#endif
	printf("    --allmark6        operate directly on all Mark6 scans found on mounted modules\n");
#ifndef HAVE_MARK6SG
	printf("        NOTE: mark6sg library not compiled in so this option is not available\n");
#endif
	printf("\n");
}

void summarizeFile(const char *fileName, int shortSum, int isMark6)
{
	struct vdif_file_summary sum;
	int r;

	if(isMark6)
	{
#ifdef HAVE_MARK6SG
		r = summarizevdifmark6(&sum, fileName, 0);
#else
		fprintf(stderr, "Error: mark6sg library support is not compiled in so the direct Mark6 option is not available.\n");
	
		exit(EXIT_FAILURE);
#endif
	}
	else
	{
		r = summarizevdiffile(&sum, fileName, 0);
	}

	if(r < 0)
	{
		fprintf(stderr, "File %s VDIF summary failed with return value %d\n\n", fileName, r);
	}
	else if(shortSum)
	{
		const int MaxFilenameLength = 512;
		double mjd1, mjd2;
		char fullFileName[MaxFilenameLength];

		mjd1 = vdiffilesummarygetstartmjd(&sum) + (sum.startSecond % 86400)/86400.0;
		mjd2 = mjd1 + (sum.endSecond - sum.startSecond + 1)/86400.0;

		if(fileName[0] != '/' && isMark6 == 0)
		{
			char path[MaxFilenameLength];
			getcwd(path, MaxFilenameLength);
			snprintf(fullFileName, MaxFilenameLength, "%s/%s", path, fileName);
		}
		else
		{
			snprintf(fullFileName, MaxFilenameLength, "%s", fileName);
		}
		printf("%s %14.8f %14.8f\n", fullFileName, mjd1, mjd2);
	}
	else
	{
		printvdiffilesummary(&sum);
	}
}
void summarizeSingleMark6File(const char *fileName, int shortSum)
{
#ifdef HAVE_MARK6SG
	static char **scanList = 0;
	static int nScan = -1;

	if(nScan == -1)
	{
		nScan = mark6_sg_list_all_scans(&scanList);
		if(nScan <= 0)
		{
			fprintf(stderr, "Error: no mark6 scans found\n");
		
			exit(EXIT_FAILURE);
		}
	}

	summarizeFile(fileName, shortSum, 1);

#else
	fprintf(stderr, "Error: mark6sg library support is not compiled in so the direct Mark6 option is not available.\n");

	exit(EXIT_FAILURE);
#endif
}

void processAllMark6Scans(int shortSum)
{
#ifdef HAVE_MARK6SG
	int nScan;
	char **scanList;

	nScan = mark6_sg_list_all_scans(&scanList);
	if(nScan > 0)
	{
		int s;
		for(s = 0; s < nScan; ++s)
		{
			summarizeFile(scanList[s], shortSum, 1);
			free(scanList[s]);
		}
		free(scanList);
	}
	else
	{
		fprintf(stderr, "No scans found on Mark6 modules\n");
	}
#else
	fprintf(stderr, "Error: mark6sg library support is not compiled in so the direct Mark6 option is not available.\n");
	
	exit(EXIT_FAILURE);
#endif
}

int main(int argc, char **argv)
{
	if(argc < 2)
	{
		usage(argv[0]);

		exit(EXIT_FAILURE);
	}
	else
	{
		int a;
		int shortSum = 0;
		int isMark6 = 0;

#ifdef HAVE_MARK6SG
		mark6_sg_set_rootpattern("/mnt/disks/[1-4]/[0-7]/data/");
#endif

		for(a = 1; a < argc; ++a)
		{
			if(strcmp(argv[a], "-s") == 0 ||
			   strcmp(argv[a], "--shortsum") == 0)
			{
				shortSum = 1;
			}
			else if(strcmp(argv[a], "-h") == 0 ||
			   strcmp(argv[a], "--help") == 0)
			{
				usage(argv[0]);

				exit(EXIT_SUCCESS);
			}
			else if(strcmp(argv[a], "-6") == 0 ||
			   strcmp(argv[a], "--mark6") == 0)
			{
				isMark6 = 1;
			}
			else if(strcmp(argv[a], "--allmark6") == 0)
			{
				processAllMark6Scans(shortSum);
				
				exit(EXIT_SUCCESS);
			}
			else
			{
				summarizeSingleMark6File(argv[a], shortSum);
			}
		}
	}

	return 0;
}

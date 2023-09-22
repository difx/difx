/***************************************************************************
 *   Copyright (C) 2013-2019 by Walter Brisken                             *
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
#include <mark6sg/mark6gather.h>
#include "mark6gather_vdif.h"
#endif

const char program[] = "vsum";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>, Mark Wainright <mwainrig@nrao.edu>";
const char version[] = "0.8";
const char verdate[] = "20190507";

static void usage(const char *pgm)
{
	printf("%s ver. %s  %s  %s\n\n", program, version, author, verdate);
	printf("A utility to summarize the contents of VDIF data files\n\n");
	printf("Usage: %s [<options>] <file1> [<file2> [ ... ] ]\n\n", pgm);
	printf("  <fileX> is the name of a VDIF data file\n\n");
	printf("  <options> can include:\n");
	printf("    -h or --help       Print this usage information and quit\n");
	printf("    -s or --shortsum   Print a short summary, also usable for input to vex2difx\n");
	printf("    -t or --timeoffset Add time offset to filetimes\n");
#ifdef HAVE_MARK6SG
	printf("    -6 or --mark6      Operate directly on Mark6 module data\n");
	printf("    --allmark6         Operate directly on all Mark6 scans found on mounted modules\n");
	printf("    --mark6slot <slot> Operate directly on all Mark6 scans found on module in <slot>\n");
#endif
	printf("\n");
}

/* NOTE: toff is never used in this function... */
static void summarizeFile(const char *fileName, int shortSum, int isMark6, int toff)
{
	struct vdif_file_summary sum;
	int r;

#ifdef HAVE_MARK6SG
	if(isMark6)
	{
		r = summarizevdifmark6(&sum, fileName, 0);
	}
	else
#endif
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
			if(getcwd(path, MaxFilenameLength) == NULL)
			{
				/* CJP Maybe wrong behaviour or warning needed */
				snprintf(fullFileName, MaxFilenameLength, "%s", fileName);
			}
			else
			{
				snprintf(fullFileName, MaxFilenameLength, "%s/%s", path, fileName);
			}
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

#ifdef HAVE_MARK6SG
static void processAllMark6Scans(int shortSum)
{
	char **fileList;
	int n;

	n = getMark6FileList(&fileList);

	if(n == 0)
	{
		printf("No Mark6 files found in %s\n", getMark6Root());
	}
	else
	{
		int i;

		for(i = 0; i < n; ++i)
		{
			summarizeFile(fileList[i], shortSum, 1, 0);
		}
	
		for(i = 0; i < n; ++i)
		{
			free(fileList[i]);
		}
		free(fileList);
	}
}

static void processMark6ScansSlot(int slot, int shortSum)
{
	char **fileList;
	int n;

	n = getMark6SlotFileList(slot, &fileList);

	if(n == 0)
	{
		printf("No Mark6 files found in /mnt/disks/%d/*/data\n", slot);
	}
	else
	{
		int i;

		for(i = 0; i < n; ++i)
		{
			summarizeFile(fileList[i], shortSum, 1, 0);
		}
	
		for(i = 0; i < n; ++i)
		{
			free(fileList[i]);
		}
		free(fileList);
	}
}
#endif

int main(int argc, char **argv)
{
	if(argc < 2)
	{
		usage(argv[0]);

		exit(EXIT_FAILURE);
	}
	else
	{
		int a, slot;
		int shortSum = 0;
		int isMark6 = 0;
		int toff = 0;

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
			else if(strcmp(argv[a], "-t") == 0 ||
			   strcmp(argv[a], "--timeoffset") == 0)
			{
			       a++;
			       toff = atoi(argv[a]);
			}
#ifdef HAVE_MARK6SG
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
			else if(strcmp(argv[a], "--mark6slot") == 0)
			{
                                ++a;
                                slot = atoi(argv[a]);
                                if(slot < 1 || slot > 4)
                                {
                                        exit(EXIT_FAILURE);
                                }
                                
				processMark6ScansSlot(slot, shortSum);
				
				exit(EXIT_SUCCESS);
			}
#endif
			else
			{
				summarizeFile(argv[a], shortSum, isMark6, toff);
			}
		}
	}

	return 0;
}

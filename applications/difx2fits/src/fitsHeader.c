/***************************************************************************
 *   Copyright (C) 2008-2017 by Walter Brisken                             *
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
// $Id: fitsHeader.c 10537 2022-07-15 20:18:57Z WalterBrisken $
// $HeadURL: https://svn.atnf.csiro.au/difx/master_tags/DiFX-2.8.1/applications/difx2fits/src/fitsHeader.c $
// $LastChangedRevision: 10537 $
// $Author: WalterBrisken $
// $LastChangedDate: 2022-07-16 04:18:57 +0800 (六, 2022-07-16) $
//
//============================================================================
#include <sys/time.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <glob.h>
#include "difx2fits.h"

#define SEC_DAY         86400.0             /* seconds in a mean solar day */
#define MUSEC_DAY       86400000000.0       /* mus in a mean solar day */
#define MJD_UNIX0       40587.0             /* MJD at beginning of unix time */

static int isFileInGlob(const char *fileName, int fileStart, const glob_t *g)
{
	int f;

	for(f = 0; f < g->gl_pathc;  ++f)
	{
		if(strcmp(fileName, g->gl_pathv[f] + fileStart) == 0)
		{
			return 1;
		}
	}

	return 0;
}

static void writeHistoryFile(const char *fileName, struct fitsPrivate *out, const struct CommandLineOptions *opts)
{
	const int maxHistoryLength = 70;
	FILE *in;
	char strng[maxHistoryLength+1];

	in = fopen(fileName, "r");
	if(!in)
	{
		printf("\nWarning: cannot load history file: %s\n                            ", fileName);
	}
	else
	{
		fitsWriteComment(out, "HISTORY", "");
		for(;;)
		{
			int j;

			fgets(strng, maxHistoryLength, in);
			if(feof(in))
			{
				break;
			}
			strng[maxHistoryLength] = 0;
			for(j = 0; strng[j]; ++j)
			{
				if(strng[j] < ' ')
				{
					strng[j] = ' ';
				}
			}
			fitsWriteComment(out, "HISTORY", strng);
		}

		fclose(in);

		if(opts->verbose > 0)
		{
			printf("Including %s as HISTORY\n", fileName);
		}
	}
}

static void writeDifxHistory(const DifxInput *D, struct fitsPrivate *out, const struct CommandLineOptions *opts)
{
	glob_t *globs;
	int j;
	int *fileStart;	/* where in full path the filename begins */

	globs = (glob_t *)calloc(D->nJob, sizeof(glob_t));
	fileStart = (int *)calloc(D->nJob, sizeof(int));

	for(j = 0; j < D->nJob; ++j)
	{
		char pattern[PATH_MAX];

		snprintf(pattern, PATH_MAX, "%s/*.history", D->job[j].outputFile);
		glob(pattern, 0, 0, globs+j);
		fileStart[j] = strlen(D->job[j].outputFile) + 1;
	}

	for(j = 0; j < D->nJob; ++j)
	{
		int f;

		for(f = 0; f < globs[j].gl_pathc; ++f)
		{
			int p;

			for(p = 0; p < j; ++p)
			{
				if(isFileInGlob(globs[j].gl_pathv[f] + fileStart[f], fileStart[p], globs+p))
				{
					break;
				}
			}
			if(p < j)
			{
				continue;
			}

			/* If we get here, then this file has unique name and should be included */
			writeHistoryFile(globs[j].gl_pathv[f], out, opts);
		}
	}
	
	for(j = 0; j < D->nJob; ++j)
	{
		globfree(globs+j);
	}

	free(globs);
	free(fileStart);
}

double timeMjd()
{
	struct timeval t;

	gettimeofday(&t, 0);
	
	return MJD_UNIX0 + t.tv_sec/SEC_DAY + t.tv_usec/MUSEC_DAY;
}

const DifxInput *DifxInput2FitsHeader(const DifxInput *D, struct fitsPrivate *out, const struct CommandLineOptions *opts)
{
	const int maxLength = 132;
	char ref_date[12];
	char str[64], strng[maxLength];
	char local_time[48];
	const char *difxLabel;
	int j;

	if(D == 0)
	{
		return 0;
	}

	mjd2fits((int)(D->mjdStart), ref_date);

	fitsWriteLogical(out, "SIMPLE", 1, "Standard FITS format");
	fitsWriteInteger(out, "BITPIX", 8, "");
	fitsWriteInteger(out, "NAXIS",  0, "");

	fitsWriteLogical(out, "EXTEND", 1, "");
	fitsWriteLogical(out, "BLOCKED", 1, "");
	fitsWriteString(out, "OBJECT", "BINARYTB", "");
	fitsWriteString(out, "TELESCOP", "VLBA", "");
	fitsWriteString(out, "OBSERVER", D->job->obsCode, "");
	fitsWriteString(out, "ORIGIN", "VLBA Correlator", "");
	fitsWriteString(out, "CORRELAT", "DIFX", "");
	if(opts->primaryBand)
	{
		fitsWriteString(out, "PRIBAND", opts->primaryBand, "");
	}
	if(opts->doVanVleck)
	{
		fitsWriteInteger(out, "VANVLECK", 1, "");
	}
	fitsWriteString(out, "DATE-OBS", ref_date, "");
	mjd2fits((int)timeMjd(), strng);
	fitsWriteString(out, "DATE-MAP", strng, "Correlation date");
	fitsWriteLogical(out, "GROUPS", 1, "");
	fitsWriteInteger(out, "GCOUNT", 0, "");
	fitsWriteInteger(out, "PCOUNT", 0, "");

	/* get current local date and time */
	timeMjd2str(timeMjd(), local_time);
	snprintf(strng, maxLength, "OPENED FITS FILE : %s", local_time);
	fitsWriteComment(out, "HISTORY", strng);

	fitsWriteComment(out, "HISTORY", "CORRELATOR = 'DIFX'");

	if(D->job->difxVersion[0])
	{
		snprintf(strng, maxLength, "CORRVERS = %s", D->job->difxVersion);
		fitsWriteComment(out, "HISTORY", strng);
	}
	difxLabel = getenv("DIFX_LABEL");
	if(difxLabel)
	{
		snprintf(strng, maxLength, "CORRLABL = %s", difxLabel);
		fitsWriteComment(out, "HISTORY", strng);
	}

	snprintf(strng, maxLength, "LOG FILE : /To/be/implemented");
	fitsWriteComment(out, "HISTORY", strng);

	snprintf(strng, maxLength, "OBSCODE : %s", D->job->obsCode);
	fitsWriteComment(out, "HISTORY", strng);

	if(D->job->obsSession[0])
	{
		snprintf(strng, maxLength, "SESSION : %s", D->job->obsSession);
		fitsWriteComment(out, "HISTORY", strng);
	}
	
	snprintf(strng, maxLength, "JOBNUM : %d", D->job->jobId);
	fitsWriteComment(out, "HISTORY", strng);

	time2str(D->mjdStart, "", str);
	snprintf(strng, maxLength, "JOBSTART : %s", str);
	fitsWriteComment(out, "HISTORY", strng);

	time2str(D->mjdStop, "", str);
	snprintf(strng, maxLength, "JOBSTOP : %s", str);
	fitsWriteComment(out, "HISTORY", strng);

	for(j = 0; j < D->nJob; ++j)
	{
		snprintf(strng, maxLength, "DIFXJOB : %d.%d.%d", D->job[j].jobId, D->job[j].subjobId, D->job[j].subarrayId);
		fitsWriteComment(out, "HISTORY", strng);

		time2str(D->job[j].mjdStart, "", str);
		snprintf(strng, maxLength, "FILESTART : %s", str);
		fitsWriteComment(out, "HISTORY", strng);

		time2str(D->job[j].mjdStart + D->job[j].duration/86400.0, "", str);
		snprintf(strng, maxLength, "FILESTOP  : %s", str);
		fitsWriteComment(out, "HISTORY", strng);
	}

	if(opts->historyFile)
	{
		writeHistoryFile(opts->historyFile, out, opts);
	}

	/* Look for and convey history from each .difx/ *.history file */
	writeDifxHistory(D, out, opts);
	
	fitsWriteEnd(out);

	return D;
}

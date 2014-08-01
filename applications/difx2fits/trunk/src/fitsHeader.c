/***************************************************************************
 *   Copyright (C) 2008-2012 by Walter Brisken                             *
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
#include <sys/time.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include "difx2fits.h"

#define SEC_DAY         86400.0             /* seconds in a mean solar day */
#define MUSEC_DAY       86400000000.0       /* mus in a mean solar day */
#define MJD_UNIX0       40587.0             /* MJD at beginning of unix time */

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
		snprintf(strng, maxLength, "DIFXJOB : %d.%d.%d", 
			D->job[j].jobId, D->job[j].subjobId, 
			D->job[j].subarrayId);
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
		FILE *in;

		in = fopen(opts->historyFile, "r");
		if(!in)
		{
			printf("\nWarning: cannot load history file: %s\n                            ", opts->historyFile);
		}
		else
		{
			for(;;)
			{
				fgets(strng, 70, in);
				if(feof(in))
				{
					break;
				}
				strng[70] = 0;
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
		}
	}
	
	fitsWriteEnd(out);

	return D;
}

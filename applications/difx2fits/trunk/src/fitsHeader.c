/***************************************************************************
 *   Copyright (C) 2008, 2009 by Walter Brisken                            *
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

const DifxInput *DifxInput2FitsHeader(const DifxInput *D, 
	struct fitsPrivate *out)
{
	char ref_date[12];
	char str[64], strng[132];
	char local_time[48];
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
	fitsWriteString(out, "DATE-OBS", ref_date, "");
	mjd2fits((int)timeMjd (), strng);
	fitsWriteString(out, "DATE-MAP", strng, "Correlation date");
	fitsWriteLogical(out, "GROUPS", 1, "");
	fitsWriteInteger(out, "GCOUNT", 0, "");
	fitsWriteInteger(out, "PCOUNT", 0, "");

	/* get current local date and time */
	timeMjd2str (timeMjd (), local_time);
	strcpy(strng, "OPENED FITS FILE : ");
	strcat(strng, local_time);
	fitsWriteComment(out, "HISTORY", strng);

	fitsWriteComment(out, "HISTORY", "CORRELATOR = 'DIFX'");

	if(D->job->difxVersion[0])
	{
		sprintf(strng, "CORRVERS = %s", D->job->difxVersion);
		fitsWriteComment(out, "HISTORY", strng);
	}

	sprintf (strng, "LOG FILE : /To/be/implemented");
	fitsWriteComment(out, "HISTORY", strng);

	sprintf (strng, "OBSCODE : %s", D->job->obsCode);
	fitsWriteComment(out, "HISTORY", strng);

	if(D->job->obsSession[0])
	{
		sprintf (strng, "SESSION : %s", D->job->obsSession);
		fitsWriteComment(out, "HISTORY", strng);
	}
	
	sprintf (strng, "JOBNUM : %d", D->job->jobId);
	fitsWriteComment(out, "HISTORY", strng);

	time2str(D->mjdStart, "", str);
	sprintf (strng, "JOBSTART : %s", str);
	fitsWriteComment(out, "HISTORY", strng);

	time2str(D->mjdStop, "", str);
	sprintf (strng, "JOBSTOP : %s", str);
	fitsWriteComment(out, "HISTORY", strng);

	for(j = 0; j < D->nJob; j++)
	{
		sprintf (strng, "DIFXJOB : %d.%d.%d", 
			D->job[j].jobId, D->job[j].subjobId, 
			D->job[j].subarrayId);
		fitsWriteComment(out, "HISTORY", strng);

		time2str(D->job[j].mjdStart, "", str);
		sprintf (strng, "FILESTART : %s", str);
		fitsWriteComment(out, "HISTORY", strng);

		time2str(D->job[j].mjdStart + D->job[j].duration/86400.0, 
			"", str);
		sprintf (strng, "FILESTOP  : %s", str);
		fitsWriteComment(out, "HISTORY", strng);
	}
	
	fitsWriteEnd(out);

	return D;
}

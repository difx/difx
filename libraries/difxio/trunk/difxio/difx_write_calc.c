/***************************************************************************
 *   Copyright (C) 2008 by Walter Brisken                                  *
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

#include <stdlib.h>
#include <string.h>
#include "difxio/difx_write.h"

int writeDifxCalc(const DifxInput *D, const char *filename)
{
	FILE *out;
	char filebase[256];
	char value[256];
	int i, l;

	if(!D)
	{
		return -1;
	}

	if(!D->job)
	{
		fprintf(stderr, "writeDifxCalc: job=0\n");
		return -1;
	}

	strcpy(filebase, filename);
	l = strlen(filebase);
	for(i = l-1; i > 0; i++)
	{
		if(filebase[i] == '.')
		{
			filebase[i] = 0;
			break;
		}
	}

	out = fopen(filename, "w");
	if(!out)
	{
		fprintf(stderr, "Cannot open %s for write\n", filename);
		return -1;
	}

	writeDifxLineInt(out, "JOB ID", D->job->jobId);
	writeDifxLineDouble(out, "JOB START TIME", "%13.7f", D->job->jobStart);
	writeDifxLineDouble(out, "JOB STOP TIME", "%13.7f", D->job->jobStop);
	writeDifxLine(out, "OBSCODE", D->job->obsCode);
	writeDifxLine(out, "DIFX VERSION", D->job->difxVersion);
	writeDifxLineInt(out, "SUBJOB ID", D->job->subjobId);
	writeDifxLineInt(out, "SUBARRAY ID", D->job->subarrayId);
	writeDifxLineDouble(out, "START MJD", "%13.7f", D->mjdStart);
	writeDifxDateLines(out, D->mjdStart);
	writeDifxLineDouble(out, "INCREMENT (SECS)", "%1.0f", D->job->modelInc);
	writeDifxLineInt(out, "SPECTRAL AVG", D->specAvg);
	writeDifxLine(out, "TAPER FUNCTION", D->job->taperFunction);
	writeDifxAntennaArray(out, D->nAntenna, D->antenna, 1, 1, 1, 0, 1);
	writeDifxScanArray(out, D->nScan, D->scan, D->config, 1, 1, 1);
	writeDifxEOPArray(out, D->nEOP, D->eop);
	writeDifxSpacecraftArray(out, D->nSpacecraft, D->spacecraft);
	sprintf(value, "%s.delay", filebase);
	writeDifxLine(out, "DELAY FILENAME", value);
	sprintf(value, "%s.uvw", filebase);
	writeDifxLine(out, "UVW FILENAME", value);
	sprintf(value, "%s.rate", filebase);
	writeDifxLine(out, "RATE FILENAME", value);
	sprintf(value, "%s.im", filebase);
	writeDifxLine(out, "IM FILENAME", value);

	fclose(out);

	return 0;
}

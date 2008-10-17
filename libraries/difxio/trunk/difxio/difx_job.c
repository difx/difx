/***************************************************************************
 *   Copyright (C) 2007 by Walter Brisken                                  *
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "difxio/difx_input.h"
#include "difxio/parsedifx.h"

const char aberCorrStrings[][16] = 
{
	"UNCORRECTED",
	"APPROXIMATE",
	"EXACT"
};

DifxJob *newDifxJobArray(int nJob)
{
	DifxJob *dj;
	int j;

	dj = (DifxJob *)calloc(nJob, sizeof(DifxJob));
	for(j = 0; j < nJob; j++)
	{
		strcpy(dj[j].obsCode, "DIFX");
		strcpy(dj[j].taperFunction, "UNIFORM");
		strcpy(dj[j].calcServer, "UNKNOWN");
		dj[j].calcProgram = -1;
		dj[j].calcVersion = -1;
	}

	return dj;
}

void deleteDifxJobArray(DifxJob *dj)
{
	if(dj)
	{
		free(dj);
	}
}

void fprintDifxJob(FILE *fp, const DifxJob *dj)
{
	fprintf(fp, "    Job ID = %d\n", dj->jobId);
	fprintf(fp, "    Project = %s\n", dj->obsCode);
	if(dj->obsSession[0])
	{
		fprintf(fp, "    Session = %s\n", dj->obsSession);
	}
	fprintf(fp, "    Start = MJD %12.6f\n", dj->mjdStart);
	fprintf(fp, "    Duration = %f sec\n", dj->duration);
	fprintf(fp, "    Model Inc = %f sec\n", dj->modelInc);
}

void printDifxJob(const DifxJob *dj)
{
	fprintDifxJob(stdout, dj);
}

void copyDifxJob(DifxJob *dest, const DifxJob *src)
{
	memcpy(dest, src, sizeof(DifxJob));
}

/* simply append dj2 after dj1 return new size on call stack : ndj */
DifxJob *mergeDifxJobArrays(const DifxJob *dj1, int ndj1,
	const DifxJob *dj2, int ndj2, int *jobIdRemap, int *ndj)
{
	DifxJob *dj;
	int i;

	for(i = 0; i < ndj2; i++)
	{
		jobIdRemap[i] = ndj1 + i;
	}
	
	*ndj = ndj1 + ndj2;
	dj = newDifxJobArray(*ndj);

	for(i = 0; i < ndj1; i++)
	{
		copyDifxJob(dj + i, dj1 + i);
	}
	for(i = 0; i < ndj2; i++)
	{
		copyDifxJob(dj + ndj1 + i, dj2 + i);
	}

	return dj;
}

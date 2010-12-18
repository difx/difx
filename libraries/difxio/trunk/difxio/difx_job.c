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

const char aberCorrStrings[][MAX_ABER_CORR_STRING_LENGTH] = 
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
		snprintf(dj[j].obsCode,       DIFXIO_OBSCODE_LENGTH,  "%s", "DIFX");
		snprintf(dj[j].taperFunction, DIFXIO_TAPER_LENGTH,    "%s", "UNIFORM");
		snprintf(dj[j].calcServer,    DIFXIO_HOSTNAME_LENGTH, "%s", "UNKNOWN");
		dj[j].calcProgram = -1;
		dj[j].calcVersion = -1;
	}

	return dj;
}

void deleteDifxJobArray(DifxJob *dj)
{
	if(dj)
	{
		if(dj->flag)
		{
			deleteDifxAntennaFlagArray(dj->flag);
			dj->flag = 0;
		}
		free(dj);
	}
}

void fprintDifxJob(FILE *fp, const DifxJob *dj)
{
	fprintf(fp, "  DifxJob : %p\n", dj);
	fprintf(fp, "    job ID = %d\n", dj->jobId);
	fprintf(fp, "    project = %s\n", dj->obsCode);
	if(dj->obsSession[0])
	{
		fprintf(fp, "    session = %s\n", dj->obsSession);
	}
	fprintf(fp, "    start = MJD %12.6f\n", dj->mjdStart);
	fprintf(fp, "    duration = %f sec\n", dj->duration);
	fprintf(fp, "    vex file = %s\n", dj->vexFile);
	fprintf(fp, "    input file = %s\n", dj->inputFile);
	fprintf(fp, "    threads (core conf) file = %s\n", dj->threadsFile);
	fprintf(fp, "    calc file = %s\n", dj->calcFile);
	fprintf(fp, "    im (model) file = %s\n", dj->imFile);
	fprintf(fp, "    flag file = %s\n", dj->flagFile);
	fprintf(fp, "    output file = %s\n", dj->outputFile);
}

void printDifxJob(const DifxJob *dj)
{
	fprintDifxJob(stdout, dj);
}

void copyDifxJob(DifxJob *dest, const DifxJob *src, int *antennaIdRemap)
{
	int f;
	
	memcpy(dest, src, sizeof(DifxJob));

	if(src->nFlag > 0)
	{
		dest->flag = newDifxAntennaFlagArray(src->nFlag);
		dest->nFlag = src->nFlag;
		for(f = 0; f < dest->nFlag; f++)
		{
			copyDifxAntennaFlag(dest->flag + f,
				src->flag + f, antennaIdRemap);
		}
	}
}

/* simply append dj2 after dj1 return new size on call stack : ndj */
DifxJob *mergeDifxJobArrays(const DifxJob *dj1, int ndj1,
	const DifxJob *dj2, int ndj2, int *jobIdRemap, 
	int *antennaIdRemap, int *ndj)
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
		copyDifxJob(dj + i, dj1 + i, 0);
	}
	for(i = 0; i < ndj2; i++)
	{
		copyDifxJob(dj + ndj1 + i, dj2 + i, antennaIdRemap);
	}

	return dj;
}

/* Warning: fileBase should be at least DIFXIO_FILENAME_LENGTH long */
void generateDifxJobFileBase(DifxJob *dj, char *fileBase)
{
	int i, p;

	fileBase[0] = 0;

	if(!dj)
	{
		return;
	}

	for(i = p = 0; dj->outputFile[i]; i++)
	{
		if(dj->outputFile[i] == '/')
		{
			if(dj->outputFile[i+1] != 0)
			{
				p = i + 1;
			}
		}
	}

	strcpy(fileBase, dj->outputFile + p);

	for(i = p = 0; fileBase[i]; i++)
	{
		if(fileBase[i] == '.')
		{
			if(fileBase[i+1] != 0)
			{
				p = i;
			}
		}
	}

	if(p > 0)
	{
		fileBase[p] = 0;
	}
}

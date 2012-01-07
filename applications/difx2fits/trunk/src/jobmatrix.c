/***************************************************************************
 *   Copyright (C) 2009-2012 by Walter Brisken                             *
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
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id$
 * $HeadURL$
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <stdlib.h>
#include "jobmatrix.h"

struct _JobMatrix
{
	int **matrix;
	int nAntenna, nTime;
	const DifxInput *D;
	const char *filebase;
	double mjdStart;		/* days */
	double deltaT;			/* seconds */
};

/* deltaT parameter is in seconds */
JobMatrix *newJobMatrix(const DifxInput *D, const char *filebase, double deltaT)
{
	JobMatrix *jm;
	int a, n, t;
	double frac;

	jm = (JobMatrix *)calloc(1, sizeof(JobMatrix));

	jm->deltaT = deltaT;
	frac = D->mjdStart - (int)(D->mjdStart);
	n = frac/(deltaT/86400.0);
	jm->mjdStart = (int)(D->mjdStart) + n*deltaT/86400.0;
	jm->nTime = (D->mjdStop - jm->mjdStart)/(deltaT/86400.0) + 1;
	jm->nAntenna = D->nAntenna;
	jm->matrix = (int **)calloc(jm->nTime, sizeof(int *));
	for(t = 0; t < jm->nTime; ++t)
	{
		jm->matrix[t] = (int *)calloc(jm->nAntenna, sizeof(int));
		for(a = 0; a < jm->nAntenna; ++a)
		{
			jm->matrix[t][a] = -1;
		}
	}
	jm->filebase = filebase;
	jm->D = D;

	return jm;
}

void writeJobMatrix(const JobMatrix *jm)
{
	int *jobList;
	FILE *out;
	int nJob;
	char label[DIFXIO_FILENAME_LENGTH];
	char outname[DIFXIO_FILENAME_LENGTH];
	int a, t, j, v;
	char name[4];
	char timeStr[40];
	char lastday[10] = "";

	if(!jm)
	{
		return;
	}

	v = snprintf(outname, DIFXIO_FILENAME_LENGTH, "%s.jobmatrix", jm->filebase);
	if(v >= DIFXIO_FILENAME_LENGTH)
	{
		fprintf(stderr, "Developer error: writeJobMatrix: outname wants %d bytes, not %d\n", v, DIFXIO_FILENAME_LENGTH);
		
		return;
	}

	nJob = jm->D->nJob;
	jobList = (int *)calloc(nJob, sizeof(int));
	
	out = fopen(outname, "w");
	if(!out)
	{
		fprintf(stderr, "Warning: cannot open %s for output.  No jobmatrix file will be produced.\n", outname);

		free(jobList);

		return;
	}

	for(a = 0; a < jm->nAntenna; ++a)
	{
		strncpy(name, jm->D->antenna[a].name, 2);
		name[2] = 0;
		fprintf(out, "%s ", name);
	}
	fprintf(out, "\n\n");

	j = 0;
	for(t = 0; t < jm->nTime; ++t)
	{
		for(a = 0; a < jm->nAntenna; ++a)
		{
			if(jm->matrix[t][a] < 0)
			{
				fprintf(out, "   ");
			}
			else if(jm->matrix[t][a] < jm->D->nJob)
			{
				jobList[jm->matrix[t][a]] = 1;
				fprintf(out, "%c  ", 'A' + (jm->matrix[t][a]%26));
			}
			else
			{
				fprintf(out, "?  ");
			}
		}

		timeMjd2str(jm->mjdStart + t*jm->deltaT/86400.0, timeStr);
		timeStr[39] = 0;
		if(strncmp(timeStr, lastday, 9) == 0)
		{
			fprintf(out, "           %s", timeStr+9);
		}
		else
		{
			strncpy(lastday, timeStr, 9);
			lastday[9] = 0;
			fprintf(out, "  %s", timeStr);
		}

		if(j < nJob && jobList[j])
		{
			generateDifxJobFileBase(jm->D->job + j, label);
			fprintf(out, "   %c = %s", 'A'+(j%26), label);
			++j;
		}

		fprintf(out, "\n");
	}

	fclose(out);
	free(jobList);
}

void deleteJobMatrix(JobMatrix *jm)
{
	if(jm)
	{
		if(jm->matrix)
		{
			int t;

			for(t = 0; t < jm->nTime; ++t)
			{
				if(jm->matrix[t])
				{
					free(jm->matrix[t]);
				}
			}
			free(jm->matrix);
		}
		free(jm);
	}
}

int feedJobMatrix(JobMatrix *jm, const struct UVrow *data, int jobId)
{
	int a, t;
	double mjd;

	if(!jm)
	{
		return -1;
	}
	a = data->baseline/256 - 1;
	if(a < 0 || a > jm->nAntenna)
	{
		return -1;
	}
	mjd = (int)(data->jd - 2400000.0) + data->iat;
	t = (mjd - jm->mjdStart)*86400.0/jm->deltaT;
	if(t < 0 || t >= jm->nTime)
	{
		return -1;
	}
	jm->matrix[t][a] = jobId;

	return 0;
}

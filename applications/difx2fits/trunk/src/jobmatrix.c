/***************************************************************************
 *   Copyright (C) 2009-2016 by Walter Brisken and Helge Rottmann          *
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

static int cmpfunc (const void * a, const void * b)
{
	return ( *(int*)a - *(int*)b );
}

/**
 * Writes the .jobmatrix file
 **/
void writeJobMatrix(const JobMatrix *jm, int passNum)
{
	int *jobList;
	int *activeJobs;
	FILE *out;
	int nJob;
	char label[DIFXIO_FILENAME_LENGTH];
	char outname[DIFXIO_FILENAME_LENGTH];
	int a, t, v, i;
	char name[4];
	char timeStr[40];
	char lastday[10] = "";

	if(!jm)
	{
		return;
	}

	if(passNum == 0)
	{
		v = snprintf(outname, DIFXIO_FILENAME_LENGTH, "%s.jobmatrix", jm->filebase);
	}
	else
	{
		v = snprintf(outname, DIFXIO_FILENAME_LENGTH, "%s_setup%d.jobmatrix", jm->filebase, passNum);
	}

	if(v >= DIFXIO_FILENAME_LENGTH)
	{
		fprintf(stderr, "Developer error: writeJobMatrix: outname wants %d bytes, not %d\n", v, DIFXIO_FILENAME_LENGTH);
		
		return;
	}

	nJob = jm->D->nJob;
	jobList = (int *)calloc(nJob, sizeof(int));
	activeJobs = (int *)calloc(jm->nAntenna, sizeof(int));

	// set jobList array to 0
	for(i = 0; i < nJob; ++i)
	{
		jobList[i] = 0;
	}

	out = fopen(outname, "w");
	if(!out)
	{
		fprintf(stderr, "Warning: cannot open %s for output.  No jobmatrix file will be produced.\n", outname);

		free(jobList);

		return;
	}

	// write header line
	for(a = 0; a < jm->nAntenna; ++a)
	{
		strncpy(name, jm->D->antenna[a].name, 2);
		name[2] = 0;
		fprintf(out, "%s ", name);
	}
	fprintf(out, "\n\n");

	for(t = 0; t < jm->nTime; ++t)
	{
		int numActiveJobs = 0;

		activeJobs = (int *)calloc(jm->nAntenna, sizeof(int));	// jobIds in the active time slice
		for(a = 0; a < jm->nAntenna; ++a)
		{
			if(jm->matrix[t][a] < 0)
			{
				fprintf(out, "   ");
			}
			else if(jm->matrix[t][a] < jm->D->nJob)
			{
				if(jobList[jm->matrix[t][a]] == 0)
				{
					jobList[jm->matrix[t][a]] = 1;
				}
				fprintf(out, "%c  ", 'A' + (jm->matrix[t][a] % 26));
				activeJobs[numActiveJobs++] = jm->matrix[t][a];
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


		// output scan label entry only on scan change
		if(numActiveJobs > 0)
		{
			int lastJob = -1;

			qsort(activeJobs, numActiveJobs, sizeof(int), cmpfunc); 
			for(i = 0; i < numActiveJobs; ++i)
			{
				int job = activeJobs[i];

				if((job != lastJob) && (jobList[job] == 1))
				{
					generateDifxJobFileBase(jm->D->job + job, label);
					fprintf(out, "   %c = %s", 'A'+(job%26), label);
					jobList[job] = 2;
				}
				lastJob = job;
				
			}
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
	mjd = (int)(data->jd - 2400000.0) + data->utc;
	t = (mjd - jm->mjdStart)*86400.0/jm->deltaT;
	if(t < 0 || t >= jm->nTime)
	{
		return -1;
	}
	jm->matrix[t][a] = jobId;

	return 0;
}

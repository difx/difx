/***************************************************************************
 *   Copyright (C) 2007-2012 by Walter Brisken & Adam Deller               *
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
#include "difxio/parsevis.h"

DifxVisRecord *newDifxVisRecord(const char *filename, int nchan)
{
	DifxVisRecord *vis;

	vis = (DifxVisRecord *)malloc(sizeof(DifxVisRecord));
	if(vis == 0)
	{
		fprintf(stderr, "newDifxVisRecord : malloc error\n");

		return 0;
	}

	vis->visdata = (cplx32f*)malloc(nchan*sizeof(cplx32f));
	if(vis->visdata == 0)
	{
		fprintf(stderr, "newDifxVisRecord : malloc error\n");
		free(vis);
		
		return 0;
	}
	
	if(strcmp(filename, "-") == 0)
	{
		vis->infile = stdin;
	}
	else
	{
		vis->infile = fopen(filename, "r");
	}
	if(vis->infile == 0)
	{
		fprintf(stderr, "Cannot open %s\n", filename);
		free(vis->visdata);
		free(vis);
		
		return 0;
	}

	vis->params = newDifxParameters();

	if(vis->params == 0)
	{
		fprintf(stderr, "newDifxVisRecord : newDifxParameters error\n");
		deleteDifxVisRecord(vis);
		
		return 0;
	}

	vis->nchan = nchan;
	vis->visnum = 0;
	vis->headerversion = 1; //default to new style
	vis->polpair[2] = 0;

	return vis;
}

void deleteDifxVisRecord(DifxVisRecord *vis)
{
	if(vis)
	{
		if(vis->infile && vis->infile != stdin)
		{
			fclose(vis->infile);
		}
		if(vis->visdata)
		{
			free(vis->visdata);
			vis->visdata = 0;
		}
		if(vis->params)
		{
			deleteDifxParameters(vis->params);
			vis->params = 0;
		}
		free(vis);
	}
}

int DifxVisRecordgetnext(DifxVisRecord *vis)
{
	const int MaxLineLength=99;
	int i, v;
	char line[MaxLineLength+1];
	char *ptr;

	/* reset the parameter list, if needed */
        if(vis->headerversion < 1)
	{
		resetDifxParameters(vis->params);
	}

	v = fread(&(vis->sync), sizeof(int), 1, vis->infile);

        if(vis->sync == VISRECORD_SYNC_WORD_DIFX1) /* old style ascii header */
	{
		line[0] = 'B';
		line[1] = 'A';
		line[2] = 'S';
		line[3] = 'E';
		ptr = fgets(line+4, MaxLineLength-4, vis->infile);
		if(ptr == 0)
		{
			return -1;
		}
		DifxParametersaddrow(vis->params, line);
		for(i = 1; i < 13; ++i)
		{
			ptr = fgets(line, MaxLineLength, vis->infile);
			if(ptr == 0)
			{
				return -1;
			}
			DifxParametersaddrow(vis->params, line);
		}
                vis->baseline = atoi(vis->params->rows[0].value);
		vis->mjd = atoi(vis->params->rows[1].value);
		vis->seconds = atof(vis->params->rows[2].value);
		vis->configindex = atoi(vis->params->rows[3].value);
		vis->sourceindex = atoi(vis->params->rows[4].value);
                vis->freqindex = atoi(vis->params->rows[5].value);
                vis->polpair[0] = vis->params->rows[6].value[0];
		vis->polpair[1] = vis->params->rows[6].value[1];
		vis->pulsarbin = atoi(vis->params->rows[7].value);
		vis->dataweight = atof(vis->params->rows[9].value);
		vis->uvw[0] = atof(vis->params->rows[10].value);
		vis->uvw[1] = atof(vis->params->rows[11].value);
                vis->uvw[2] = atof(vis->params->rows[12].value);
	}
	else if (vis->sync == VISRECORD_SYNC_WORD_DIFX2)
	{
		v = fread(&(vis->headerversion), sizeof(int), 1, vis->infile);
		if(vis->headerversion == 1) /* new style binary header */
		{
			v  = fread(&(vis->baseline), sizeof(int), 1, vis->infile);
			v += fread(&(vis->mjd), sizeof(int), 1, vis->infile);
			v += fread(&(vis->seconds), sizeof(double), 1, vis->infile);
			v += fread(&(vis->configindex), sizeof(int), 1, vis->infile);
			v += fread(&(vis->sourceindex), sizeof(int), 1, vis->infile);
			v += fread(&(vis->freqindex), sizeof(int), 1, vis->infile);
			v += fread(&(vis->polpair), 1, 2, vis->infile);
			v += fread(&(vis->pulsarbin), sizeof(int), 1, vis->infile);
			v += fread(&(vis->dataweight), sizeof(double), 1, vis->infile);
			v += fread(&(vis->uvw), sizeof(double), 3, vis->infile);
			if(v != 13)	/* 13 is total number of items read above */
			{
				return -1;
			}
		}
		else /* dunno what to do */
		{
			fprintf(stderr, "Error: DifxVisRecordgetnext: got a sync of %x and version of %d for visibility number %d\n", vis->sync, vis->headerversion, vis->visnum);

			return -1;
		}
	}
	else
	{
		fprintf(stderr, "Error: DifxVisRecordgetnext: got a sync of %x for visibility number %d\n", vis->sync, vis->visnum);

		return -1;
	}

	v = fread(vis->visdata, sizeof(cplx32f), vis->nchan, vis->infile);
	if(v < vis->nchan)
	{
		fprintf(stderr, "Error: DifxVisRecordgetnext: incomplete visibility record number %d\n",
			vis->visnum);

		return -1;
	}

	++vis->visnum;

	return vis->visnum;
}

int DifxVisRecordfindnext(DifxVisRecord *vis, int baseline, int freqid, 
	const char *pol)
{
	int v;

	for(;;)
	{
		v = DifxVisRecordgetnext(vis);
		if(v < 0)
		{
			return -1;
		}
#if 0
		printf("%d %d    %d %d    %s %s\n",
			baseline, vis->baseline,
			freqid, vis->freqindex,
			pol, vis->polpair);
#endif
		if(baseline >= 0 && baseline != vis->baseline)
		{
			continue;
		}
		if(pol && (strcmp(pol, vis->polpair) != 0))
		{
			continue;
		}
		if(freqid >= 0 && freqid != vis->freqindex)
		{
			continue;
		}
		break;
	}

	return vis->visnum;
}

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
		}
		if(vis->params)
		{
			deleteDifxParameters(vis->params);
		}
		free(vis);
	}
}

int DifxVisRecordgetnext(DifxVisRecord *vis)
{
	int i, v;
	char line[100];
	char *ptr;

	/* reset the parameter list */
	resetDifxParameters(vis->params);

	for(i = 0; i < 13; i++)
	{
		ptr = fgets(line, 99, vis->infile);
		if(ptr == 0)
		{
			return -1;
		}
		DifxParametersaddrow(vis->params, line);
	}

	v = fread(vis->visdata, sizeof(cplx32f), vis->nchan, vis->infile);
	if(v < vis->nchan)
	{
		return -1;
	}

	vis->visnum++;

	return vis->visnum;
}

int DifxVisRecordfindnext(DifxVisRecord *vis, int baseline, int freqid, 
	const char *pol)
{
	int bl, fi, v;
	const char *p;

	for(;;)
	{
		v = DifxVisRecordgetnext(vis);
		if(v < 0)
		{
			return -1;
		}
		bl = atoi(vis->params->rows[0].value);
		fi = atoi(vis->params->rows[5].value);
		p  = vis->params->rows[6].value;
#if 0
		printf("%d %d    %d %d    %s %s\n",
			baseline, bl,
			freqid, fi,
			pol, p);
#endif
		if(baseline >= 0 && baseline != bl)
		{
			continue;
		}
		if(pol && (strcmp(pol, p) != 0))
		{
			continue;
		}
		if(freqid >= 0 && freqid != fi)
		{
			continue;
		}
		break;
	}

	return vis->visnum;
}

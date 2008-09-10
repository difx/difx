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


DifxSource *newDifxSourceArray(int nSource)
{
	DifxSource *ds;
	int s;

	ds = (DifxSource *)calloc(nSource, sizeof(DifxSource));
	for(s = 0; s < nSource; s++)
	{
		ds[s].configId = -1;
		ds[s].spacecraftId = -1;
		ds[s].fitsSourceId = -1;
	}
	
	return ds;
}

void deleteDifxSourceArray(DifxSource *ds)
{
	if(ds)
	{
		free(ds);
	}
}

void fprintDifxSource(FILE *fp, const DifxSource *ds)
{
	fprintf(fp, "  DifxSource [%s] : %p\n", ds->name, ds);
	fprintf(fp, "    RA  =  %10.7f\n", ds->ra);
	fprintf(fp, "    Dec = %+11.7f\n", ds->dec);
	fprintf(fp, "    Calcode = %s\n", ds->calCode);
	fprintf(fp, "    Qualifier = %d\n", ds->qual);
	fprintf(fp, "    ConfigId = %d\n", ds->configId);
	fprintf(fp, "    SpacecraftId = %d\n", ds->spacecraftId);
	fprintf(fp, "    FITS SourceId = %d\n", ds->fitsSourceId);
}

void printDifxSource(const DifxSource *ds)
{
	fprintDifxSource(stdout, ds);
}

void fprintDifxSourceSummary(FILE *fp, const DifxSource *ds)
{
	fprintf(fp, "  %s\n", ds->name);
	fprintf(fp, "    RA  =  %10.7f\n", ds->ra);
	fprintf(fp, "    Dec = %+11.7f\n", ds->dec);
	fprintf(fp, "    Calcode = %s\n", ds->calCode);
	fprintf(fp, "    Qualifier = %d\n", ds->qual);
	if(ds->spacecraftId >= 0)
	{
		fprintf(fp, "    SpacecraftId = %d\n", ds->spacecraftId);
	}
}

void printDifxSourceSummary(const DifxSource *ds)
{
	fprintDifxSourceSummary(stdout, ds);
}

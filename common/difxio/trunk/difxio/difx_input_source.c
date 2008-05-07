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

void printDifxSource(const DifxSource *ds)
{
	printf("  DifxSource [%s] : %p\n", ds->name, ds);
	printf("    RA  =  %10.7f\n", ds->ra);
	printf("    Dec = %+11.7f\n", ds->dec);
	printf("    Calcode = %s\n", ds->calCode);
	printf("    Qualifier = %d\n", ds->qual);
	printf("    ConfigId = %d\n", ds->configId);
	printf("    SpacecraftId = %d\n", ds->spacecraftId);
	printf("    FITS SourceId = %d\n", ds->fitsSourceId);
}


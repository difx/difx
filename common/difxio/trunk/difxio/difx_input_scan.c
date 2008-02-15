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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "difxio/difx_input.h"


DifxScan *newDifxScanArray(int nScan)
{
	DifxScan *ds;
	int s;

	ds = (DifxScan *)calloc(nScan, sizeof(DifxScan));
	for(s = 0; s < nScan; s++)
	{
		ds[s].configId = -1;
		ds[s].sourceId = -1;
	}
	
	return ds;
}

void deleteDifxScanArray(DifxScan *ds, int nScan)
{
	int s;
	if(ds)
	{
		for(s = 0; s < nScan; s++)
		{
			if(ds[s].model)
			{
				deleteDifxModelArray(ds[s].model, 
					ds[s].nAntenna);
			}
		}
		free(ds);
	}
}

void printDifxScan(const DifxScan *ds)
{
	printf("  DifxScan [%s] : %p\n", ds->name, ds);
	printf("    Start = MJD %12.6f\n", ds->mjdStart);
	printf("    End   = MJD %12.6f\n", ds->mjdEnd);
	printf("    Calcode = %s\n", ds->calCode);
	printf("    Qualifier = %d\n", ds->qual);
	printf("    nPoint = %d\n", ds->nPoint);
	printf("    nAntenna %d\n", ds->nAntenna);
	printf("    Source ID = %d\n", ds->sourceId);
	printf("    Config ID = %d\n", ds->configId);
	if(ds->nPoint > 1 && ds->nAntenna > 1)
	{
		printDifxModel(ds->model[0] - 1);
		printDifxModel(ds->model[0]);
		printDifxModel(ds->model[1] - 1);
		printDifxModel(ds->model[1]);
	}
}


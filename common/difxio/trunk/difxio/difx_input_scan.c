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

void copyDifxScan(DifxScan *dest, const DifxScan *src,
	const int *antennaIdRemap, const int *configIdRemap)
{
	int i, srcAntenna, destAntenna;

	dest->mjdStart = src->mjdStart;
	dest->mjdEnd   = src->mjdEnd;
	dest->ra       = src->ra;
	dest->dec      = src->dec;
	dest->qual     = src->qual;
	strcpy(dest->name, src->name);
	for(i = 0; i < 4; i++)
	{
		dest->calCode[i] = src->calCode[i];
	}
	if(configIdRemap)
	{
		dest->configId = configIdRemap[src->configId];
	}
	else
	{
		dest->configId = src->configId;
	}
	dest->nPoint   = src->nPoint;

	/* figure out how many antennas needed in this scan */
	dest->nAntenna = src->nAntenna;
	for(srcAntenna = 0; srcAntenna < src->nAntenna; srcAntenna++)
	{
		destAntenna = antennaIdRemap[srcAntenna];
		if(destAntenna+1 > dest->nAntenna)
		{
			dest->nAntenna = destAntenna+1;
		}
	}

	/* allocate space for model info and copy from original.  This is
	 * somewhat brute force, low level stuff */
	dest->model = (DifxModel **)calloc(dest->nAntenna, sizeof(DifxModel *));
	for(srcAntenna = 0; srcAntenna < src->nAntenna; srcAntenna++)
	{
		destAntenna = antennaIdRemap[srcAntenna];

		/* FIXME -- move below to dupDifxModelColumn */
		dest->model[destAntenna] = (DifxModel *)calloc(dest->nPoint+3,
			sizeof(DifxModel));
		dest->model[destAntenna]++;
		memcpy(dest->model[destAntenna]-1, src->model[srcAntenna]-1,
			(dest->nPoint+3)*sizeof(DifxModel));
	}
}

/* FIXME -- should I merge sort the two lists to keep maximal ordering? */
DifxScan *mergeDifxScanArrays(const DifxScan *ds1, int nds1,
	const DifxScan *ds2, int nds2, 
	const int *antennaIdRemap, const int *configIdRemap)
{
	DifxScan *ds;
	int i;
	int swapOrder = 0;

	ds = newDifxScanArray(nds1+nds2);

	/* swap ds1 and ds2 if ds1 comes later */
	/* treat each scanarray separately -- assign each a Remap
	 * for antennaIds and configIds, set to Null for at least one */
	if(nds1 > 0 && nds2 > 0)
	{
		if(ds1[0].mjdStart > ds2[0].mjdStart)
		{
			swapOrder = 1;
		}
	}

	if(swapOrder)
	{
		/* write ds2 first */
		for(i = 0; i < nds2; i++)
		{
			copyDifxScan(ds + i, ds2 + i,
				antennaIdRemap, configIdRemap);
		}
		for(i = 0; i < nds1; i++)
		{
			copyDifxScan(ds + nds2 + i, ds1 + i, 
				0, 0);
		}
	}
	else
	{
		for(i = 0; i < nds1; i++)
		{
			copyDifxScan(ds + i, ds1 + i, 
				0, 0);
		}
		for(i = 0; i < nds2; i++)
		{
			copyDifxScan(ds + nds1 + i, ds2 + i,
				antennaIdRemap, configIdRemap);
		}
	}

	return ds;
}

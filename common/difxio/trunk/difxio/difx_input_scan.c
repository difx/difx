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
	const int *jobIdRemap, const int *antennaIdRemap, 
	const int *configIdRemap)
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
	if(jobIdRemap)
	{
		dest->jobId = jobIdRemap[src->jobId];
	}
	else
	{
		dest->jobId = src->jobId;
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

	/* allocate space for model info and copy from original. */
	dest->model = (DifxModel **)calloc(dest->nAntenna, sizeof(DifxModel *));
	for(srcAntenna = 0; srcAntenna < src->nAntenna; srcAntenna++)
	{
		destAntenna = antennaIdRemap[srcAntenna];

		dest->model[destAntenna] = dupDifxModelColumn(
			src->model[srcAntenna], dest->nPoint);
	}
}

/* Merge sort the two lists of scans.  This is intended to allow merging of
 * more than two DifxInputs in any order.
 */
DifxScan *mergeDifxScanArrays(const DifxScan *ds1, int nds1,
	const DifxScan *ds2, int nds2, 
	const int *jobIdRemap, const int *antennaIdRemap, 
	const int *configIdRemap)
{
	DifxScan *ds;
	int i=0, i1=0, i2=0, src;

	ds = newDifxScanArray(nds1+nds2);

	for(;;)
	{
		if(i1 >= nds1)
		{
			i1 = -1;
		}
		if(i2 >= nds2)
		{
			i2 = -1;
		}
		if(i1 < 0 && i2 < 0)
		{
			break;
		}

		/* determine which ScanArray to take from */
		if(i1 == -1)
		{
			src = 2;
		}
		else if(i2 == -1)
		{
			src = 1;
		}
		else if(ds1[i1].mjdStart <= ds2[0].mjdStart)
		{
			src = 1;
		}
		else
		{
			src = 2;
		}

		/* do the copy and increments */
		if(src == 1)
		{
			copyDifxScan(ds + i, ds1 + i1, 0, 0, 0);
			i++;
			i1++;
		}
		else
		{
			copyDifxScan(ds + i, ds2 + i2,
				jobIdRemap, antennaIdRemap, configIdRemap);
			i++;
			i2++;
		}
	}

	return ds;
}

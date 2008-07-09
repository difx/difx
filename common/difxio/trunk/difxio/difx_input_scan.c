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
			if(ds[s].im)
			{
				deleteDifxPolyModelArray(ds[s].im,
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
	printf("    startPoint = %d\n", ds->startPoint);
	printf("    nAntenna %d\n", ds->nAntenna);
	printf("    SourceId = %d\n", ds->sourceId);
	printf("    ConfigId = %d\n", ds->configId);
	if(ds->nPoint > 1 && ds->nAntenna > 1)
	{
		if(ds->model[0])
		{
			printDifxModel(ds->model[0] - 1);
			printDifxModel(ds->model[0]);
		}
		if(ds->model[1])
		{
			printDifxModel(ds->model[1] - 1);
			printDifxModel(ds->model[1]);
		}
	}
}

void copyDifxScan(DifxScan *dest, const DifxScan *src,
	const int *jobIdRemap, const int *antennaIdRemap, 
	const int *configIdRemap)
{
	int i, srcAntenna;

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
	if(configIdRemap && src->configId >= 0)
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

	/* allocate space for model info and copy from original. */
	dest->model = (DifxModel **)calloc(dest->nAntenna, 
		sizeof(DifxModel *));
	for(srcAntenna = 0; srcAntenna < src->nAntenna; srcAntenna++)
	{
		dest->model[srcAntenna] = dupDifxModelColumn(
			src->model[srcAntenna], dest->nPoint);
	}
	if(src->im)
	{
		dest->im = (DifxPolyModel **)calloc(dest->nAntenna, 
			sizeof(DifxPolyModel *));
		for(srcAntenna = 0; srcAntenna < src->nAntenna; srcAntenna++)
		{
			dest->im[srcAntenna] = dupDifxPolyModelColumn(
				src->im[srcAntenna], dest->nPoly);
		}
	}
}

/* Merge sort the two lists of scans.  This is intended to allow merging of
 * more than two DifxInputs in any order.
 */
DifxScan *mergeDifxScanArrays(const DifxScan *ds1, int nds1,
	const DifxScan *ds2, int nds2, 
	const int *jobIdRemap, const int *antennaIdRemap, 
	const int *configIdRemap, int *nds)
{
	DifxScan *ds;
	int i=0, i1=0, i2=0, src;

	*nds = nds1 + nds2;
	ds = newDifxScanArray(*nds);

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
		else if(ds1[i1].mjdStart <= ds2[i2].mjdStart)
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
			if(ds1[i1].configId >= 0)
			{
				copyDifxScan(ds + i, ds1 + i1, 0, 0, 0);
				i++;
			}
			i1++;
		}
		else
		{
			if(ds2[i2].configId >= 0)
			{
				copyDifxScan(ds + i, ds2 + i2, jobIdRemap, 
					antennaIdRemap, configIdRemap);
				i++;
			}
			i2++;
		}
	}

	*nds = i;

	return ds;
}

#if 0

/* this is a complicated thought in progress */


int DifxScansAppendable(const DifxScan *ds1, const DifxScan *ds2)
{
	if(!ds1 || !ds2)
	{
		fprintf(stderr, "ERROR : DifxScansAppendable : null input\n");
		return 0;
	}

	if(ds1->sourceId == ds2->sourceId &&
	   ds1->configId == ds2->configId &&
	   ds1->jobId    == ds2->jobId)
	{
		return 1;
	}

	return 0;
}

/* concattenate consecutive compatible scans */
int simplifyDifxScanArray(DifxScan *scans, int *nScan)
{
	int i, j, n;

	if(*nScan < 2)
	{
		return 0;
	}

	j = 0;				/* dest pointer */
	for(i = 1; i < *nScan; i++)	/* src pointer */
	{
		if(DifxScansAppendable(scans+j, scans+i))
		{
			scans[j].mjdEnd = scans[i].mjdEnd;
			n = scans[j].nPoint + scans[i].nPoint;
			for(a = 0; a < scans[j].nAntenna; a++)
			{
				scans[j].model[a]--;
				scans[j].model[a] = (DifxModel *)realloc(
					scans[j].model[a],
					(n+3)*sizeof(DifxModel));
				memcpy(scans[j].model[a]+scans[j].nPoint,
					scans[i].model[a]
				scans[j].model[a]++;
			}
			scans[j].nPoint = n;
		}
		else
		{
			j++;
		}
	}

	*nScan = j+1;

	return 0;
}


#endif

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


DifxPulsar *newDifxPulsarArray(int nPulsar)
{
	DifxPulsar *dp;

	dp = (DifxPulsar *)calloc(nPulsar, sizeof(DifxPulsar));

	return dp;
}

/* grow the size of a pulsar array by 1 element */
DifxPulsar *growDifxPulsarArray(DifxPulsar *dp, int origSize)
{
	dp = (DifxPulsar *)realloc(dp, (origSize+1)*sizeof(DifxPulsar));
	memset(dp+origSize, 0, sizeof(DifxPulsar));

	return dp;
}

void deleteDifxPulsarInternals(DifxPulsar *dp)
{
	if(dp->polyco)
	{
		deleteDifxPolycoArray(dp->polyco, dp->nPolyco);
		dp->polyco = 0;
	}
	if(dp->binEnd)
	{
		free(dp->binEnd);
		dp->binEnd = 0;
	}
	if(dp->binWeight)
	{
		free(dp->binWeight);
		dp->binWeight = 0;
	}
}

void deleteDifxPulsarArray(DifxPulsar *dp, int nPulsar)
{
	int p;

	if(!dp)
	{
		return;
	}

	for(p = 0; p < nPulsar; p++)
	{
		deleteDifxPulsarInternals(dp + p);
	}
	free(dp);
}

void fprintDifxPulsar(FILE *fp, const DifxPulsar *dp)
{
	fprintf(fp, "  Difx Pulsar : %p\n", dp);
	if(dp)
	{
		fprintf(fp, "    Filename = %s\n", dp->fileName);
		fprintf(fp, "    Number of bins = %d\n", dp->nBin);
		fprintf(fp, "    Scrunch = %d\n", dp->scrunch);
		fprintf(fp, "    Number of polycos : %d\n", dp->nPolyco);
		fprintDifxPolycoArray(fp, dp->polyco, dp->nPolyco);
	}
}

void printDifxPulsar(const DifxPulsar *dp)
{
	fprintDifxPulsar(stdout, dp);
}

int DifxPulsarArrayGetMaxPolyOrder(const DifxPulsar *dp, int nPulsar)
{
	int p;
	int n, max=0;

	if(nPulsar == 0)
	{
		return 0;
	}

	for(p = 0; p < nPulsar; p++)
	{
		n = DifxPolycoArrayGetMaxPolyOrder(dp[p].polyco, dp[p].nPolyco);
		if(n > max)
		{
			max = n;
		}
	}

	return max;
}

int isSameDifxPulsar(const DifxPulsar *dp1, const DifxPulsar *dp2)
{
	if(strcmp(dp1->fileName, dp2->fileName) == 0)
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

void copyDifxPulsar(DifxPulsar *dest, const DifxPulsar *src)
{
	snprintf(dest->fileName, DIFXIO_FILENAME_LENGTH, "%s", src->fileName);
	dest->nPolyco = src->nPolyco;
	dest->polyco = dupDifxPolycoArray(src->polyco, src->nPolyco);
	dest->nBin = src->nBin;
	dest->binEnd = (double *)malloc(dest->nBin*sizeof(double));
	memcpy(dest->binEnd, src->binEnd, dest->nBin*sizeof(double));
	dest->binWeight = (double *)malloc(dest->nBin*sizeof(double));
	memcpy(dest->binWeight, src->binWeight, dest->nBin*sizeof(double));
	dest->scrunch = src->scrunch;
}

DifxPulsar *dupDifxPulsarArray(const DifxPulsar *src, int nPulsar)
{
	DifxPulsar *dp;
	int p;

	dp = newDifxPulsarArray(nPulsar);
	for(p = 0; p < nPulsar; p++)
	{
		copyDifxPulsar(dp + p, src + p);
	}

	return dp;
}

/* merge two DifxPulsar tables into an new one.  pulsarIdRemap will contain the
 * mapping from dp2's old indices to that of the merged set
 */
DifxPulsar *mergeDifxPulsarArrays(const DifxPulsar *dp1, int ndp1,
	const DifxPulsar *dp2, int ndp2, int *pulsarIdRemap, int *ndp)
{
	DifxPulsar *dp = 0;
	int i, j;

	/* check for trivial cases */
	if(ndp1 <= 0 && ndp2 <= 0)
	{
		*ndp = 0;
		return 0;
	}
	if(ndp2 <= 0)
	{
		*ndp = ndp1;
		return dupDifxPulsarArray(dp1, ndp1);
	}
	if(ndp1 <= 0)
	{
		*ndp = ndp2;
		for(i = 0; i < ndp2; i++)
		{
			pulsarIdRemap[i] = i;
		}
		return dupDifxPulsarArray(dp2, ndp2);
	}

	/* both input arrays are non-trivial -- merge them */
	*ndp = ndp1;
	for(j = 0; j < ndp2; j++)
	{
		for(i = 0; i < ndp1; i++)
		{
			if(isSameDifxPulsar(dp1 + i, dp2 + j))
			{
				pulsarIdRemap[j] = i;
				break;
			}
		}
		if(i == ndp1)
		{
			pulsarIdRemap[j] = *ndp;
			(*ndp)++;
		}
	}

	/* Allocate and copy */
	dp = newDifxPulsarArray(*ndp);
	for(i = 0; i < ndp1; i++)
	{
		copyDifxPulsar(dp + i, dp1 + i);
	}
	for(j = 0; j < ndp2; j++)
	{
		i = pulsarIdRemap[j];
		if(i >= ndp1)
		{
			copyDifxPulsar(dp + i, dp2 + j);
		}
	}

	return dp;
}


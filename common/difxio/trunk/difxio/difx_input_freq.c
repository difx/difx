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


DifxFreq *newDifxFreqArray(int nFreq)
{
	DifxFreq *df;

	df = (DifxFreq *)calloc(nFreq, sizeof(DifxFreq));

	return df;
}

void deleteDifxFreqArray(DifxFreq *df)
{
	if(df)
	{
		free(df);
	}
}

void printDifxFreq(const DifxFreq *df)
{
	printf("  Difx Freq : %p\n", df);
	printf("    Freq = %f MHz\n", df->freq);
	printf("    Bandwidth = %f MHz\n", df->bw);
	printf("    Sideband = %c\n", df->sideband);
}

int isSameDifxFreq(const DifxFreq *df1, const DifxFreq *df2)
{
	if(df1->freq     == df2->freq &&
	   df1->bw       == df2->bw   &&
	   df1->sideband == df2->sideband)
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

void copyDifxFreq(DifxFreq *dest, const DifxFreq *src)
{
	dest->freq     = src->freq;
	dest->bw       = src->bw;
	dest->sideband = src->sideband;
}

/* merge two DifxFreq tables into an new one.  dfRemap will contain the
 * mapping from df2's old freq entries to that of the merged set
 */
DifxFreq *mergeDifxFreqArrays(const DifxFreq *df1, int ndf1,
	const DifxFreq *df2, int ndf2, int *freqIdRemap)
{
	int ndf;
	int i, j;
	DifxFreq *df;

	ndf = ndf1;

	/* first identify entries that differ and assign new freqIds to them */
	for(j = 0; j < ndf2; j++)
	{
		for(i = 0; i < ndf1; i++)
		{
			if(isSameDifxFreq(df1 + i, df2 + j))
			{
				freqIdRemap[j] = i;
				break;
			}
		}
		if(i == ndf1)
		{
			freqIdRemap[j] = ndf;
			ndf++;
		}
	}

	df = newDifxFreqArray(ndf);
	
	/* now copy df1 */
	for(i = 0; i < ndf1; i++)
	{
		copyDifxFreq(df + i, df1 + i);
	}

	/* now copy unique members of df2 */
	for(j = 0; j < ndf2; j++)
	{
		if(freqIdRemap[j] >= ndf1)
		{
			copyDifxFreq(df + freqIdRemap[j], df2 + j);
		}
	}

	return df;
}


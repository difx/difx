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
#include "difxio/difx_write.h"


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

void fprintDifxFreq(FILE *fp, const DifxFreq *df)
{
	fprintf(fp, "  Difx Freq : %p\n", df);
	fprintf(fp, "    Freq = %f MHz\n", df->freq);
	fprintf(fp, "    Bandwidth = %f MHz\n", df->bw);
	fprintf(fp, "    Sideband = %c\n", df->sideband);
}

void printDifxFreq(const DifxFreq *df)
{
	fprintDifxFreq(stdout, df);
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

int simplifyDifxFreqs(DifxInput *D)
{
	int f, f1;
	int f0;
	int d, r;
	int n0;

	n0 = D->nFreq;

	for(f=1;;)
	{
		if(f >= D->nFreq)
		{
			break;
		}

		for(f1 = 0; f1 < f; f1++)
		{
			if(isSameDifxFreq(D->freq+f, D->freq+f1))
			{
				break;
			}
		}
		if(f == f1)	/* no match found */
		{
			f++;	/* advance to next freq */
		}
		else		/* found match */
		{
			/* 1. Renumber this and all higher freqs */
			for(d = 0; d < D->nDatastream; d++)
			{
				for(r = 0; r < D->datastream[d].nFreq; r++)
				{
					f0 = D->datastream[d].freqId[r];
					if(f0 == f)
					{
						f0 = f1;
					}
					else if(f0 > f)
					{
						f0--;
					}
					D->datastream[d].freqId[r] = f0;
				}
			}

			/* 2. reduce number of freqs */
			D->nFreq--;

			/* 3. Delete this freq and bump up higher ones */
			if(f < D->nFreq)
			{
				for(f1 = f; f1 < D->nFreq; f1++)
				{
					copyDifxFreq(D->freq+f1, D->freq+f1+1);
				}
			}
		}
	}

	return n0 - D->nFreq;
}

/* merge two DifxFreq tables into an new one.  freqIdRemap will contain the
 * mapping from df2's old freq entries to that of the merged set
 */
DifxFreq *mergeDifxFreqArrays(const DifxFreq *df1, int ndf1,
	const DifxFreq *df2, int ndf2, int *freqIdRemap,
	int *ndf)
{
	DifxFreq *df;
	int i, j;

	*ndf = ndf1;

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
			freqIdRemap[j] = *ndf;
			(*ndf)++;
		}
	}

	/* Allocate and copy */
	df = newDifxFreqArray(*ndf);
	for(i = 0; i < ndf1; i++)
	{
		copyDifxFreq(df + i, df1 + i);
	}
	for(j = 0; j < ndf2; j++)
	{
		i = freqIdRemap[j];
		if(i >= ndf1)
		{
			copyDifxFreq(df + i, df2 + j);
		}
	}

	return df;
}

int writeDifxFreqArray(FILE *out, int nFreq, const DifxFreq *df)
{
	int n;
	int i;
	char sb[2];

	writeDifxLineInt(out, "FREQ ENTRIES", nFreq);
	n = 1;
	sb[1] = 0;

	for(i = 0; i < nFreq; i++)
	{
		writeDifxLineDouble1(out, "FREQ (MHZ) %d", i, 
			"%10.8f", df[i].freq);
		writeDifxLineDouble1(out, "BW (MHZ) %d", i,
			"%10.8f", df[i].bw);
		sb[0] = df[i].sideband;
		writeDifxLine1(out, "SIDEBAND %d", i, sb);
	}

	return n;
}

/***************************************************************************
 *   Copyright (C) 2008-2010 by Walter Brisken & Adam Deller               *
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

void DifxFreqAllocTones(DifxFreq *df, int nTone)
{
	if(!df)
	{
		fprintf(stderr, "Error: DifxFreqAllocTones: b = 0 nTone = %d\n", nTone);
		return;
	}
	if(df->tone)
	{
		free(df->tone);
		df->tone = 0;
	}

	if(nTone > 0)
	{
		df->tone = (int *)calloc(nTone, sizeof(int) );
		df->nTone = nTone;
	}
	else
	{
		df->nTone = 0;
	}
}

void deleteDifxFreqInternals(DifxFreq *df)
{
	DifxFreqAllocTones(df, 0);
}

void deleteDifxFreqArray(DifxFreq *df, int nFreq)
{
	int f;

	if(df)
	{
		for(f = 0; f < nFreq; f++)
		{
			deleteDifxFreqInternals(df + f);
		}
		free(df);
	}
}

void fprintDifxFreq(FILE *fp, const DifxFreq *df)
{
	int t;

	fprintf(fp, "  Difx Freq : %p\n", df);
	fprintf(fp, "    Freq = %f MHz\n", df->freq);
	fprintf(fp, "    Bandwidth = %f MHz\n", df->bw);
	fprintf(fp, "    Sideband = %c\n", df->sideband);
        fprintf(fp, "    Num Chan = %d\n", df->nChan);
        fprintf(fp, "    Spec Avg = %d\n", df->specAvg);
        fprintf(fp, "    Oversamp = %d\n", df->overSamp);
        fprintf(fp, "    Decimation = %d\n", df->decimation);
	fprintf(fp, "    Num tones = %d\n", df->nTone);
	if(df->nTone > 0)
	{
		fprintf(fp, "     ");
		for(t = 0; t < df->nTone; t++)
		{
			fprintf(fp, " %d", df->tone[t]);
		}
		fprintf(fp, "\n");
	}
}

void printDifxFreq(const DifxFreq *df)
{
	fprintDifxFreq(stdout, df);
}

int isSameDifxFreqToneSet(const DifxFreq *df1, const DifxFreq *df2)
{
	int t;

	if(df1->nTone != df2->nTone)
	{
		return 0;
	}

	if(df1->nTone > 0)
	{
		for(t = 0; t < df1->nTone; t++)
		{
			if(df1->tone[t] != df2->tone[t])
			{
				return 0;
			}
		}
	}

	return 1;
}

int isSameDifxFreq(const DifxFreq *df1, const DifxFreq *df2)
{
	if(df1->freq       == df2->freq &&
	   df1->bw         == df2->bw   &&
	   df1->sideband   == df2->sideband &&
	   df1->specAvg    == df2->specAvg &&
	   df1->nChan      == df2->nChan &&
	   df1->overSamp   == df2->overSamp &&
	   df1->decimation == df2->decimation &&
	   isSameDifxFreqToneSet(df1, df2) )
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
	int t;

	dest->freq       = src->freq;
	dest->bw         = src->bw;
	dest->sideband   = src->sideband;
	dest->nChan      = src->nChan;
	dest->specAvg    = src->specAvg;
	dest->overSamp   = src->overSamp;
	dest->decimation = src->decimation;

	DifxFreqAllocTones(dest, src->nTone);
	if(src->nTone > 0)
	{
		for(t = 0; t < src->nTone; t++)
		{
			dest->tone[t] = src->tone[t];
		}
	}
}

int simplifyDifxFreqs(DifxInput *D)
{
	int f, f1;
	int f0;
	int d, r;
	int n0;

	n0 = D->nFreq;
	if(n0 < 2)
	{
		return 0;
	}

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
				for(r = 0; r < D->datastream[d].nRecFreq; r++)
				{
					f0 = D->datastream[d].recFreqId[r];
					if(f0 == f)
					{
						f0 = f1;
					}
					else if(f0 > f)
					{
						f0--;
					}
					D->datastream[d].recFreqId[r] = f0;
				}
				for(r = 0; r < D->datastream[d].nZoomFreq; r++)
				{
					f0 = D->datastream[d].zoomFreqId[r];
					if(f0 == f)
					{
						f0 = f1;
					}
					else if(f0 > f)
					{
						f0--;
					}
					D->datastream[d].zoomFreqId[r] = f0;
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
	int n, i, t;
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
                writeDifxLineInt1(out, "NUM CHANNELS %d", i, df[i].nChan);
		writeDifxLineInt1(out, "CHANS TO AVG %d", i, df[i].specAvg);
		writeDifxLineInt1(out, "OVERSAMPLE FAC. %d", i, df[i].overSamp);
		writeDifxLineInt1(out, "DECIMATION FAC. %d", i, df[i].decimation);
		writeDifxLineInt1(out, "PHASE CALS %d OUT", i, df[i].nTone);
		if(df[i].nTone > 0)
		{
			for(t = 0; t < df[i].nTone; t++)
			{
				writeDifxLineInt2(out, "PHASE CAL %d/%d INDEX", i, t, df[i].tone[t]);
			}
		}
	}

	return n;
}

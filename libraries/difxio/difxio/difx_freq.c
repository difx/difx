/***************************************************************************
 *   Copyright (C) 2008-2022 by Walter Brisken, Adam Deller & Helge Rottmann *
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
#include <math.h>
#include "difxio/difx_input.h"
#include "difxio/difx_write.h"

const char freqMergeModeNames[][MAX_FREQ_MERGE_MODE_STRING_LENGTH] =
{
	"strict",
	"union",
	"illegal"
};

enum FreqMergeMode stringToFreqMergeMode(const char *str)
{
	enum FreqMergeMode f;

	for(f = 0; f < NumFreqMergeModes; ++f)
	{
		if(strcasecmp(str, freqMergeModeNames[f]) == 0)
		{
			break;
		}
	}

	return f;
}


DifxFreq *newDifxFreqArray(int nFreq)
{
	DifxFreq *df;

	df = (DifxFreq *)calloc(nFreq+1, sizeof(DifxFreq));

	return df;
}

void DifxFreqAllocTones(DifxFreq *df, int nTone)
{
	if(!df)
	{
		fprintf(stderr, "Error: DifxFreqAllocTones: df = 0 nTone = %d\n", nTone);
		return;
	}
	if(df->tone)
	{
		free(df->tone);
		df->tone = 0;
	}

	if(nTone > 0)
	{
		df->tone = (int *)calloc(nTone+1, sizeof(int) );
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
	if(df)
	{
		int f;

		for(f = 0; f < nFreq; ++f)
		{
			deleteDifxFreqInternals(df + f);
		}
		free(df);
	}
}

void fprintDifxFreq(FILE *fp, const DifxFreq *df)
{
	fprintf(fp, "  Difx Freq : %p\n", df);
	fprintf(fp, "    Freq = %f MHz\n", df->freq);
	fprintf(fp, "    Bandwidth = %f MHz\n", df->bw);
	fprintf(fp, "    Sideband = %c\n", df->sideband);
	fprintf(fp, "    Num Chan = %d\n", df->nChan);
	fprintf(fp, "    Spec Avg = %d\n", df->specAvg);
	if(df->rxName[0])
	{
		fprintf(fp, "    Rx name = %s\n", df->rxName);
	}
	fprintf(fp, "    Num tones = %d  [", df->nTone);
	if(df->nTone > 0)
	{
		int t;

		for(t = 0; t < df->nTone; ++t)
		{
			if(t > 0)
			{
				fprintf(fp, " ");
			}
			fprintf(fp, "%d", df->tone[t]);
		}
	}
	fprintf(fp, "]\n");
}

void printDifxFreq(const DifxFreq *df)
{
	fprintDifxFreq(stdout, df);
}

int isSameDifxFreqToneSet(const DifxFreq *df1, const DifxFreq *df2)
{
	if(df1->nTone != df2->nTone)
	{
		return 0;
	}

	if(df1->nTone > 0)
	{
		int t;

		for(t = 0; t < df1->nTone; ++t)
		{
			if(df1->tone[t] != df2->tone[t])
			{
				return 0;
			}
		}
	}

	return 1;
}

int isSameDifxFreq(const DifxFreq *df1, const DifxFreq *df2, int AllPcalTones )
{
        if ( AllPcalTones == 0 ){
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
        else {
	     if(df1->freq       == df2->freq &&
	        df1->bw         == df2->bw   &&
	        df1->sideband   == df2->sideband &&
	        df1->specAvg    == df2->specAvg &&
	        df1->nChan      == df2->nChan &&
	        df1->overSamp   == df2->overSamp &&
	        df1->decimation == df2->decimation )
	     {
		     return 1;
	     }
	     else
	     {
		     return 0;
	     }
        }
}

int isDifxIFInsideDifxFreq(const DifxIF *di, const DifxFreq *df)
{
	double imax, imin, fmax, fmin;
	const double epsilon=1.0e-7;	/* 0.1 Hz tolerance */

	if(di->sideband == 'U')
	{
		imin = di->freq;
		imax = di->freq + di->bw;
	}
	else if(di->sideband == 'L')
	{
		imin = di->freq - di->bw;
		imax = di->freq;
	}
	else
	{
		fprintf(stderr, "Error: isDifxIFInsideDifxFreq: encountered DifxIF with sideband = '%c'; this needs to be one of U or L\n", di->sideband);

		exit(0);
	}

	if(df->sideband == 'U')
	{
		fmin = df->freq;
		fmax = df->freq + df->bw;
	}
	else if(df->sideband == 'L')
	{
		fmin = df->freq - df->bw;
		fmax = df->freq;
	}
	else
	{
		fprintf(stderr, "Error: isDifxIFInsideDifxFreq: encountered DifxFreq with sideband = '%c'; this needs to be one of U or L\n", df->sideband);

		exit(0);
	}

	return (imin + epsilon >= fmin && imax - epsilon <= fmax);
}

void copyDifxFreq(DifxFreq *dest, const DifxFreq *src)
{
	if(dest != src)
	{
		if(dest == 0 || src == 0)
		{
			fprintf(stderr, "Error: copyDifxFreq: src=%p dest=%p but both must be non-null\n", src, dest);

			exit(EXIT_FAILURE);
		}
		*dest = *src;
		dest->tone = 0;

		if(src->nTone > 0)
		{
			int t;

			DifxFreqAllocTones(dest, src->nTone);
			
			for(t = 0; t < src->nTone; ++t)
			{
				dest->tone[t] = src->tone[t];
			}
		}
	}
	else
	{
		fprintf(stderr, "Developer error: copyDifxFreq: src = dest.  Bad things will be coming...\n");
	}
}

/* this function possibly rearranges the FREQ table entries to be in 
 * an order such that for each datastream all record freqs precede
 * all zoom freqs */
int reorderDifxFreqs(DifxInput *D)
{
	int n;			/* number of frequencies */
	int i, j, d, o;
	char **lessThan;	/* if lessThan[a][b] then a needs to precede b */
	int *newOrder;
	DifxFreq *oldFreq;
	int nError = 0;

	/* 1. Initialize arrays */

	n = D->nFreq;

	newOrder = (int *)calloc(n+1, sizeof(int));
	for(i = 0; i < n; ++i)
	{
		newOrder[i] = -1;	/* not yet determined */
	}

	lessThan = (char **)calloc(n+1, sizeof(char *));
	for(i = 0; i < n; ++i)
	{
		lessThan[i] = (char *)calloc(n, sizeof(char));	/* zero is false */
	}

	/* 2. populate lessThan array */

	for(d = 0; d < D->nDatastream; ++d)
	{
		if(D->datastream[d].nRecFreq == 0 && D->datastream[d].nZoomFreq == 0)
		{
			continue;	/* no constraint here */
		}
		for(i = 0; i < D->datastream[d].nRecFreq; ++i)
		{
			int recFreqIndex;

			recFreqIndex = D->datastream[d].recFreqId[i];
			if(recFreqIndex < 0 || recFreqIndex >= n)
			{
				fprintf(stderr, "Developer error: reorderDifxFreqs: recFreqIndex=%d nFreq=%d\n", recFreqIndex, n);

				exit(EXIT_FAILURE);
			}

			for(j = 0; j < D->datastream[d].nZoomFreq; ++j)
			{
				int zoomFreqIndex;

				zoomFreqIndex = D->datastream[d].zoomFreqId[j];
				if(zoomFreqIndex < 0 || zoomFreqIndex >= n)
				{
					fprintf(stderr, "Developer error: reorderDifxFreqs: zoomFreqIndex=%d nFreq=%d\n", zoomFreqIndex, n);

					exit(EXIT_FAILURE);
				}

				lessThan[recFreqIndex][zoomFreqIndex] = 1;

				if(recFreqIndex == zoomFreqIndex)
				{
					int antId = D->datastream[d].antennaId;

					fprintf(stderr, "Error: Antenna %s has a zoom band that is the same as a parent band. If intentional, specify a non-zero PCal tone interval for parent to make bands 'different'\n", D->antenna[antId].name);
					fprintDifxFreq(stderr, D->freq + recFreqIndex);
					++nError;
				}
			}
		}
	}

	if(nError > 0)
	{
		exit(EXIT_FAILURE);
	}

	/* 3. iterate, collecting those that aren't preceded by anything else */

	for(o = 0; o < n; ++o)
	{
		for(i = 0; i < n; ++i)
		{
			if(newOrder[i] >= 0)
			{
				continue;	/* this one already found */
			}
			for(j = 0; j < n; ++j)
			{
				if(j == i)
				{
					continue;
				}
				if(lessThan[j][i])	/* i not ready to be selected */
				{
					break;
				}
			}
			if(j == n)	/* nothing proceeds i */
			{
				for(j = 0; j < n; ++j)
				{
					lessThan[j][i] = lessThan[i][j] = 0;	/* take i out of circulation */
				}

				newOrder[i] = o;

				break;
			}
		}
		if(i == n)	/* bad news: no option found */
		{
			break;
		}
	}

	/* 4. Partial clean up */

	for(i = 0; i < n; ++i)
	{
		free(lessThan[i]);
	}
	free(lessThan);

	if(o < n)	/* failure! */
	{
		free(newOrder);
		fprintf(stderr, "Error: reorderDifxFreqs: No ordering of FREQ table will satisfy mpifxcorr\n");
		fprintf(stderr, "Files will be written but mpifxcorr will not cope with them!\n");

		return -1;
	}

	/* 5. Do reordering */

	oldFreq = D->freq;
	D->freq = newDifxFreqArray(n);

	for(i = 0; i < n; ++i)
	{
		copyDifxFreq(D->freq+newOrder[i], oldFreq+i);
	}

	for(d = 0; d < D->nDatastream; ++d)
	{
		for(i = 0; i < D->datastream[d].nRecFreq; ++i)
		{
			D->datastream[d].recFreqId[i] = newOrder[D->datastream[d].recFreqId[i]];
		}
		for(i = 0; i < D->datastream[d].nZoomFreq; ++i)
		{
			D->datastream[d].zoomFreqId[i] = newOrder[D->datastream[d].zoomFreqId[i]];
		}
	}

	for(j = 0; j < D->nFreqUnsimplified; j++)
	{
		D->freqIdRemap[j] = newOrder[D->freqIdRemap[j]];
	}

	/* 6. Final cleanup */

	deleteDifxFreqArray(oldFreq, n);
	free(newOrder);

	return 0;
}


int simplifyDifxFreqs(DifxInput *D)
{
	int f, n0, v, b;
	const int DEBUG_BOOKKEEPING = 0;
	DifxFreq *oldFreq = NULL; // note: use only in if(DEBUG_BOOKKEEPING){} blocks!

	n0 = D->nFreq;
	D->nFreqUnsimplified = D->nFreq;
	if(n0 < 2)
	{
		return 0;
	}

	if(DEBUG_BOOKKEEPING)
	{
		oldFreq = newDifxFreqArray(D->nFreqUnsimplified);
		for(f = 0; f < D->nFreqUnsimplified; ++f)
		{
			copyDifxFreq(oldFreq+f, D->freq+f);
		}

		printf("simplifyDifxFreqs(): pre-simplify status is nFreqUnsimplified=%d nFreq=%d freqIdRemap={", D->nFreqUnsimplified, D->nFreq);
		for(f = 0; f < D->nFreqUnsimplified; ++f)
		{
		  printf(" %d:%d", f, D->freqIdRemap[f]);
		}
		printf(" }\n");
	}

	/* Pass I: make a linear remapping list to weed out duplicate D->freqs, without modifying D->freqs in the loop */
	for(f = 1; f < D->nFreqUnsimplified; f++)
	  {
		int f1, f2;

		for(f1 = 0; f1 < f; ++f1)
		{
			if(isSameDifxFreq(D->freq+f, D->freq+f1, D->AllPcalTones))
			{
				break;
			}
		}
		if(f1 != f) /* found match */
		{
			/* repoint the top freq 'f' to the matching earlier freq 'f1' */
			D->freqIdRemap[f] = f1;

			/* simulate deletion of duplicate freq 'f' as under Pass II, by shifting down the block of remaining freqs */
			for(f2 = f + 1; f2 < D->nFreqUnsimplified; f2++)
			{
				--D->freqIdRemap[f2];
			}
		}
	}

	/* Pass II: actually simplify, modifying the D->freqs list during the loop */
	for(f = 1;;)
	{
		int f1;

		if(f >= D->nFreq)
		{
			break;
		}

		for(f1 = 0; f1 < f; ++f1)
		{
			if(isSameDifxFreq(D->freq+f, D->freq+f1, D->AllPcalTones))
			{
				break;
			}
		}
		if(f == f1)	/* no match found */
		{
			++f;	/* advance to next freq */
		}
		else		/* found match */
		{
			int d;

			/* 1. Renumber this and all higher freqs */
			for(d = 0; d < D->nDatastream; ++d)
			{
				int r;

				for(r = 0; r < D->datastream[d].nRecFreq; ++r)
				{
					int f0;

					f0 = D->datastream[d].recFreqId[r];
					if(f0 == f)
					{
						f0 = f1;
					}
					else if(f0 > f)
					{
						--f0;
					}
					D->datastream[d].recFreqId[r] = f0;
				}
				for(r = 0; r < D->datastream[d].nZoomFreq; ++r)
				{
					int f0;

					f0 = D->datastream[d].zoomFreqId[r];
					if(f0 == f)
					{
						f0 = f1;
					}
					else if(f0 > f)
					{
						--f0;
					}
					D->datastream[d].zoomFreqId[r] = f0;
				}
			}

			/* 2. reduce number of freqs */
			--D->nFreq;

			/* 3. Delete this freq and drop down higher ones */
			if(f < D->nFreq)
			{
				for(f1 = f; f1 < D->nFreq; ++f1)
				{
					copyDifxFreq(D->freq+f1, D->freq+f1+1);
				}
			}
		}
	}

	/* 4. Reorder the frequency table so that Datastream rec freq indices precede zoom freq indices */
	v = reorderDifxFreqs(D); // note: also keeps D->freqIdRemap[] in sync with the reordering

	/* 5. DiFX Outputbands -specific: adjust Baseline TARGET FREQ refs to reflect the above remap+reorder */
	// note: difx_baseline.c might be another place to apply this, otoh all messing with the original FREQ table is confined here
	for(b = 0; b < D->nBaseline; b++)
	{
		for(f = 0; f < D->baseline[b].nFreq; f++)
		{
			D->baseline[b].destFq[f] = D->freqIdRemap[D->baseline[b].destFq[f]];
		}
	}

	if(DEBUG_BOOKKEEPING)
	{
		printf("simplifyDifxFreqs(): post-reorder status is nFreqUnsimplified=%d nFreq=%d freqIdRemap={", D->nFreqUnsimplified, D->nFreq);
		for(f = 0; f < D->nFreqUnsimplified; ++f)
		{
			printf(" %d:%d", f, D->freqIdRemap[f]);
		}
		printf(" }\n");

		/* Verify that the remapping table is intact */
		for(f = 0; f < D->nFreqUnsimplified; ++f)
		{
			if(!isSameDifxFreq(oldFreq+f, D->freq+D->freqIdRemap[f], D->AllPcalTones))
			{
				printf("Programmer error in difxio simplifyDifxFreqs: freq Id %d after simplification now points to incompatible freq %d\n", f, D->freqIdRemap[f]);
				printf("old %d ", f);
				printDifxFreq(oldFreq+f);
				printf("new %d ", D->freqIdRemap[f]);
				printDifxFreq(D->freq+D->freqIdRemap[f]);
			}
		}

		deleteDifxFreqArray(oldFreq, D->nFreqUnsimplified);
	}

	if(v < 0)
	{
		return 0;
	}
	else
	{
		return n0 - D->nFreq;
	}
}

/* @brief merge two DifxFreq tables into an new one.  freqIdRemap will contain the
 * mapping from df2's old freq entries to that of the merged set
 */
DifxFreq *mergeDifxFreqArrays(const DifxFreq *df1, int ndf1, const DifxFreq *df2, int ndf2, int *freqIdRemap, int *ndf, int AllPcalTones)
{
	DifxFreq *df;
	int i, j;

	*ndf = ndf1;

	/* first identify entries that differ and assign new freqIds to them */
	for(j = 0; j < ndf2; ++j)
	{
		for(i = 0; i < ndf1; ++i)
		{
			if(isSameDifxFreq(df1 + i, df2 + j, AllPcalTones))
			{
				freqIdRemap[j] = i;
				break;
			}
		}
		if(i == ndf1)
		{
			freqIdRemap[j] = *ndf;
			++*ndf;
		}
	}

	/* Allocate and copy */
	df = newDifxFreqArray(*ndf);
	for(i = 0; i < ndf1; ++i)
	{
		copyDifxFreq(df + i, df1 + i);
	}
	for(j = 0; j < ndf2; ++j)
	{
		i = freqIdRemap[j];
		if(i >= ndf1)
		{
			copyDifxFreq(df + i, df2 + j);
		}
	}

	return df;
}

/* Use this to see if a frequency or bandwidth (in Hz) has meaningful
 * non-integral value (e.g., differs from integer by more than 1 part
 * in 10^4 or 100 microHz, less than ten turns per day)
 *
 * Note that use of DiFX with fractional Hz bandwidth or frequency
 * likely results in unstable phases
 */
static int isQuasiInteger(double d)
{
	return fabs(d - (long long int)d) < 0.0001 ? 1 : 0;
}

int writeDifxFreqArray(FILE *out, int nFreq, const DifxFreq *df)
{
	int n, i;
	char sb[2];

	writeDifxLineInt(out, "FREQ ENTRIES", nFreq);
	n = 1;
	sb[1] = 0;

	for(i = 0; i < nFreq; ++i)
	{
		/* For FREQ and BW: use the above function to determine
		 * how many digits to write.  It seems that some spurious
		 * non-zero digits can be appended in some cases.
		 */
		if(isQuasiInteger(df[i].freq*1000000.0))
		{
			writeDifxLineDouble1(out, "FREQ (MHZ) %d", i, "%8.6f", df[i].freq);
		}
		else
		{
			writeDifxLineDouble1(out, "FREQ (MHZ) %d", i, "%17.15f", df[i].freq);
		}
		if(isQuasiInteger(df[i].bw*1000000.0))
		{
			writeDifxLineDouble1(out, "BW (MHZ) %d", i, "%8.6f", df[i].bw);
		}
		else
		{
			writeDifxLineDouble1(out, "BW (MHZ) %d", i, "%17.15f", df[i].bw);
		}
		sb[0] = df[i].sideband;
		writeDifxLine1(out, "SIDEBAND %d", i, sb);
		if(strlen(df[i].rxName) > 0)
		{
			writeDifxLine1(out, "RX NAME %d", i, df[i].rxName);
		}
                writeDifxLineInt1(out, "NUM CHANNELS %d", i, df[i].nChan);
		writeDifxLineInt1(out, "CHANS TO AVG %d", i, df[i].specAvg);
		writeDifxLineInt1(out, "OVERSAMPLE FAC. %d", i, df[i].overSamp);
		writeDifxLineInt1(out, "DECIMATION FAC. %d", i, df[i].decimation);
		writeDifxLineInt1(out, "PHASE CALS %d OUT", i, df[i].nTone);
		if(df[i].nTone > 0)
		{
			int t;

			for(t = 0; t < df[i].nTone; ++t)
			{
				writeDifxLineInt2(out, "PHASE CAL %d/%d INDEX", i, t, df[i].tone[t]);
			}
		}
	}

	return n;
}

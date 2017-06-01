/***************************************************************************
 *   Copyright (C) 2017 by Walter Brisken                                  *
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
// $Id: difx_config.c 7647 2017-02-16 20:32:32Z WalterBrisken $
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/difxio/trunk/difxio/difx_config.c $
// $LastChangedRevision: 7647 $
// $Author: WalterBrisken $
// $LastChangedDate: 2017-02-16 14:32:32 -0600 (Thu, 16 Feb 2017) $
//
//============================================================================

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "difxio/difx_input.h"

DifxFreqSet *newDifxFreqSetArray(int nFreqSet)
{
	DifxFreqSet *dfs;

	dfs = (DifxFreqSet *)calloc(nFreqSet, sizeof(DifxFreqSet));

	return dfs;
}

void allocateDifxFreqSetFreqMap(DifxFreqSet *dfs, int nFreq)
{
	int f;

	dfs->nFreq = nFreq;
	dfs->freqId2IF = (int *)calloc(nFreq+1, sizeof(int));

	for(f = 0; f < nFreq; ++f)
	{
		dfs->freqId2IF[f] = -1;	/* meaning not set */
	}
	dfs->freqId2IF[nFreq] = -2;	/* end of array marker */
}

void deleteDifxFreqSetInternals(DifxFreqSet *dfs)
{
	if(dfs->IF)
	{
		free(dfs->IF);
		dfs->IF = 0;
	}
	if(dfs->freqId2IF)
	{
		free(dfs->freqId2IF);
		dfs->freqId2IF = 0;
	}
}

void deleteDifxFreqSetArray(DifxFreqSet *dfs, int nFreqSet)
{
	int fsId;

	if(dfs)
	{
		for(fsId = 0; fsId < nFreqSet; ++fsId)
		{
			deleteDifxFreqSetInternals(dfs + fsId);
		}
		free(dfs);
	}
}

void fprintDifxFreqSet(FILE *fp, const DifxFreqSet *dfs)
{
	fprintf(fp, "  Difx Freq Set : %p\n", dfs);

	if(dfs->freqId2IF)
	{
		int i;

		fprintf(fp, "    frequency to IF map =");
		for(i = 0; i < dfs->nFreq; ++i)
		{
			fprintf(fp, " %d", dfs->freqId2IF[i]);
		}
		fprintf(fp, "\n");
	}
	fprintf(fp, "    nIF = %d\n", dfs->nIF);
	if(dfs->nIF > 0)
	{
		int i;

		for(i = 0; i < dfs->nIF; ++i)
		{
			fprintDifxIF(fp, dfs->IF + i);
		}
	}
}

void printDifxFreqSet(const DifxFreqSet *dfs)
{
	fprintDifxFreqSet(stdout, dfs);
}

void fprintDifxFreqSetSummary(FILE *fp, const DifxFreqSet *dfs)
{
	fprintf(fp, "  Difx Freq Set : %p\n", dfs);

	if(dfs->nIF > 0)
	{
		int i;

		for(i = 0; i < dfs->nIF; ++i)
		{
			fprintDifxIFSummary(fp, dfs->IF + i);
		}
	}
}

void printDifxFreqSetSummary(const DifxFreqSet *dfs)
{
	fprintDifxFreqSetSummary(stdout, dfs);
}

/* does not compare the fitsFreqId value */
int isSameDifxFreqSet(const DifxFreqSet *dfs1, const DifxFreqSet *dfs2)
{
	if(dfs1->nIF != dfs2->nIF)
	{
		return 0;
	}
	if(dfs1->IF && dfs2->IF)
	{
		int i;

		for(i = 0; i < dfs1->nIF; ++i)
		{
			if(!isSameDifxIF(dfs1->IF + i, dfs2->IF + i))
			{
				return 0;
			}
		}
	}
	else if(dfs1->IF || dfs2->IF)
	{
		return 0;
	}

	return 1;
}

int isDifxFreqSetSX(const DifxFreqSet *dfs)
{
	int i;
	int hasS = 0;
	int hasX = 0;

	for(i = 0; i < dfs->nIF; ++i)
	{
		double freq;

		freq = dfs->IF[i].freq;
		if(freq > 2.0 && freq < 3.8)
		{
			hasS = 1;
		}
		if(freq > 7.5 && freq < 10.5)
		{
			hasX = 1;
		}
	}

	return (hasS & hasX);
}

void copyDifxFreqSet(DifxFreqSet *dest, const DifxFreqSet *src)
{
	dest->nIF = src->nIF;
	if(dest->nIF > 0)
	{
		int i;

		dest->IF = newDifxIFArray(dest->nIF);
		for(i = 0; i < dest->nIF; ++i)
		{
			copyDifxIF(dest->IF + i, src->IF + i);
		}
	}
	else
	{
		dest->IF = 0;
	}
	if(dest->nFreq > 0)
	{
		int f;

		allocateDifxFreqSetFreqMap(dest, dest->nFreq);
		for(f = 0; f < dest->nFreq; ++f)
		{
			dest->freqId2IF[f] = src->freqId2IF[f];
		}
	}
	else
	{
		dest->nFreq = 0;
		dest->freqId2IF = 0;
	}
}

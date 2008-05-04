/***************************************************************************
 *   Copyright (C) 2007 by Walter Brisken                                  *
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


/* assumes map is a heirarchical array, each array being null terminated. */
void deleteBaselineFreq2IF(int ***map)
{
	int a1, a2;
	
	if(map)
	{
		for(a1 = 0; map[a1]; a1++)
		{
			for(a2 = 0; map[a1][a2]; a2++)
			{
				free(map[a1][a2]);
			}
			free(map[a1]);
		}
		free(map);
	}
}

void printBaselineFreq2IF(int ***map, int nAnt, int nChan)
{
	int a1, a2, c;

	printf("      nAnt = %d  nChan = %d\n", nAnt, nChan);

	for(a2 = 0; a2 < nAnt; a2++)
	{
		printf("      ");
		for(c = 0; c < nChan; c++)
		{
			for(a1 = 0; a1 < nAnt; a1++)
			{
				printf("%2d", map[a1][a2][c]);
			}
			printf("   ");
		}
		printf("\n");
	}
}

int makeBaselineFreq2IF(DifxInput *D, int configId)
{
	int ***map;
	DifxConfig *dc;
	DifxBaseline *db;
	DifxDatastream *dd;
	int baselineId;
	int a, b,  d, f;
	int a1, a2;
	int rcA, rcB, fqA, fqB, bandId, nFreq;

	if(!D)
	{
		fprintf(stderr, "makeBaselineFreq2IF: D = 0\n");
		return -1;
	}
	if(!D->config || D->nConfig <= configId || configId < 0)
	{
		fprintf(stderr, "makeBaselineFreq2IF: "
			"configId problem.\n");
		return -1;
	}
	dc = D->config + configId;

	/* map[Antenna1][Antenna2][recChan] -- all indices 0-based */
	/* allocate first two dimensions larger than needed for null pads */
	map = (int ***)calloc(D->nAntenna+1, sizeof(int **));
	for(a1 = 0; a1 < D->nAntenna; a1++)
	{
		map[a1] = (int **)calloc(D->nAntenna+1, sizeof(int *));
		for(a2 = 0; a2 < D->nAntenna; a2++)
		{
			map[a1][a2] = (int *)calloc(dc->nIF, sizeof(int));
		}
	}
	dc->baselineFreq2IF = map;

	/* Fill in cross corr terms */
	for(b = 0; b < dc->nBaseline; b++)
	{
		baselineId = dc->baselineId[b];
		if(baselineId < 0 || baselineId >= D->nBaseline)
		{
			printf("Error! baselineId=%d\n", baselineId);
		}
		db = D->baseline + baselineId;
		if(db->dsA < 0 || db->dsB < 0)
		{	
			printf("Error! dsA=%d dsB=%d\n", db->dsA, db->dsB);
		}
		a1 = D->datastream[db->dsA].antennaId;
		a2 = D->datastream[db->dsB].antennaId;
		if(a1 < 0 || a1 >= D->nAntenna)
		{
			printf("Error! makeBaselineFreq2IF : a1=%d nA=%d",
				a1, D->nAntenna);
		}
		if(a2 < 0 || a2 >= D->nAntenna)
		{
			printf("Error! makeBaselineFreq2IF : a2=%d nA=%d",
				a2, D->nAntenna);
		}
		nFreq = db->nFreq;
		for(f = 0; f < nFreq; f++)
		{
			rcA = db->recChanA[f][0];
			rcB = db->recChanB[f][0];
			fqA = D->datastream[db->dsA].RCfreqId[rcA];
			fqB = D->datastream[db->dsB].RCfreqId[rcB];
			if(fqA != fqB)
			{
				fprintf(stderr, "Baseline %d-%d freq %d "
					"correlates different freqs!\n",
					a1, a2, f);
			}
			if(fqA < 0 || fqA >= D->nFreq)
			{
				printf("Error! makeBaselineFreq2IF : f=%d nF=%d",
					fqA, D->nFreq);
			}
			bandId = dc->freqId2IF[fqA];
			map[a1][a2][f] = bandId;
		}
	}

	/* Fill in auto corr terms */
	for(d = 0; d < dc->nDatastream; d++)
	{
		dd = D->datastream + dc->datastreamId[d];
		a = dd->antennaId;
		if(a < 0 || a >= D->nAntenna)
		{
			printf("Error! makeBaselineFreq2IF : a=%d nA=%d",
				a, D->nAntenna);
		}
		nFreq = dd->nFreq;
		for(f = 0; f < nFreq; f++)
		{
			bandId = dc->freqId2IF[dd->freqId[f]];
			map[a][a][f] = bandId;
		}
	}

	return 0;
}


DifxIF *newDifxIFArray(int nIF)
{
	DifxIF *di;

	di = (DifxIF *)calloc(nIF, sizeof(DifxConfig));
	
	return di;
}

void deleteDifxIFArray(DifxIF *di)
{
	if(di)
	{
		free(di);
	}
}

void printDifxIF(const DifxIF *di)
{
	printf("    Difx IF : %p\n", di);
	printf("      Freq = %f MHz\n", di->freq);
	printf("      Bandwidth = %f MHz\n", di->bw);
	printf("      Sideband = %c\n", di->sideband);
	if(di->nPol == 1)
	{
		printf("      Pol = %c\n", di->pol[0]);
	}
	else if(di->nPol == 2)
	{
		printf("      Pols = %c, %c\n", di->pol[0], di->pol[1]);
	}
	else
	{
		printf("      nPol = %d\n", di->nPol);
	}
}


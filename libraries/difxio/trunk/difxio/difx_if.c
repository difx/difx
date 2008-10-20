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

void fprintBaselineFreq2IF(FILE *fp, int ***map, int nAnt, int nChan)
{
	int a1, a2, c;

	fprintf(fp, "      nAnt = %d  nChan = %d\n", nAnt, nChan);

	for(a2 = 0; a2 < nAnt; a2++)
	{
		fprintf(fp, "      ");
		for(c = 0; c < nChan; c++)
		{
			for(a1 = 0; a1 < nAnt; a1++)
			{
				fprintf(fp, "%2d", map[a1][a2][c]);
			}
			fprintf(fp, "   ");
		}
		fprintf(fp, "\n");
	}
}

void printBaselineFreq2IF(int ***map, int nAnt, int nChan)
{
	fprintBaselineFreq2IF(stdout, map, nAnt, nChan);
}

int makeBaselineFreq2IF(DifxInput *D, int configId)
{
	int ***map;
	DifxConfig *dc;
	DifxBaseline *db;
	DifxDatastream *dd;
	int baselineId;
	int a, b,  d, f;
	int a1, a2, nAnt;
	int ddf;
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

	if(dc->ant2dsId == 0)
	{
		fprintf(stderr, "makeBaselineFreq2IF: ant2dsId = 0\n");
		return -1;
	}

	nAnt = dc->nAntenna;

	/* map[Antenna1][Antenna2][recChan] -- all indices 0-based */
	/* allocate first two dimensions larger than needed for null pads */
	map = (int ***)calloc(nAnt+1, sizeof(int **));
	for(a1 = 0; a1 < nAnt; a1++)
	{
		map[a1] = (int **)calloc(nAnt+1, sizeof(int *));
		for(a2 = 0; a2 < nAnt; a2++)
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
			fprintf(stderr, "Error! baselineId=%d\n", baselineId);
		}
		db = D->baseline + baselineId;
		if(db->dsA < 0 || db->dsB < 0)
		{	
			fprintf(stderr, "Error! dsA=%d dsB=%d\n", 
				db->dsA, db->dsB);
		}
		for(a1 = 0; a1 < nAnt; a1++) 
		{
			if(dc->ant2dsId[a1] == db->dsA)
			{
				break;
			}
		}
		if(a1 < 0 || a1 >= nAnt)
		{
			fprintf(stderr, 
				"Error! makeBaselineFreq2IF : a1=%d nA=%d",
				a1, D->nAntenna);
		}
		for(a2 = 0; a2 < nAnt; a2++) 
		{
			if(dc->ant2dsId[a2] == db->dsB)
			{
				break;
			}
		}
		if(a2 < 0 || a2 >= D->nAntenna)
		{
			fprintf(stderr,
				"Error! makeBaselineFreq2IF : a2=%d nA=%d",
				a2, D->nAntenna);
		}
		nFreq = db->nFreq;
		for(f = 0; f < nFreq; f++)
		{
			rcA = db->recChanA[f][0];
			rcB = db->recChanB[f][0];

			if(rcA < 0 || rcA >= D->datastream[db->dsA].nRecChan)
			{
				fprintf(stderr, "Error! makeBaselineFreq2IF : "
					"rcA = %d, range = %d\n",
					rcA, D->datastream[db->dsA].nRecChan);
				exit(0);
			}

			if(rcB < 0 || rcB >= D->datastream[db->dsB].nRecChan)
			{
				fprintf(stderr, "Error! makeBaselineFreq2IF : "
					"rcB = %d, range = %d\n",
					rcB, D->datastream[db->dsB].nRecChan);
				exit(0);
			}

			fqA = D->datastream[db->dsA].RCfreqId[rcA];
			fqB = D->datastream[db->dsB].RCfreqId[rcB];

			if(fqA < 0 || fqA >= D->datastream[db->dsA].nFreq)
			{
				fprintf(stderr, "Error! makeBaselineFreq2IF : "
					"local freq id=%d out of range %d\n",
					fqA, D->datastream[db->dsA].nFreq);
				fqA = 0;
				exit(0);
			}
			if(fqB < 0 || fqB >= D->datastream[db->dsB].nFreq)
			{
				fprintf(stderr, "Error! makeBaselineFreq2IF : "
					"local freq id=%d out of range %d\n",
					fqB, D->datastream[db->dsB].nFreq);
				fqB = 0;
				exit(0);
			}

			/* map from local freqid to freqid table index */
			fqA = D->datastream[db->dsA].freqId[fqA];
			fqB = D->datastream[db->dsB].freqId[fqB];
			if(fqA != fqB)
			{
				fprintf(stderr, "Baseline %d-%d freq %d "
					"correlates different freqs!\n",
					a1, a2, f);
			}
			
			if(fqA < 0 || fqA >= D->nFreq)
			{
				fprintf(stderr,"Error! "
					"makeBaselineFreq2IF : f=%d nF=%d",
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
		for(a = 0; a < nAnt; a++)
		{
			if(dc->ant2dsId[a] == dc->datastreamId[d])
			{
				break;
			}
		}
		if(a < 0 || a >= nAnt)
		{
			fprintf(stderr,
				"Warning! makeBaselineFreq2IF : a=%d nA=%d\n",
				a, nAnt);
			continue;
		}
		nFreq = dd->nFreq;
		for(f = 0; f < nFreq; f++)
		{
			ddf = dd->freqId[f];
			if(ddf >= 0 && ddf < dc->nIF)
			{
				bandId = dc->freqId2IF[ddf];
				map[a][a][ddf] = bandId;
			}
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

void fprintDifxIF(FILE *fp, const DifxIF *di)
{
	fprintf(fp, "    Difx IF : %p\n", di);
	fprintf(fp, "      Freq = %f MHz\n", di->freq);
	fprintf(fp, "      Bandwidth = %f MHz\n", di->bw);
	fprintf(fp, "      Sideband = %c\n", di->sideband);
	if(di->nPol == 1)
	{
		fprintf(fp, "      Pol = %c\n", di->pol[0]);
	}
	else if(di->nPol == 2)
	{
		fprintf(fp, "      Pols = %c, %c\n", di->pol[0], di->pol[1]);
	}
	else
	{
		fprintf(fp, "      nPol = %d\n", di->nPol);
	}
}

void printDifxIF(const DifxIF *di)
{
	fprintDifxIF(stdout, di);
}

void fprintDifxIFSummary(FILE *fp, const DifxIF *di)
{
	char pols[8];

	if(di->nPol == 1)
	{
		sprintf(pols, "(%c)", di->pol[0]);
	}
	else if(di->nPol == 2)
	{
		sprintf(pols, "(%c,%c)", di->pol[0], di->pol[1]);
	}
	else
	{
		sprintf(pols, "(%d)", di->nPol);
	}

	fprintf(fp, "    Freq=%f MHz  BW=%f MHz Sideband=%c Pols=%s\n",
		di->freq, di->bw, di->sideband, pols);
}

void printDifxIFSummary(const DifxIF *di)
{
	fprintDifxIFSummary(stdout, di);
}

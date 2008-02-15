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


DifxConfig *newDifxConfigArray(int nConfig)
{
	DifxConfig *dc;
	int c;

	dc = (DifxConfig *)calloc(nConfig, sizeof(DifxConfig));
	for(c = 0; c < nConfig; c++)
	{
		dc[c].doPolar = -1;
		dc[c].pulsarId = -1;
	}
	
	return dc;
}

void deleteDifxConfigArray(DifxConfig *dc)
{
	if(dc)
	{
		if(dc->IF)
		{
			free(dc->IF);
		}
		if(dc->indexDS)
		{
			free(dc->indexDS);
		}
		if(dc->indexBL)
		{
			free(dc->indexBL);
		}
		if(dc->freqId2IF)
		{
			free(dc->freqId2IF);
		}
		if(dc->baselineFreq2IF)
		{
			deleteBaselineFreq2IF(dc->baselineFreq2IF);
		}
		free(dc);
	}
}

void printDifxConfig(const DifxConfig *dc)
{
	int i;
	int nAnt;

	printf("  Difx Config [%s] : %p\n", dc->name, dc);
	printf("    tInt  = %f sec\n", dc->tInt);
	printf("    nChan = %d\n", dc->nChan);
	printf("    postFFringe = %d\n", dc->postFFringe);
	printf("    quadDelayInterp = %d\n", dc->quadDelayInterp);
	printf("    pulsarId = %d\n", dc->pulsarId);
	printf("    polarization [%d] = %c%c\n", 
		dc->nPol, dc->pol[0], dc->pol[1]);
	printf("    doPolar = %d\n", dc->doPolar);
	printf("    quantization bits = %d\n", dc->quantBits);
	printf("    datastream ids =");
	for(i = 0; dc->indexDS[i] >= 0; i++)
	{
		printf(" %d", dc->indexDS[i]);
	}
	printf("\n");
	printf("    baseline ids =");
	for(i = 0; dc->indexBL[i] >= 0; i++)
	{
		printf(" %d", dc->indexBL[i]);
		if(i % 12 == 11 && dc->indexBL[i+1] >= 0)
		{
			printf("\n                  ");
		}
	}
	printf("\n");
	printf("    frequency to IF map =");
	for(i = 0; dc->freqId2IF[i] >= 0; i++)
	{
		printf(" %d", dc->freqId2IF[i]);
	}
	printf("\n");
	for(i = 0; i < dc->nIF; i++)
	{
		printDifxIF(dc->IF+i);
	}

	/* count number of antennas in the array first */
	for(nAnt = 0; dc->baselineFreq2IF[nAnt]; nAnt++);
	
	printf("    baselineFreq2IF map:\n");
	printBaselineFreq2IF(dc->baselineFreq2IF, nAnt, dc->nIF);
}


int DifxConfigGetPolId(const DifxConfig *dc, char polName)
{
	if(dc->pol[0] == polName)
	{
		return 0;
	}
	if(dc->pol[1] == polName)
	{
		return 1;
	}
	return -1;
}

int DifxConfigRecChan2IFPol(const DifxInput *D, int configId,
	int antennaId, int recChan, int *bandId, int *polId)
{
	DifxConfig *dc;
	DifxDatastream *ds;
	int datastreamId;
	
	if(recChan < 0 || antennaId < 0)
	{
		*bandId = -1;
		*polId = -1;
		return 0;
	}
	
	if(!D)
	{
		return -1;
	}
	if(configId < 0 || configId >= D->nConfig)
	{
		return -1;
	}

	dc = D->config + configId;
	datastreamId = dc->indexDS[antennaId];
	ds = D->datastream + datastreamId;

	if(recChan >= ds->nRecChan)
	{
		fprintf(stderr, "DifxConfigRecChan2IFPol : recChan=%d out"
			" of range %d\n", recChan, ds->nRecChan);
		return -1;
	}
	
	*bandId = ds->RCfreqId[recChan];
	*polId = DifxConfigGetPolId(dc, ds->RCpolName[recChan]);

	return 0;
}

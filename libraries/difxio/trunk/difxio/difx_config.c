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
		if(dc->datastreamId)
		{
			free(dc->datastreamId);
		}
		if(dc->baselineId)
		{
			free(dc->baselineId);
		}
		if(dc->freqId2IF)
		{
			free(dc->freqId2IF);
		}
		if(dc->baselineFreq2IF)
		{
			deleteBaselineFreq2IF(dc->baselineFreq2IF);
		}
		if(dc->ant2dsId)
		{
			free(dc->ant2dsId);
		}
		free(dc);
	}
}

void DifxConfigAllocDatastreamIds(DifxConfig *dc, int nDatastream, int start)
{
	int i;

	if(dc->datastreamId)
	{
		free(dc->datastreamId);
	}
	dc->datastreamId = (int *)malloc((nDatastream+1)*sizeof(int));
	dc->datastreamId[nDatastream] = -1;
	for(i = 0; i < nDatastream; i++)
	{
		dc->datastreamId[i] = i + start;
	}
	dc->nDatastream = nDatastream;
}

void DifxConfigAllocBaselineIds(DifxConfig *dc, int nBaseline, int start)
{
	int i;

	if(dc->baselineId)
	{
		free(dc->baselineId);
	}
	dc->baselineId = (int *)malloc((nBaseline+1)*sizeof(int));
	dc->baselineId[nBaseline] = -1;
	for(i = 0; i < nBaseline; i++)
	{
		dc->baselineId[i] = i + start;
	}
	dc->nBaseline = nBaseline;
}

void fprintDifxConfig(FILE *fp, const DifxConfig *dc)
{
	int i;
	int nAnt;

	fprintf(fp, "  Difx Config [%s] : %p\n", dc->name, dc);
	fprintf(fp, "    tInt  = %f sec\n", dc->tInt);
	fprintf(fp, "    nChan = %d\n", dc->nChan);
	fprintf(fp, "    postFFringe = %d\n", dc->postFFringe);
	fprintf(fp, "    quadDelayInterp = %d\n", dc->quadDelayInterp);
	fprintf(fp, "    pulsarId = %d\n", dc->pulsarId);
	fprintf(fp, "    polarization [%d] = %c%c\n", 
		dc->nPol, dc->pol[0], dc->pol[1]);
	fprintf(fp, "    doPolar = %d\n", dc->doPolar);
	fprintf(fp, "    quantization bits = %d\n", dc->quantBits);
	fprintf(fp, "    datastream ids [%d] =", dc->nDatastream);
	for(i = 0; i < dc->nDatastream; i++)
	{
		fprintf(fp, " %d", dc->datastreamId[i]);
	}
	fprintf(fp, "\n");
	fprintf(fp, "    baseline ids [%d] =", dc->nBaseline);
	for(i = 0; i < dc->nBaseline; i++)
	{
		fprintf(fp, " %d", dc->baselineId[i]);
	}
	fprintf(fp, "\n");
	if(dc->freqId2IF)
	{
		fprintf(fp, "    frequency to IF map =");
		for(i = 0; dc->freqId2IF[i] >= 0; i++)
		{
			fprintf(fp, " %d", dc->freqId2IF[i]);
		}
		fprintf(fp, "\n");
	}
	if(dc->ant2dsId)
	{
		fprintf(fp, "    ant2dsId[%d] =", dc->nAntenna);
		for(i = 0; i < dc->nAntenna; i++)
		{
			fprintf(fp, " %d", dc->ant2dsId[i]);
		}
		fprintf(fp, "\n");
	}
	fprintf(fp, "    nIF = %d\n", dc->nIF);
	if(dc->nIF > 0)
	{
		for(i = 0; i < dc->nIF; i++)
		{
			fprintDifxIF(fp, dc->IF+i);
		}
	}

	if(dc->baselineFreq2IF)
	{
		/* count number of antennas in the array first */
		for(nAnt = 0; dc->baselineFreq2IF[nAnt]; nAnt++);
		
		fprintf(fp, "    baselineFreq2IF map:\n");
		fprintBaselineFreq2IF(fp, dc->baselineFreq2IF, nAnt, dc->nIF);
	}
}

void printDifxConfig(const DifxConfig *dc)
{
	fprintDifxConfig(stdout, dc);
}

void fprintDifxConfigSummary(FILE *fp, const DifxConfig *dc)
{
	int i;

	fprintf(fp, "  Difx Config [%s]\n", dc->name);
	fprintf(fp, "    tInt  = %f sec\n", dc->tInt);
	fprintf(fp, "    nChan = %d\n", dc->nChan);
	if(dc->pulsarId >= 0)
	{
		fprintf(fp, "    pulsarId = %d\n", dc->pulsarId);
	}
	fprintf(fp, "    doPolar = %d\n", dc->doPolar);
	fprintf(fp, "    quantization bits = %d\n", dc->quantBits);
	if(dc->nIF > 0)
	{
		for(i = 0; i < dc->nIF; i++)
		{
			fprintDifxIFSummary(fp, dc->IF+i);
		}
	}
}

void printDifxConfigSummary(const DifxConfig *dc)
{
	fprintDifxConfigSummary(stdout, dc);
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

/* antennaId is the index to D->antenna */
int DifxConfigRecChan2IFPol(const DifxInput *D, int configId,
	int antennaId, int recChan, int *bandId, int *polId)
{
	DifxConfig *dc;
	DifxDatastream *ds;
	int datastreamId;
	int d;
	
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
		return -2;
	}

	dc = D->config + configId;
	for(datastreamId = 0; datastreamId < dc->nDatastream; datastreamId++)
	{
		/* get index to D->datastream from local ds index */
		d = dc->datastreamId[datastreamId];
		if(d < 0 || d >= D->nDatastream)
		{
			continue;
		}
		ds = D->datastream + d;
		/* now compare the absolute antennaIds */
		if(antennaId == ds->antennaId)
		{
			break;
		}
	}
	if(datastreamId >= dc->nDatastream)
	{
		return -4;
	}

	if(recChan >= ds->nRecChan)
	{
		return -3;
	}
	
	*bandId = ds->RCfreqId[recChan];
	*polId = DifxConfigGetPolId(dc, ds->RCpolName[recChan]);

	return 0;
}

void copyDifxConfig(DifxConfig *dest, const DifxConfig *src,
	const int *baselineIdRemap, const int *datastreamIdRemap, 
	const int *pulsarIdRemap)
{
	int i, n, a;

	dest->nAntenna = src->nAntenna;
	if(src->ant2dsId)
	{
		dest->ant2dsId = (int *)malloc((src->nAntenna+1)*sizeof(int));
		for(a = 0; a < src->nAntenna; a++)
		{
			if(src->ant2dsId[a] < 0)
			{
				dest->ant2dsId[a] = src->ant2dsId[a];
			}
			else if(datastreamIdRemap) 
			{
				dest->ant2dsId[a] = 
					datastreamIdRemap[src->ant2dsId[a]];
			}
			else
			{
				dest->ant2dsId[a] = src->ant2dsId[a];
			}
		}
		dest->ant2dsId[src->nAntenna] = -1;
	}

	dest->tInt = src->tInt;
	dest->nChan = src->nChan;
	strcpy(dest->name, src->name);
	dest->postFFringe = src->postFFringe;
	dest->quadDelayInterp = src->quadDelayInterp;
	if(pulsarIdRemap && src->pulsarId >= 0)
	{
		dest->pulsarId = pulsarIdRemap[src->pulsarId];
	}
	else
	{
		dest->pulsarId = src->pulsarId;
	}
	dest->nPol = src->nPol;
	for(i = 0; i < dest->nPol; i++)
	{
		dest->pol[i] = src->pol[i];
	}
	dest->doPolar = src->doPolar;
	dest->quantBits = src->quantBits;
	dest->nBaseline = src->nBaseline;
	dest->nDatastream = src->nDatastream;

	n = dest->nBaseline;
	dest->baselineId = (int *)calloc(n+1, sizeof(int));
	dest->baselineId[n] = -1;
	if(baselineIdRemap)
	{
		for(i = 0; i < n; i++)
		{
			dest->baselineId[i] = 
				baselineIdRemap[src->baselineId[i]];
		}
	}
	else
	{
		for(i = 0; i < n; i++)
		{
			dest->baselineId[i] = src->baselineId[i];
		}
	}
	
	n = dest->nDatastream;
	dest->datastreamId = (int *)calloc(n+1, sizeof(int));
	dest->datastreamId[n] = -1;
	if(datastreamIdRemap)
	{
		for(i = 0; i < n; i++)
		{
			dest->datastreamId[i] = 
				datastreamIdRemap[src->datastreamId[i]];
		}
	}
	else
	{
		for(i = 0; i < n; i++)
		{
			dest->datastreamId[i] = src->datastreamId[i];
		}
	}
}

DifxConfig *mergeDifxConfigArrays(const DifxConfig *dc1, int ndc1,
	const DifxConfig *dc2, int ndc2, int *configIdRemap,
	const int *baselineIdRemap, const int *datastreamIdRemap,
	const int *pulsarIdRemap, int *ndc)
{
	int i, j;
	DifxConfig *dc;

	*ndc = ndc1;

	for(j = 0; j < ndc2; j++)
	{
		configIdRemap[j] = *ndc;
		(*ndc)++;
	}

	dc = newDifxConfigArray(*ndc);
	
	/* copy df1 */
	for(i = 0; i < ndc1; i++)
	{
		copyDifxConfig(dc + i, dc1 + i, 0, 0, 0);
	}

	/* copy df2 */
	for(j = 0; j < ndc2; j++)
	{
		if(configIdRemap[j] >= ndc1)
		{
			copyDifxConfig(dc + configIdRemap[j], dc2 + j,
				baselineIdRemap, datastreamIdRemap, 
				pulsarIdRemap);
		}
	}

	return dc;
}

int writeDifxConfigArray(FILE *out, int nConfig, const DifxConfig *dc)
{
	int i, j;
	int n;
	const DifxConfig *config;

	writeDifxLineInt(out, "NUM CONFIGURATIONS", nConfig);
	n = 1;

	for(i = 0; i < nConfig; i++)
	{
		config = dc + i;

		writeDifxLine(out, "CONFIG SOURCE", config->name);
		writeDifxLineDouble(out, "INT TIME (SEC)", "%8.6f",
			config->tInt);
		writeDifxLineInt(out, "NUM CHANNELS", config->nChan);
		writeDifxLineInt(out, "CHANNELS TO AVERAGE", config->specAvg);
		writeDifxLineInt(out, "OVERSAMPLE FACTOR", config->overSamp);
		writeDifxLineInt(out, "DECIMATION FACTOR", config->decimation);
		writeDifxLineInt(out, "BLOCKS PER SEND", 
			config->blocksPerSend);
		writeDifxLineInt(out, "GUARD BLOCKS", config->guardBlocks);
		writeDifxLine(out, "POST-F FRINGE ROT", "FALSE");
		writeDifxLine(out, "QUAD DELAY INTERP", "TRUE");
		writeDifxLine(out, "WRITE AUTOCORRS", "TRUE");
		if(config->pulsarId >= 0)
		{
			writeDifxLine(out, "PULSAR BINNING", "TRUE");
			/* FIXME : more stuff here */
		}
		else
		{
			writeDifxLine(out, "PULSAR BINNING", "FALSE");
		}
		for(j = 0; j < config->nDatastream; j++)
		{
			writeDifxLineInt1(out, "DATASTREAM %d INDEX", j,
				config->datastreamId[j]);
		}
		for(j = 0; j < config->nBaseline; j++)
		{
			writeDifxLineInt1(out, "BASELINE %d INDEX", j,
				config->baselineId[j]);
		}

		n += (10 + config->nDatastream + config->nBaseline);
	}

	return n;
}

/***************************************************************************
 *   Copyright (C) 2008-2017 by Walter Brisken                             *
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
#include "difxio/difx_write.h"

DifxConfig *newDifxConfigArray(int nConfig)
{
	DifxConfig *dc;
	int configId;

	dc = (DifxConfig *)calloc(nConfig, sizeof(DifxConfig));
	for(configId = 0; configId < nConfig; ++configId)
	{
		dc[configId].doPolar = -1;
		dc[configId].pulsarId = -1;
		dc[configId].strideLength = 16;
		dc[configId].xmacLength = 32;
		dc[configId].numBufferedFFTs = 1;
		dc[configId].fringeRotOrder = 1;
	}
	
	return dc;
}

void DifxConfigAllocDatastreamIds(DifxConfig *dc, int nDatastream, int start)
{
	int configDatastreamId;

	if(dc->datastreamId)
	{
		free(dc->datastreamId);
	}
	dc->datastreamId = (int *)calloc(nDatastream+1, sizeof(int));
	dc->datastreamId[nDatastream] = -1;
	for(configDatastreamId = 0; configDatastreamId < nDatastream; ++configDatastreamId)
	{
		dc->datastreamId[configDatastreamId] = configDatastreamId + start;
	}
	dc->nDatastream = nDatastream;
}

void DifxConfigAllocBaselineIds(DifxConfig *dc, int nBaseline, int start)
{
	int configBaselineId;

	if(dc->baselineId)
	{
		free(dc->baselineId);
	}
	dc->baselineId = (int *)calloc(nBaseline+1, sizeof(int));
	dc->baselineId[nBaseline] = -1;
	for(configBaselineId = 0; configBaselineId < nBaseline; ++configBaselineId)
	{
		dc->baselineId[configBaselineId] = configBaselineId + start;
	}
	dc->nBaseline = nBaseline;
}

void deleteDifxConfigInternals(DifxConfig *dc)
{
	if(dc->datastreamId)
	{
		free(dc->datastreamId);
		dc->datastreamId = 0;
	}
	if(dc->baselineId)
	{
		free(dc->baselineId);
		dc->baselineId = 0;
	}
	if(dc->ant2dsId)
	{
		free(dc->ant2dsId);
		dc->ant2dsId = 0;
	}
}

void deleteDifxConfigArray(DifxConfig *dc, int nConfig)
{
	int configId;

	if(dc)
	{
		for(configId = 0; configId < nConfig; ++configId)
		{
			deleteDifxConfigInternals(dc + configId);
		}
		free(dc);
	}
}

void fprintDifxConfig(FILE *fp, const DifxConfig *dc)
{
	int i;
	char p0, p1;

	fprintf(fp, "  Difx Config [%s] : %p\n", dc->name, dc);
	fprintf(fp, "    tInt  = %f sec\n", dc->tInt);
	fprintf(fp, "    Subint nanoseconds = %d\n", dc->subintNS);
	fprintf(fp, "    Guard nanoseconds = %d\n", dc->guardNS);
	fprintf(fp, "    fringeRotOrder = %d\n", dc->fringeRotOrder);
	fprintf(fp, "    strideLength = %d\n", dc->strideLength);
	fprintf(fp, "    xmacLength = %d\n", dc->xmacLength);
	fprintf(fp, "    numBufferedFFTs = %d\n", dc->numBufferedFFTs);
	fprintf(fp, "    pulsarId = %d\n", dc->pulsarId);
	p0 = dc->pol[0] ? dc->pol[0] : ' ';
	p1 = dc->pol[1] ? dc->pol[1] : ' ';
	fprintf(fp, "    polarization [%d] = %c%c\n", dc->nPol, p0, p1);
	fprintf(fp, "    polMask = 0x%03x\n", dc->polMask);
	fprintf(fp, "    doPolar = %d\n", dc->doPolar);
	fprintf(fp, "    quantization bits = %d\n", dc->quantBits);
	fprintf(fp, "    datastream ids [%d] =", dc->nDatastream);
	for(i = 0; i < dc->nDatastream; ++i)
	{
		fprintf(fp, " %d", dc->datastreamId[i]);
	}
	fprintf(fp, "\n");
	fprintf(fp, "    baseline ids [%d] =", dc->nBaseline);
	for(i = 0; i < dc->nBaseline; ++i)
	{
		fprintf(fp, " %d", dc->baselineId[i]);
	}
	fprintf(fp, "\n");
	if(dc->ant2dsId)
	{
		fprintf(fp, "    ant2dsId[%d] =", dc->nAntenna);
		for(i = 0; i < dc->nAntenna; ++i)
		{
			fprintf(fp, " %d", dc->ant2dsId[i]);
		}
		fprintf(fp, "\n");
	}
}

void printDifxConfig(const DifxConfig *dc)
{
	fprintDifxConfig(stdout, dc);
}

void fprintDifxConfigSummary(FILE *fp, const DifxConfig *dc)
{
	if(!dc)
	{
		return;
	}
	fprintf(fp, "  Difx Config [%s]\n", dc->name);
	fprintf(fp, "    tInt  = %f sec\n", dc->tInt);
	if(dc->pulsarId >= 0)
	{
		fprintf(fp, "    pulsarId = %d\n", dc->pulsarId);
	}
	fprintf(fp, "    doPolar = %d\n", dc->doPolar);
	fprintf(fp, "    quantization bits = %d\n", dc->quantBits);
	fprintf(fp, "    freqSetId = %d\n", dc->freqSetId);
}

void printDifxConfigSummary(const DifxConfig *dc)
{
	fprintDifxConfigSummary(stdout, dc);
}

int isSameDifxConfig(const DifxConfig *dc1, const DifxConfig *dc2)
{
	int i;

	if(dc1->tInt != dc2->tInt ||
	   dc1->subintNS != dc2->subintNS ||
	   dc1->guardNS != dc2->guardNS ||
	   dc1->fringeRotOrder != dc2->fringeRotOrder ||
	   dc1->strideLength != dc2->strideLength ||
	   dc1->xmacLength != dc2->xmacLength ||
	   dc1->numBufferedFFTs != dc2->numBufferedFFTs ||
	   dc1->pulsarId != dc2->pulsarId ||
	   dc1->nPol != dc2->nPol ||
	   dc1->doPolar != dc2->doPolar ||
	   dc1->doAutoCorr != dc2->doAutoCorr ||
	   dc1->quantBits != dc2->quantBits ||
	   dc1->nAntenna != dc2->nAntenna ||
	   dc1->nDatastream != dc2->nDatastream ||
	   dc1->nBaseline != dc2->nBaseline)
	{
		return 0;
	}

	for(i = 0; i < dc1->nPol; ++i)
	{
		if(dc1->pol[i] != dc2->pol[i])
		{
			return 0;
		}
	}

	for(i = 0; i < dc1->nDatastream; ++i)
	{
		if(dc1->datastreamId[i] != dc2->datastreamId[i])
		{
			return 0;
		}
	}

	for(i = 0; i < dc1->nBaseline; ++i)
	{
		if(dc1->baselineId[i] != dc2->baselineId[i])
		{
			return 0;
		}
	}

	return 1;
}

int DifxConfigGetPolId(const DifxInput *D, int configId, char polName)
{
	if(D->AntPol == 0)
	{
		if(D->config[configId].pol[0] == polName)
		{
			return 0;
		}
		if(D->config[configId].pol[1] == polName)
		{
			return 1;
		}

		return -1;
	}
	else
	{
		switch(polName)
		{
			case 'R':
				return 0;
			case 'L':
				return 1;
			case 'X':
				return 0;
			case 'Y':
				return 1;
			case 'H':
				return 0;
			case 'V':
				return 1;
			default:
				return -1;
		}
	}
}


/* Inputs:
 *   D         -- pointer to DifxInput structure
 *   configId  -- index for D->config (must be in [0 to D->nConfig-1])
 *   antennaId -- index to D->antenna (must be in [0 to D->nAntenna-1])
 *   recBand   -- record band number for specified antenna.  This is
 *                index to the datastream "local frequency table"
 * Outputs:
 *   freqId    -- index to D->freq (in [0 to D->nFreq-1])
 *                or -1 on out of range
 *   polId     -- polarization ID for config.  pols are prioritized
 *                in order R, L, X, Y.  Value is 0 or 1
 *                or -1 on out of range
 * Return:
 *   0         -- success
 *   <0        -- some error occurred
 */
int DifxConfigRecBand2FreqPol(const DifxInput *D, int configId, int antennaId, int recBand, int *freqId, int *polId)
{
	DifxConfig *dc;
	DifxDatastream *ds = 0;
	int configDatastreamId;	/* the datastream index in the config */
	int dsId;		/* the global datastream index (do D->datastream) */
	int localFqId;
	
	if(recBand < 0 || antennaId < 0)
	{
		*freqId = -1;
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
	for(configDatastreamId = 0; configDatastreamId < dc->nDatastream; ++configDatastreamId)
	{
		/* get index to D->datastream from local ds index */
		dsId = dc->datastreamId[configDatastreamId];
		if(dsId < 0 || dsId >= D->nDatastream)
		{
			continue;
		}
		ds = D->datastream + dsId;
		/* now compare the absolute antennaIds */
		if(antennaId == ds->antennaId)
		{
			if(recBand < ds->nRecBand)
			{
				break;
			}
			else
			{
				recBand -= ds->nRecBand;
			}
		}
	}

	if(configDatastreamId >= dc->nDatastream || ds == 0)
	{
		return -4;
	}

	if(recBand >= ds->nRecBand)
	{
		return -3;
	}

	localFqId = ds->recBandFreqId[recBand];
	if(localFqId < 0 || localFqId >= ds->nRecFreq)
	{
		fprintf(stderr, "DifxConfigRecBand2FreqPol: localFqId=%d is out of range (nFreq=%d)\n", localFqId, ds->nRecFreq);

		return -5;
	}

	*freqId = ds->recFreqId[localFqId];
	*polId = DifxConfigGetPolId(D, configId, ds->recBandPolName[recBand]);

	return 0;
}

void copyDifxConfig(DifxConfig *dest, const DifxConfig *src, const int *baselineIdRemap, const int *datastreamIdRemap, const int *pulsarIdRemap)
{
	int i, n, a;

	dest->nAntenna = src->nAntenna;
	if(src->ant2dsId)
	{
		dest->ant2dsId = (int *)calloc(src->nAntenna+1, sizeof(int));
		for(a = 0; a < src->nAntenna; ++a)
		{
			if(src->ant2dsId[a] < 0)
			{
				dest->ant2dsId[a] = src->ant2dsId[a];
			}
			else if(datastreamIdRemap) 
			{
				dest->ant2dsId[a] = datastreamIdRemap[src->ant2dsId[a]];
			}
			else
			{
				dest->ant2dsId[a] = src->ant2dsId[a];
			}
		}
		dest->ant2dsId[src->nAntenna] = -1;
	}

	dest->tInt = src->tInt;
	dest->subintNS = src->subintNS;
	dest->guardNS = src->guardNS;
	snprintf(dest->name, DIFXIO_NAME_LENGTH, "%s", src->name);
	dest->fringeRotOrder = src->fringeRotOrder;
	dest->strideLength = src->strideLength;
	dest->xmacLength = src->xmacLength;
	dest->numBufferedFFTs = src->numBufferedFFTs;
	if(pulsarIdRemap && src->pulsarId >= 0)
	{
		dest->pulsarId = pulsarIdRemap[src->pulsarId];
	}
	else
	{
		dest->pulsarId = src->pulsarId;
	}
	dest->nPol = src->nPol;
	for(i = 0; i < dest->nPol; ++i)
	{
		dest->pol[i] = src->pol[i];
	}
	dest->doPolar = src->doPolar;
	dest->doAutoCorr = src->doAutoCorr;
	dest->quantBits = src->quantBits;
	dest->nBaseline = src->nBaseline;
	dest->nDatastream = src->nDatastream;

	n = dest->nBaseline;
	dest->baselineId = (int *)calloc(n+1, sizeof(int));
	dest->baselineId[n] = -1;
	if(baselineIdRemap)
	{
		for(i = 0; i < n; ++i)
		{
			dest->baselineId[i] = baselineIdRemap[src->baselineId[i]];
		}
	}
	else
	{
		for(i = 0; i < n; ++i)
		{
			dest->baselineId[i] = src->baselineId[i];
		}
	}
	
	n = dest->nDatastream;
	dest->datastreamId = (int *)calloc(n+1, sizeof(int));
	dest->datastreamId[n] = -1;
	if(datastreamIdRemap)
	{
		for(i = 0; i < n; ++i)
		{
			dest->datastreamId[i] = datastreamIdRemap[src->datastreamId[i]];
		}
	}
	else
	{
		for(i = 0; i < n; ++i)
		{
			dest->datastreamId[i] = src->datastreamId[i];
		}
	}
}

void moveDifxConfig(DifxConfig *dest, DifxConfig *src)
{
	memcpy(dest, src, sizeof(DifxConfig));

	/* unlink some pointers to prevent double freeing */
	src->datastreamId = 0;
	src->baselineId = 0;
	src->ant2dsId = 0;
}

int simplifyDifxConfigs(DifxInput *D)
{
	int c, c1, c0, s, r;
	int n0;
	DifxRule *dr;
	DifxConfig *dc;

	n0 = D->nConfig;
	if(n0 < 2)
	{
		return 0;
	}

	for(c=1;;)
	{
		if(c >= D->nConfig)
		{
			break;
		}

		for(c1 = 0; c1 < c; ++c1)
		{
			if(isSameDifxConfig(D->config+c, D->config+c1))
			{
				break;
			}
		}
		if(c == c1)
		{
			++c;
		}
		else
		{
			/* 1. renumber this and all higher references to configs */
			for(s = 0; s < D->nScan; ++s)
			{
				c0 = D->scan[s].configId;
				if(c0 == c)
				{
					c0 = c1;
				}
				else if(c0 > c)
				{
					--c0;
				}
				D->scan[s].configId = c0;
			}

			/* 2. Change all rules referring to this config to refer to identical config */
			for(r = 0; r < D->nRule; ++r)
			{
				dr = D->rule + r;
				dc = D->config + c;
				if(strcmp(dc->name, dr->configName) == 0)
				{
					dc = D->config + c1;
					snprintf(dr->configName, DIFXIO_NAME_LENGTH, "%s", dc->name);
				}
			}

			/* 3. reduce number of configs */
			--D->nConfig;

			/* 4. delete this config and bump up higher ones */
			deleteDifxConfigInternals(D->config+c);
			for(c1 = c; c1 < D->nConfig; ++c1)
			{
				moveDifxConfig(D->config + c1, D->config + c1 + 1);
			}
		}
	}

	return n0 - D->nConfig;
}

DifxConfig *mergeDifxConfigArrays(const DifxConfig *dc1, int ndc1, const DifxConfig *dc2, int ndc2, int *configIdRemap,
	const int *baselineIdRemap, const int *datastreamIdRemap, const int *pulsarIdRemap, int *ndc)
{
	int c;
	DifxConfig *dc;

	*ndc = ndc1;

	for(c = 0; c < ndc2; ++c)
	{
		configIdRemap[c] = *ndc;
		++(*ndc);
	}

	dc = newDifxConfigArray(*ndc);
	
	/* copy dc1 */
	for(c = 0; c < ndc1; ++c)
	{
		copyDifxConfig(dc + c, dc1 + c, 0, 0, 0);
	}

	/* copy dc2 */
	for(c = 0; c < ndc2; ++c)
	{
		if(configIdRemap[c] >= ndc1)
		{
			copyDifxConfig(dc + configIdRemap[c], dc2 + c, baselineIdRemap, datastreamIdRemap, pulsarIdRemap);
		}
	}

	return dc;
}

int writeDifxConfigArray(FILE *out, int nConfig, const DifxConfig *dc, const DifxPulsar *pulsar, const DifxPhasedArray *phasedarray)
{
	int configId;
	int n = 0;

	writeDifxLineInt(out, "NUM CONFIGURATIONS", nConfig);
	++n;

	for(configId = 0; configId < nConfig; ++configId)
	{
		const DifxConfig *config;
		int dsId, blId;

		config = dc + configId;

		writeDifxLine(out, "CONFIG NAME", config->name);
		writeDifxLineDouble(out, "INT TIME (SEC)", "%8.6f", config->tInt);
		writeDifxLineInt(out, "SUBINT NANOSECONDS", config->subintNS);
		writeDifxLineInt(out, "GUARD NANOSECONDS", config->guardNS);
		writeDifxLineInt(out, "FRINGE ROTN ORDER", config->fringeRotOrder);
		writeDifxLineInt(out, "ARRAY STRIDE LENGTH", config->strideLength);
		writeDifxLineInt(out, "XMAC STRIDE LENGTH", config->xmacLength);
		writeDifxLineInt(out, "NUM BUFFERED FFTS", config->numBufferedFFTs);
		if(config->doAutoCorr)
		{
			writeDifxLine(out, "WRITE AUTOCORRS", "TRUE");
		}
		else
		{
			writeDifxLine(out, "WRITE AUTOCORRS", "FALSE");
		}
		if(config->pulsarId >= 0 && pulsar)
		{
			writeDifxLine(out, "PULSAR BINNING", "TRUE");
			writeDifxLine(out, "PULSAR CONFIG FILE", pulsar[config->pulsarId].fileName);
		}
		else
		{
			writeDifxLine(out, "PULSAR BINNING", "FALSE");
		}
		if(config->phasedArrayId >= 0 && phasedarray)
		{
			writeDifxLine(out, "PHASED ARRAY", "TRUE");
			writeDifxLine(out, "PHASED ARRAY CONNFIG FILE", phasedarray[config->phasedArrayId].fileName);
		}
		else
		{
			writeDifxLine(out, "PHASED ARRAY", "FALSE");
		}
		for(dsId = 0; dsId < config->nDatastream; ++dsId)
		{
			writeDifxLineInt1(out, "DATASTREAM %d INDEX", dsId, config->datastreamId[dsId]);
		}
		for(blId = 0; blId < config->nBaseline; ++blId)
		{
			writeDifxLineInt1(out, "BASELINE %d INDEX", blId, config->baselineId[blId]);
		}

		n += (10 + config->nDatastream + config->nBaseline);
	}

	return n;
}

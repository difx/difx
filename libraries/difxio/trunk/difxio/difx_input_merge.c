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


/* FIXME -- add condition structure */
int areDifxInputsMergable(const DifxInput *D1, const DifxInput *D2)
{
	if(D1->specAvg != D2->specAvg ||
	   //D1->nFFT != D2->nFFT ||
	   //D1->startChan != D2->startChan ||
	   //D1->nInChan != D2->nInChan ||
	   //D1->nOutChan != D2->nOutChan ||
	   D1->job->aberCorr != D2->job->aberCorr ||
	   strncmp(D1->job->difxVersion, D2->job->difxVersion, 64))
	{
		return 0;
	}

	if(D1->inputFileVersion != D2->inputFileVersion)
	{
		printf("Warning: merging input files of different versions!\n");
		printf("  This might work, if you are lucky!\n");
	}
	
	return 1;
}

/* This function determines if two DifxInput arenot mergable
 * because difxio does not currently support it, but could
 * in the future */
int areDifxInputsCompatible(const DifxInput *D1, const DifxInput *D2)
{
	int f;
	int a1, a2;

	if(D1->nFreq != D2->nFreq)
	{
		return 0;
	}

	for(f = 0; f < D1->nFreq; f++)
	{
		if(isSameDifxFreq(D1->freq + f, D2->freq + f) == 0)
		{
			return 0;
		}
	}

	for(a1 = 0; a1 < D1->nAntenna; a1++)
	{
		for(a2 = 0; a2 < D2->nAntenna; a2++)
		{
			if(isSameDifxAntenna(D1->antenna + a1, D2->antenna + a2))
			{
				if(!isSameDifxAntennaClock(D1->antenna + a1, D2->antenna + a2))
				{
					return 0;
				}
			}
		}
	}

	return 1;
}

static void printRemap(const char *name, const int *Remap, int n)
{
	int i;
	printf("  %s Remap =", name);
	if(n == 0 || Remap == 0)
	{
		printf(" None\n");
	}
	else
	{
		for(i = 0; i < n; i++)
		{
			printf(" %d", Remap[i]);
		}
		printf("\n");
	}
}

DifxInput *mergeDifxInputs(const DifxInput *D1, const DifxInput *D2, 
	int verbose)
{
	DifxInput *D;
	int *jobIdRemap = 0;
	int *freqIdRemap = 0;
	int *antennaIdRemap = 0;
	int *datastreamIdRemap = 0;
	int *baselineIdRemap = 0;
	int *pulsarIdRemap = 0;
	int *configIdRemap = 0;
	int *sourceIdRemap = 0;
	int *spacecraftIdRemap = 0;

	if(!D1 || !D2)
	{
		return 0;
	}

	/* allocate some scratch space */
	jobIdRemap        = (int *)calloc(D2->nJob, sizeof(int));
	freqIdRemap       = (int *)calloc(D2->nFreq, sizeof(int));
	antennaIdRemap    = (int *)calloc(D2->nAntenna, sizeof(int));
	datastreamIdRemap = (int *)calloc(D2->nDatastream, sizeof(int));
	baselineIdRemap   = (int *)calloc(D2->nBaseline, sizeof(int));
	sourceIdRemap     = (int *)calloc(D2->nSource, sizeof(int));
	if(D2->nPulsar > 0)
	{
		pulsarIdRemap = (int *)calloc(D2->nPulsar, sizeof(int));
	}
	configIdRemap     = (int *)calloc(D2->nConfig, sizeof(int));
	if(D2->nSpacecraft > 0)
	{
		spacecraftIdRemap = (int *)calloc(D2->nSpacecraft, sizeof(int));
	}

	/* allocate the big D */
	D = newDifxInput();

	/* copy over / merge some of DifxInput top level parameters */
	D->specAvg = D1->specAvg;
	//D->nFFT = D1->nFFT;
	D->startChan = D1->startChan;
	D->nInChan = D1->nInChan;
	D->nOutChan = D1->nOutChan;
	if(D1->visBufferLength > D2->visBufferLength)
	{
		D->visBufferLength = D1->visBufferLength;
	}
	else
	{
		D->visBufferLength = D2->visBufferLength;
	}
	if(D1->mjdStart < D2->mjdStart)
	{
		D->mjdStart = D1->mjdStart;
	}
	else
	{
		D->mjdStart = D2->mjdStart;
	}
	if(D1->mjdStop > D2->mjdStop)
	{
		D->mjdStop = D1->mjdStop;
	}
	else
	{
		D->mjdStop = D2->mjdStop;
	}

	/* merge DifxAntenna table */
	D->antenna = mergeDifxAntennaArrays(D1->antenna, D1->nAntenna,
		D2->antenna, D2->nAntenna, antennaIdRemap, &(D->nAntenna));

	/* merge DifxJob table */
	D->job = mergeDifxJobArrays(D1->job, D1->nJob, D2->job, D2->nJob,
		jobIdRemap, antennaIdRemap, &(D->nJob));

	/* merge DifxFreq table */
	D->freq = mergeDifxFreqArrays(D1->freq, D1->nFreq,
		D2->freq, D2->nFreq, freqIdRemap, &(D->nFreq));

	/* merge DifxDatastream table */
	D->datastream = mergeDifxDatastreamArrays(D1->datastream, 
		D1->nDatastream, D2->datastream, D2->nDatastream,
		datastreamIdRemap, freqIdRemap, antennaIdRemap,
		&(D->nDatastream));

	/* merge DifxBaseline table */
	D->baseline = mergeDifxBaselineArrays(D1->baseline, D1->nBaseline,
		D2->baseline, D2->nBaseline, baselineIdRemap,
		datastreamIdRemap, &(D->nBaseline));

	/* merge DifxPulsar table */
	D->pulsar = mergeDifxPulsarArrays(D1->pulsar, D1->nPulsar,
		D2->pulsar, D2->nPulsar, pulsarIdRemap, &(D->nPulsar));

	/* merge DifxConfig table */
	D->config = mergeDifxConfigArrays(D1->config, D1->nConfig, 
		D2->config, D2->nConfig, configIdRemap, 
		baselineIdRemap, datastreamIdRemap, pulsarIdRemap, 
		&(D->nConfig));

        /* merge DifxSource table */
	//printf("About to merge two source arrays (vals %p, %p)", D1->source, D2->source);
	//printf("With numbers of sources %d, %d\n", D2->nSource, D2->nSource);
	D->source = mergeDifxSourceArrays(D1->source, D1->nSource,
		D2->source, D2->nSource, sourceIdRemap, &(D->nSource));

	/* merge DifxScan table */
	D->scan = mergeDifxScanArrays(D1->scan, D1->nScan, D2->scan, D2->nScan,
		sourceIdRemap, jobIdRemap, configIdRemap, antennaIdRemap, 
		&(D->nScan));

	/* merge DifxEOP table */
	D->eop = mergeDifxEOPArrays(D1->eop, D1->nEOP, D2->eop, D2->nEOP,
		&(D->nEOP));
	
	/* merge DifxSpacecraft table */
	D->spacecraft = mergeDifxSpacecraft(D1->spacecraft, D1->nSpacecraft,
		D2->spacecraft, D2->nSpacecraft,
		spacecraftIdRemap, &(D->nSpacecraft));

	/* print remappings */
	if(verbose > 1)
	{
		printRemap("jobId", jobIdRemap, D2->nJob);
		printRemap("freqId", freqIdRemap, D2->nFreq);
		printRemap("antennaId", antennaIdRemap, D2->nAntenna);
		printRemap("sourceId", sourceIdRemap, D2->nSource);
		printRemap("datastreamId", datastreamIdRemap, D2->nDatastream);
		printRemap("baselineId", baselineIdRemap, D2->nBaseline);
		printRemap("pulsarId", pulsarIdRemap, D2->nPulsar);
		printRemap("configId", configIdRemap, D2->nConfig);
		printRemap("spacecraftId", spacecraftIdRemap, D2->nSpacecraft);
	}
	
	/* clean up */
	free(jobIdRemap);
	free(freqIdRemap);
	free(antennaIdRemap);
	free(datastreamIdRemap);
	free(baselineIdRemap);
	if(pulsarIdRemap)
	{
		free(pulsarIdRemap);
	}
	free(configIdRemap);
	free(sourceIdRemap);
	if(spacecraftIdRemap)
	{
		free(spacecraftIdRemap);
	}

	return D;
}

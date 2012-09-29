/***************************************************************************
 *   Copyright (C) 2008-2012 by Walter Brisken                             *
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


#warning "FIXME: add condition structure"
int areDifxInputsMergable(const DifxInput *D1, const DifxInput *D2)
{
#warning "FIXME: startChan, nInChan and nOutChan are probably vestigial; clean them up?"
	if(D1->specAvg != D2->specAvg ||
	   D1->job->aberCorr != D2->job->aberCorr ||
	   strncmp(D1->job->difxVersion, D2->job->difxVersion, DIFXIO_VERSION_LENGTH) ||
	   strncmp(D1->job->difxLabel, D2->job->difxLabel, DIFXIO_VERSION_LENGTH))
	{
		return 0;
	}

#warning "FIXME: check that D2 has no remappings as those would become lost"
	
	return 1;
}

/* This function determines if two DifxInput are not mergable
 * because difxio does not currently support it, but could
 * in the future */
int areDifxInputsCompatible(const DifxInput *D1, const DifxInput *D2)
{
	int f;
	int a1;

	if(D1->nFreq != D2->nFreq)
	{
		return 0;
	}

	for(f = 0; f < D1->nFreq; ++f)
	{
		if(isSameDifxFreq(D1->freq + f, D2->freq + f) == 0)
		{
			return 0;
		}
	}

	for(a1 = 0; a1 < D1->nAntenna; ++a1)
	{
		int a2;

		for(a2 = 0; a2 < D2->nAntenna; ++a2)
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

DifxInput *mergeDifxInputs(const DifxInput *D1, const DifxInput *D2, int verbose)
{
	DifxInput *D;
	DifxJob *job;
	int *jobIdRemap;
	int *freqIdRemap;
	int *antennaIdRemap;
	int *datastreamIdRemap;
	int *baselineIdRemap;
	int *pulsarIdRemap;
	int *configIdRemap;
	int *sourceIdRemap;
	int *spacecraftIdRemap;

	if(!D1 || !D2)
	{
		return 0;
	}

	/* allocate some scratch space */
	jobIdRemap        = newRemap(D2->nJob);
	freqIdRemap       = newRemap(D2->nFreq);
	antennaIdRemap    = newRemap(D2->nAntenna);
	datastreamIdRemap = newRemap(D2->nDatastream);
	baselineIdRemap   = newRemap(D2->nBaseline);
	sourceIdRemap     = newRemap(D2->nSource);
	configIdRemap     = newRemap(D2->nConfig);
	if(D2->nPulsar > 0)
	{
		pulsarIdRemap = newRemap(D2->nPulsar);
	}
	else
	{
		pulsarIdRemap = 0;
	}
	if(D2->nSpacecraft > 0)
	{
		spacecraftIdRemap = newRemap(D2->nSpacecraft);
	}
	else
	{
		spacecraftIdRemap = 0;
	}

	/* allocate the big D */
	D = newDifxInput();

	/* copy over / merge some of DifxInput top level parameters */
	D->specAvg = D1->specAvg;
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

	/* store the remappings of the added job in case it is useful later */
	job = D->job + D1->nJob;	/* points to first (probably only) job from D2 */
	job->jobIdRemap = jobIdRemap;
	job->freqIdRemap = freqIdRemap;
	job->antennaIdRemap = antennaIdRemap;
	job->datastreamIdRemap = datastreamIdRemap;
	job->baselineIdRemap = baselineIdRemap;
	job->pulsarIdRemap = pulsarIdRemap;
	job->configIdRemap = configIdRemap;
	job->sourceIdRemap = sourceIdRemap;
	job->spacecraftIdRemap = spacecraftIdRemap;

	return D;
}

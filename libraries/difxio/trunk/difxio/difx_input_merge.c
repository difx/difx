/***************************************************************************
 *   Copyright (C) 2008-2016 by Walter Brisken                             *
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

unsigned int difxInputCompatibilityStatistics[NumDifxInputCompatibilityStatistics] = { 0 };

const char difxInputCompatibilityDescriptions[][MAX_COMPATIBILITY_DESCRIPTION_LENGTH] =
{
	"DiFX versions differ",
	"Number of frequencies differ",
	"Frequencies differ",
	"EOPs differ",
	"Clock models differ",

	"DEVELOPER ERROR: you should never see this line"
};

void resetDifxInputCompatibilityStatistics()
{
	unsigned int i;

	for(i = 0; i < NumDifxInputCompatibilityStatistics; ++i)
	{
		difxInputCompatibilityStatistics[i] = 0;
	}
}

/* FIXME: check for overlapping time, very long gaps in time, ... */
unsigned int printDifxInputCompatibilityStatistics(int verbose)
{
	int i;
	unsigned int n = 0;

	for(i = 0; i < NumDifxInputCompatibilityStatistics; ++i)
	{
		n += difxInputCompatibilityStatistics[i];
	}

	if(verbose < 0)
	{
		return n;
	}

	switch(verbose)
	{
		case 0:
			if(n > 0)
			{
				printf("Note: At least one FITS file splitting was performed due to incompatibility\n");
				printf("of the DiFX jobs.  Rerun with increased verbosity to get some insight as to why.\n");
			}
			break;
		case 1:
			if(n > 0)
			{
				printf("The following conditions resulted in one or more FITS file splitting:\n");
				for(i = 0; i < NumDifxInputCompatibilityStatistics; ++i)
				{
					printf("  %s\n", difxInputCompatibilityDescriptions[i]);
				}
			}
			else
			{
				printf("No FITS file splitting based on DiFX job incompatibility occurred\n");
			}
			break;
		default:
			printf("The following reasons for file splitting were encountered as follows:\n");
			for(i = 0; i < NumDifxInputCompatibilityStatistics; ++i)
			{
				printf("  %u times: %s\n", difxInputCompatibilityStatistics[i], difxInputCompatibilityDescriptions[i]);
			}
			break;
	}

	return n;
}

/* This function determines if two DifxInput are not mergable
 * because difxio does not currently support it, but could
 * in the future */
int areDifxInputsCompatible(const DifxInput *D1, const DifxInput *D2, const DifxMergeOptions *mergeOptions)
{
	DifxMergeOptions defaultMergeOptions;
	int f;
	int a1;

	if(mergeOptions == 0)
	{
		resetDifxMergeOptions(&defaultMergeOptions);
		mergeOptions = &defaultMergeOptions;
	}

	if(D1->specAvg != D2->specAvg ||
	   strncmp(D1->job->difxVersion, D2->job->difxVersion, DIFXIO_VERSION_LENGTH) ||
	   strncmp(D1->job->difxLabel, D2->job->difxLabel, DIFXIO_VERSION_LENGTH))
	{
		++difxInputCompatibilityStatistics[DifxInputCompatibilityVersion];

		return 0;
	}

	switch(mergeOptions->freqMergeMode)
	{
	case FreqMergeModeStrict:
		if(D1->nFreq != D2->nFreq)
		{
			++difxInputCompatibilityStatistics[DifxInputCompatibilityNFreq];

			return 0;
		}

		for(f = 0; f < D1->nFreq; ++f)
		{
			if(isSameDifxFreq(D1->freq + f, D2->freq + f) == 0)
			{
				++difxInputCompatibilityStatistics[DifxInputCompatibilityFreqSet];

				return 0;
			}
		}
		break;
	case FreqMergeModeUnion:
		/* nothing to check here */
		break;

	default:
		/* should never happen */
		fprintf(stderr, "Developer error: Unsupported Frequency Merge Mode %d\n", (int)(mergeOptions->freqMergeMode));
		exit(0);
	}

	if(areDifxEOPsCompatible(D1->eop, D1->nEOP, D2->eop, D2->nEOP, mergeOptions) == 0)
	{
		++difxInputCompatibilityStatistics[DifxInputCompatibilityEOP];

		return 0;
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
					++difxInputCompatibilityStatistics[DifxInputCompatibilityClock];

					return 0;
				}
			}
		}
	}

	return 1;
}

DifxInput *mergeDifxInputs(const DifxInput *D1, const DifxInput *D2, const DifxMergeOptions *mergeOptions)
{
	DifxMergeOptions defaultMergeOptions;
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

	if(mergeOptions == 0)
	{
		resetDifxMergeOptions(&defaultMergeOptions);
		mergeOptions = &defaultMergeOptions;
	}

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
	D->source = mergeDifxSourceArrays(D1->source, D1->nSource,
		D2->source, D2->nSource, sourceIdRemap, &(D->nSource));

	/* merge DifxScan table */
	D->scan = mergeDifxScanArrays(D1->scan, D1->nScan, D2->scan, D2->nScan,
		sourceIdRemap, jobIdRemap, configIdRemap, antennaIdRemap, 
		&(D->nScan));

	/* merge DifxEOP table */
	D->eop = mergeDifxEOPArrays(D1->eop, D1->nEOP, D2->eop, D2->nEOP, &(D->nEOP), mergeOptions);
	
	/* merge DifxSpacecraft table */
	D->spacecraft = mergeDifxSpacecraft(D1->spacecraft, D1->nSpacecraft,
		D2->spacecraft, D2->nSpacecraft,
		spacecraftIdRemap, &(D->nSpacecraft));

	/* handle some random bits */
	if(D1->job->aberCorr != D2->job->aberCorr)
	{
		D->job->aberCorr = AberCorrMixed;
		if(D1->job->aberCorr != AberCorrMixed && D2->job->aberCorr != AberCorrMixed)
		{
			fprintf(stderr, "Warning: merging two DifxInput structures that have had different aberration correction types\n");
		}
	}

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

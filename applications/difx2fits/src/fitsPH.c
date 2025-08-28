/***************************************************************************
 *   Copyright (C) 2008-2021 by Walter Brisken & John Morgan & Leonid Petrov *
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
// $Id: fitsPH.c 10492 2022-06-06 23:26:40Z WalterBrisken $
// $HeadURL: https://svn.atnf.csiro.au/difx/master_tags/DiFX-2.8.1/applications/difx2fits/src/fitsPH.c $
// $LastChangedRevision: 10492 $
// $Author: WalterBrisken $
// $LastChangedDate: 2022-06-07 07:26:40 +0800 (二, 2022-06-07) $
//
//============================================================================

#include <stdlib.h>
#include <sys/types.h>
#include <strings.h>
#include "config.h"
#include "difx2fits.h"
#include "other.h"
#include "util.h"
#define  TIM__SMALL 0.001	/* [s] */
#define  FITSPH_DEBUG 0

const float pcaltiny = 1e-10;

#warning "FIXME: even this could be too small!"
const int MaxLineLength = 80000;
const double DefaultDifxCableCalExtrapolate = 2.0; /* The timerange a cable cal measurement 
						    * is valid over is the integration window
						    * multiplied by this factor */




/* This function is not really well posed and although it looks like it
 * belongs in difxio it should stay here until its fundamental problem is
 * fixed.  That is that it implicitly assumes one datastream per antenna.
 * to get around that, the specific datastream should be passed into this
 * function.  --WFB
 */
int DifxInputGetIFsByRecBand(int *IFs, int *polId, const DifxInput *D, int antId, int configId, int recBand, int maxCount)
{
	const DifxConfig *dc;
	const DifxFreqSet *dfs;
	const DifxDatastream *dd = 0;
	const DifxFreq *df;
	int n = 0;
	int i, d, p;
	int dsId, localFreqId;
	char polName;

	if(!D)
	{
		return -1;
	}
	if(recBand < 0 || antId >= D->nAntenna || antId < 0 || configId >= D->nConfig || configId < 0)
	{
		return -2;
	}

	dc = D->config + configId;
	dfs = D->freqSet + dc->freqSetId;

	for(d = 0; d < dc->nDatastream; ++d)
	{
		dsId = dc->datastreamId[d];
		if(dsId >= 0 && dsId < D->nDatastream)
		{
			if(D->datastream[dsId].antennaId == antId)
			{
				dd = D->datastream + dsId;

				break;
			}
		}
	}

	if(!dd)
	{
		return 0;
	}

	if(recBand >= dd->nRecBand)
	{
		return -3;
	}

	polName = dd->recBandPolName[recBand];

	if(dc->nPol > 0 && dc->pol[0] == polName)
	{
		*polId = 0;
	}
	else if(dc->nPol > 1 && dc->pol[1] == polName)
	{
		*polId = 1;
	}
	else
	{
		fprintf(stderr, "Developer error: DifxInputGetIFsByRecBand: polName=%c not in configId=%d\n", polName, configId);

		return -4;
	}

	localFreqId = dd->recBandFreqId[recBand];
	df = D->freq + dd->recFreqId[localFreqId];

	for(i = 0; i < dfs->nIF; ++i)
	{
		if(isDifxIFInsideDifxFreq(dfs->IF + i, df))
		{
			for(p = 0; p < dfs->IF[i].nPol; ++p)
			{
				if(dfs->IF[i].pol[p] == polName)
				{
					if(n < maxCount)
					{
						IFs[n] = i;
					}
					++n;
				}
			}
		}
	}

	return n;
}

/* This function is not really well posed and although it looks like it
 * belongs in difxio it should stay here until its fundamental problem is
 * fixed.  That is that it implicitly assumes one datastream per antenna.
 * to get around that, the specific datastream should be passed into this
 * function.  --WFB
 */
int DifxInputGetIFsByRecFreq(int *IFs, const DifxInput *D, int dsId, int configId, int recFreq, int polId, int maxCount)
{
	const DifxConfig *dc;
	const DifxFreqSet *dfs;
	const DifxFreq *df;
	int n = 0;
	int i, p;
	char polName;

	if(!D)
	{
		return -1;
	}
	if(recFreq < 0 || dsId >= D->nDatastream || dsId < 0 || configId >= D->nConfig || configId < 0)
	{
		return -2;
	}

	dc = D->config + configId;
	dfs = D->freqSet + dc->freqSetId;

	if(polId >= dc->nPol)
	{
		return -1;
	}

	polName = dc->pol[polId];

	if(recFreq >= D->nFreq)
	{
		return -3;
	}

	df = D->freq + recFreq;

	for(i = 0; i < dfs->nIF; ++i)
	{
		if(isDifxIFInsideDifxFreq(dfs->IF + i, df))
		{
			for(p = 0; p < dfs->IF[i].nPol; ++p)
			{
				if(dfs->IF[i].pol[p] == polName)
				{
					if(n < maxCount)
					{
						IFs[n] = i;
					}
					++n;
				}
			}
		}
	}

	return n;
}

/* Go through pcal file, determine maximum number of tones.
 * Note that this one file can contain different numbers of tones at different times,
 * so the output should only be used to set an upper limit on array sizes and
 * things of that ilk.
 */
static int getNTone(const char *filename, const char *antenna, double t1, double t2, int verbose)
{
	FILE *in;
	char line[MaxLineLength+1];
	int n, nTone, maxnTone=0;
	int maxnToneNearby = 0;
	double t;
	char *rv;
	char antName1[DIFXIO_NAME_LENGTH];

	in = fopen(filename, "r");
	if(!in)
	{
		fprintf(stderr, "\nError opening %s .  This should never happen!\n", filename);

		return -1;
	}
	
	for(;;)
	{
		rv = fgets(line, MaxLineLength, in);
		if(!rv)
		{
			break;
		}
		n = sscanf(line, "%31s%lf%*f%*f%*d%d", antName1, &t, &nTone);
		if(verbose > 2)
		{
			printf("DEBUG: getNTone: ant %s, nTone %d\n", antName1, nTone);
		}
		if(n != 3)
		{
			continue;
		}
		if(strcmp(antName1, antenna) != 0)
		{
			continue;
		}
		if(t >= t1 && t <= t2)
		{
			if(nTone > maxnTone)
			{
				maxnTone = nTone;
			}
		}
		else if(nTone > maxnToneNearby)
		{
			maxnToneNearby = nTone;
		}
	}
	fclose(in);
	if(maxnTone == 0)
	{
		maxnTone = -maxnToneNearby;
	}

	return maxnTone;
}

int isUndefinedVLBA(double v)
{
	/* 999 is a VLBA magic number meaning "not available" */
	return (v > 999.89 && v < 999.91);
}

/* The following function is for parsing a line of the station-extracted `pcal' file. 
 * Here there is an implicit assumption that there is one datastream per antenna 
 */
static int parsePulseCal(const char *line, 
	int antId,
	int *sourceId,
	double *time, float *timeInt, double *cableCal,
	double pulseCalFreq[2][array_MAX_TONES], 
	float pulseCalRe[2][array_MAX_TONES], 
	float pulseCalIm[2][array_MAX_TONES], 
	float pulseCalDeltaT[2][array_MAX_TONES],
	float stateCount[2][array_MAX_STATES*array_MAX_BANDS],
	float pulseCalRate[2][array_MAX_TONES],
	int refDay, const DifxInput *D, int *configId, 
	int doAll, int year, const struct CommandLineOptions *opts)
{
	int IFs[array_MAX_BANDS];
	int states[array_MAX_STATES];
	int np, nb, nt, ns, nIF;
	int nRecBand, recBand;
	int n, p, i;
	int polId, tone, s;
	int toneIndex;
	int pol, band;
	int scanId;
	double A;
	float B, C;
	double mjd;
	char antName[DIFXIO_NAME_LENGTH];
	
	/* Note: This is a particular NaN variant the FITS-IDI format/convention 
	 * wants, namely 0xFFFFFFFF */
	union
	{
		int32_t i32;
		float f;
	} nan;
	nan.i32 = -1;
	
	for(pol = 0; pol < 2; ++pol)
	{
		for(toneIndex = 0; toneIndex < array_MAX_TONES; ++toneIndex)
		{
			pulseCalFreq[pol][toneIndex] = 0.0;
			pulseCalRe[pol][toneIndex] = nan.f;
			pulseCalIm[pol][toneIndex] = nan.f;
			pulseCalDeltaT[pol][toneIndex] = 0.0;
			pulseCalRate[pol][toneIndex] = 0.0;
		}
		for(s = 0; s < array_MAX_STATES*array_MAX_BANDS; ++s)
		{
			stateCount[pol][s] = nan.f;
		}
	}

	n = sscanf(line, "%31s%lf%f%lf%d%d%d%d%d%n", antName, time, timeInt, cableCal, &np, &nb, &nt, &ns, &nRecBand, &p);
	if(n != 9)
	{
		return -1;
	}
	line += p;

	if(ns > array_MAX_STATES)
	{
		fprintf(stderr, "Developer error: parsePulseCal: array_MAX_STATES=%d is to small (needs to be %d)\n", array_MAX_STATES, ns);

		exit(EXIT_FAILURE);
	}

	/* A VLBA specialty! */
	if(isUndefinedVLBA(*cableCal))
	{
		*cableCal = nan.f;
	}

	if(*time > 50000)	/* must be an MJD */
	{
		mjd = *time;
		*time -= (int)(D->mjdStart);
	}
	else	/* must be day of year */
	{
		*time -= refDay;
		if(*time < -300)	/* must be new years crossing */
		{
			*time += DaysThisYear(year);
		}
		else if(*time > 300) /* must be partial project after new year */
		{
			*time -= DaysLastYear(year);
		}
		mjd = *time + (int)(D->mjdStart);
	}

	if((mjd < D->mjdStart || mjd > D->mjdStop) && (doAll == 0))
	{
		return -1;
	}

	scanId = DifxInputGetScanIdByAntennaId(D, mjd, antId);
	if(scanId < 0)
	{
		if(doAll)
		{
			scanId = 0;
		}
		else
		{
			return -3;
		}
	}

	if(opts->phaseCentre >= D->scan[scanId].nPhaseCentres)
	{
		return -3;
	}

	*sourceId = D->scan[scanId].phsCentreSrcs[opts->phaseCentre];
	*configId = D->scan[scanId].configId;
	if(*sourceId < 0 || *configId < 0)	/* not in scan */
	{
		return -3;
	}

	*cableCal *= 1e-12;	/* convert to s from ps */

	/* Read in pulse cal information */
	for(pol = 0; pol < np; ++pol)
	{
		/* Loop over number of recorded bands in .pc file. */
		for(band = 0; band < nb; ++band)
		{
			for(tone = 0; tone < nt; ++tone)
			{
				n = sscanf(line, "%d%lf%f%f%n", &recBand, &A, &B, &C, &p);
				if(n < 4)
				{
					return -4;
				}
				line += p;
				if(recBand < 0 || recBand >= nRecBand)
				{
					continue;
				}
				if(!isUndefinedVLBA(B) && !isUndefinedVLBA(C))
				{
					/* get a list of IFs (which could be zoom bands) corresponding to a recBand */
					nIF = DifxInputGetIFsByRecBand(IFs, &polId, D, antId, *configId, recBand, array_MAX_BANDS);
					if(nIF > 0 && polId >= 0 && polId < 2)
					{
						for(i = 0; i < nIF; ++i)
						{
							toneIndex = tone + IFs[i]*nt;

							/* Subtlety here: in case multiple recBands correspond to the same FITS IF,
							 * let the one corresponding to the first survive; this is the default 
							 * elsewhere in difx2fits and other programs */
							if(pulseCalFreq[polId][toneIndex] == 0.0)
							{
								pulseCalFreq[polId][toneIndex] = A*1.0e6;	/* convert to Hz from MHz */
								pulseCalRe[polId][toneIndex] = B*cos(C*M_PI/180.0);
								pulseCalIm[polId][toneIndex] = B*sin(C*M_PI/180.0);
								pulseCalDeltaT[polId][toneIndex] = (*timeInt)*86400.0;
							}
						}
					}
				}
			}
		}
	}
	
	if(ns > 0)
	{
		/* Read in state count information */
		for(pol = 0; pol < np; ++pol)
		{
			/* Loop over number of recorded bands in .pc file. */
			for(band = 0; band < nb; ++band)
			{
				n = sscanf(line, "%d%n", &recBand, &p);
				line += p;

				for(s = 0; s < ns; ++s)
				{
					n = sscanf(line, "%f%n", &B, &p);
					if(n < 1)
					{
						return -5;
					}
					line += p;
					states[s] = B;
				}

				nIF = DifxInputGetIFsByRecBand(IFs, &polId, D, antId, *configId, recBand, array_MAX_BANDS);
				if(nIF > 0 && polId >= 0 && polId < 2)
				{
					for(s = 0; s < ns; ++s)
					{
						for(i = 0; i < nIF; ++i)
						{
							stateCount[polId][s + i*4] = states[s];
						}
					}
				}
			}
		}
	}

	return 0;
}

/* The following function is for parsing a line of the 
 * station-extracted `pcal' file and extracting only the cable cal
 * value. Any values with their time centroid outside of a scan
 * will be discarded */
static int parsePulseCalCableCal(const char *line, int antId, int *sourceId, int *scanId,
	double *time, float *timeInt, double *cableCal, int refDay, const DifxInput *D, int *configId, 
	int year, const struct CommandLineOptions *opts)
{
	int n, p;
	double mjd;
	char antName[DIFXIO_NAME_LENGTH];

	/* Note: This is a particular NaN variant the FITS-IDI format/convention 
	 * wants, namely 0xFFFFFFFF */
	union
	{
		int32_t i32;
		float f;
	} nan;
	nan.i32 = -1;
	
	n = sscanf(line, "%31s%lf%f%lf%n", antName, time, timeInt, cableCal, &p);
	if(n != 4)
	{
		return -1;
	}

	if(*time > 50000)		/* must be an MJD */
	{
		mjd = *time;
		*time -= (int)(D->mjdStart);
	}
	else				/* must be day of year */
	{
		*time -= refDay;
		if(*time < -300)	/* must be new years crossing */
		{
			*time += DaysThisYear(year);
		}
		else if(*time > 300)	/* must be partial project after new year */
		{
			*time -= DaysLastYear(year);
		}
		mjd = *time + (int)(D->mjdStart);
	}

	if(mjd < D->mjdStart || mjd > D->mjdStop)
	{
		return -1;
	}

	*scanId = DifxInputGetScanIdByAntennaId(D, mjd, antId);
	if(*scanId < 0)
	{	
		return -3;
	}

	if(opts->phaseCentre >= D->scan[*scanId].nPhaseCentres)
	{
		return -3;
	}

	*sourceId = D->scan[*scanId].phsCentreSrcs[opts->phaseCentre];
	*configId = D->scan[*scanId].configId;
	if(*sourceId < 0 || *configId < 0)	/* not in scan */
	{
		return -3;
	}
	
	if(isUndefinedVLBA(*cableCal))
	{
		*cableCal = nan.f;
	}
	else
	{
		/* convert from ps to s */
		*cableCal *= 1e-12;
	}

	return 0;
}

/* The following function is for parsing a line of the files containing
 * the DiFX-extracted pulse cals of one datastream. The pulse cal data
 * are stored into output arrays (pulseCalFreq[] etc) that are zeroed
 * before being populated. Output arrays are organized by FITS IF number
 * and tone number. Since the pulse cal line might not contain data of
 * all IFs some regions of the output array may remain "blank".
 */
static int parseDifxPulseCal(const char *line, 
	const int *originalDsIds, int nds,
	int nBand, int nTone,
	int *sourceId, int *scanId, double *time, int jobId,
	double pulseCalFreq[2][array_MAX_TONES], 
	float pulseCalRe[2][array_MAX_TONES], 
	float pulseCalIm[2][array_MAX_TONES], 
	float stateCount[2][array_MAX_STATES*array_MAX_BANDS],
	float pulseCalRate[2][array_MAX_TONES],
	int refDay, const DifxInput *D, int *configId, 
	int year, const struct CommandLineOptions *opts)
{
	const DifxFreq *df;
	const DifxDatastream *dd;
	const DifxConfig *dc;
	const DifxFreqSet *dfs;
	int nt;		/* Max number of tones per band which equals number of tone records per rec band in the PCAL file */
	int nRecBand, nIF, nRecTone;
	double toneFreq[array_MAX_TONES];
	int IFs[array_MAX_BANDS];
	int n, p, i;
	int nSkip;
	int tone;
	int toneIndex;
	int band, pol, bandPol, recFreq;
	int nonTiny = 0;
	float A, B, C;
	char P[2];
	double mjd;
	char antName[DIFXIO_NAME_LENGTH];
	float timeInt, dt;
	int originalDsId;	/* from PCAL file */
	int currentDsId;	/* index to datastream in merged jobs */ 
	int slot;
	int hasValidOriginalDsId;

	/* Note: This is a particular NaN variant the FITS-IDI format/convention 
	 * wants, namely 0xFFFFFFFF */
	union
	{
		int32_t i32;
		float f;
	} nan;
	nan.i32 = -1;
	
	for(pol = 0; pol < 2; ++pol)
	{
		for(toneIndex = 0; toneIndex < array_MAX_TONES; ++toneIndex)
		{
			pulseCalFreq[pol][toneIndex] = 0.0;
			pulseCalRe[pol][toneIndex] = 0.0;
			pulseCalIm[pol][toneIndex] = 0.0;
			pulseCalRate[pol][toneIndex] = 0.0;
		}
		for(i = 0; i < array_MAX_STATES*array_MAX_BANDS; ++i)
		{
			/* No state counts are available for difx extracted pcals */
			stateCount[pol][i] = nan.f;
		}
	}

	n = sscanf(line, "%31s%lf%f%d%d%d%n", antName, time, &timeInt, &originalDsId, &nRecBand, &nt, &p);

	hasValidOriginalDsId = 0;
	for(i = 0; i < nds; ++i)
	{
		if(originalDsId == originalDsIds[i])
		{
			hasValidOriginalDsId = 1;
			break;
		}
	}

	if(!hasValidOriginalDsId)
	{
		static int nDStreamMatchError = 0;

		++nDStreamMatchError;
		if(nDStreamMatchError <= 20)
		{
			printf("\nWarning: parseDifxPulseCal: %s datastream '%d' on current line of PCAL file doesn't match one of the expected:", antName, originalDsId);
			for(i = 0; i < nds; ++i)
			{
				printf(" %d", originalDsIds[i]);
			}
			printf("\n");
		}
		if(nDStreamMatchError == 20)
		{
			printf(" ^-Note: No more warnings of this kind will be produced\n");
		}
		return -1;
	}

	if(nt > array_MAX_TONES)
	{
		static int nTooManyError = 0;

		++nTooManyError;
		if(nTooManyError <= 20)
		{
			fprintf(stderr, "Error: too many tones!  %d > array_MAX_TONES = %d\n", nt, array_MAX_TONES);
		}
		if(nTooManyError == 20)
		{
			printf(" ^-Note: No more errors of this kind will be produced\n");
		}

		return -1;
	}

	if(n != 6)
	{
		static int nNotParsableError = 0;

		++nNotParsableError;
		if(nNotParsableError <= 20)
		{
			fprintf(stderr, "Error: parseDifxPulseCal: header information not parsable (n=%d)\n", n);
			fprintf(stderr, "       Line: %s\n", line);
		}
		if(nNotParsableError == 20)
		{
			printf(" ^-Note: No more errors of this kind will be produced\n");
		}

		return -1;
	}
	line += p;

	if(*time > 50000)	/* must be an MJD */
	{
		mjd = *time;
		*time -= (int)(D->mjdStart);
	}
	else	/* must be day of year */
	{
		*time -= refDay;
		if(*time < -300)	/* must be new years crossing */
		{
			*time += DaysThisYear(year);
		}
		else if(*time > 300) /* must be partial project after new year */
		{
			*time -= DaysLastYear(year);
		}
		mjd = *time + (int)(D->mjdStart);
	}

	if(mjd < D->mjdStart || mjd > D->mjdStop)
	{
		return -2;
	}

	if(D->job[jobId].datastreamIdRemap)
	{
		currentDsId = D->job[jobId].datastreamIdRemap[originalDsId];
		if(currentDsId < 0)
		{
			return -9;
		}
	}
	else
	{
		currentDsId = originalDsId;
	}

	dd = D->datastream + currentDsId;

	*scanId = DifxInputGetScanIdByAntennaId(D, mjd, dd->antennaId);
	if(*scanId < 0)
	{	
		return -3;
	}

	if(opts->phaseCentre >= D->scan[*scanId].nPhaseCentres)
	{
		return -4;
	}

	*sourceId = D->scan[*scanId].phsCentreSrcs[opts->phaseCentre];
	*configId = D->scan[*scanId].configId;
	if(*sourceId < 0 || *configId < 0)	/* not in scan */
	{
		return -5;
	}

	dc = D->config + *configId;
	dfs = D->freqSet + dc->freqSetId;

	dt = dc->tInt/(86400.0*2.001);  
	if(D->scan[*scanId].mjdStart > mjd-dt || D->scan[*scanId].mjdEnd < mjd+dt)
	{
		return -6;
	}
	if(isAntennaFlagged(D->job + jobId, mjd, dd->antennaId))
	{
		return -7;
	}

	slot = 0;
	/* Read in pulse cal information */
	for(band = 0; band < dd->nRecBand; ++band)
	{
		int toneCount;
		int localFqId;

		toneCount = 0;

		localFqId = dd->recBandFreqId[band];
		recFreq = dd->recFreqId[localFqId];
		bandPol = DifxConfigGetPolId(D, *configId, dd->recBandPolName[band]);

		df = D->freq + recFreq;
		// assert(nt == df->nTone);

		/* this pol/band combination is not used.  Read all of the dummies from PCAL file */
		for(tone = 0; tone < nt; ++tone)	/* nt is taken from line header and is max number of tones */
		{
			n = sscanf(line, "%f%1s%f%f%n", &A, P, &B, &C, &p);
			++slot;
			if(n < 4)
			{
				static int nScanError = 0;

				++nScanError;
				if(nScanError <= 20)
				{
					printf("\nWarning: parseDifxPulseCal: Error scanning line.  DiFX PCAL file is malformed.  slot=%d n=%d band=%d tone=%d\n", slot, n, band, tone);
					printf("line=%s\n", line);
				}
				if(nScanError == 20)
				{
					printf(" ^-Note: No more warnings of this kind will be produced\n");
				}
				
				return -8;
			}
			line += p;

			/* set up pcal information for this recFreq (only up to nRecTones)*/
			/* nRecTone is simply the number of tones that fall within the recorded band */
			/* not all of them may be desired. */
			nRecTone = DifxDatastreamGetPhasecalTones(toneFreq, dd, df, nt, opts->allpcaltones);

			nSkip = 0;				/* number of tones skipped because they weren't selected in the .input file */
			if(nRecTone <= tone)
			{
				continue;
			}

			/* Only write out specified frequencies */
			if(toneFreq[tone] < 0.0)
			{
				++nSkip;

				continue;
			}

			/* Check polarization */
			if(P[0] != D->polPair[bandPol])
			{
				static int nPolMatchError = 0;

				++nPolMatchError;
				if(nPolMatchError <= 20 && !opts->antpol)
				{
					printf("\nWarning: parseDifxPulseCal: polarization in PCAL file '%c' doesn't match expected '%c' for antenna %d, mjd %12.6f  slot=%d  bandPol=%d\n", P[0], D->polPair[bandPol], dd->antennaId, mjd, slot, bandPol);
				}
				if(nPolMatchError == 20)
				{
					printf(" ^-Note: No more warnings of this kind will be produced\n");
				}
			}

			/* Check that frequency is correct */
			if(A != toneFreq[tone])
			{
				static int nFreqMatchError = 0;

				++nFreqMatchError;
				if(nFreqMatchError <= 20)
				{
					printf("\nWarning: parseDifxPulseCal: tone frequency in PCAL file %g doesn't match expected %g for antenna %d, mjd %12.6f  slot=%d\n", A, toneFreq[tone], dd->antennaId, mjd, slot);
				}
				if(nFreqMatchError == 20)
				{
					printf(" ^-Note: No more warnings of this kind will be produced\n");
				}
			}

			if(fabs(B) > pcaltiny || fabs(C) > pcaltiny)
			{
				++nonTiny;	/* there is something here! */
			}
			else
			{
				/* Replace bad pcal values with zeroes */
				B = 0.0;
				C = 0.0;
			}

			/* fill in actual pcal info */
			nIF = DifxInputGetIFsByRecFreq(IFs, D, currentDsId, *configId, recFreq, bandPol, array_MAX_BANDS);
			if(nIF > 0)
			{
				for(i = 0; i < nIF; ++i)
				{
					if(dfs->IF[IFs[i]].sideband == 'U')
					{
						/* ensure the IF does indeed contain the tone; one recFreq may have several IFs (zooms) and not all have this tone! */
						if((toneFreq[tone] < dfs->IF[IFs[i]].freq) || (toneFreq[tone] > (dfs->IF[IFs[i]].freq + dfs->IF[IFs[i]].bw)))
						{
							continue;
						}
					}
					else
					{
						if((toneFreq[tone] < (dfs->IF[IFs[i]].freq - dfs->IF[IFs[i]].bw) || (toneFreq[tone] > dfs->IF[IFs[i]].freq)))
						{
							continue;
						}
					}
					if(df->sideband == 'U')
					{
						toneIndex = IFs[i]*nTone + toneCount;
					}
					else
					{
						toneIndex = (IFs[i]+1)*nTone - toneCount - 1;	/* make LSB ascend in frequency */
					}
					pulseCalFreq[bandPol][toneIndex] = toneFreq[tone]*1.0e6;	/* MHz to Hz */
					pulseCalRe[bandPol][toneIndex] = B;
					pulseCalIm[bandPol][toneIndex] = C;
				}
			}
			++toneCount;

			if(toneCount > nTone)
			{
				static int nTooManyError;

				++nTooManyError;
				if(nTooManyError <= 20)
				{
					printf("Warning: parseDifxPulseCal: too many pulse cal tones found!  toneCount=%d nTone=%d antenna=%d mjd=%12.6f slot=%d\n", toneCount, nTone, dd->antennaId, mjd, slot);
				}
				if(nTooManyError == 20)
				{
					printf(" ^-Note: No more warnings of this kind will be produced\n");
				}
			}
		} /* loop over tones */
	} /* loop over recbands */
	if(nonTiny == 0)
	{
		return -9;
	}

	return 0;
}

static int countTones(const DifxDatastream *dd)
{
	int t;
	int n = 0;

	for(t = 0; t < dd->nRecTone; ++t)
	{
		if(dd->recToneOut[t])
		{
			++n;
		}
	}

	return n;
}

/* Create FITS PH table out of available TSM-derived VLBA classic pulse cal data files (.pcal),
 * non-DiFX cable cal files (.cablecal), or DiFX-extracted phase cal tone files (PCAL_*)
 */
const DifxInput *DifxInput2FitsPH(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out,
	const struct CommandLineOptions *opts)
{
	const int maxDatastreams = 8;	// per antenna

	char stateFormFloat[8];
	char toneFormDouble[8];
	char toneFormFloat[8];
	
	/*  define the flag FITS table columns */
	struct fitsBinTableColumn columns[] =
	{
		{"TIME", "1D", "time of center of interval", "DAYS"},
		{"TIME_INTERVAL", "1E", "time span of datum", "DAYS"},
		{"SOURCE_ID", "1J", "source id number from source tbl", 0},
		{"ANTENNA_NO", "1J", "antenna id from array geom. tbl", 0},
		{"ARRAY", "1J", "????", 0},
		{"FREQID", "1J", "freq id from frequency tbl", 0},
		{"CABLE_CAL", "1D", "cable length calibration", "SECONDS"},
		{"STATE_1", stateFormFloat, "state counts (4 per baseband)", 0},
		{"PC_FREQ_1", toneFormDouble, "Pcal recorded frequency", "Hz"},
		{"PC_REAL_1", toneFormFloat, "Pcal real", 0},
		{"PC_IMAG_1", toneFormFloat, "Pcal imag", 0},
		{"PC_RATE_1", toneFormFloat, "Pcal rate", 0},
		{"STATE_2", stateFormFloat, "state counts (4 per baseband)", 0},
		{"PC_FREQ_2", toneFormDouble, "Pcal recorded frequency", "Hz"},
		{"PC_REAL_2", toneFormFloat, "Pcal real", 0},
		{"PC_IMAG_2", toneFormFloat, "Pcal imag", 0},
		{"PC_RATE_2", toneFormFloat, "Pcal rate", 0}
	};

	int nColumn;
	int nRowBytes;
	char *fitsbuf, *p_fitsbuf;
	char line[MaxLineLength+1];
	int nBand, nPol;
	int nTone=0;
	int nDifxTone;
	int nAccum = 0;
	int lastnWindow;
	double time, dumpTime, accumStart=0.0, accumEnd=0.0;
	float timeInt, dumpTimeInt;
	int cableScanId, nextCableScanId, 
	    lineCableScanId, lineCableSourceId, lineCableConfigId;
	int newScanId = -1, newSourceId = -1, newConfigId = -1;
	double cableCal, nextCableCal, cableCalOut, lineCableCal;
	double cableTime, nextCableTime, lineCableTime;
	float cablePeriod, nextCablePeriod, lineCablePeriod;
	double cableSigma, nextCableSigma;
	double pulseCalFreq[2][array_MAX_TONES]; /* DiFX-only phase cal, temporary data */
	float pulseCalRe[2][array_MAX_TONES];
	float pulseCalIm[2][array_MAX_TONES];
	double pulseCalFreqAcc[2][array_MAX_TONES]; /* non-DiFX pulse cal data or DiFX-only but merged averaged data, for FITS writeout */
	float pulseCalReAcc[2][array_MAX_TONES]; 
	float pulseCalImAcc[2][array_MAX_TONES]; 
	float pulseCalDeltaT[2][array_MAX_TONES];
	float stateCount[2][array_MAX_STATES*array_MAX_BANDS];
	float pulseCalRate[2][array_MAX_TONES];
	int configId = -1;
	int sourceId = -1;
	int scanId;
	int refDay;
	int antennaId, k, t, n, v;
	int doDump = 0;
	int doAll = 0;
	int nWindow;
	double start, stop;
	double windowDuration=0.0, dumpWindow=0.0;
	FILE *inTSM=0;	/* file pointer for TSM-derived (VLBA classic) pulse cal data */
	FILE *inDifx=0;	/* file pointer for DiFX-derived pulse cal data */
	char *rv, *rv_cable;
	int year, month, day;
	/* The following are 1-based indices for FITS format */
	int32_t antId1, arrayId1, sourceId1, freqId1;
	char **pcalSourceFile;	/* [antId] : points to non-DiFX source of pcal information */
	int pcalExists = 0;
	int firstDsId;
	int *jobxref;
	int use_cable_cal;
	const char* use_cable_cal_str;
	char antName[DIFXIO_NAME_LENGTH];

	/* Note: This is a particular NaN variant the FITS-IDI format/convention 
	 * wants, namely 0xFFFFFFFF, or 0xFFFFFFFFFFFFFFFF for double */
	union
	{
		int64_t i64;
		double d;
		float f;
	} nan;
	nan.i64 = -1;

	if(D == 0)
	{
		return D;
	}

	printf("\n");
	nBand = D->nIF;
	nPol = D->nPol;
	scanId = -1;

	/* Check DIFX_USE_CABLE_CAL whether to use cable cal. Default: to use (1). 
	 * if DIFX_USE_CABLE_CAL is not 1, then not to use.
	 */
	use_cable_cal = 1; 
	use_cable_cal_str = getenv("DIFX_USE_CABLE_CAL");
	if(use_cable_cal_str != NULL)
	{
		use_cable_cal = atoi(use_cable_cal_str);
	}
	rv       = NULL;
	rv_cable = NULL;

	jobxref = malloc(D->nJob * sizeof(scanId));

	mjd2dayno((int)(D->mjdStart), &refDay);
	mjd2date((int)(D->mjdStart), &year, &month, &day);

	start = D->mjdStart - (int)(D->mjdStart);
	stop  = D->mjdStop  - (int)(D->mjdStart);

	pcalSourceFile = (char **)calloc(D->nAntenna, sizeof(char **));

	/* First go through and find out maximum number of tones to be encountered.
	 * This is more complicated than it might seem as tones can come from 
	 * different places.  Store the place to find tones for each antenna in
	 * pcalSourceFile[] .
	 */
	inTSM = fopen("pcal", "r");
	if(inTSM)
	{
		fclose(inTSM);
		inTSM = 0;
		pcalExists = 1;
	}

	for(antennaId = 0; antennaId < D->nAntenna; ++antennaId)
	{
		char pcalFile[DIFXIO_FILENAME_LENGTH];

		v = snprintf(pcalFile, DIFXIO_FILENAME_LENGTH, "%s%s.%s.pcal", D->job->obsCode, D->job->obsSession, D->antenna[antennaId].name);
		if(v >= DIFXIO_FILENAME_LENGTH)
		{
			fprintf(stderr, "Developer error: DifxInput2FitsPH: DIFXIO_FILENAME_LENGTH=%d is too small.  Wants to be %d.\n", DIFXIO_FILENAME_LENGTH, v+1);

			exit(EXIT_FAILURE);
		}

		v = globcase(__FUNCTION__, "*.*.pcal", pcalFile);
		if(v == 0)
		{
			/* try looking for a pure cable cal file */
			v = snprintf(pcalFile, DIFXIO_FILENAME_LENGTH, "%s%s.%s.cablecal", D->job->obsCode, D->job->obsSession, D->antenna[antennaId].name);
			if(v >= DIFXIO_FILENAME_LENGTH)
			{
				fprintf(stderr, "Developer error: DifxInput2FitsPH: DIFXIO_FILENAME_LENGTH=%d is too small.  Wants to be %d.\n", DIFXIO_FILENAME_LENGTH, v+1);

				exit(EXIT_FAILURE);
			}
		
			v = globcase(__FUNCTION__, "*.*.cablecal", pcalFile);
		}
		if(v == 0)
		{
			if(pcalExists)
			{
				strcpy(pcalFile, "pcal");
			}
			else
			{
				continue;
			}
		}

		v = getNTone(pcalFile, D->antenna[antennaId].name, refDay + start, refDay + stop, opts->verbose);
		pcalSourceFile[antennaId] = strdup(pcalFile);
		if(v != 0)
		{
			if(abs(v) > nTone)
			{
				nTone = v;
			}
		}
	}
	if(nTone < 0)
	{
		/* If we are here with nTone < 0, that means that no tones were found within the
		 * time period specified by this job, but that potentially useful tones just outside
		 * that time range do exist.
		 *
		 * setting doAll to 1 causes the time range restriction to be dropped when choosing
		 * whether or not to save a tone.
		 */

		nTone = -nTone;
		doAll = 1;
	}
	if(opts->verbose)
	{
		printf("    Number of pcal tones from external files: %d\n", nTone);
	}

	nDifxTone = DifxInputGetMaxTones(D);
	if(opts->verbose)
	{
		printf("    Number of DiFX exteracted pcal tones:     %d\n", nDifxTone);
	}
	if(nDifxTone == 0)
	{
		printf("    No Tones extracted by DiFX\n");
	}
	else
	{
		printf("    DiFX-extracted tones available:");
	}

	if(nDifxTone > nTone)
	{
		nTone = nDifxTone;
	}

	if(nTone*nBand > array_MAX_TONES)
	{
		printf("Developer Error: DifxInput2FitsPH: nTone(=%d)*nBand(=%d) exceeds array_MAX_TONES(=%d).  No pulse cal data will be enFITSulated.\n", nTone, nBand, array_MAX_TONES);

		exit(EXIT_FAILURE);
	}
	if(nTone == 0)
	{
		free(pcalSourceFile);

		return D;
	}

	sprintf(stateFormFloat, "%dE", 4*nBand);
	sprintf(toneFormFloat,  "%dE", nTone*nBand);
	sprintf(toneFormDouble, "%dD", nTone*nBand);
	
	if(nPol == 2)
	{
		nColumn = NELEMENTS(columns);
	}
	else
	{
		nColumn = NELEMENTS(columns) - 5;
	}
	
	nRowBytes = FitsBinTableSize(columns, nColumn);

	/* calloc space for storing table in FITS format */
	fitsbuf = (char *)calloc(nRowBytes, 1);
	if(fitsbuf == 0)
	{
		fprintf(stderr, "Error: DifxInput2FitsPH: Memory allocation failure\n");

		exit(EXIT_FAILURE);
	}

	fitsWriteBinTable(out, nColumn, columns, nRowBytes, "PHASE-CAL");
	arrayWriteKeys (p_fits_keys, out);
	fitsWriteInteger(out, "NO_POL", nPol, "");
	fitsWriteInteger(out, "NO_TONES", nTone, "");
	fitsWriteInteger(out, "TABREV", 2, "");
	fitsWriteEnd(out);

	arrayId1 = 1;

	printf("  ");
	for(antennaId = 0; antennaId < D->nAntenna; ++antennaId)
	{
		int maxDifxTones;	/* maximum number of tones expected for any job on a given antenna, summed over all datastreams */
		int jobId, jobIdUnsorted;

		maxDifxTones = 0;

		for(jobId = 0; jobId < D->nJob; ++jobId)
		{
			int originalDsIds[maxDatastreams];	/* datastream IDs in the jobs that was run */
			int nds, nt;
			int d;

			nds = DifxInputGetOriginalDatastreamIdsByAntennaIdJobId(originalDsIds, D, antennaId, jobId, maxDatastreams);

			if(nds <= 0)
			{
				/* antenna not present in this job */
				continue;	/* to next job */
			}

			nt = 0;
			for(d = 0; d < nds; ++d)
			{
				int mergedDsId;

				if(D->job[jobId].datastreamIdRemap)
				{
					mergedDsId = D->job[jobId].datastreamIdRemap[originalDsIds[d]];
				}
				else
				{
					mergedDsId = originalDsIds[d];
				}
				if(mergedDsId >= 0)
				{
					int ct;

					ct = countTones(&(D->datastream[mergedDsId]));
					nt += ct;
				}
				else
				{
					fprintf(stderr, "Weird: mergedDsId=%d\n", mergedDsId);

					exit(EXIT_FAILURE);
				}
			}

			if(nt > maxDifxTones)
			{
				maxDifxTones = nt;
			}
		}

		if(pcalSourceFile[antennaId] && use_cable_cal == 1)
		{
			/* Open cable calibration file */
			
			inTSM = fopen(pcalSourceFile[antennaId], "r");
			if(inTSM == 0)
			{
				fprintf(stderr, "Error: cannot open %s.  This should never happen!\n", pcalSourceFile[antennaId]);

				continue;
			}
			if(FITSPH_DEBUG > 1)
			{
				printf("%s line %4d D->antenna[antennaId].name: %s jobId: %d opened cable cal file %s\n",
					__FILE__, __LINE__, D->antenna[antennaId].name, jobId, pcalSourceFile[antennaId]);
			}
		}
		else
		{
			inTSM = 0;
		}

		for(k = 0; k < 2; ++k)
		{
			for(t = 0; t < array_MAX_TONES; ++t)
			{
				pulseCalFreqAcc[k][t] = 0.0;
				pulseCalReAcc[k][t] = 0.0;
				pulseCalImAcc[k][t] = 0.0;
				pulseCalDeltaT[k][t] = 0.0;
			}
		}
		nWindow = 0;
		nAccum = 0;

		scanId = -2;
		sourceId = -2;
		printf(" %s", D->antenna[antennaId].name);
		fflush(stdout);

		/* Generate the cross-reference table jobxref in order to process jobs in the chronological orer */
		v = sortjobpcal(D, antennaId, jobxref);

		for(jobIdUnsorted = 0; jobIdUnsorted < D->nJob; ++jobIdUnsorted)
		{
			glob_t globBuffer;
			int nDifxFile = 0;
			int curDifxFile = 0;
			double mjdLast = 0.0;
			int nDifxAntennaTones;
			int freqSetId;
			int originalDsIds[maxDatastreams];
			int originalDsId = -1;
			int nds;	// number of datastreams for this antenna for this job
			int d;
			int ndumps = 0;	// How many times pcal data have been dumped
			int nLine, nLines;

			printf(".");
			fflush(stdout);

			jobId = jobxref[jobIdUnsorted]; /* Get sorted Job Id index */
			if(FITSPH_DEBUG > 0)
			{
				printf("%s line %4d D->antenna[antennaId].name: %s jobIdUnsorted: %d jobId: %d use_cable_cal: %d inTSM: %p\n",
					__FILE__, __LINE__, D->antenna[antennaId].name, jobIdUnsorted, jobId, use_cable_cal, inTSM);
			}
			if(jobId < 0)
			{
				/* Antenna antennaId did not observe in this job and did not produce a phase calibration file to parse */
				break;
			}
			nds = DifxInputGetOriginalDatastreamIdsByAntennaIdJobId(originalDsIds, D, antennaId, jobId, maxDatastreams);

			if(nds <= 0)
			{
				/* antenna not present in this job */
				continue;	/* to next job */
			}

			nDifxAntennaTones = 0;

			firstDsId = -1;

			/* get the number of tones DiFX should have extracted */
			for(d = 0; d < nds; ++d)
			{
				int mergedDsId;

				if(D->job[jobId].datastreamIdRemap)
				{
					mergedDsId = D->job[jobId].datastreamIdRemap[originalDsIds[d]];
				}
				else
				{
					mergedDsId = originalDsIds[d];
				}
				if(mergedDsId >= 0)
				{
					nDifxAntennaTones += countTones(&(D->datastream[mergedDsId]));
					if(firstDsId < 0)
					{
						firstDsId = mergedDsId;
					}
				}
			}

			if(D->datastream[firstDsId].phaseCalIntervalMHz > 0 && nDifxAntennaTones > 0)
			{
				char globPattern[DIFXIO_FILENAME_LENGTH];

				v = snprintf(globPattern, DIFXIO_FILENAME_LENGTH, "%s/PCAL*%s", D->job[jobId].outputFile, D->antenna[antennaId].name);
				if(v >= DIFXIO_FILENAME_LENGTH)
				{
					fprintf(stderr, "Developer error: DifxInput2FitsPH: DIFXIO_NAME_LENGTH = %d, need to be longer: %d\n", DIFXIO_NAME_LENGTH, v+1);

					exit(EXIT_FAILURE);
				}

				v = glob2(__FUNCTION__, globPattern, 0, 0, &globBuffer);
				
				/* Sort PCAL files chronologically */
				v = glob2_sort_pcal_files( &globBuffer);

				nDifxFile = globBuffer.gl_pathc;

				if(nDifxFile == 0)	/* no files found */
				{
					fprintf(stderr, "\nWarning: No PCAL files matching %s found for job %s antenna %s\n", globPattern, D->job[jobId].outputFile, D->antenna[antennaId].name);
					globfree(&globBuffer);

					continue;	/* to next job */
				}
				
				curDifxFile = 0;
				inDifx = fopen(globBuffer.gl_pathv[curDifxFile], "r");
				if(!inDifx)
				{
					fprintf(stderr, "Warning: PCAL file %s could not be opened for read!\n", globBuffer.gl_pathv[curDifxFile]);
				
					continue;
				}
				if(FITSPH_DEBUG > 1)
				{
					printf ( "%s line %4d D->antenna[antennaId].name: %s opened pcal file %s inDifx: %p\n",
						__FILE__, __LINE__, D->antenna[antennaId].name, globBuffer.gl_pathv[curDifxFile], inDifx);
				}
			}
			else
			{
				/* Don't mix DiFX and station pcals within a given station */
				if(maxDifxTones > 0)
				{
					printf("Warning: no pcals for Antenna %s in JobId %d\n", D->antenna[antennaId].name, jobId);

					continue;	/* to next job */
				}
			}

			/* At this point at least one of inTSM and inDifx should be non-zero */
			/* If both are nonzero, inDifx points to pulse cal data and inTSM points to cable cal data */
			if(inDifx == 0 && inTSM == 0)
			{
				printf("<No tones found>");

				continue;
			}

			/* set defaults */
			cableScanId = -1;
			cableCal = 0.0;
			cableTime = 0.0;
			cablePeriod= -1.0;
			nextCableScanId = -1;
			nextCableCal = 0.0;
			nextCableTime = 0.0;
			nextCablePeriod = -1.0;
			lineCableScanId = -1;
			lastnWindow = nWindow;
			nLines = 0;

			if(inDifx && opts->DifxPcalAvgSeconds < TIM__SMALL)
			{
				/* Count the number lines in the pcal difx file that are used for computation of the accumulated pcal (nLines) */
				while(1)
				{
					rv = fgets(line, MaxLineLength, inDifx);
					if(!rv)
					{
						break;
					}
					if(line[0] == '#')
					{
						continue;	/* to next line in file */
					}
					originalDsId = parseDifxPulseCal(line, originalDsIds, nds, nBand, nTone, &newSourceId, &newScanId, &time, jobId,
							pulseCalFreq, pulseCalRe, pulseCalIm, stateCount, pulseCalRate, refDay, D, &newConfigId, year, opts);
					if(originalDsId >= 0)
					{
						nLines++;
					}
				}
				fclose(inDifx);
				inDifx = fopen(globBuffer.gl_pathv[curDifxFile], "r");
			}

			nLine = 0;
			while(1)	/* each pass here reads one line from a pcal file */
			{
				if(inTSM && !nDifxTone)	/* try reading pcal file if no DiFX-supplied pulse cal is available */
				{
					rv = fgets(line, MaxLineLength, inTSM);
					if(rv)
					nLine++;
					{
						/* ignore possible comment lines */
						if(line[0] == '#')
						{
							continue;	/* to next line in file */
						}
						else 
						{
							if(FITSPH_DEBUG > 1)
							{
								printf("%s line %4d %.64s \n", __FILE__, __LINE__, line);
							}
							n = sscanf(line, "%31s", antName);
							if(n != 1 || strcmp(antName, D->antenna[antennaId].name))
							{
								continue;	/* to next line in file */	
							}
							v = parsePulseCal(line, antennaId, &sourceId, &time, &timeInt, &cableCal, pulseCalFreqAcc, pulseCalReAcc, pulseCalImAcc, pulseCalDeltaT, stateCount, pulseCalRate, refDay, D, &configId, doAll, year, opts);
							if(v < 0)
							{
								continue;	/* to next line in file */
							}
						}
					}
					doDump = 1;	/* write out every line for pcal file -- i.e., no averaging */
				}
				else /* reading difx-extracted pcals */
				{	
					if(!inDifx)
					{
						printf("\n    No DiFX file to read for antenna %s", D->antenna[antennaId].name);
						break;
					}
					rv = fgets(line, MaxLineLength, inDifx);
					if(!rv)
					{
						fclose(inDifx);
						inDifx = 0;

						/* try advancing through the file list */
						for(++curDifxFile; curDifxFile < nDifxFile; ++curDifxFile)
						{
							inDifx = fopen(globBuffer.gl_pathv[curDifxFile], "r");
							if(inDifx)
							{
								rv = fgets(line, MaxLineLength, inDifx);
							}
							else
							{
								fprintf(stderr, "Error: open of file %s failed.  This should never happen and indicates a real problem.\n", globBuffer.gl_pathv[curDifxFile]);
							}
							if(rv)
							{
								break;
							}
						}
						if(!inDifx)
						{
							break;
						}
					}
					if(rv)
					{
						/* ignore possible comment lines */
						if(line[0] == '#')
						{
							continue;	/* to next line in file */
						}
						else 
						{
							double mjdRecord;
							
							originalDsId = parseDifxPulseCal(line, originalDsIds, nds, nBand, nTone, &newSourceId, &newScanId, &time, jobId,
										pulseCalFreq, pulseCalRe, pulseCalIm, stateCount, pulseCalRate,
										refDay, D, &newConfigId, year, opts);

							if(FITSPH_DEBUG > 2)
							{
								printf("%s line %4d file: %s nLine: %d nLines: %d line: %.80s originalDsId: %d \n",
									__FILE__, __LINE__, globBuffer.gl_pathv[curDifxFile], nLine, nLines, line, originalDsId);
							}
							if(originalDsId < 0)
							{
								continue;	/* to next line in file */
							}

							mjdRecord = time - refDay + (int)(D->mjdStart);

							if(mjdRecord < mjdLast)
							{
								/* probably due to overlapping PCAL files */

								continue;	/* to next line in file */
							}
							else
							{
								mjdLast = mjdRecord;
							}
						}
						nLine++;
						if(scanId != newScanId)
						{
							double s, e;
							DifxScan *scan;

							/* Get ready to dump last scan (if it's not the first run through)
							 * Work out time average windows so that there is an integer number within the scan
							 * Get all of the relevant cable cal values for this antenna
							 * n.b. we can safely assume that we are at  valid pcal entry since v > 0 */
							if(scanId != -2 && opts->DifxPcalAvgSeconds > TIM__SMALL)
							{
								doDump = 1;
							}
							else
							{
								scanId = newScanId;
								sourceId = newSourceId;
								configId = newConfigId;
							}

							scan = D->scan + newScanId;
							s = time;	/* time of first pcal record */
							e = scan->mjdEnd - (int)(D->mjdStart);

							nWindow = (int)((e - s + (0.5*D->config[newConfigId].tInt/86400.))/(opts->DifxPcalAvgSeconds/86400.0) + 0.5);

							if(nWindow < 1)
							{
								nWindow = 1;
							}
							windowDuration = (e - s)/nWindow;
							dumpWindow = s + windowDuration;
							ndumps = 0;
						}
						else if(time > dumpWindow && dumpWindow > 0.0)
						{
							doDump = 1;

							while(time > dumpWindow)
							{
								dumpWindow += windowDuration;
							}
						}
						else if(nWindow == 1 && ndumps == 0 && D->config[newConfigId].tInt*nAccum > opts->DifxPcalAvgSeconds - TIM__SMALL && opts->DifxPcalAvgSeconds > TIM__SMALL)
						{
							doDump = 1;
						}
						else if(nLines > 0 && nLine == nLines && ndumps == 0 && opts->DifxPcalAvgSeconds < TIM__SMALL && nAccum > 0)
						{
							/* This is the last useable line in the file, but we still did not dump it.
							 * when DifxPcalAvgSeconds == 0.0, we are in the pcal over a whole scan mode,
							 * therefore, we need dump it.
							 */
							doDump = 1;
							if(FITSPH_DEBUG > 1)
							{
								printf ( "%s line %4d D->antenna[antennaId].name: %s pcal_file: %s Reached last line nAccum: %d\n",
									__FILE__, __LINE__, D->antenna[antennaId].name, globBuffer.gl_pathv[curDifxFile], nAccum);
							}
						}
					}
					else if(jobIdUnsorted == D->nJob-1)
					{
						/* end of last job reached */
						doDump = 1;
					}
				}

				/*
				 * Write a PH table entry if indicated.
				 * The written pcal data are either the newly read .pcal data (pulseCalReAcc[] etc) for which we do not support further time averaging,
				 * or are the older accumulated DiFX phase cal data (pulseCalReAcc[] etc) averaged without the above most recent data (pulseCalRe[] etc).
				 * The written pcal data are augmented by TSM cable cal data if available.
				 */
				if(doDump && configId >= 0)
				{
					if(nDifxTone)
					{
						dumpTime = (accumStart + accumEnd)*0.5;
						dumpTimeInt = nAccum*D->config[configId].tInt/86400;
						if(nAccum > 0)
						{
							ndumps = ndumps + 1;
						}
						/* Divide pcals through by integration time in seconds */
						for(k = 0; k < nPol; ++k)
						{
							for(t = 0; t < nTone*nBand; ++t)
							{
								if(pulseCalDeltaT[k][t] > 0.0)
								{
									pulseCalReAcc[k][t] /= pulseCalDeltaT[k][t];
									pulseCalImAcc[k][t] /= pulseCalDeltaT[k][t];
								}
							}
						}

						/* Include TSM cable cal data if available */
						if(inTSM)
						{
							while(nextCableTime < dumpTime || lineCableScanId < 0)
							{
								rv_cable = fgets(line, MaxLineLength, inTSM);
								if(!rv_cable)
								{
							                cableCalOut = 0.0;
									break;	/* to out of cablecal search */
								}
									
								/* ignore possible comment lines */
								if(line[0] == '#')
								{
									continue;	/* to next line in file */
								}
								else 
								{
									n = sscanf(line, "%31s", antName);
									if(n != 1 || strcmp(antName, D->antenna[antennaId].name))
									{
										continue;	/* to next line in file */	
									}
									v = parsePulseCalCableCal(line, antennaId, &lineCableSourceId, &lineCableScanId, &lineCableTime, &lineCablePeriod, &lineCableCal, refDay, D, &lineCableConfigId, year, opts);
									if(v < 0)
									{
										continue;	/* to next line in file */
									}

									if(lineCablePeriod <= 0.0)
									{
										lineCablePeriod = 60.0/86400.0;
									}

									cableScanId = nextCableScanId;
									cableTime = nextCableTime;
									cablePeriod = nextCablePeriod;
									cableCal = nextCableCal;
									nextCableScanId = lineCableScanId;
									nextCableTime = lineCableTime;
									nextCablePeriod = lineCablePeriod;
									nextCableCal = lineCableCal;
								}
							}
							/* next choose which cable cal value to use (if any) */
							nextCableSigma = 2*fabs(dumpTime - nextCableTime)/(nextCablePeriod);
							cableSigma = 2*fabs(dumpTime - cableTime)/(cablePeriod);
							if(cableSigma > DefaultDifxCableCalExtrapolate && nextCableSigma > DefaultDifxCableCalExtrapolate)
							{
								cableCalOut = nan.f;
							}
							else if(cableScanId != scanId && nextCableScanId != scanId)
							{
								cableCalOut = nan.f;
							}
							else if(cableScanId == scanId && nextCableScanId != scanId)
							{
								cableCalOut = cableCal;
							}
							else if(cableScanId != scanId && nextCableScanId == scanId)
							{
								cableCalOut = nextCableCal;
							}
							else if(fabs(cableTime-dumpTime) > fabs(nextCableTime-dumpTime))
							{
								cableCalOut = nextCableCal;
							}
							else
							{
								cableCalOut = cableCal;
							}
						}
						else
						{
							cableCalOut = 0.0;
						}
					}	/* end if nDifxTone */
					else
					{
						/* Default cable cal data when TSM file not available */
						dumpTime = time;
						dumpTimeInt = timeInt;
						cableCalOut = cableCal;
					}

					/* Write the finalized newest record of phase cal and cable cal data into FITS table */
					freqSetId = D->config[configId].freqSetId;
					freqId1 = freqSetId + 1;
					if(sourceId >= 0)
					{
						sourceId1 = D->source[sourceId].fitsSourceIds[freqSetId] + 1;
					}
					else
					{
						sourceId1 = 0;
					}
					antId1 = antennaId + 1;
					p_fitsbuf = fitsbuf;
					if(!nDifxTone || nAccum*D->config[configId].tInt > 0.25*opts->DifxPcalAvgSeconds || (nAccum > 0 && lastnWindow == 1)) /* only write a row if a reasonable amount of data were averaged */
					{
						FITS_WRITE_ITEM(dumpTime, p_fitsbuf);
						FITS_WRITE_ITEM(dumpTimeInt, p_fitsbuf);
						FITS_WRITE_ITEM(sourceId1, p_fitsbuf);
						FITS_WRITE_ITEM(antId1, p_fitsbuf);
						FITS_WRITE_ITEM(arrayId1, p_fitsbuf);
						FITS_WRITE_ITEM(freqId1, p_fitsbuf);
						FITS_WRITE_ITEM(cableCalOut, p_fitsbuf);

						for(k = 0; k < nPol; ++k)
						{
							int l;

							for(l = 0; l < nTone*nBand; ++l) /* find and mask all no-data entries before writeout */
							{
								if(pulseCalReAcc[k][l] == 0.0 && pulseCalImAcc[k][l] == 0.0)
								{
									pulseCalFreqAcc[k][l] = nan.d;
									pulseCalReAcc[k][l] = nan.f;
									pulseCalImAcc[k][l] = nan.f;
									pulseCalRate[k][l] = nan.f;
								}
							}

							FITS_WRITE_ARRAY(stateCount[k], p_fitsbuf, 4*nBand);
							if(nTone > 0)
							{
								FITS_WRITE_ARRAY(pulseCalFreqAcc[k], p_fitsbuf, nTone*nBand);
								FITS_WRITE_ARRAY(pulseCalReAcc[k], p_fitsbuf, nTone*nBand);
								FITS_WRITE_ARRAY(pulseCalImAcc[k], p_fitsbuf, nTone*nBand);
								FITS_WRITE_ARRAY(pulseCalRate[k], p_fitsbuf, nTone*nBand);
							}
						}

						testFitsBufBytes(p_fitsbuf - fitsbuf, nRowBytes, "PH");
						
#ifndef WORDS_BIGENDIAN
						FitsBinRowByteSwap(columns, nColumn, fitsbuf);
#endif
						fitsWriteBinRow(out, fitsbuf);
					}

					/* Wrote data (or perhaps not), now clear the accumulators */
					nAccum = 0;
					doDump = 0;
					scanId = newScanId;
					sourceId = newSourceId;
					configId = newConfigId;
					for(k = 0; k < nPol; ++k)
					{
						for(t = 0; t < nTone*nBand; ++t)
						{
							pulseCalFreqAcc[k][t] = 0.0;
							pulseCalReAcc[k][t] = 0.0;
							pulseCalImAcc[k][t] = 0.0;
							pulseCalDeltaT[k][t] = 0.0;
						}
					}

				}	/* end if doDump */

				if(!rv)
				{
					/* after dumping, if needed, we go to the next job */
					break;
				}
				if(nLines > 0 && nLine == nLines && opts->DifxPcalAvgSeconds < TIM__SMALL)
				{
					break;
				}

				/* Accumulate data of current line into joint output array (multi-datastream multi-line pcal => joint record) */
				if(originalDsId >= 0)
				{
					for(k = 0; k < nPol; ++k)
					{
						for(t = 0; t < nTone*nBand; ++t)
						{
							if(pulseCalFreq[k][t] > 0) /* tone has data on current phase-cal line */
							{
								if(pulseCalFreqAcc[k][t] == 0)
								{
									pulseCalFreqAcc[k][t] = pulseCalFreq[k][t];
								}

								if(pulseCalFreqAcc[k][t] != pulseCalFreq[k][t])
								{
									fprintf(stderr, "Fatal error in processing phase calibrition. Weird: existing pulseCalFreqAcc[PC tone %d] of %.3f MHz and difx pcal file pulseCalFreq[PC tone %d] of %.3f MHz do not match, possible FITS IF numbering issue\n",
										t, pulseCalFreqAcc[k][t]*1e-6, t, pulseCalFreq[k][t]*1e-6);
									if(nDifxTone)
									{
										fprintf(stderr, "Error in processing PCAL file %s\n", globBuffer.gl_pathv[curDifxFile]);
									}
									exit(EXIT_FAILURE);
								}
								pulseCalReAcc[k][t] += pulseCalRe[k][t];
								pulseCalImAcc[k][t] += pulseCalIm[k][t];
								pulseCalDeltaT[k][t] += D->config[configId].tInt;
							}
						}
					}
				}
				if(nAccum == 0)
				{
					accumStart = time;
				}
				accumEnd = time;
				++nAccum;
			}	/* end while (line-by-line) */
			
			if(nDifxFile > 0)
			{
				globfree(&globBuffer);
			}
			
			if(!nDifxTone)
			{
				break;	/* to next antenna */
			}

			/* DiFX-based pcal data is in per-job files; make sure at end of each job there are none open. */
			if(inDifx)
			{
				fclose(inDifx);
				inDifx = 0;
			}

		}	/* end job loop */

		/* TSM-based pcal data is in per-antenna files; makes sure at end of each antenna loop there are none open. */
		if(inTSM)
		{
			fclose(inTSM);
			inTSM = 0;
		}

	}	/* end antenna loop */

	free(fitsbuf);
	free(jobxref);

	for(antennaId = 0; antennaId < D->nAntenna; ++antennaId)
	{
		if(pcalSourceFile[antennaId])
		{
			free(pcalSourceFile[antennaId]);
		}
	}
	free(pcalSourceFile);

	printf("\n");

	return D;
}

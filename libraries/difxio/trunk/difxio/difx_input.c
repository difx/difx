/***************************************************************************
 *   Copyright (C) 2007-2010 by Walter Brisken & Adam Deller               *
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
#include "difxio/parsedifx.h"

/* allocate empty structure, do minimal initialization */
DifxInput *newDifxInput()
{
	DifxInput *D;

	D = (DifxInput *)calloc(1, sizeof(DifxInput));
	D->specAvg = 1;
	D->visBufferLength = 32;

	return D;
}

void deleteDifxInput(DifxInput *D)
{
	if(D)
	{
		if(D->config)
		{
			deleteDifxConfigArray(D->config,
				D->nConfig);
		}
		if(D->datastream)
		{
			deleteDifxDatastreamArray(D->datastream, 
				D->nDatastream);
		}
		if(D->baseline)
		{
			deleteDifxBaselineArray(D->baseline,
				D->nBaseline);
		}
		if(D->freq)
		{
			deleteDifxFreqArray(D->freq, D->nFreq);
		}
		if(D->antenna)
		{
			deleteDifxAntennaArray(D->antenna, D->nAntenna);
		}
		if(D->scan)
		{
			deleteDifxScanArray(D->scan, D->nScan);
		}
		if(D->source)
		{
			deleteDifxSourceArray(D->source, D->nSource);
		}
		if(D->eop)
		{
			deleteDifxEOPArray(D->eop);
		}
		if(D->job)
		{
			deleteDifxJobArray(D->job);
		}
		if(D->rule)
		{
			deleteDifxRuleArray(D->rule);
		}
		if(D->nThread)
		{
			DifxInputAllocThreads(D, 0);
		}
		free(D);
	}
}

void DifxConfigMapAntennas(DifxConfig *dc, const DifxDatastream *ds)
{
	int a, maxa = 0, d;

	if(dc->ant2dsId)
	{
		free(dc->ant2dsId);
	}

	for(d = 0; d < dc->nDatastream; d++)
	{
		a = ds[dc->datastreamId[d]].antennaId;
		if(a > maxa)
		{
			maxa = a;
		}
	}

	dc->nAntenna = maxa+1;
	dc->ant2dsId = (int *)malloc((maxa+2)*sizeof(int));
	for(a = 0; a < maxa; a++)
	{
		dc->ant2dsId[a] = -1;
	}

	for(d = 0; d < dc->nDatastream; d++)
	{
		a = ds[dc->datastreamId[d]].antennaId;
		dc->ant2dsId[a] = dc->datastreamId[d];
	}
}

void fprintDifxInput(FILE *fp, const DifxInput *D)
{
	int i;

	fprintf(fp, "DifxInput : %p\n", D);
	if(!D)
	{
		return;
	}

	fprintf(fp, "  input file = %s\n", D->inputFile);
	fprintf(fp, "  threads (core conf) file = %s\n", D->threadsFile);
	fprintf(fp, "  calc file = %s\n", D->calcFile);
	fprintf(fp, "  im (model) file = %s\n", D->imFile);
	fprintf(fp, "  output file = %s\n", D->outputFile);
	fprintf(fp, "  mjdStart = %14.8f\n", D->mjdStart);
	fprintf(fp, "  mjdStop  = %14.8f\n", D->mjdStop);
	fprintf(fp, "  vis buffer length = %d\n", D->visBufferLength);
	fprintf(fp, "  Input Channels = %d\n", D->nInChan);
	fprintf(fp, "  Start Channel = %d\n", D->startChan);
	fprintf(fp, "  Spectral Avg = %d\n", D->specAvg);
	fprintf(fp, "  Output Channels = %d\n", D->nOutChan);

	fprintf(fp, "  nJob = %d\n", D->nJob);
	for(i = 0; i < D->nJob; i++)
	{
		fprintDifxJob(fp, D->job + i);
	}

	fprintf(fp, "  nConfig = %d\n", D->nConfig);
	for(i = 0; i < D->nConfig; i++)
	{
		fprintDifxConfig(fp, D->config + i);
	}

	fprintf(fp, "  nFreq = %d\n", D->nFreq);
	for(i = 0; i < D->nFreq; i++)
	{
		fprintDifxFreq(fp, D->freq + i);
	}

	fprintf(fp, "  nAntenna = %d\n", D->nAntenna);
	for(i = 0; i < D->nAntenna; i++)
	{
		fprintDifxAntenna(fp, D->antenna + i);
	}

	fprintf(fp, "  nSource = %d\n", D->nSource);
	for(i = 0; i < D->nSource; i++)
	{
		fprintDifxSource(fp, D->source + i);
	}

	fprintf(fp, "  nScan = %d\n", D->nScan);
	for(i = 0; i < D->nScan; i++)
	{
		fprintDifxScan(fp, D->scan + i);
	}

	fprintf(fp, "  nEOP = %d\n", D->nEOP);
	if(D->eop) for(i = 0; i < D->nEOP; i++)
	{
		fprintDifxEOP(fp, D->eop + i);
	}

	fprintf(fp, "  nDatastreamEntries = %d\n", D->nDatastream);
	for(i = 0; i < D->nDatastream; i++)
	{
		fprintDifxDatastream(fp, D->datastream + i);
	}

	fprintf(fp, "  nBaselineEntries = %d\n", D->nBaseline);
	for(i = 0; i < D->nBaseline; i++)
	if(D->nBaseline > 1)
	{
		fprintDifxBaseline(fp, D->baseline + i);
	}

	fprintf(fp, "  nSpacecraft = %d\n", D->nSpacecraft);
	for(i = 0; i < D->nSpacecraft; i++)
	{
		fprintDifxSpacecraft(fp, D->spacecraft + i);
	}

	fprintf(fp, "  nPulsar = %d\n", D->nPulsar);
	for(i = 0; i < D->nPulsar; i++)
	{
		fprintDifxPulsar(fp, D->pulsar + i);
	}

	fprintf(fp, "\n");
}

void printDifxInput(const DifxInput *D)
{
	fprintDifxInput(stdout, D);
}

void fprintDifxInputSummary(FILE *fp, const DifxInput *D)
{
	int i;

	fprintf(fp, "Summary\n");
	if(!D)
	{
		return;
	}

	fprintf(fp, "  mjdStart = %14.8f\n", D->mjdStart);
	fprintf(fp, "  mjdStop  = %14.8f\n", D->mjdStop);
	//fprintf(fp, "  Input Channels = %d\n", D->nInChan);
	fprintf(fp, "  Start Channel = %d\n", D->startChan);
	fprintf(fp, "  Spectral Avg = %d\n", D->specAvg);
	//fprintf(fp, "  Output Channels = %d\n", D->nOutChan);

	fprintf(fp, "  nJob = %d\n", D->nJob);
	for(i = 0; i < D->nJob; i++)
	{
		fprintDifxJob(fp, D->job + i);
	}

	fprintf(fp, "  nConfig = %d\n", D->nConfig);
	for(i = 0; i < D->nConfig; i++)
	{
		fprintDifxConfigSummary(fp, D->config + i);
	}

	fprintf(fp, "  nAntenna = %d\n", D->nAntenna);
	for(i = 0; i < D->nAntenna; i++)
	{
		fprintDifxAntennaSummary(fp, D->antenna + i);
	}

	fprintf(fp, "  nSource = %d\n", D->nSource);
	for(i = 0; i < D->nSource; i++)
	{
		fprintDifxSourceSummary(fp, D->source + i);
	}

	fprintf(fp, "  nScan = %d\n", D->nScan);
	for(i = 0; i < D->nScan; i++)
	{
		fprintDifxScanSummary(fp, D->scan + i);
	}

	fprintf(fp, "  nEOP = %d\n", D->nEOP);
	if(D->eop) for(i = 0; i < D->nEOP; i++)
	{
		fprintDifxEOPSummary(fp, D->eop + i);
	}

	if(D->nSpacecraft > 0)
	{
		fprintf(fp, "  nSpacecraft = %d\n", D->nSpacecraft);
	}
	for(i = 0; i < D->nSpacecraft; i++)
	{
		fprintDifxSpacecraft(fp, D->spacecraft + i);
	}

	if(D->nPulsar > 0)
	{
		fprintf(fp, "  nPulsar = %d\n", D->nPulsar);
	}
	for(i = 0; i < D->nPulsar; i++)
	{
		fprintDifxPulsar(fp, D->pulsar + i);
	}
}

void printDifxInputSummary(const DifxInput *D)
{
	fprintDifxInputSummary(stdout, D);
}

static int *deriveAntMap(const DifxInput *D, DifxParameters *p, int *nTelescope)
{
	int r, nTel, nFound, t, a;
	int *antMap;

	r = DifxParametersfind(p, 0, "NUM TELESCOPES");
	if(r >= 0)
	{
		nTel = atoi(DifxParametersvalue(p, r));
	}
	else
	{
		fprintf(stderr, "deriveAntMap: NUM TELESCOPES not defined\n");

		return 0;
	}
	
	if(nTel < D->nAntenna)
	{
		fprintf(stderr, "deriveAntMap: NUM TELESCOPES too small: \n%d < %d\n", 
			nTel, D->nAntenna);
	}

	antMap = (int *)malloc(nTel* sizeof(int));

	nFound = 0;
	for(t = 0; t < nTel; t++)
	{
		r = DifxParametersfind1(p, r+1, "TELESCOPE %d NAME", t);
		if(r < 0)
		{
			fprintf(stderr, "deriveAntMap: TELESCOPE %d NAME not found\n", t);
			free(antMap);
			
			return 0;
		}
		a = DifxInputGetAntennaId(D, DifxParametersvalue(p, r));
		if(a < 0)
		{
			antMap[t] = -1;
		}
		else
		{
			antMap[t] = a;
			nFound++;
		}
	}

	if(nFound < D->nAntenna)
	{
		fprintf(stderr, "deriveAntMap: too few antenna name matches\n");
		free(antMap);

		return 0;
	}

	if(nTelescope)
	{
		*nTelescope = nTel;
	}

	return antMap;
}

static int polMaskValue(char polName)
{
	switch(polName)
	{
		case 'R':
		case 'r':
			return DIFXIO_POL_R;
		case 'L':
		case 'l':
			return DIFXIO_POL_L;
		case 'X':
		case 'x':
			return DIFXIO_POL_X;
		case 'Y':
		case 'y':
			return DIFXIO_POL_Y;
		default:
			return DIFXIO_POL_ERROR;
	}
}

/* This function populates four array fields (and their
 * scalar-valued companions) in a DifxConfig object:
 *   freqId2IF[]
 *   freqIdUsed[]
 *   IF[]
 *   pol[]
 */
static int generateAipsIFs(DifxInput *D, int configId)
{
	DifxConfig *dc;
	DifxBaseline *db;
	int bl, blId, dsId, rc, fqId, localFqId;
	int *freqIds;
	int p, f, i;
	char polName;

	if(configId < 0)
	{
		fprintf(stderr, "Warning: generateAipsIFs: configId = %d\n", configId);

		return 0;
	}

	dc = D->config + configId;


	/* Prepare some arrays */
	if(dc->freqIdUsed)
	{
		free(dc->freqIdUsed);
	}
	dc->freqIdUsed = (int *)calloc(D->nFreq+1, sizeof(int));
	dc->freqIdUsed[D->nFreq] = -1;

	if(dc->IF)
	{
		deleteDifxIFArray(dc->IF);
		dc->IF = 0;
	}
	D->nIF = 0;

	if(dc->freqId2IF)
	{
		free(dc->freqId2IF);
	}
	dc->freqId2IF = (int *)malloc((D->nFreq+2)*sizeof(int));
	for(f = 0; f < D->nFreq; f++)
	{
		dc->freqId2IF[f] = -1;
	}
	dc->freqId2IF[D->nFreq] = -2;	/* special list terminator */

	dc->polMask = 0;
	dc->nPol = 0;
	dc->pol[0] = dc->pol[1] = ' ';


	/* Look for used D->freq[] entries and polarization entries */
	for(bl = 0; bl < dc->nBaseline; bl++)
	{
		blId = dc->baselineId[bl];
		if(blId < 0)
		{
			continue;
		}
		db = D->baseline + blId;

		/* f here refers to the baseline frequency list */
		for(f = 0; f < db->nFreq; f++)
		{
			if(db->nPolProd[f] < 0)
			{
				continue;
			}
			for(p = 0; p < db->nPolProd[f]; p++)
			{
				dsId = db->dsA;
				rc = db->recBandA[f][p];
				localFqId = D->datastream[dsId].recBandFreqId[rc];
				polName = D->datastream[dsId].recBandPolName[rc];
				fqId = D->datastream[dsId].recFreqId[localFqId];
				dc->freqIdUsed[fqId]++;
				dc->polMask |= polMaskValue(polName);

				dsId = db->dsB;
				rc = db->recBandB[f][p];
				localFqId = D->datastream[dsId].recBandFreqId[rc];
				polName = D->datastream[dsId].recBandPolName[rc];
				fqId = D->datastream[dsId].recFreqId[localFqId];
				dc->freqIdUsed[fqId]++;
				dc->polMask |= polMaskValue(polName);
			}
		}
	}

	if(dc->polMask & DIFXIO_POL_ERROR || 
	   dc->polMask == 0 || 
	   ((dc->polMask & DIFXIO_POL_RL) && (dc->polMask & DIFXIO_POL_XY)) )
	{
		fprintf(stderr, "Error: generateAipsIFs: polMask = 0x%03x is unsupported!\n", dc->polMask);

		return -1;
	}

	/* populate polarization matrix for this configuration */
	if(dc->polMask & DIFXIO_POL_R)
	{
		dc->pol[dc->nPol] = 'R';
		dc->nPol++;
	}
	if(dc->polMask & DIFXIO_POL_L)
	{
		dc->pol[dc->nPol] = 'L';
		dc->nPol++;
	}
	if(dc->polMask & DIFXIO_POL_X)
	{
		dc->pol[dc->nPol] = 'X';
		dc->nPol++;
	}
	if(dc->polMask & DIFXIO_POL_Y)
	{
		dc->pol[dc->nPol] = 'Y';
		dc->nPol++;
	}

	/* Actually construct the IF array */

	/* First count IFs and make map to freqId */
	freqIds = (int *)calloc(D->nFreq, sizeof(int));
	for(fqId = 0; fqId < D->nFreq; fqId++)
	{
		if(dc->freqIdUsed[fqId] > 0)
		{
			freqIds[dc->nIF] = fqId;
			dc->nIF++;
		}
	}

	/* Then actually build it */
	dc->IF = newDifxIFArray(dc->nIF);
	for(i = 0; i < dc->nIF; i++)
	{
		f = freqIds[i];
		dc->freqId2IF[f]   = i;
		dc->IF[i].freq     = D->freq[f].freq;
		dc->IF[i].bw       = D->freq[f].bw;
		dc->IF[i].sideband = D->freq[f].sideband;
		dc->IF[i].nPol     = dc->nPol;
		dc->IF[i].pol[0]   = dc->pol[0];
		dc->IF[i].pol[1]   = dc->pol[1];
	}

	free(freqIds);

	return 0;
}

static DifxInput *parseDifxInputCommonTable(DifxInput *D, 
	const DifxParameters *ip)
{
	const char commonKeys[][MAX_DIFX_KEY_LEN] =
	{
		"CALC FILENAME",
		"CORE CONF FILENAME",
		"EXECUTE TIME (SEC)",
		"START MJD",
		"START SECONDS",
		"ACTIVE DATASTREAMS",
		"ACTIVE BASELINES",
		"VIS BUFFER LENGTH",
		"OUTPUT FILENAME"
	};
	const int N_COMMON_ROWS = sizeof(commonKeys)/sizeof(commonKeys[0]);
	int N;
	int rows[N_COMMON_ROWS];
	int v;
	
	if(!D || !ip)
	{
		return 0;
	}

	N = DifxParametersbatchfind(ip, 0, commonKeys, N_COMMON_ROWS, rows);
	if(N < N_COMMON_ROWS)
	{
		fprintf(stderr, "populateInput: N < N_COMMON_ROWS %d < %d\n", N, N_COMMON_ROWS);

		return 0;
	}

	/* Initialize some of the structures */
	
	D->job->duration = atoi(DifxParametersvalue(ip, rows[2]));
	D->job->mjdStart = atoi(DifxParametersvalue(ip, rows[3])) +
		      atof(DifxParametersvalue(ip, rows[4]))/86400.0;
	if(atof(DifxParametersvalue(ip, rows[4])) != atoi(DifxParametersvalue(ip, rows[4])))
	{
		D->fracSecondStartTime = 1;
	}
	D->job->activeDatastreams = atoi(DifxParametersvalue(ip, rows[5]));
	D->job->activeBaselines   = atoi(DifxParametersvalue(ip, rows[6]));
	D->visBufferLength        = atoi(DifxParametersvalue(ip, rows[7]));
	v = snprintf(D->calcFile, DIFXIO_FILENAME_LENGTH, "%s", DifxParametersvalue(ip, rows[0]));
	if(v >= DIFXIO_FILENAME_LENGTH)
	{
		fprintf(stderr, "populateInput: CALC FILENAME too long (%d > %d)\n", v, DIFXIO_FILENAME_LENGTH-1);

		return 0;
	}
	v = snprintf(D->threadsFile, DIFXIO_FILENAME_LENGTH, "%s", DifxParametersvalue(ip, rows[1]));
	if(v >= DIFXIO_FILENAME_LENGTH)
	{
		fprintf(stderr, "populateInput: CORE CONF FILENAME too long (%d > %d)\n", v, DIFXIO_FILENAME_LENGTH-1);

		return 0;
	}
	v = snprintf(D->outputFile, DIFXIO_FILENAME_LENGTH, "%s", DifxParametersvalue(ip, rows[8]));
	if(v >= DIFXIO_FILENAME_LENGTH)
	{
		fprintf(stderr, "populateInput: OUTPUT FILENAME too long (%d > %d)\n", v, DIFXIO_FILENAME_LENGTH-1);

		return 0;
	}

	if(DifxParametersfind(ip, 0, "DATA HEADER O/RIDE") > 0)
	{
		printf("Warning: parsing old version of input file.\n");
		printf("  Watch out for unusual behavior!\n");
		D->inputFileVersion = 1;	/* pre-Perth Merge version */
	}

	return D;
}	

/* return -1 on a failure */
static int loadPhasedArrayConfigFile(DifxInput *D, const char *fileName)
{
	DifxParameters *pp;
	DifxPhasedArray *dpa;
	int r;

	pp = newDifxParametersfromfile(fileName);
	if(!pp) 
	{
		fprintf(stderr, "Problem opening or reading %s\n", fileName);

		return -1;
	}

	D->phasedarray = growDifxPhasedarrayArray(D->phasedarray, D->nPhasedArray);
	dpa = D->phasedarray + D->nPhasedArray;

	/* Fill in the info about quantisation, format etc for this phased array output */
	r = DifxParametersfind(pp, 0, "OUTPUT TYPE");
	snprintf(dpa->outputType, DIFXIO_NAME_LENGTH, "%s", DifxParametersvalue(pp, r));
	r = DifxParametersfind(pp, r, "OUTPUT FORMAT");
	snprintf(dpa->outputFormat, DIFXIO_NAME_LENGTH, "%s", DifxParametersvalue(pp, r));
	r = DifxParametersfind(pp, r, "ACC TIME (NS)");
	dpa->accTime = atoi(DifxParametersvalue(pp, r));
	r = DifxParametersfind(pp, r, "COMPLEX OUTPUT");
	dpa->complexOutput = 0;
	if(strcasecmp(DifxParametersvalue(pp, r), "TRUE"))
	{
		dpa->complexOutput = 1;
	}
	r = DifxParametersfind(pp, r, "OUTPUT BITS");
	dpa->quantBits = atoi(DifxParametersvalue(pp, r));

	return D->nPhasedArray-1;
}

/* return -1 on a failure */
int loadPulsarConfigFile(DifxInput *D, const char *fileName)
{
	DifxParameters *pp;
	DifxPulsar *dp;
	int i, r;
	int nPolycoFiles;

	pp = newDifxParametersfromfile(fileName);
	if(!pp)
	{
		fprintf(stderr, "Problem opening or reading %s\n", fileName);

		return -1;
	}

	D->pulsar = growDifxPulsarArray(D->pulsar, D->nPulsar);
	dp = D->pulsar + D->nPulsar;

	r = DifxParametersfind(pp, 0, "NUM POLYCO FILES");
	if(r < 0)
	{
		deleteDifxParameters(pp);
		fprintf(stderr, "NUM POLYCO FILES not found\n");

		return -1;
	}
	nPolycoFiles = atoi(DifxParametersvalue(pp, r));
	dp->nPolyco = 0;
	dp->polyco = 0;

	snprintf(dp->fileName, DIFXIO_FILENAME_LENGTH, "%s", fileName);

	for(i = 0; i < nPolycoFiles; i++)
	{
		r = DifxParametersfind1(pp, r, "POLYCO FILE %d", i);
		if(r < 0)
		{
			deleteDifxParameters(pp);
			fprintf(stderr, "POLYCO FILE %d not found\n", i);

			return -1;
		}
		r = loadPulsarPolycoFile(&dp->polyco, &dp->nPolyco,
			DifxParametersvalue(pp, r));
		if(r < 0)
		{
			deleteDifxParameters(pp);

			return -1;
		}
	}

	r = DifxParametersfind(pp, 0, "NUM PULSAR BINS");
	if(r < 0)
	{
		deleteDifxParameters(pp);
		fprintf(stderr, "NUM PULSAR BINS not found\n");

		return -1;
	}
	dp->nBin = atoi(DifxParametersvalue(pp, r));
	dp->binEnd = (double *)calloc(dp->nBin, sizeof(double));
	dp->binWeight = (double *)calloc(dp->nBin, sizeof(double));

	r = DifxParametersfind(pp, 0, "SCRUNCH OUTPUT");
	if(r < 0)
	{
		deleteDifxParameters(pp);
		fprintf(stderr, "SCRUNCH OUTPUT not found\n");

		return -1;
	}
	if(strcasecmp(DifxParametersvalue(pp, r), "TRUE") == 0)
	{
		dp->scrunch = 1;
	}

	for(i = 0; i < dp->nBin; i++)
	{
		r = DifxParametersfind1(pp, r, "BIN PHASE END %d", i);
		if(r < 0)
		{
			deleteDifxParameters(pp);
			fprintf(stderr, "BIN PHASE END %d not found\n", i);

			return -1;
		}
		dp->binEnd[i] = atof(DifxParametersvalue(pp, r));

		r = DifxParametersfind1(pp, r, "BIN WEIGHT %d", i);
		if(r < 0)
		{
			deleteDifxParameters(pp);
			fprintf(stderr, "BIN WEIGHT %d not found\n", i);

			return -1;
		}
		dp->binWeight[i] = atof(DifxParametersvalue(pp, r));
	}

	D->nPulsar++;
	deleteDifxParameters(pp);

	return D->nPulsar-1;
}

static DifxInput *parseDifxInputConfigurationTable(DifxInput *D,
	const DifxParameters *ip)
{
	const char configKeys[][MAX_DIFX_KEY_LEN] =
	{
		"CONFIG NAME",
		"INT TIME (SEC)",
		"SUBINT NANOSECONDS",
		"GUARD NANOSECONDS",
		"FRINGE ROTN ORDER",
		"ARRAY STRIDE LENGTH",
		"XMAC STRIDE LENGTH",
		"NUM BUFFERED FFTS",
		"WRITE AUTOCORRS",
		"PULSAR BINNING",
		"PHASED ARRAY"
	};
	const int N_CONFIG_ROWS = sizeof(configKeys)/sizeof(configKeys[0]);
	int a, b, c, r, N;
	int rows[N_CONFIG_ROWS];
	DifxConfig *dc;

	if(!D || !ip)
	{
		return 0;
	}

	r = DifxParametersfind(ip, 0, "NUM CONFIGURATIONS");
	if(r < 0)
	{
		fprintf(stderr, "NUM CONFIGURATIONS not found\n");

		return 0;
	}
	D->nConfig  = atoi(DifxParametersvalue(ip, r));
	D->config   = newDifxConfigArray(D->nConfig);
	rows[N_CONFIG_ROWS-1] = 0;	/* initialize start */
	for(c = 0; c < D->nConfig; c++)
	{
		dc = D->config + c;
		N = DifxParametersbatchfind(ip, rows[N_CONFIG_ROWS-1], 
			configKeys, N_CONFIG_ROWS, rows);
		if(N < N_CONFIG_ROWS)
		{
			fprintf(stderr, "parseDifxInputConfigurations: N < N_CONFIG_ROWS %d "
				"< %d\n", N, N_CONFIG_ROWS);

			return 0;
		}
		snprintf(dc->name, DIFXIO_NAME_LENGTH, "%s", DifxParametersvalue(ip, rows[0]));
		dc->tInt           = atof(DifxParametersvalue(ip, rows[1]));
		dc->subintNS       = atoi(DifxParametersvalue(ip, rows[2]));
		dc->guardNS        = atoi(DifxParametersvalue(ip, rows[3]));
		dc->fringeRotOrder = atoi(DifxParametersvalue(ip, rows[4]));
		dc->strideLength   = atoi(DifxParametersvalue(ip, rows[5]));
		dc->xmacLength     = atoi(DifxParametersvalue(ip, rows[6]));
		dc->numBufferedFFTs= atoi(DifxParametersvalue(ip, rows[7]));
		dc->doAutoCorr     = 
			abs(strcmp("FALSE", DifxParametersvalue(ip, rows[8])));
		dc->nDatastream  = D->job->activeDatastreams;
		dc->nBaseline    = D->job->activeBaselines;

		/* pulsar stuff */
		if(strcmp(DifxParametersvalue(ip, rows[9]), "TRUE") == 0)
		{
			r = DifxParametersfind(ip, rows[9], 
				"PULSAR CONFIG FILE");
			if(r <= 0)
			{
				fprintf(stderr, "input file row %d : PULSAR CONFIG FILE expected\n",
					rows[9] + 2);

				return 0;
			}
			dc->pulsarId = loadPulsarConfigFile(D,
				DifxParametersvalue(ip, r));
			if(dc->pulsarId < 0)
			{
				return 0;
			}
		}
		/* phased array stuff */
		if(strcmp(DifxParametersvalue(ip, rows[10]), "TRUE") == 0)
		{
			r = DifxParametersfind(ip, rows[10],
				"PHASED ARRAY CONFIG FILE");
			if(r <= 0)
			{
				fprintf(stderr, "input file row %d : PHASED ARRAY CONFIG FILE expected\n",
					rows[10] + 2);

				return 0;
			}
			dc->phasedArrayId = loadPhasedArrayConfigFile(D,
				DifxParametersvalue(ip, r));
			if(dc->phasedArrayId < 0)
			{
				return 0;
			}
		}
		N = strlen(dc->name);
		dc->doPolar = -1;	/* to be calculated later */

		/* initialize datastream index array */
		dc->datastreamId = (int *)malloc(sizeof(int)*
			(dc->nDatastream + 1));
		
		/* here "a" is "datastream # within conf", not "antenna" */
		for(a = 0; a <= D->nDatastream; a++)
		{
			dc->datastreamId[a] = -1;
		}

		/* populate datastream index array */
		/* here "a" is "datastream # within conf", not "antenna" */
		for(a = 0; a < dc->nDatastream; a++)
		{

			r = DifxParametersfind1(ip, r+1, 
				"DATASTREAM %d INDEX", a);
			if(r < 0)
			{
				fprintf(stderr, "DATASTREAM %d INDEX not found\n", a);

				return 0;
			}
			dc->datastreamId[a] = atoi(DifxParametersvalue(ip, r));
		}

		/* initialize baseline index array; -1 terminated */
		dc->baselineId = (int *)malloc(sizeof(int)* (dc->nBaseline+1));
		for(b = 0; b <= dc->nBaseline; b++)
		{
			dc->baselineId[b] = -1;
		}

		/* populate baseline index array */
		for(b = 0; b < dc->nBaseline; b++)
		{
			r = DifxParametersfind1(ip, r+1, 
				"BASELINE %d INDEX", b);
			if(r < 0)
			{
				fprintf(stderr, "BASELINE %d INDEX not found\n", b);

				return 0;
			}
			dc->baselineId[b] = atoi(DifxParametersvalue(ip, r));
		}
	}

	return D;
}

static DifxInput *parseDifxInputRuleTable(DifxInput *D,
	const DifxParameters *ip)
{
	int r, rule;

	r = DifxParametersfind(ip, 0, "NUM RULES");
	if(r<0)
	{
		fprintf(stderr, "NUM RULES not found\n");

		return 0;
	}
	D->nRule = atoi(DifxParametersvalue(ip, r));
	D->rule  = newDifxRuleArray(D->nRule);
	for(rule=0;rule<D->nRule;rule++)
	{
		r = DifxParametersfind1(ip, r+1, "RULE %d SOURCE", rule);
		if(r>=0)
		{
			snprintf(D->rule[rule].sourceName, DIFXIO_NAME_LENGTH, "%s", DifxParametersvalue(ip, r));
		}
		r = DifxParametersfind1(ip, r+1, "RULE %d SCAN ID", rule);
		if(r>=0)
		{
			snprintf(D->rule[rule].scanId, DIFXIO_NAME_LENGTH, "%s", DifxParametersvalue(ip, r));
		}
		r = DifxParametersfind1(ip, r+1, "RULE %d CALCODE", rule);
		if(r>=0)
		{
			snprintf(D->rule[rule].calCode, DIFXIO_CALCODE_LENGTH, "%s", DifxParametersvalue(ip, r));
		}
		r = DifxParametersfind1(ip, r+1, "RULE %d QUAL", rule);
		if(r>=0)
		{
			D->rule[rule].qual = atoi(DifxParametersvalue(ip, r));
		}
		r = DifxParametersfind1(ip, r+1, "RULE %d MJD START", rule);
		if(r>=0)
		{
			D->rule[rule].mjdStart = atof(DifxParametersvalue(ip, r));
		}
		r = DifxParametersfind1(ip, r+1, "RULE %d MJD STOP", rule);
		if(r>=0)
		{
			D->rule[rule].mjdStop = atof(DifxParametersvalue(ip, r));
		}
		r = DifxParametersfind1(ip, r+1, "RULE %d CONFIG NAME", rule);
		if(r<0)
		{
			fprintf(stderr, "RULE %d CONFIG NAME not found\n", rule);

			return 0;
		}
		snprintf(D->rule[rule].configName, DIFXIO_NAME_LENGTH, "%s", DifxParametersvalue(ip, r));
	}
	return D;
}

static DifxInput *parseDifxInputFreqTable(DifxInput *D, 
	const DifxParameters *ip)
{
	const char freqKeys[][MAX_DIFX_KEY_LEN] =
	{
		"FREQ (MHZ) %d",
		"BW (MHZ) %d",
		"SIDEBAND %d",
		"NUM CHANNELS %d",
		"CHANS TO AVG %d",
		"OVERSAMPLE FAC. %d",
		"DECIMATION FAC. %d"
	};
	const int N_FREQ_ROWS = sizeof(freqKeys)/sizeof(freqKeys[0]);
	int b, r, t, N;
	int rows[N_FREQ_ROWS];
	
	if(!D || !ip)
	{
		return 0;
	}

	r = DifxParametersfind(ip, 0, "FREQ ENTRIES");
	if(r < 0)
	{
		fprintf(stderr, "FREQ ENTRIES not found\n");

		return 0;
	}
	D->nFreq    = atoi(DifxParametersvalue(ip, r));
	D->freq     = newDifxFreqArray(D->nFreq);
	rows[N_FREQ_ROWS-1] = 0;	/* initialize start */
	for(b = 0; b < D->nFreq; b++)
	{
		N = DifxParametersbatchfind1(ip, rows[N_FREQ_ROWS-1], freqKeys, 
			b, N_FREQ_ROWS, rows);
		if(N < N_FREQ_ROWS)
		{
			fprintf(stderr, "populateInput: N < N_FREQ_ROWS %d < %d\n", N, N_FREQ_ROWS);

			return 0;
		}
		D->freq[b].freq     = atof(DifxParametersvalue(ip, rows[0]));
		D->freq[b].bw       = atof(DifxParametersvalue(ip, rows[1]));
		D->freq[b].sideband = DifxParametersvalue(ip, rows[2])[0];
		D->freq[b].nChan    = atoi(DifxParametersvalue(ip, rows[3]));
		D->freq[b].specAvg  = atoi(DifxParametersvalue(ip, rows[4]));
		D->freq[b].overSamp = atoi(DifxParametersvalue(ip, rows[5]));
		D->freq[b].decimation = atoi(DifxParametersvalue(ip, rows[6]));
		
		r = DifxParametersfind1(ip, rows[6]+1, "PHASE CALS %d OUT", b);
		if(r > 0)
		{
			DifxFreqAllocTones(&(D->freq[b]), atoi(DifxParametersvalue(ip, r)));
			for(t = 0; t < D->freq[b].nTone; t++)
			{
				r = DifxParametersfind2(ip, r+1, "PHASE CAL %d/%d INDEX", b, t);
				if(r < 0)
				{
					fprintf(stderr, "PHASE CAL %d/%d INDEX not found in .input file\n", b, t);
				}
				D->freq[b].tone[t] = atoi(DifxParametersvalue(ip, r));
			}
		}

		D->nInChan = D->freq[b].nChan;
		D->nOutChan = D->freq[b].nChan/D->freq[b].specAvg;
	}
	
	return D;
}

static DifxInput *parseDifxInputTelescopeTable(DifxInput *D, 
	const DifxParameters *ip)
{
	const char antKeys[][MAX_DIFX_KEY_LEN] =
	{
		"TELESCOPE NAME %d",
		"CLOCK REF MJD %d",
		"CLOCK POLY ORDER %d"
	};
	const int N_ANT_ROWS = sizeof(antKeys)/sizeof(antKeys[0]);
	int a, i, r, N;
	int rows[N_ANT_ROWS];

	if(!D || !ip)
	{
		return 0;
	}

	r = DifxParametersfind(ip, 0, "TELESCOPE ENTRIES");
	if(r < 0)
	{
		fprintf(stderr, "TELESCOPE ENTRIES not found\n");

		return 0;
	}
	D->nAntenna = atoi(DifxParametersvalue(ip, r));
	D->antenna  = newDifxAntennaArray(D->nAntenna);

	rows[N_ANT_ROWS-1] = 0;		/* initialize start */
	for(a = 0; a < D->nAntenna; a++)
	{
		N = DifxParametersbatchfind1(ip, rows[N_ANT_ROWS-1], antKeys,
			a, N_ANT_ROWS, rows);
		if(N < N_ANT_ROWS)
		{
			fprintf(stderr, "populateInput: N < N_ANT_ROWS %d < %d\n", N, N_ANT_ROWS);

			return 0;
		}
		snprintf(D->antenna[a].name, DIFXIO_NAME_LENGTH, "%s", DifxParametersvalue(ip, rows[0]));
		D->antenna[a].clockrefmjd = atof(DifxParametersvalue(ip, rows[1]));
		D->antenna[a].clockorder  = atoi(DifxParametersvalue(ip, rows[2]));
		r = rows[2];
		for(i=0;i<D->antenna[a].clockorder+1;i++)
		{
			r = DifxParametersfind2(ip, r, "CLOCK COEFF %d/%d", a, i);
			D->antenna[a].clockcoeff[i] = atof(DifxParametersvalue(ip, r));
		}
	}

	return D;
}

static DifxInput *parseDifxInputDatastreamTable(DifxInput *D,
	const DifxParameters *ip)
{
	int a, e, i, r, r1, r2, v, nr, nz;
	int nRecBand, nZoomBand;

	if(!D || !ip)
	{
		return 0;
	}

	r = DifxParametersfind(ip, 0, "DATASTREAM ENTRIES");
	if(r < 0)
	{
		fprintf(stderr, "Cannot find DATASTREAM TABLE\n");

		return 0;
	}
	D->nDatastream = atoi(DifxParametersvalue(ip, r));
	D->datastream = newDifxDatastreamArray(D->nDatastream);

	r2 = DifxParametersfind(ip, r+1, "DATA BUFFER FACTOR");
	if(r2 > 0)
	{
		D->dataBufferFactor = atoi(DifxParametersvalue(ip, r2));
	}

	r2 = DifxParametersfind(ip, r+1, "NUM DATA SEGMENTS");
	if(r2 > 0)
	{
		D->nDataSegments = atoi(DifxParametersvalue(ip, r2));
	}

	for(e = 0; e < D->nDatastream; e++)
	{
		r = DifxParametersfind(ip, r+1, "TELESCOPE INDEX");
		if(r < 0)
		{
			fprintf(stderr, "TELESCOPE INDEX not found\n");
			
			return 0;
		}
		D->datastream[e].antennaId = atoi(DifxParametersvalue(ip, r));
		
		r = DifxParametersfind(ip, r+1, "DATA FORMAT");
		if(r < 0)
		{
			fprintf(stderr, "DATA FORMAT not found\n");
			
			return 0;
		}
		snprintf(D->datastream[e].dataFormat, DIFXIO_NAME_LENGTH, "%s",
			DifxParametersvalue(ip, r));
	
		r = DifxParametersfind(ip, r+1, "QUANTISATION BITS");
		if(r < 0)
		{
			fprintf(stderr, "Cannot determine quantization bits\n");
			
			return 0;
		}
		D->datastream[e].quantBits = atoi(DifxParametersvalue(ip, r));

		r = DifxParametersfind(ip, r+1, "DATA FRAME SIZE");
		if(r < 0)
		{
			fprintf(stderr, "Cannot determine data frame size\n");
			
			return 0;
		}
		D->datastream[e].dataFrameSize = 
			atoi(DifxParametersvalue(ip, r));

		r = DifxParametersfind(ip, r+1, "DATA SAMPLING");
		if(r < 0)
		{
			fprintf(stderr, "DATA SAMPLING not found\n");
			
			return 0;
		}
		D->datastream[e].dataSampling = stringToSamplingType( DifxParametersvalue(ip, r) );
		if(D->datastream[e].dataSampling >= NumSamplingTypes)
		{
			fprintf(stderr, "Error: DATA SAMPLING was %s and is not supported\n", 
				DifxParametersvalue(ip, r) );
			
			return 0;
		}
		
		r = DifxParametersfind(ip, r+1, "DATA SOURCE");
		if(r < 0)
		{
			fprintf(stderr, "Cannot determine data source\n");
			
			return 0;
		}
		D->datastream[e].dataSource = stringToDataSource( DifxParametersvalue(ip, r) );
		if(D->datastream[e].dataSource >= NumDataSources)
		{
			fprintf(stderr, "Error: DATA SOURCE was %s and is not supported\n",
				DifxParametersvalue(ip, r) );
			
			return 0;
		}

		/* note use of r1 here: this parameter is optional and not unique. */
		r1 = DifxParametersfind(ip, r+1, "TCAL FREQUENCY");
		if(r1 > 0 && r1 < r+5)
		{
			D->datastream[e].tcalFrequency = atoi(DifxParametersvalue(ip, r1));
		}

		r = DifxParametersfind(ip, r+1, "PHASE CAL INT (MHZ)");
		if(r < 0)
		{
			fprintf(stderr, "Cannot determine phase cal interval\n");

			return 0;
		}
		D->datastream[e].phaseCalIntervalMHz = atoi(DifxParametersvalue(ip, r));

		r = DifxParametersfind(ip, r+1, "NUM RECORDED FREQS");
		if(r < 0)
		{
			fprintf(stderr, "NUM RECORDED FREQS not found\n");
			
			return 0;
		}

		DifxDatastreamAllocFreqs(D->datastream + e, 
			atoi(DifxParametersvalue(ip, r)));

		nRecBand = 0;
		for(i = 0; i < D->datastream[e].nRecFreq; i++)
		{
			r = DifxParametersfind1(ip, r+1, 
				"REC FREQ INDEX %d", i);
			D->datastream[e].recFreqId[i] = 
				atoi(DifxParametersvalue(ip, r));
			r = DifxParametersfind1(ip, r+1, 
				"CLK OFFSET %d (us)", i);
			D->datastream[e].clockOffset[i] =
				atof(DifxParametersvalue(ip, r));
			r = DifxParametersfind1(ip, r+1,
				"FREQ OFFSET %d (Hz)", i);
			D->datastream[e].freqOffset[i] =
				atof(DifxParametersvalue(ip, r));
			r = DifxParametersfind1(ip, r+1, 
				"NUM REC POLS %d", i);
			D->datastream[e].nRecPol[i] = 
				atoi(DifxParametersvalue(ip, r));
			nRecBand += D->datastream[e].nRecPol[i];
		}

		/* count rec chans to make sure we have enough */
		nr = 0;
		for(i = 0; ; i++)
		{
			v = DifxParametersfind1(ip, r,
				"REC BAND %d POL", i);
			if(v <= 0 || v > r+2*i+2)
			{
				break;
			}
			nr++;
		}

		if(nr > nRecBand)
		{
			nRecBand = nr;
		}

		DifxDatastreamAllocBands(D->datastream + e, nRecBand);

		for(i = 0; i < nRecBand; i++)
		{
			r = DifxParametersfind1(ip, r+1,
				"REC BAND %d POL", i);
			if(r < 0)
			{
				fprintf(stderr, "Warning: parseDifxInputDatastreamTable: "
					"REC BAND %d POL not found\n", i);
				continue;
			}
			D->datastream[e].recBandPolName[i] = 
				DifxParametersvalue(ip, r)[0];
			r = DifxParametersfind1(ip, r+1,
				"REC BAND %d INDEX", i);
			if(r < 0)
			{
				fprintf(stderr, "Error: parseDifxInputDatastreamTable: "
					"REC BAND %d INDEX not found\n", i);

				return 0;
			}
			a = atoi(DifxParametersvalue(ip, r));
			D->datastream[e].recBandFreqId[i] = a;
		}
		//Now do the zoom freqs/bands
		r = DifxParametersfind(ip, r+1, "NUM ZOOM FREQS");
		if(r < 0)
		{
			fprintf(stderr, "NUM ZOOM FREQS not found\n");
			
			return 0;
		}
		nZoomBand = 0;
		for(i = 0; i < D->datastream[e].nZoomFreq; i++)
		{
			r = DifxParametersfind1(ip, r+1,
				"ZOOM FREQ INDEX %d", i);
			D->datastream[e].zoomFreqId[i] =
				atoi(DifxParametersvalue(ip, r));
			r = DifxParametersfind1(ip, r+1,
				"NUM ZOOM POLS %d", i);
			D->datastream[e].nZoomPol[i] =
				atoi(DifxParametersvalue(ip, r));
			nZoomBand += D->datastream[e].nZoomPol[i];
		}
		/* count zoom chans to make sure we have enough */
		nz = 0;
		for(i = 0; ; i++)
		{
			v = DifxParametersfind1(ip, r,
				"REC BAND %d POL", i);
			if(v <= 0 || v > r+2*i+2)
			{
				break;
			}
			nz++;
		}

		if(nz > nZoomBand)
		{
			nZoomBand = nz;
		}
		for(i = 0; i < nZoomBand; i++)
		{
			r = DifxParametersfind1(ip, r+1,
				"ZOOM BAND %d POL", i);
			if(r < 0)
			{
				fprintf(stderr, "Warning: parseDifxInputDatastreamTable: "
					"ZOOM BAND %d POL not found\n", i);
				continue;
			}
			D->datastream[e].zoomBandPolName[i] =
				DifxParametersvalue(ip, r)[0];
			r = DifxParametersfind1(ip, r+1,
				"ZOOM BAND %d INDEX", i);
			if(r < 0)
			{
				fprintf(stderr, "Error: parseDifxInputDatastreamTable: "
					"ZOOM BAND %d INDEX not found\n", i);
				
				return 0;
			}
			a = atoi(DifxParametersvalue(ip, r));
			D->datastream[e].zoomBandFreqId[i] = D->datastream[e].zoomFreqId[a];
		}
		//Figure out how many phase cal tones and their location for each band
		DifxDatastreamCalculatePhasecalTones(&(D->datastream[e]), &(D->freq[D->datastream[e].recFreqId[0]]));
	}

	return D;
}

static DifxInput *parseDifxInputBaselineTable(DifxInput *D,
	const DifxParameters *ip)
{
	int b, f, p, r;
	
	if(!D || !ip)
	{
		return 0;
	}

	r = DifxParametersfind(ip, 0, "BASELINE ENTRIES");
	D->nBaseline = atoi(DifxParametersvalue(ip, r));
	D->baseline = newDifxBaselineArray(D->nBaseline);

	for(b = 0; b < D->nBaseline; b++)
	{
		r = DifxParametersfind1(ip, r+1, "D/STREAM A INDEX %d", b);
		if(r < 0)
		{
			fprintf(stderr, "D/STREAM A INDEX %d not found\n", b);

			return 0;
		}
		D->baseline[b].dsA = atoi(DifxParametersvalue(ip, r));
		r = DifxParametersfind1(ip, r+1, "D/STREAM B INDEX %d", b);
		if(r < 0)
		{
			fprintf(stderr, "D/STREAM B INDEX %d not found\n", b);
			
			return 0;
		}
		D->baseline[b].dsB = atoi(DifxParametersvalue(ip, r));
		r = DifxParametersfind1(ip, r+1, "NUM FREQS %d", b);
		if(r < 0)
		{
			fprintf(stderr, "NUM FREQS %d not found\n", b);
			
			return 0;
		}
		DifxBaselineAllocFreqs(D->baseline + b, atoi(DifxParametersvalue(ip, r)));
		
		for(f = 0; f < D->baseline[b].nFreq; f++)
		{
			r = DifxParametersfind2(ip, r+1, "POL PRODUCTS %d/%d",
				b, f);
			if(r < 0)
			{
				fprintf(stderr, "POL PRODUCTS %d/%d not found\n", b, f);
			
				return 0;
			}
			DifxBaselineAllocPolProds(D->baseline + b, f, 
				atoi(DifxParametersvalue(ip, r)));
			for(p = 0; p < D->baseline[b].nPolProd[f]; p++)
			{
				r = DifxParametersfind1(ip, r+1, 
					"D/STREAM A BAND %d", p);
				if(r < 0)
				{
					fprintf(stderr, "D/STREAM A BAND %d not found\n", p);

					return 0;
				}
				D->baseline[b].recBandA[f][p] =
					atoi(DifxParametersvalue(ip, r));
				r = DifxParametersfind1(ip, r+1, 
					"D/STREAM B BAND %d", p);
				if(r < 0)
				{
					fprintf(stderr, "D/STREAM B BAND %d not found\n", p);

					return 0;
				}
				D->baseline[b].recBandB[f][p] =
					atoi(DifxParametersvalue(ip, r));
			}
		}
	}

	return D;
}

static DifxInput *parseDifxInputDataTable(DifxInput *D, 
	const DifxParameters *ip)
{
	DifxDatastream *ds;
	int i, j, r, N;

	if(!D || !ip)
	{
		return 0;
	}

	r = 1;
	for(j = 0; j < D->nAntenna; j++)
	{
		ds = D->datastream + j;
		r = DifxParametersfind1(ip, r, "D/STREAM %d FILES", j);
		if(r < 0)
		{
			fprintf(stderr, "D/STREAM %d FILES not found\n", j);

			return 0;
		}
		N = atoi(DifxParametersvalue(ip, r));
		if(N < 0)
		{
			fprintf(stderr, "D/STREAM %d FILES has illegal value [%s]\n", j,
				DifxParametersvalue(ip, r));

			return 0;
		}
		if(N > 0)
		{
			DifxDatastreamAllocFiles(ds, N);
			for(i = 0; i < N; i++)
			{
				r = DifxParametersfind2(ip, r, "FILE %d/%d", j, i);
				if(r < 0)
				{
					fprintf(stderr, "FILE %d/%d not found\n", j, i);

					return 0;
				}
				ds->file[i] = strdup( DifxParametersvalue(ip, r) );
			}
		}
 	}

	return D;
}

static DifxInput *parseDifxInputNetworkTable(DifxInput *D,
        const DifxParameters *ip)
{
        DifxDatastream *ds;
        int i, r;

	if(!D || !ip)
	{
		return 0;
	}

	r = 1;
        for(i = 0; i < D->nDatastream; i++)
        {
                ds = D->datastream + i;

                r = DifxParametersfind1(ip, r, "PORT NUM %d", i);
                if(r > 0)
                {
                        ds->networkPort = atoi(DifxParametersvalue(ip, r));
                }

                r = DifxParametersfind1(ip, r, "TCP WINDOW (KB) %d", i);
                if(r > 0)
                {
                        ds->windowSize = atoi(DifxParametersvalue(ip, r));
                }
        }

        return D;
}

static DifxInput *deriveDifxInputValues(DifxInput *D)
{
	int dsId, fqId, c, e, qb, v;
	DifxDatastream *ds;
	
	if(!D)
	{
		return 0;
	}

	if(D->nConfig < 1)
	{
		fprintf(stderr, "deriveDifxInputValues : nConfig < 1\n");

		return 0;
	}

	/* Set reference frequency to the lowest of the freqs */
	D->refFreq = 0.0;
	for(fqId = 0; fqId < D->nFreq; fqId++)
	{
		if(D->freq[fqId].freq < D->refFreq || D->refFreq <= 0.0)
		{
			D->refFreq = D->freq[fqId].freq;
		}
	}

	for(c = 0; c < D->nConfig; c++)
	{
		/* determine number of bits, or zero if different among
		 * antennas */
		D->config[c].quantBits = -1;
		qb = 0;

		for(dsId = 0; dsId < D->config[c].nDatastream; dsId++)
		{
			e = D->config[c].datastreamId[dsId];
			if(e < 0)
			{
				continue;
			}
			ds = D->datastream + e;
			if(qb == 0)
			{
				qb = ds->quantBits;
			}
			else if(qb != ds->quantBits)
			{
				qb = -1;
				break;
			}
		}
		if(qb < 0)
		{
			qb = 0;
		}
		D->config[c].quantBits = qb;
	}

	for(c = 0; c < D->nConfig; c++)
	{
		v = generateAipsIFs(D, c);
		if(v < 0)
		{
			fprintf(stderr, "Fatal error processing configId %d\n", c);

			return 0;
		}
	}

	return D;
}

static DifxInput *populateInput(DifxInput *D, const DifxParameters *ip)
{
	if(!D || !ip)
	{
		fprintf(stderr, "populateInput: D = 0 or ip = 0\n");

		return 0;
	}

	/* COMMON SETTINGS */
	D = parseDifxInputCommonTable(D, ip);
	
	/* CONFIGURATIONS */
	D = parseDifxInputConfigurationTable(D, ip);

	/* RULES */
	D = parseDifxInputRuleTable(D, ip);
	
	/* FREQ TABLE */
	D = parseDifxInputFreqTable(D, ip);

	/* TELESCOPE TABLE */
	D = parseDifxInputTelescopeTable(D, ip);
	
	/* DATASTREAM TABLE */
	D = parseDifxInputDatastreamTable(D, ip);

	/* BASELINE TABLE */
	D = parseDifxInputBaselineTable(D, ip);

	/* DATA TABLE */
	D = parseDifxInputDataTable(D, ip);

	/* NETWORK TABLE */
	D = parseDifxInputNetworkTable(D, ip);

	/* THREADS per CORE */
	DifxInputLoadThreads(D);

	return D;
}

static DifxInput *populateCalc(DifxInput *D, DifxParameters *cp)
{
	const char initKeys[][MAX_DIFX_KEY_LEN] = 
	{
		"JOB ID",
		"OBSCODE",
		"NUM TELESCOPES",
		"NUM SOURCES",
		"NUM SCANS",
		"NUM EOPS"
	};
	const int N_INIT_ROWS = sizeof(initKeys)/sizeof(initKeys[0]);
	
	const char antKeys[][MAX_DIFX_KEY_LEN] =
	{
		"TELESCOPE %d NAME",
		"TELESCOPE %d MOUNT",
		"TELESCOPE %d OFFSET (m)",
		"TELESCOPE %d X (m)",
		"TELESCOPE %d Y (m)",
		"TELESCOPE %d Z (m)"
	};
	const int N_ANT_ROWS = sizeof(antKeys)/sizeof(antKeys[0]);

        const char srcKeys[][MAX_DIFX_KEY_LEN] =
	{
		"SOURCE %d NAME",
		"SOURCE %d RA",
		"SOURCE %d DEC",
                "SOURCE %d CALCODE",
                "SOURCE %d QUAL",
	};
	const int N_SRC_ROWS = sizeof(srcKeys)/sizeof(srcKeys[0]);

	const char eopKeys[][MAX_DIFX_KEY_LEN] =
	{
		"EOP %d TIME (mjd)",
		"EOP %d TAI_UTC (sec)",
		"EOP %d UT1_UTC (sec)",
		"EOP %d XPOLE (arcsec)",
		"EOP %d YPOLE (arcsec)"
	};
	const int N_EOP_ROWS = sizeof(eopKeys)/sizeof(eopKeys[0]);
	
	const char spacecraftKeys[][MAX_DIFX_KEY_LEN] =
	{
		"SPACECRAFT %d NAME",
		"SPACECRAFT %d ROWS"
	};
	const int N_SPACECRAFT_ROWS = 
		sizeof(spacecraftKeys)/sizeof(spacecraftKeys[0]);
	
	int rows[20];
	int a, i, j, k, c, s, N, row, n, r, applies, src, v;
	const char *str;
	double time;
	//int nFound, nTel, nSrc, startsec, dursec;
	int nFound, nTel, startsec, dursec;

	if(!D)
	{
		return 0;
	}

	N = DifxParametersbatchfind(cp, 0, initKeys, N_INIT_ROWS, rows);
	if(N < N_INIT_ROWS)
	{
		return 0;
	}

	D->job->jobId    = atoi(DifxParametersvalue(cp, rows[0]));
	snprintf(D->job->obsCode, DIFXIO_OBSCODE_LENGTH, "%s",
		DifxParametersvalue(cp, rows[1]));
	nTel             = atoi(DifxParametersvalue(cp, rows[2]));
	D->nSource       = atoi(DifxParametersvalue(cp, rows[3]));
	D->nScan         = atoi(DifxParametersvalue(cp, rows[4]));
	D->nEOP          = atoi(DifxParametersvalue(cp, rows[5]));

	if(D->nAntenna == 0)
	{
		fprintf(stderr, "Error: populateCalc: D->nAntenna == 0\n");

		return 0;
	}
	D->source = newDifxSourceArray(D->nSource);
	D->scan = newDifxScanArray(D->nScan);

	//if(D-> nSource == 0)
	//{
	//	fprintf(stderr, "Error: populateCalc: D->nSource == 0\n");
	//	return 0;
	//}

	//if(D->nScan == 0)
	//{
	//	D->nScan = atoi(DifxParametersvalue(cp, rows[2]));
	//	D->scan = newDifxScanArray(D->nScan);
	//}

	//if(D->nScan != atoi(DifxParametersvalue(cp, rows[2])))
	//{
	//	fprintf(stderr, ".calc NUM SCANS = %d; .im NUM SCANS = %d\n",
	//		atoi(DifxParametersvalue(cp, rows[2])), D->nScan);
	//	return 0;
	//}

	if(D->nEOP > 0)
	{
		D->eop = newDifxEOPArray(D->nEOP);
	}

	row = DifxParametersfind(cp, 0, "DIFX VERSION");
	if(row > 0)
	{
		snprintf(D->job->difxVersion, DIFXIO_VERSION_LENGTH, "%s",
			DifxParametersvalue(cp, row));
	}

	row = DifxParametersfind(cp, 0, "SESSION");
	if(row >= 0)
	{
		snprintf(D->job->obsSession, DIFXIO_SESSION_LENGTH, "%s",
			DifxParametersvalue(cp, row));
	}
	row = DifxParametersfind(cp, 0, "TAPER FUNCTION");
	if(row >= 0)
	{
		snprintf(D->job->taperFunction, DIFXIO_TAPER_LENGTH, "%s",
			DifxParametersvalue(cp, row));
	}
	row = DifxParametersfind(cp, 0, "VEX FILE");
	if(row >= 0)
	{
		snprintf(D->job->vexFile, DIFXIO_FILENAME_LENGTH, "%s",
			DifxParametersvalue(cp, row));
	}
	row = DifxParametersfind(cp, 0, "DUTY CYCLE");
	if(row >= 0)
	{
		D->job->dutyCycle = atof(DifxParametersvalue(cp, row));
	}
	row = DifxParametersfind(cp, 0, "JOB START TIME");
	if(row >= 0)
	{
		D->job->jobStart = atof(DifxParametersvalue(cp, row));
	}
	row = DifxParametersfind(cp, 0, "JOB STOP TIME");
	if(row >= 0)
	{
		D->job->jobStop = atof(DifxParametersvalue(cp, row));
	}
	row = DifxParametersfind(cp, 0, "SUBJOB ID");
	if(row >= 0)
	{
		D->job->subjobId = atoi(DifxParametersvalue(cp, row));
	}
	row = DifxParametersfind(cp, 0, "SUBARRAY ID");
	if(row >= 0)
	{
		D->job->subarrayId = atoi(DifxParametersvalue(cp, row));
	}
	row = DifxParametersfind(cp, 0, "SPECTRAL AVG");
	if(row >= 0)
	{
		D->specAvg = atoi(DifxParametersvalue(cp, row));
	}
	else
	{
		D->specAvg = 1;
	}
	//row = DifxParametersfind(cp, 0, "OUTPUT CHANNELS");
	//if(row >= 0)
	//{
	//	nch = atof(DifxParametersvalue(cp, row));
	//	if(nch >= 1)
	//	{
	//		D->nOutChan = nch;
	//	}
	//	else
	//	{
	//		D->nOutChan = D->config[0].nChan*nch/D->specAvg;
	//	}
	//}
	//row = DifxParametersfind(cp, 0, "START CHANNEL");
	//if(row >= 0)
	//{
	//	nch = atof(DifxParametersvalue(cp, row));
	//	if(nch >= 1)
	//	{
	//		D->startChan = nch;
	//	}
	//	else
	//	{
	//		D->startChan = D->config[0].nChan*nch;
	//	}
	//}

	//row = DifxParametersfind(cp, 0, "NUM TELESCOPES");
	//if(row >= 0)
	//{
	//	nTel = atoi(DifxParametersvalue(cp, row));
	//}
	//else
	//{
	//	fprintf(stderr, "populateCalc: NUM TELESCOPES not defined\n");
	//	return 0;
	//}
	
	if(nTel < D->nAntenna)
	{
		fprintf(stderr, "populateCalc: NUM TELESCOPES too small: \n"
			"%d < %d\n", nTel, D->nAntenna);
	}

	rows[N_ANT_ROWS-1] = 0;		/* initialize start */
	nFound = 0;
	for(i = 0; i < nTel; i++)
	{
		N = DifxParametersbatchfind1(cp, rows[N_ANT_ROWS-1], antKeys,
			i, N_ANT_ROWS, rows);
		if(N < N_ANT_ROWS)
		{
			if(i == 0)
			{
				fprintf(stderr, "Warning: no antenna axis offsets available\n");
				break;
			}
			else
			{
				return 0;
			}
		}
		a = DifxInputGetAntennaId(D, DifxParametersvalue(cp, rows[0]));
		if(a < 0)
		{
			fprintf(stderr, "populateCalc: skipping telescope \n"
				"table entry %d\n", i);
			continue;
		}
		nFound++;
		D->antenna[a].mount = stringToMountType(DifxParametersvalue(cp, rows[1]));
		if(D->antenna[a].mount >= AntennaMountOther)
		{
			fprintf(stderr, "Warning: populateCalc: unknown antenna mount type encountered\n"
				"for telescope table entry %d: %s.  Changing to AZEL\n",
				i, DifxParametersvalue(cp, rows[1]));
		}
		D->antenna[a].offset[0]= atof(DifxParametersvalue(cp, rows[2]));
		D->antenna[a].offset[1]= 0.0;	/* FIXME */
		D->antenna[a].offset[2]= 0.0;	/* FIXME */
		D->antenna[a].X        = atof(DifxParametersvalue(cp, rows[3]));
		D->antenna[a].Y        = atof(DifxParametersvalue(cp, rows[4]));
		D->antenna[a].Z        = atof(DifxParametersvalue(cp, rows[5]));
		row = DifxParametersfind1(cp, 0, "TELESCOPE %d SHELF", a);
		if(row > 0)
		{
			snprintf(D->antenna[a].shelf, DIFXIO_SHELF_LENGTH, "%s", 
				DifxParametersvalue(cp, row));
		}
	}
	
	if(nFound < D->nAntenna)
	{
		fprintf(stderr, "populateCalc: too few antenna matches\n");
		return 0;
	}

        //row = DifxParametersfind(cp, 0, "NUM SOURCES");
        //if(row >= 0)
        //{
        //        nSrc = atoi(DifxParametersvalue(cp, row));
        //}
        //else
        //{
        //        fprintf(stderr, "populateCalc: NUM SOURCES not defined\n");
        //        return 0;
        //}

        //if(nSrc < D->nSource)
        //{
        //        fprintf(stderr, "populateCalc: NUM SOURCES too small: \n"
        //                "%d < %d\n", nSrc, D->nSource);
        //}

        rows[N_SRC_ROWS-1] = 0;         /* initialize start */
        //for(i = 0; i < nSrc; i++)
	for(i = 0; i < D->nSource; i++)
        {
                N = DifxParametersbatchfind1(cp, rows[N_SRC_ROWS-1], srcKeys,
                        i, N_SRC_ROWS, rows);
                if(N < N_SRC_ROWS)
                {
                        return 0;
                }
		snprintf(D->source[i].name, DIFXIO_NAME_LENGTH, "%s", DifxParametersvalue(cp, rows[0]));
		D->source[i].ra = atof(DifxParametersvalue(cp, rows[1]));
		D->source[i].dec = atof(DifxParametersvalue(cp, rows[2]));
		snprintf(D->source[i].calCode, DIFXIO_CALCODE_LENGTH, "%s", DifxParametersvalue(cp, rows[3]));
		D->source[i].qual = atoi(DifxParametersvalue(cp, rows[4]));
		//The fitsSourceId is left unset for now - the only way to set this is by calling updateDifxInput
		//D->source[i].fitsSourceId = i;
		row = DifxParametersfind1(cp, 0, "SOURCE %d PM RA (ARCSEC/YR)", i);
		if(row > 0) {
			D->source[i].pmRA = atoi(DifxParametersvalue(cp, row));
			row = DifxParametersfind1(cp, row, "SOURCE %d PM DEC (ARCSEC/YR)", i);
			D->source[i].pmDec = atoi(DifxParametersvalue(cp, row));
			row = DifxParametersfind1(cp, row, "SOURCE %d PARALLAX (ARCSEC)", i);
                        D->source[i].pmDec = atoi(DifxParametersvalue(cp, row));
			row = DifxParametersfind1(cp, row, "SOURCE %d PM EPOCH (MJD)", i);
                        D->source[i].pmEpoch = atoi(DifxParametersvalue(cp, row));
		}
        }


	rows[N_EOP_ROWS-1] = 0;		/* initialize start */
	if(D->eop) for(i = 0; i < D->nEOP; i++)
	{
		N = DifxParametersbatchfind1(cp, rows[N_EOP_ROWS-1], eopKeys,
			i, N_EOP_ROWS, rows);
		if(N < N_EOP_ROWS)
		{
			return 0;
		}
		D->eop[i].mjd     = atof(DifxParametersvalue(cp, rows[0])) + .5;
		D->eop[i].tai_utc = atof(DifxParametersvalue(cp, rows[1])) + .5;
		D->eop[i].ut1_utc = atof(DifxParametersvalue(cp, rows[2]));
		D->eop[i].xPole   = atof(DifxParametersvalue(cp, rows[3]));
		D->eop[i].yPole   = atof(DifxParametersvalue(cp, rows[4]));
	}

	k = 0;
	for(i = 0; i < D->nScan; i++)
	{
		row = DifxParametersfind1(cp, 0, "SCAN %d START (S)", i);
                if(row < 0) {
                    fprintf(stderr, "SCAN %d START (S) not found\n", i);
                    return 0;
                }
		startsec = atoi(DifxParametersvalue(cp, row));
		row = DifxParametersfind1(cp, row, "SCAN %d DUR (S)", i);
                if(row < 0) {
                    fprintf(stderr, "SCAN %d DUR (S) not found\n", i);
                    return 0;
                }
		dursec = atoi(DifxParametersvalue(cp, row));
		D->scan[i].nAntenna = nTel;
		D->scan[i].startSeconds = startsec;
		D->scan[i].durSeconds   = dursec;
		D->scan[i].mjdStart = D->job->mjdStart + startsec/86400.0;
		D->scan[i].mjdEnd   = D->job->mjdStart + (startsec+dursec)/86400.0;
		row = DifxParametersfind1(cp, row, "SCAN %d OBS MODE NAME", i);
		if(row < 0) {
			fprintf(stderr, "SCAN %d OBS MODE NAME not found\n", i);
			return 0;
		}
                snprintf(D->scan[i].obsModeName, DIFXIO_NAME_LENGTH, "%s", DifxParametersvalue(cp, row));
		row = DifxParametersfind1(cp, row, "SCAN %d UVSHIFT INTERVAL (NS)", i);
		if(row < 0) {
		    fprintf(stderr, "SCAN %d UVSHIFT INTERVAL (NS) not found\n", i);
		    return 0;
		}	
		D->scan[i].maxNSBetweenUVShifts = atoi(DifxParametersvalue(cp, row));
		row = DifxParametersfind1(cp, row, "SCAN %d AC AVG INTERVAL (NS)", i);
		if(row < 0) {
			fprintf(stderr, "SCAN %d AC AVG INTERVAL (NS) not found\n", i);
			return 0;
		}
		D->scan[i].maxNSBetweenACAvg = atoi(DifxParametersvalue(cp, row));
		row = DifxParametersfind1(cp, row, "SCAN %d POINTING SRC", i);
		if(row < 0) {
                    fprintf(stderr, "SCAN %d POINTING SRC not found\n", i);
                    return 0;
                }
		D->scan[i].pointingCentreSrc = atoi(DifxParametersvalue(cp, row));
		row = DifxParametersfind1(cp, row, "SCAN %d NUM PHS CTRS", i);
                if(row < 0) {
                    fprintf(stderr, "SCAN %d NUM PHS CTRS not found\n", i);
                    return 0;
                }
		D->scan[i].nPhaseCentres = atoi(DifxParametersvalue(cp, row));
		if(D->scan[i].nPhaseCentres > MAX_PHS_CENTRES) {
		    fprintf(stderr, "SCAN %d NUM PHS CTRS (%d) exceeds max (%d)\n", 
				     i, D->scan[i].nPhaseCentres, MAX_PHS_CENTRES);
		    return 0;
		}
		for(j=0;j<D->scan[i].nPhaseCentres;j++) {
		    row = DifxParametersfind2(cp, row, "SCAN %d PHS CTR %d", i, j);
		    if(row < 0) {
			fprintf(stderr, "SCAN %d PHS CTR %d not found\n", i, j);
                    	return 0;
		    }
		    D->scan[i].phsCentreSrcs[j] = atoi(DifxParametersvalue(cp, row));
		    D->scan[i].orgjobPhsCentreSrcs[j] = D->scan[i].phsCentreSrcs[j];
                }
		D->scan[i].configId = -1;
		for(r=0;r<D->nRule;r++)
		{
			applies = 0;
			for(src=0;src<D->scan[i].nPhaseCentres;src++)
			{
				//printf("Checking if rule %d applies to scan %d, phase centre %d\n", r, i, src);
				//printf("The phase centre src index is %d\n", D->scan[i].phsCentreSrcs[src]);
				//printf("And its name is %s\n", D->source[D->scan[i].phsCentreSrcs[src]].name);
				//printf("Rule sourceName is %s\n", D->rule[r].sourceName);
				if(ruleAppliesToScanSource(&(D->rule[r]), &(D->scan[i]), &(D->source[D->scan[i].phsCentreSrcs[src]])) != 0)
				{
					applies = 1;
				}
			}
			if(ruleAppliesToScanSource(&(D->rule[r]), &(D->scan[i]), &(D->source[D->scan[i].pointingCentreSrc])) != 0)
			{
				applies = 1;
			}
			if(applies > 0)
			{
				//printf("Yes, it does apply!\n");
				for(c=0;c<D->nConfig;c++)
				{
					if(strcmp(D->rule[r].configName, D->config[c].name) == 0)
					{
						if(D->scan[i].configId < 0 || D->scan[i].configId == c)
						{
							D->scan[i].configId = c;
						}
						else
						{
							fprintf(stderr, "Warning: Rules produce conflicting configs for scan %d!\n", i);
						}
						applies = 0;
					}
				}
				if(applies > 0)
				{
					fprintf(stderr, "Couldn't find the config (%s) that is supposed to match rule %d\n", 
						D->rule[r].configName, r);
				}
			}
		}
		//cname = DifxParametersvalue(cp, row);
		//for(c = 0; c < D->nConfig; c++)
		//{
		//	if(strcmp(cname, D->config[c].name) == 0)
		//	{
		//		D->scan[k].configId = c;
		//		break;
		//	}
		//}
		//if(c == D->nConfig)
		//{
		//	D->scan[k].configId = -1;
		//	if(strcmp(cname, "SCAN_GAP") != 0)
		//	{
		//		fprintf(stderr, "Warning: ignoring source without "
		//			"config! id=%d  name=%s  pointing center "
		//			"name=%s\n", i, cname, 
		//			D->source[D->scan[k].pointingCentreSrc].name);
		//	}
		//}
		k++;
	}
	D->nScan = k;
	//fprintf(stdout, "Got %d scans\n", k);

	row = DifxParametersfind(cp, 0, "NUM SPACECRAFT");
	if(row >= 0)
	{
		D->nSpacecraft = atoi(DifxParametersvalue(cp, row));
		D->spacecraft  = newDifxSpacecraftArray(D->nSpacecraft);
	}

	rows[N_SPACECRAFT_ROWS-1] = 0;
	if(D->spacecraft) for(s = 0; s < D->nSpacecraft; s++)
	{
		N = DifxParametersbatchfind1(cp, rows[N_SPACECRAFT_ROWS-1], 
			spacecraftKeys, s, N_SPACECRAFT_ROWS, rows);
		if(N < N_SPACECRAFT_ROWS)
		{
			fprintf(stderr, "Spacecraft %d table screwed up\n", s);
			return 0;
		}
		snprintf(D->spacecraft[s].name, DIFXIO_NAME_LENGTH, "%s", DifxParametersvalue(cp, rows[0]));
		D->spacecraft[s].nPoint = 
			atoi(DifxParametersvalue(cp, rows[1]));
		D->spacecraft[s].pos = (sixVector *)calloc(
			D->spacecraft[s].nPoint, sizeof(sixVector));
		row = rows[N_SPACECRAFT_ROWS-1];
		for(i = 0; i < D->spacecraft[s].nPoint; i++)
		{
			row = DifxParametersfind2(cp, row+1,
				"SPACECRAFT %d ROW %d", s, i);
			if(row < 0)
			{
				fprintf(stderr, "Spacecraft %d table, row %d"
					" screwed up\n", s, i);
				return 0;
			}
			str = DifxParametersvalue(cp, row);
			n = sscanf(str, "%lf%Lf%Lf%Lf%Lf%Lf%Lf",
				&time,
				&(D->spacecraft[s].pos[i].X),
				&(D->spacecraft[s].pos[i].Y),
				&(D->spacecraft[s].pos[i].Z),
				&(D->spacecraft[s].pos[i].dX),
				&(D->spacecraft[s].pos[i].dY),
				&(D->spacecraft[s].pos[i].dZ));
			if(n != 7)
			{
				fprintf(stderr, "Spacecraft %d table, row %d"
					" screwed up\n", s, i);
				return 0;
			}
			D->spacecraft[s].pos[i].mjd = (int)time;
			time -= D->spacecraft[s].pos[i].mjd;
			/* Force to be exactly on second boundary */
			D->spacecraft[s].pos[i].fracDay = 
				((int)(time*86400.0 + 0.5))/86400.0;
		}
	}

	row = DifxParametersfind(cp, 0, "IM FILENAME");
	if(row < 0)
	{
		fprintf(stderr, "File %s has no IM FILENAME specified!\n", D->calcFile);
		return 0;
	}
	v = snprintf(D->imFile, DIFXIO_FILENAME_LENGTH, "%s", DifxParametersvalue(cp, row) );
	if(v >= DIFXIO_FILENAME_LENGTH)
	{
		fprintf(stderr, "File %s IM FILENAME is too long\n", D->calcFile);
		return 0;
	}

	return D;
}

static DifxInput *parseCalcServerInfo(DifxInput *D, DifxParameters *p)
{
	int r;

	if(!D)
	{
		return 0;
	}

	r = DifxParametersfind(p, 0, "CALC SERVER");
	if(r >= 0)
	{
		snprintf(D->job->calcServer, DIFXIO_HOSTNAME_LENGTH, "%s",
			DifxParametersvalue(p, r));
	}

	r = DifxParametersfind(p, 0, "CALC PROGRAM");
	if(r >= 0)
	{
		D->job->calcProgram = atoi(DifxParametersvalue(p, r));
	}

	r = DifxParametersfind(p, 0, "CALC VERSION");
	if(r >= 0)
	{
		D->job->calcVersion = atoi(DifxParametersvalue(p, r));
	}

	return D;
}

int parsePoly1(DifxParameters *p, int r, char *key, int i1, int i2,
	double *array, int n)
{
	const char *v;
	int i, l, m;
	double d;

	if(r < 0)
	{
		return -1;
	}

	r = DifxParametersfind2(p, r, key, i1, i2);
	if(r < 0)
	{
		return -1;
	}

	v = DifxParametersvalue(p, r);
	m = 0;
	for(i = 0; i < n; i++)
	{
		if(sscanf(v+m, "%lf%n", &d, &l) < 1)
		{
			return -1;
		}
		m += l;
		array[i] = d;
	}

	return r;
}

static DifxInput *populateIM(DifxInput *D, DifxParameters *mp)
{
	int a, t, p, r, s, src, nScan, nTel;
	DifxScan *scan;
	int mjd, sec;
	int order, interval;
	enum AberCorr ac;
	int *antMap;

	if(!D)
	{
		return 0;
	}

	if(!mp)
	{
		return D;
	}

	antMap = deriveAntMap(D, mp, &nTel);
	if(antMap == 0)
	{
		fprintf(stderr, "populateIM: deriveAntMap failed\n");
		return 0;
	}

	D = parseCalcServerInfo(D, mp);

	r = DifxParametersfind(mp, 0, "POLYNOMIAL ORDER");
	if(r < 0)
	{
		fprintf(stderr, "IM: POLYNOMIAL ORDER not found\n");
		return 0;
	}
	order = atoi(DifxParametersvalue(mp, r));
	D->job->polyOrder = order;

	r = DifxParametersfind(mp, 0, "INTERVAL (SECS)");
	if(r < 0)
	{
		fprintf(stderr, "IM: INTERVAL (SECS) not found\n");
		free(antMap);
		return 0;
	}
	interval = atoi(DifxParametersvalue(mp, r));
	D->job->polyInterval = interval;
	
	r = DifxParametersfind(mp, 0, "ABERRATION CORR");
	if(r < 0)
	{
		fprintf(stderr, "Assuming aberration not corrected\n");
	}
	for(ac = 0; ac < NumAberCorrOptions; ac++)
	{
		if(strcmp(aberCorrStrings[ac], DifxParametersvalue(mp, r)) == 0)
		{
			D->job->aberCorr = ac;
		}
	}

	/* Now get the models! */
	r = DifxParametersfind(mp, 0, "NUM SCANS");
	if(r < 0)
	{
		fprintf(stderr, "IM: NUM SCANS not found\n");
		free(antMap);
		return 0;
	}

	nScan = atoi(DifxParametersvalue(mp, r));
	if(D->nScan != nScan)
	{
		fprintf(stderr, "IM: NUM SCANS disagrees\n");
		free(antMap);
		return 0;
	}

	for(s = 0; s < nScan; s++)
	{
		//printf("Looping through scan %d/%d\n", s+1, nScan);
		/* FIXME -- validate source name, ... */

		scan = D->scan + s;

		r = DifxParametersfind1(mp, r, "SCAN %d NUM POLY", s);
		if(r < 0)
		{
			fprintf(stderr, "IM: Malformed scan\n");
			free(antMap);
			return 0;
		}

		scan->nPoly = atoi(DifxParametersvalue(mp, r));
		scan->im = newDifxPolyModelArray(scan->nAntenna, scan->nPhaseCentres + 1, 
						 scan->nPoly);

		//printf("Created PolyModelArray, with %d antennas\n", scan->nAntenna);
		for(p = 0; p < scan->nPoly; p++)
		{
			//printf("Working on poly %d/%d\n", p+1, scan->nPoly);
			r = DifxParametersfind2(mp, r, "SCAN %d POLY %d MJD",
				s, p);
			if(r < 0)
			{
				printf("Could not find SCAN %d POLY %d MJD", s, p);
				free(antMap);
				return 0;
			}
			mjd = atoi(DifxParametersvalue(mp, r));
			r = DifxParametersfind2(mp, r, "SCAN %d POLY %d SEC",
				s, p);
			if(r < 0)
			{
				printf("Could not find SCAN %d POLY %d SEC", s, p);
				free(antMap);
				return 0;
			}
			sec = atoi(DifxParametersvalue(mp, r));

			for(src = 0; src < scan->nPhaseCentres+1; src++)
			{
				for(t = 0; t < nTel; t++)
				{
					a = antMap[t];
                                	if(a < 0)
                                	{
                                        	continue;
                                	}
					scan->im[a][src][p].mjd = mjd;
					scan->im[a][src][p].sec = sec;
					scan->im[a][src][p].order = order;
					scan->im[a][src][p].validDuration = interval;
					r = parsePoly1(mp, r, "SRC %d ANT %d DELAY (us)", 
						src, t, scan->im[a][src][p].delay, order+1);
					r = parsePoly1(mp, r, "SRC %d ANT %d DRY (us)", src,
						t, scan->im[a][src][p].dry, order+1);
					r = parsePoly1(mp, r, "SRC %d ANT %d WET (us)", src,
						t, scan->im[a][src][p].wet, order+1);
					r = parsePoly1(mp, r, "SRC %d ANT %d U (m)", src,
						t, scan->im[a][src][p].u, order+1);
					r = parsePoly1(mp, r, "SRC %d ANT %d V (m)", src,
						t, scan->im[a][src][p].v, order+1);
					r = parsePoly1(mp, r, "SRC %d ANT %d W (m)", src,
						t, scan->im[a][src][p].w, order+1);
					if(r < 0)
					{
						printf("Could not find SRC %d ANT %d W (m)\n", src, t);
						return 0;
					}
				}
			}
		}
	}

	free(antMap);

	return D;
}

static int populateFlags(DifxInput *D, const char *flagfile)
{
	FILE *in;
	double mjd1, mjd2;
	int i, j=0, n=0, a, p;
	char line[1000];
	char *ptr;
	int nUndecoded = 0;
	DifxJob *J;

	J = D->job;
	if(!D)
	{
		return 0;
	}

	in = fopen(flagfile, "r");
	if(!in)
	{
		return 0;
	}

	ptr = fgets(line, 999, in);
	if(ptr == 0)
	{
		fprintf(stderr, "Warning: premature end of file %s\n",
			flagfile);
		fclose(in);
		return 0;
	}
	p = sscanf(line, "%d", &n);
	if(p == 1 && n > 0 && n < 10000)
	{
		J->nFlag = n;
		J->flag = newDifxAntennaFlagArray(J->nFlag);
		for(i = 0; i < n; i++)
		{
			ptr = fgets(line, 999, in);
                        if(ptr == 0)
                        {
                                fprintf(stderr, "Warning: premature end of file %s\n",
                                        flagfile);
                                J->nFlag = i;
                                break;
                        }
                        line[999] = 0;

                        /* Allow read of plain numbers */
                        p = sscanf(line, "%lf%lf%d", &mjd1, &mjd2, &a);
                        if(p != 3)
                        {
                                /* or formatted in one particular way */
                                p = sscanf(line, "  mjd(%lf,%lf)%d", &mjd1, &mjd2, &a);
                        }
                        if(p == 3)
                        {
                                if(a < 0 || a >= D->nAntenna)
                                {
                                        fprintf(stderr, "populateFlags : file=%s line=%d: a=%d\n",
                                                flagfile, i+2, a);
                                        nUndecoded++;
                                }
                                else
                                {
                                        J->flag[j].mjd1  = mjd1;
                                        J->flag[j].mjd2  = mjd2;
                                        J->flag[j].antennaId = a;
                                        j++;
                                }
                        }
                        else
                        {
                                nUndecoded++;
			}
		}
	}
	else if(n > 0)
	{
		fprintf(stderr, "populateFlags : unreasonable "
			"number of flags : %d\n", n);
	}

	if(nUndecoded > 0)
	{
		fprintf(stderr, "Warning: %d flags from file %s were not properly parsed\n",
			nUndecoded, flagfile);
		J->nFlag = j;
	}

	fclose(in);

	return n;
}

int isAntennaFlagged(const DifxJob *J, double mjd, int antennaId)
{
        int f;

        for(f = 0; f < J->nFlag; f++)
        {
                if(J->flag[f].antennaId == antennaId)
                {
                        if(mjd > J->flag[f].mjd1 && mjd < J->flag[f].mjd2)
                        {
                                return 1;
                        }
                }
        }

        return 0;
}

DifxInput *allocateSourceTable(DifxInput *D, int length)
{
	if(!D)
	{
		return 0;
	}
	D->source = newDifxSourceArray(length);
	D->nSource = 0;
	return D;
}

/* take DifxInput structure and derive the source table.
 */
//DifxInput *deriveSourceTable(DifxInput *D)
//{
//	int i, n=0, s, sc;
//
//	if(!D)
//	{
//		return 0;
//	}
//
//	if(D->nScan < 1 || D->scan == 0)
//	{
//		fprintf(stderr, "No scans to work with!\n");
//		return 0;
//	}
//
//	/* for now be wasteful and allocate enough memory for each
//	 * scan to be its own source 
//	 */
//	D->source = newDifxSourceArray(D->nScan);
//	D->nSource = 0;
//
//	for(s = 0; s < D->nScan; s++)
//	{
//		for(i = 0; i < n; i++)
//		{
//			if(D->source[i].ra       == D->scan[s].ra  &&
//			   D->source[i].dec      == D->scan[s].dec &&
//			   D->source[i].qual     == D->scan[s].qual &&
//			   D->source[i].configId == D->scan[s].configId &&
//			   strcmp(D->source[i].calCode, D->scan[s].calCode) 
//			   	== 0 &&
//			   strcmp(D->source[i].name, D->scan[s].name) == 0)
//			{
//				break;
//			}
//		}
//		
//		if(i >= n)
//		{
//			strcpy(D->source[n].name, D->scan[s].name);
//			strcpy(D->source[i].calCode, D->scan[s].calCode);
//			D->source[n].ra       = D->scan[s].ra;
//			D->source[n].dec      = D->scan[s].dec;
//			D->source[n].qual     = D->scan[s].qual;
//			D->source[i].configId = D->scan[s].configId;
//			n++;
//		}
//
//		D->scan[s].sourceId = i;
//	}
//
//	D->nSource = n;
//
//	/* Look for spacecraft */
//	if(D->nSpacecraft > 0 && D->nSource > 0)
//	{
//		for(s = 0; s < D->nSource; s++)
//		{
//			for(sc = 0; sc < D->nSpacecraft; sc++)
//			{
//				if(strcmp(D->spacecraft[sc].name,
//				          D->source[s].name) == 0)
//				{
//					D->source[s].spacecraftId = sc;
//					break;
//				}
//			}
//		}
//	}
//
//	return D;
//}

#if 0
/* The following function is only here to override the following function
 * in testing */
static DifxInput *setFitsSourceIds(DifxInput *D)
{
	int i;

	if(!D)
	{
		return 0;
	}

	if(D->nSource < 1 || D->source == 0)
	{
		fprintf(stderr, "No sources to work with!\n");
		return 0;
	}

	for(i = 0; i < D->nSource; i++)
	{
		D->source[i].fitsSourceId = i;
	}

	return D;
}
#endif

static DifxInput *deriveFitsSourceIds(DifxInput *D)
{
	int a, i, j, match, n=0, k, l;
	int *fs;
	int *fc;

	if(!D)
	{
		return 0;
	}

	if(D->nSource < 1 || D->source == 0)
	{
		fprintf(stderr, "No sources to work with!\n");
		return 0;
	}

	fs = (int *)calloc(D->nSource*D->nConfig, sizeof(int));
	fc = (int *)calloc(D->nSource*D->nConfig, sizeof(int));

	for(i = 0; i < D->nSource; i++)
	{
		D->source[i].numFitsSourceIds = D->nConfig;
		D->source[i].fitsSourceIds = (int*)malloc(D->nConfig * sizeof(int));
		for(k = 0; k < D->nConfig; k++)
		{
			match = -1;
			if(n > 0) for(a = 0; a < n; a++)
			{
				j = fs[a];
				l = fc[a];
				if(D->source[i].ra         == D->source[j].ra  &&
				   D->source[i].dec        == D->source[j].dec &&
				   D->source[i].qual       == D->source[j].qual &&
				   D->config[k].fitsFreqId == D->config[l].fitsFreqId &&
				   strcmp(D->source[i].calCode, D->source[j].calCode) 
				   	== 0 &&
				   strcmp(D->source[i].name, D->source[j].name) == 0)
				{
					match = a;
					break;
				}
			}
			if(match < 0)
			{
				D->source[i].fitsSourceIds[k] = n;
				fs[n] = i;
				fc[n] = k;
				n++;
			}
			else
			{
				D->source[i].fitsSourceIds[k] = match;
			}
		}
	}

	free(fs);
	free(fc);
	
	return D;
}
	
static void setOrbitingAntennas(DifxInput *D)
{
	int a, sc;

	if(!D)
	{
		return;
	}
	
	if(D->nSpacecraft > 0 && D->nAntenna > 0)
	{
		for(a = 0; a < D->nAntenna; a++)
		{
			for(sc = 0; sc < D->nSpacecraft; sc++)
			{
				if(strcmp(D->spacecraft[sc].name,
				          D->antenna[a].name) == 0)
				{
					D->antenna[a].spacecraftId = sc;
					break;
				}
			}
		}
	}

	return;
}


static void setGlobalValues(DifxInput *D)
{
	DifxConfig *dc;
	int i, j, c, p, n, nIF, nPol;
	int doPolar, qb;
	double bw;
	int hasR = 0;
	int hasL = 0;
	char pol[2];
	double mjdStop;

	if(!D)
	{
		return;
	}
	
	D->nIF = -1;
	D->nPol = 0;
	D->doPolar = 0;
	D->nPolar = -1;
	D->chanBW = -1.0;
	D->quantBits = -1;
	strcpy(D->polPair, "  ");

	D->mjdStart = D->mjdStop = D->job->mjdStart;

	for(j = 0; j < D->nJob; j++)
	{
		if(D->job[j].mjdStart < D->mjdStart)
		{
			D->mjdStart = D->job[j].mjdStart;
		}
		mjdStop = D->job[j].mjdStart + D->job[j].duration/86400.0;
		if(mjdStop > D->mjdStop)
		{
			D->mjdStop = mjdStop;
		}
	}

	for(c = 0; c < D->nConfig; c++)
	{
		dc = D->config + c;

		nIF = dc->nIF;
		qb  = dc->quantBits;
		if(D->nIF < nIF)
		{
			D->nIF = nIF;
		}
		if(D->quantBits < 0)
		{
			D->quantBits = qb;
		}
		else if(D->quantBits != qb)
		{
			D->quantBits = 0;
		}
		doPolar = DifxConfigCalculateDoPolar(dc, D->baseline);
		if(D->doPolar < doPolar)
		{
			D->doPolar = doPolar;
		}
		for(i = 0; i < nIF; i++)
		{
			nPol   = dc->IF[i].nPol;
			bw     = dc->IF[i].bw;
			pol[0] = dc->IF[i].pol[0];
			pol[1] = dc->IF[i].pol[1];
			if(doPolar)
			{
				nPol *= 2;
			}
			if(D->nPolar < nPol)
			{
				D->nPolar = nPol;
			}
			if(D->chanBW < 0.0)
			{
				D->chanBW = bw;
			}
			else if(D->chanBW != bw)
			{
				D->chanBW = 0.0;
				return; 
			}
			if(nPol > 0)
			{
				n = nPol > 1 ? 2 : 1;
				for(p = 0; p < n; p++)
				{
					switch(pol[p])
					{
						case 'R':
							hasR = 1;
							break;
						case 'L':
							hasL = 1;
							break;
					}
				}
			}
		}
	}
	if(D->nPolar == 4)
	{
		D->nPol = 2;
		D->doPolar = 1;
	}
	else
	{
		D->nPol = D->nPolar;
		D->doPolar = 0;
	}
	if(hasR)
	{
		D->polPair[0] = 'R';
		if(hasL)
		{
			D->polPair[1] = 'L';
		}
	}
	else
	{
		if(hasL)
		{
			D->polPair[0] = 'L';
		}
	}
}

static int sameFQ(const DifxConfig *C1, const DifxConfig *C2)
{
	int i, p;
	
	if(C1->nIF != C2->nIF)
	{
		return 0;
	}

	for(i = 0; i < C1->nIF; i++)
	{
		if(C1->IF[i].freq != C2->IF[i].freq)
		{
			return 0;
		}
		if(C1->IF[i].bw != C2->IF[i].bw)
		{
			return 0;
		}
		if(C1->IF[i].sideband != C2->IF[i].sideband)
		{
			return 0;
		}
		if(C1->IF[i].nPol != C2->IF[i].nPol)
		{
			return 0;
		}
		for(p = 0; p < C1->IF[i].nPol; p++)
		{
			if(C1->IF[i].pol[p] != C2->IF[i].pol[p])
			{
				return 0;
			}
		}
	}

	return 1;
}

static int calcFreqIds(DifxInput *D)
{
	int c, d;
	int nFQ = 0;

	if(!D)
	{
		return 0;
	}

	if(D->nConfig < 1)
	{
		return 0;
	}
	
	D->config[0].fitsFreqId = nFQ;
	nFQ++;
	
	if(D->nConfig < 2)
	{
		return 1;
	}

	for(c = 1; c < D->nConfig; c++)
	{
		D->config[c].fitsFreqId = -1;
		for(d = 0; d < c; d++)
		{
			if(sameFQ(&(D->config[c]), &(D->config[d])))
			{
				D->config[c].fitsFreqId = D->config[d].fitsFreqId;
				d = c; /* terminate inner loop */
			}
		}
		if(D->config[c].fitsFreqId == -1)
		{
			D->config[c].fitsFreqId = nFQ;
			nFQ++;
		}
	}

	return nFQ;
}

DifxInput *updateDifxInput(DifxInput *D)
{
	D = deriveDifxInputValues(D);
	calcFreqIds(D);
	D = deriveFitsSourceIds(D);
	//D = setFitsSourceIds(D);
	setGlobalValues(D);
	setOrbitingAntennas(D);
	
	return D;
}

DifxInput *loadDifxInput(const char *filePrefix)
{
	DifxParameters *ip, *cp, *mp;
	DifxInput *D, *DSave;
	char inputFile[DIFXIO_FILENAME_LENGTH];
	char flagFile[DIFXIO_FILENAME_LENGTH];
	const char *calcFile;
	const char *modelFile;
	int c, r;

	/* make .input filename and open it. */
	r = snprintf(inputFile, DIFXIO_FILENAME_LENGTH,
		"%s.input", filePrefix);
	if(r >= DIFXIO_FILENAME_LENGTH)
	{
		return 0;
	}

	ip = newDifxParametersfromfile(inputFile);
	if(!ip)
	{
		return 0;
	}

	/* get .calc filename and open it. */
	r = DifxParametersfind(ip, 0, "CALC FILENAME");
	if(r < 0)
	{
		return 0;
	}
	calcFile = DifxParametersvalue(ip, r);

	cp = newDifxParametersfromfile(calcFile);
	if(!cp)
	{
		deleteDifxParameters(ip);
		
		return 0;
	}

	/* get .im filename and open it. */
	r = DifxParametersfind(cp, 0, "IM FILENAME");
	if(r < 0)
	{
		return 0;
	}
	modelFile = DifxParametersvalue(cp, r);

	mp = newDifxParametersfromfile(modelFile);

	D = DSave = newDifxInput();
	snprintf(D->inputFile, DIFXIO_FILENAME_LENGTH, "%s", inputFile);

	/* When creating a DifxInput via this function, there will always
	 * be a single DifxJob
	 */
	D->job = newDifxJobArray(1);
	D->nJob = 1;
	snprintf(D->job->fileBase, DIFXIO_FILENAME_LENGTH, "%s", filePrefix);
	D = populateInput(D, ip);
	D = populateCalc(D, cp);
	if(mp)
	{
		D = populateIM(D, mp);
	}

	if(!D)
	{
		deleteDifxInput(DSave);
	}
	
	deleteDifxParameters(ip);
	deleteDifxParameters(cp);
	deleteDifxParameters(mp);

	/* read in flags, if any */
	sprintf(flagFile,  "%s.flag",  filePrefix);
	populateFlags(D, flagFile);


	for(c = 0; c < D->nConfig; c++)
	{
		DifxConfigMapAntennas(D->config + c, D->datastream);
	}
	
	return D;
}

DifxInput *loadDifxCalc(const char *filePrefix)
{
	DifxParameters *ip, *cp;
	DifxInput *D, *DSave;
	char inputFile[DIFXIO_FILENAME_LENGTH];
	const char *calcFile;
	int c, r;

	r = snprintf(inputFile, DIFXIO_FILENAME_LENGTH,
		"%s.input", filePrefix);
	if(r >= DIFXIO_FILENAME_LENGTH)
	{
		return 0;
	}

	ip = newDifxParametersfromfile(inputFile);
	if(!ip)
	{
		return 0;
	}

	r = DifxParametersfind(ip, 0, "CALC FILENAME");
	if(r < 0)
	{
		return 0;
	}

	calcFile = DifxParametersvalue(ip, r);

	cp = newDifxParametersfromfile(calcFile);
	if(!cp)
	{
		deleteDifxParameters(ip);
		
		return 0;
	}

	D = DSave = newDifxInput();

	/* When creating a DifxInput via this function, there will always
	 * be a single DifxJob
	 */
	D->job = newDifxJobArray(1);
	D->nJob = 1;
	snprintf(D->job->fileBase, DIFXIO_FILENAME_LENGTH, "%s", filePrefix);
	D = populateInput(D, ip);
	D = populateCalc(D, cp);

	if(!D)
	{
		deleteDifxInput(DSave);
		return 0;
	}

	deleteDifxParameters(ip);
	deleteDifxParameters(cp);

	for(c = 0; c < D->nConfig; c++)
	{
		DifxConfigMapAntennas(D->config + c, D->datastream);
	}
	
	return D;
}

/* return -1 if no suitable source found */
int DifxInputGetScanIdByJobId(const DifxInput *D, double mjd, int jobId)
{
	int scanId;

	if(!D)
	{
		return -1;
	}

	if(mjd <= D->job[jobId].mjdStart) 
	{
		return -1;
	}

	for(scanId = 0; scanId < D->nScan; scanId++)
	{
		if(mjd <= D->scan[scanId].mjdEnd && 
		   D->scan[scanId].jobId == jobId)
		{
			//printf("Found scanId, which was %d\n", scanId);
			return scanId;
		}
	}

	return -1;
}

/* return -1 if no suitable scan found */
int DifxInputGetScanIdByAntennaId(const DifxInput *D, double mjd, 
	int antennaId)
{
	int d, c, scanId, dsId, antId=0;
	const DifxConfig *config;

	if(!D)
	{
		return -1;
	}

	for(scanId = 0; scanId < D->nScan; scanId++)
	{
		c = D->scan[scanId].configId;
		if(c < 0 || c >= D->nConfig)
		{
			continue;
		}
		config = D->config + c;

		/* here "d" is "datastream # within conf.", not "antenanId" */
		for(d = 0; d < config->nDatastream; d++)
		{
			dsId = config->datastreamId[d];
			if(dsId < 0 || 
			   dsId >= D->nDatastream ||
			   dsId >= config->nDatastream)
			{
				continue;
			}
			
			antId = D->datastream[dsId].antennaId;
			if(antId < 0 || antId >= D->nAntenna)
			{
				continue;
			}
			
			if(antennaId == antId)
			{
				break;
			}
		}
		if(d == config->nDatastream)
		{
			continue;
		}

		if(isAntennaFlagged(D->job + D->scan[scanId].jobId, mjd, antennaId))
		{
			continue;
		}
		
		if(mjd <  D->scan[scanId].mjdEnd   &&
		   mjd >= D->scan[scanId].mjdStart &&
		   D->scan[scanId].im[antId] != 0)
		{
			return scanId;
		}
	}

	return -1;
}

/* return -1 if no suitable scan found */
/* FIXME -- this function is ill-posed */
int DifxInputGetScanId(const DifxInput *D, double mjd)
{
	int c, scanId;

	if(!D)
	{
		return -1;
	}

	for(scanId = 0; scanId < D->nScan; scanId++)
	{
		c = D->scan[scanId].configId;
		if(c < 0 || c >= D->nConfig)
		{
			continue;
		}
		if(mjd < D->scan[scanId].mjdEnd && mjd >= D->scan[scanId].mjdStart)
		{
			return scanId;
		}
	}

	return -1;
}

int DifxInputGetPointingSourceIdByJobId(const DifxInput *D, double mjd, int jobId)
{
	int scanId;

	scanId = DifxInputGetScanIdByJobId(D, mjd, jobId);
	if(scanId < 0 || scanId >= D->nScan)
	{
		return -1;
	}
	else
	{
		return D->scan[scanId].pointingCentreSrc;
	}
}

int DifxInputGetPointingSourceIdByAntennaId(const DifxInput *D, double mjd, 
	int antennaId)
{
	int scanId;

	scanId = DifxInputGetScanIdByAntennaId(D, mjd, antennaId);
	if(scanId < 0 || scanId >= D->nScan)
	{
		return -1;
	}
	else
	{
		return D->scan[scanId].pointingCentreSrc;
	}
}

/* return 0-based index of antName, or -1 if not in array */
int DifxInputGetAntennaId(const DifxInput *D, const char *antennaName)
{
	int a;
	
	if(!D)
	{
		return -1;
	}

	for(a = 0; a < D->nAntenna; a++)
	{
		if(strcmp(D->antenna[a].name, antennaName) == 0)
		{
			return a;
		}
	}

	return -1;
}

static int AntennaCompare(const void *a1, const void *a2)
{
	DifxAntenna *A1, *A2;

	A1 = (DifxAntenna *)a1;
	A2 = (DifxAntenna *)a2;

	return strcmp(A1->name, A2->name);
}

/* go through antenna table and reorder.  Then clean up the collateral
 * damage */ 
int DifxInputSortAntennas(DifxInput *D, int verbose)
{
	int a, a2, d, f, s, j;
	int *old2new;
	int changed = 0;
	DifxJob *job;
	DifxPolyModel ***p2;

	if(!D)
	{
		return -1;
	}
	if(D->nAntenna < 2)
	{
		return -1;
	}
	
	old2new = (int *)calloc(D->nAntenna, sizeof(int));
	
	/* sort antenna table and derive reorder table */
	for(a = 0; a < D->nAntenna; a++)
	{
		D->antenna[a].origId = a;
	}

	if(verbose > 0)
	{
		printf("Pre-sort :");
		for(a = 0; a < D->nAntenna; a++)
		{
			printf(" %s", D->antenna[a].name);
		}
		printf("\n");
	}
	qsort(D->antenna, D->nAntenna, sizeof(DifxAntenna), AntennaCompare);
	if(verbose > 0)
	{
		printf("Post-sort:");
		for(a = 0; a < D->nAntenna; a++)
		{
			printf(" %s", D->antenna[a].name);
		}
		printf("\n");
	}

	/* look for no-sort condition and leave successfully if no sort done */
	for(a = 0; a < D->nAntenna; a++)
	{
		old2new[D->antenna[a].origId] = a;
		if(D->antenna[a].origId != a)
		{
			changed++;
		}
	}
	if(changed == 0)
	{
		free(old2new);
		return 0;
	}

	/* OK -- antennas have been reordered.  Fix the tables. */

	/* 1. Datastream table */
	for(d = 0; d < D->nDatastream; d++)
	{
		D->datastream[d].antennaId =
			old2new[D->datastream[d].antennaId];
	}

	/* 2. Flags */
	for(j = 0; j < D->nJob; j++)
	{
		job = D->job + j;

		for(f = 0; f < job->nFlag; f++)
		{
			job->flag[f].antennaId = 
				old2new[job->flag[f].antennaId];
		}
	}

	/* 3. The model tables for each scan */
	for(s = 0; s < D->nScan; s++)
	{
		/* correct the polynomial model table */
		if(D->scan[s].im)
		{
			p2 = (DifxPolyModel ***)calloc(D->nAntenna*(D->scan[s].nPhaseCentres+1), sizeof(DifxPolyModel *));
			for(a = 0; a < D->scan[s].nAntenna; a++)
			{
				if(D->scan[s].im[a])
				{
					a2 = old2new[a];
					if(a2 < 0 || a2 >= D->nAntenna)
					{
						fprintf(stderr, "Error: DifxInputSortAntennas: "
                                                "old2new[%d] = %d; nAnt = %d\n",
                                                a, a2, D->scan[s].nAntenna);
	                                        continue;
					}
                                
                                	p2[a2] = D->scan[s].im[a];
				}
                        }

                        free(D->scan[s].im);
                        D->scan[s].im = p2;
		}
	
		D->scan[s].nAntenna = D->nAntenna;
	}

	free(old2new);

	/* success */
	return 0;
}

/* note -- this will not work if different integration times are requested within one job */
int DifxInputSimFXCORR(DifxInput *D)
{
	DifxConfig *dc;
	const DifxDatastream *dd;
	double quantum;
	double tInt, sec, mjdStart;
	double sec_old, deltasec;
	int c, d, n, mjd;
	int speedUp = 4, su, fanout;
	int nBitstream, sampRate;

	if(!D)
	{
		fprintf(stderr, "Error: DifxInputSimFXCORR: D = 0\n");
		return -1;
	}

	if(D->nDatastream < 1 || D->nConfig < 1 || D->nFreq < 1)
	{
		fprintf(stderr, "Error: DifxInputSimFXCORR: incomplete DifxInput structure\n");
		return -2;
	}

	for(d = 0; d < D->nDatastream; d++)
	{
		dd = D->datastream + d;
		if(dd->quantBits < 1)
		{
			fprintf(stderr, "Error: datastream %d quantBits=%d\n", d, dd->quantBits);
			continue;
		}
		if(dd->nRecBand < 1)
		{
			fprintf(stderr, "Error: datastream %d nRecBand=%d\n", d, dd->nRecBand);
			continue;
		}

		nBitstream = dd->dataFrameSize/20000;
		nBitstream *= 8;
		fanout = nBitstream/(dd->quantBits*dd->nRecBand);
		sampRate = (int)(D->freq[dd->recFreqId[0]].bw*2.0+0.00001);
		if(sampRate < 1)
		{
			continue;
		}
		su = 8*fanout/sampRate;
		if(su < speedUp)
		{
			speedUp = su;
		}
	}
	if(speedUp > 4)
	{
		fprintf(stderr, "Warning: reducing speedUp to 4 from %d\n", speedUp);
		speedUp = 4;
	}

	/* the quantum of integration time */
	quantum = 0.131072*speedUp;

	for(c = 0; c < D->nConfig; c++)
	{
		dc = D->config + c;
		n = (int)(dc->tInt/quantum + 0.5);
		dc->tInt = n * quantum;
	}

	/* here use the first config's tInt to derive the start time of the job */
	tInt = D->config[0].tInt;

	mjd = D->mjdStart;
	sec = sec_old = (D->mjdStart - mjd)*86400.0;

	n = sec / tInt;
	sec = n*tInt;

	deltasec = sec - sec_old;

	mjdStart = mjd + sec/86400.0;
	if(mjdStart < D->mjdStart)
	{
		n++;
		mjdStart += tInt/86400.0;
		deltasec += tInt;
	}

	/* Work around problem that occurs if frac sec >= 0.5 */
	while(86400.0*mjdStart - (long long)(86400.0*mjdStart) > 0.49999)
	{
		n++;
		mjdStart += tInt/86400.0;
		deltasec += tInt;
	}

	/* recompute to avoid bad rounding issues */
	mjdStart = mjd + n*tInt/86400.0;

	D->mjdStart = mjdStart;
	D->job[0].jobStart = D->mjdStart;
	D->fracSecondStartTime = 1;

	printf("FXCORR Simulator: delayed job start time by %8.6f seconds\n",
                deltasec);

	/* Now that clocks have their own reference times, this doesn't matter */

	/* FIXME -- reset BLOCKSPERSEND here? */

	return 0;
}

/* FIXME: Perhaps this needs special treatment for band match/zoom bands */
int DifxInputGetMaxTones(const DifxInput *D)
{
	int f;
	int maxTones = 0;

	for(f = 0; f < D->nFreq; f++)
	{
		if(D->freq[f].nTone > maxTones)
		{
			maxTones = D->freq[f].nTone;
		}
	}

	return maxTones;
}

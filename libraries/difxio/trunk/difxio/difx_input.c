/***************************************************************************
 *   Copyright (C) 2007, 2008, 2009 by Walter Brisken                      *
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
		if(D->freq)
		{
			deleteDifxFreqArray(D->freq);
		}
		if(D->antenna)
		{
			deleteDifxAntennaArray(D->antenna,
				D->nAntenna);
		}
		if(D->scan)
		{
			deleteDifxScanArray(D->scan, D->nScan);
		}
		if(D->source)
		{
			deleteDifxSourceArray(D->source);
		}
		if(D->eop)
		{
			deleteDifxEOPArray(D->eop);
		}
		if(D->flag)
		{
			deleteDifxAntennaFlagArray(D->flag);
		}
		if(D->job)
		{
			deleteDifxJobArray(D->job);
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

	fprintf(fp, "  mjdStart = %14.8f\n", D->mjdStart);
	fprintf(fp, "  mjdStop  = %14.8f\n", D->mjdStop);
	fprintf(fp, "  vis buffer length = %d\n", D->visBufferLength);
	fprintf(fp, "  FFT size = %d\n", D->nFFT);
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

	/*
	fprintf(fp, "  nFlags = %d\n", D->nFlag);
	fprintDifxAntennaFlagArray(fp, D->flag, D->nFlag);
	 */

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
	fprintf(fp, "  FFT size = %d\n", D->nFFT);
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

	if(D->nFlag > 0)
	{
		fprintf(fp, "  nFlags = %d\n", D->nFlag);
		fprintDifxAntennaFlagArray(fp, D->flag, D->nFlag);
	}
}

void printDifxInputSummary(const DifxInput *D)
{
	fprintDifxInputSummary(stdout, D);
}

static int parseUVWs(DifxModel **model, int nAntenna, int *antMap, int row, 
	const char *str)
{
	int a, antId;
	int n = 0, l;
	double u, v, w;

	if(*model == 0)
	{
		fprintf(stderr, "Error : parseUVWs : model is 0\n");
		exit(0);
	}
	
	for(a = 0; a < nAntenna; a++)
	{
		if(sscanf(str+n, "%lf%lf%lf%n", &u, &v, &w, &l) < 3)
		{
			fprintf(stderr, "UVW value for ant %d, row %d "
				"not parsed\n", a, row);
			return 0;
		}
		n += l;
		if(antMap)
		{
			antId = antMap[a];
		}
		else
		{
			antId = a;
		}
		if(antId >= 0)
		{
			model[antId][row].u = u;
			model[antId][row].v = v;
			model[antId][row].w = w;
		}
	}

	return 0;
}

static int parseDelays(DifxModel **model, int nAntenna, int *antMap, int row, 
	const char *str)
{
	int a, antId;
	int n = 0, l;
	double t;

	if(*model == 0)
	{
		fprintf(stderr, "Error : parseDelays : model is 0\n");
		exit(0);
	}
	
	for(a = 0; a < nAntenna; a++)
	{
		if(sscanf(str+n, "%lf%n", &t, &l) < 1)
		{
			fprintf(stderr, "Delay value for ant %d, row %d "
				"not parsed\n", a, row);
			return 0;
		}
		n += l;
		if(antMap)
		{
			antId = antMap[a];
		}
		else
		{
			antId = a;
		}
		if(antId >= 0)
		{
			model[antId][row].t = t;
		}
	}
	
	return a;
}

static int parseRates(DifxModel **model, int nAntenna, int *antMap, int row, 
	const char *str)
{
	int a, antId;
	int n = 0, l;
	double rate, dry, wet; /* units: us/s, us, us */

	if(*model == 0)
	{
		fprintf(stderr, "Error : parseRates : model is 0\n");
		exit(0);
	}
	
	for(a = 0; a < nAntenna; a++)
	{
		if(sscanf(str+n, "%lf%lf%lf%n", &rate, &dry, &wet, &l) < 1)
		{
			fprintf(stderr, "Rate value for ant %d, row %d "
				"not parsed\n", a, row);
			return 0;
		}
		n += l;
		if(antMap)
		{
			antId = antMap[a];
		}
		else
		{
			antId = a;
		}
		if(antId >= 0)
		{
			model[antId][row].dt  = rate;
			model[antId][row].dry = dry;
			model[antId][row].wet = wet;
		}
	}
	
	return a;
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
		fprintf(stderr, "deriveAntMap: NUM TELESCOPES too small: \n"
			"%d < %d\n", nTel, D->nAntenna);
	}

	antMap = (int *)malloc(nTel* sizeof(int));

	nFound = 0;
	for(t = 0; t < nTel; t++)
	{
		r = DifxParametersfind1(p, r+1, "TELESCOPE %d NAME", t);
		if(r < 0)
		{
			fprintf(stderr, "deriveAntMap: TELESCOPE %d NAME not "
				"found\n", t);
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

/* add x to list of integers if not already in list.  return new list size */
static int addtolist(int *list, int x, int n)
{
	int i;
	
	if(n < 0)
	{
		return n;
	}
	if(n == 0)
	{
		list[0] = x;
		return 1;
	}
	for(i = 0; i < n; i++)
	{
		if(list[i] == x)
		{
			return n;
		}
	}
	list[n] = x;

	return n+1;
}

static int makeFreqId2IFmap(DifxInput *D, int configId)
{
	DifxConfig *dc;
	DifxDatastream *ds;
	int *freqIds;
	int p, a, f, c, i;
	int maxFreqId=0;
	int nPol = 0;
	int haspol[4] = {0, 0, 0, 0};	/* indices are: 0-R, 1-L, 2-X, 3-Y */

	if(configId < 0)
	{
		fprintf(stderr, "Warning: makeFreqId2IFmap: configId = %d\n",
			configId);
		return 0;
	}

	dc = D->config + configId;
	dc->nIF = 0;

	freqIds = (int *)calloc(D->nFreq, sizeof(int));

	/* go through datastreams associated with this config and collect all
	 * distinct Freq table ids
	 */
	
	/* here "a" is "datastream # within configuration", not "antenna" */
	for(a = 0; a < dc->nDatastream; a++)
	{
		if(dc->datastreamId[a] < 0)
		{
			continue;
		}
		
		ds = D->datastream + dc->datastreamId[a];
		for(f = 0; f < ds->nFreq; f++)
		{
			dc->nIF = addtolist(freqIds, ds->freqId[f], dc->nIF);
			if(ds->freqId[f] > maxFreqId)
			{
				maxFreqId = ds->freqId[f];
			}
		}
	}
	
	/* Allocate space */
	if(dc->freqId2IF)
	{
		free(dc->freqId2IF);
	}
	dc->freqId2IF = (int *)calloc((maxFreqId+2), sizeof(int));
	dc->freqId2IF[maxFreqId+1] = -1;

	/* determine which polarizations are present.  All IFs in this 
	 * config will maintain slots for all polariazations, even if
	 * this ends up making wasted space in FITS files
	 */

	/* here "a" is "datastream # within configuration", not "antenna" */
	for(a = 0; a < dc->nDatastream; a++)
	{
		if(dc->datastreamId[a] < 0)
		{
			continue;
		}

		ds = D->datastream + dc->datastreamId[a];
		for(c = 0; c < ds->nRecChan; c++)
		{
			switch(ds->RCpolName[c])
			{
				case 'R': 
					haspol[0] = 1; 
					break;
				case 'L': 
					haspol[1] = 1; 
					break;
				case 'X': 
					haspol[2] = 1; 
					break;
				case 'Y': 
					haspol[3] = 1; 
					break;
				default:
					fprintf(stderr, "Warning: Unknown pol %c\n",
						ds->RCpolName[c]);
			}
		}
	}
	for(p = 0; p < 4; p++)
	{
		nPol += haspol[p];
	}
	if(nPol != 1 && nPol != 2)
	{
		fprintf(stderr, "Weird number of polarizations: %d\n", nPol);
		return -1;
	}
	dc->nPol = nPol;
	p = 0;
	dc->pol[1] = ' ';
	if(haspol[0])
	{
		dc->pol[p++] = 'R';
	}
	if(haspol[1])
	{
		dc->pol[p++] = 'L';
	}
	if(haspol[2])
	{
		dc->pol[p++] = 'X';
	}
	if(haspol[3])
	{
		dc->pol[p++] = 'Y';
	}

	dc->IF = newDifxIFArray(dc->nIF);
	for(i = 0; i < dc->nIF; i++)
	{
		f = freqIds[i];
		dc->freqId2IF[f]   = i;
		dc->IF[i].freq     = D->freq[f].freq;
		dc->IF[i].bw       = D->freq[f].bw;
		dc->IF[i].sideband = D->freq[f].sideband;
		dc->IF[i].nPol     = nPol;
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
		"EXECUTE TIME (SEC)",
		"START MJD",
		"START SECONDS",
		"ACTIVE DATASTREAMS",
		"ACTIVE BASELINES",
		"VIS BUFFER LENGTH"
	};
	const int N_COMMON_ROWS = sizeof(commonKeys)/sizeof(commonKeys[0]);
	int N;
	int rows[N_COMMON_ROWS];
	
	if(!D || !ip)
	{
		return 0;
	}

	N = DifxParametersbatchfind(ip, 0, commonKeys, N_COMMON_ROWS, rows);
	if(N < N_COMMON_ROWS)
	{
		fprintf(stderr, "populateInput: N < N_COMMON_ROWS %d < %d\n",
			N, N_COMMON_ROWS);
		return 0;
	}

	/* Initialize some of the structures */
	D->job->duration = atoi(DifxParametersvalue(ip, rows[0]));
	D->job->mjdStart = atoi(DifxParametersvalue(ip, rows[1])) +
		      atof(DifxParametersvalue(ip, rows[2]))/86400.0;
	D->job->activeDatastreams =
		      atoi(DifxParametersvalue(ip, rows[3]));
	D->job->activeBaselines =
		      atoi(DifxParametersvalue(ip, rows[4]));
	D->visBufferLength =
		      atoi(DifxParametersvalue(ip, rows[5]));

	if(DifxParametersfind(ip, 0, "DATA HEADER O/RIDE") > 0)
	{
		printf("Warning -- parsing old version of input file.\n");
		printf("  Watch out for unusual behavior!\n");
		D->inputFileVersion = 1;	/* pre-Perth Merge version */
	}

	return D;
}	

/* return -1 on a failure */
static int loadPulsarConfigFile(DifxInput *D, const char *fileName)
{
	DifxParameters *pp;
	DifxPulsar *dp;
	int i, r;

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
	dp->nPolyco = atoi(DifxParametersvalue(pp, r));
	dp->polyco = newDifxPolycoArray(dp->nPolyco);

	strcpy(dp->fileName, fileName);

	for(i = 0; i < dp->nPolyco; i++)
	{
		r = DifxParametersfind1(pp, r, "POLYCO FILE %d", i);
		if(r < 0)
		{
			deleteDifxParameters(pp);
			fprintf(stderr, "POLYCO FILE %d not found\n", i);
			return -1;
		}
		r = loadPulsarPolycoFile(&dp->polyco[i], 
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
		"CONFIG SOURCE",
		"INT TIME (SEC)",
		"NUM CHANNELS",
		"CHANNELS TO AVERAGE",
		"OVERSAMPLE FACTOR",
		"DECIMATION FACTOR",
		"BLOCKS PER SEND",
		"GUARD BLOCKS",
		"POST-F FRINGE ROT",
		"QUAD DELAY INTERP",
		"PULSAR BINNING"
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
			fprintf(stderr, "parseDifxInputConfigurations: "
				"N < N_CONFIG_ROWS %d "
				"< %d\n", N, N_CONFIG_ROWS);
			return 0;
		}
		strcpy(dc->name,         DifxParametersvalue(ip, rows[0]));
		dc->tInt          = atof(DifxParametersvalue(ip, rows[1]));
		dc->nChan         = atoi(DifxParametersvalue(ip, rows[2]));
		dc->specAvg       = atoi(DifxParametersvalue(ip, rows[3]));
		dc->overSamp      = atoi(DifxParametersvalue(ip, rows[4]));
		dc->decimation    = atoi(DifxParametersvalue(ip, rows[5]));
		dc->blocksPerSend = atoi(DifxParametersvalue(ip, rows[6]));
		dc->guardBlocks   = atoi(DifxParametersvalue(ip, rows[7]));
		
		dc->postFFringe = 
			abs(strcmp("FALSE", DifxParametersvalue(ip, rows[8])));
		dc->quadDelayInterp = 
			abs(strcmp("FALSE", DifxParametersvalue(ip, rows[9])));
		dc->nDatastream  = D->job->activeDatastreams;
		dc->nBaseline    = D->job->activeBaselines;

		/* pulsar stuff */
		if(strcmp(DifxParametersvalue(ip, rows[5]), "TRUE") == 0)
		{
			r = DifxParametersfind(ip, rows[5], 
				"PULSAR CONFIG FILE");
			if(r <= 0)
			{
				fprintf(stderr, "input file row %d : "
					"PULSAR CONFIG FILE expected\n",
					rows[5] + 2);
				return 0;
			}
			dc->pulsarId = loadPulsarConfigFile(D,
				DifxParametersvalue(ip, r));
			if(dc->pulsarId < 0)
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
				fprintf(stderr, 
					"DATASTREAM %d INDEX not found\n", a);
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
				fprintf(stderr, 
					"BASELINE %d INDEX not found\n", b);
				return 0;
			}
			dc->baselineId[b] = atoi(DifxParametersvalue(ip, r));
		}
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
		"SIDEBAND %d"
	};
	const int N_FREQ_ROWS = sizeof(freqKeys)/sizeof(freqKeys[0]);
	int b, r, N;
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
			fprintf(stderr, "populateInput: N < N_FREQ_ROWS %d "
				"< %d\n", N, N_FREQ_ROWS);
			return 0;
		}
		D->freq[b].freq     = atof(DifxParametersvalue(ip, rows[0]));
		D->freq[b].bw       = atof(DifxParametersvalue(ip, rows[1]));
		D->freq[b].sideband = DifxParametersvalue(ip, rows[2])[0];
	}
	
	return D;
}

static DifxInput *parseDifxInputTelescopeTable(DifxInput *D, 
	const DifxParameters *ip)
{
	const char antKeys[][MAX_DIFX_KEY_LEN] =
	{
		"TELESCOPE NAME %d",
		"CLOCK DELAY (us) %d",
		"CLOCK RATE(us/s) %d"
	};
	const int N_ANT_ROWS = sizeof(antKeys)/sizeof(antKeys[0]);
	int a, r, N;
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
			fprintf(stderr, "populateInput: N < N_ANT_ROWS %d "
				"< %d\n", N, N_ANT_ROWS);
			return 0;
		}
		strcpy(D->antenna[a].name, DifxParametersvalue(ip, rows[0]));
		D->antenna[a].delay = atof(DifxParametersvalue(ip, rows[1]));
		D->antenna[a].rate  = atof(DifxParametersvalue(ip, rows[2]));
	}

	return D;
}

static DifxInput *parseDifxInputDatastreamTable(DifxInput *D,
	const DifxParameters *ip)
{
	int a, e, i, r, r2, v, nr;
	int nRecChan;

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
		strncpy(D->datastream[e].dataFormat,
			DifxParametersvalue(ip, r), 31);
	
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

		r = DifxParametersfind(ip, r+1, "DATA SOURCE");
		if(r < 0)
		{
			fprintf(stderr, "Cannot determine data source\n");
			return 0;
		}
		strncpy(D->datastream[e].dataSource, 
			DifxParametersvalue(ip, r), 31);
		D->datastream[e].dataSource[31] = 0;

		r = DifxParametersfind(ip, r+1, "NUM FREQS");
		if(r < 0)
		{
			fprintf(stderr, "NUM FREQS not found\n");
			return 0;
		}

		DifxDatastreamAllocFreqs(D->datastream + e, 
			atoi(DifxParametersvalue(ip, r)));

		nRecChan = 0;
		for(i = 0; i < D->datastream[e].nFreq; i++)
		{
			r = DifxParametersfind1(ip, r+1, 
				"FREQ TABLE INDEX %d", i);
			D->datastream[e].freqId[i] = 
				atoi(DifxParametersvalue(ip, r));
			r = DifxParametersfind1(ip, r+1, 
				"CLK OFFSET %d (us)", i);
			D->datastream[e].clockOffset[i] =
				atof(DifxParametersvalue(ip, r));
			r = DifxParametersfind1(ip, r+1, 
				"NUM POLS %d", i);
			D->datastream[e].nPol[i] = 
				atoi(DifxParametersvalue(ip, r));
			nRecChan += D->datastream[e].nPol[i];
		}

		/* count rec chans to make sure we have enough */
		nr = 0;
		for(i = 0; ; i++)
		{
			v = DifxParametersfind1(ip, r,
				"INPUT BAND %d POL", i);
			if(v <= 0 || v > r+2*i+2)
			{
				break;
			}
			nr++;
		}

		if(nr > nRecChan)
		{
			nRecChan = nr;
		}

		DifxDatastreamAllocRecChans(D->datastream + e, nRecChan);

		for(i = 0; i < nRecChan; i++)
		{
			r = DifxParametersfind1(ip, r+1,
				"INPUT BAND %d POL", i);
			if(r < 0)
			{
				fprintf(stderr, "Warning: parseDifxInputDatastreamTable: "
					"INPUT BAND %d POL not found\n", i);
				continue;
			}
			D->datastream[e].RCpolName[i] = 
				DifxParametersvalue(ip, r)[0];
			r = DifxParametersfind1(ip, r+1,
				"INPUT BAND %d INDEX", i);
			if(r < 0)
			{
				fprintf(stderr, "Error: parseDifxInputDatastreamTable: "
					"INPUT BAND %d INDEX not found\n", i);
				return 0;
			}
			a = atoi(DifxParametersvalue(ip, r));
			D->datastream[e].RCfreqId[i] = D->datastream[e].freqId[a];
		}
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
				fprintf(stderr, "POL PRODUCTS %d/%d "
					"not found\n", b, f);
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
					fprintf(stderr, "D/STREAM A BAND %d "
						"not found\n", p);
					return 0;
				}
				D->baseline[b].recChanA[f][p] =
					atoi(DifxParametersvalue(ip, r));
				r = DifxParametersfind1(ip, r+1, 
					"D/STREAM B BAND %d", p);
				if(r < 0)
				{
					fprintf(stderr, "D/STREAM B BAND %d "
						"not found\n", p);
					return 0;
				}
				D->baseline[b].recChanB[f][p] =
					atoi(DifxParametersvalue(ip, r));
			}
		}
	}

	return D;
}

static DifxInput *parseDifxInputDataTable(DifxInput *D, 
	const DifxParameters *ip)
{
	int a, i, N;
	int r = 1;
	const char *value;
	DifxAntenna *da;

	if(!D || !ip)
	{
		return 0;
	}

	for(a = 0; a < D->nAntenna; a++)
	{
		da = D->antenna + a;

		strcpy(da->vsn, "none");

		r = DifxParametersfind1(ip, r, "D/STREAM %d FILES", a);
		if(r < 0)
		{
			fprintf(stderr, "D/STREAM %d FILES not found\n", a);
			return 0;
		}

		N = atoi(DifxParametersvalue(ip, r));
		if(N < 1)
		{
			fprintf(stderr, "D/STREAM %d FILES has illegal value [%s]\n", a,
				DifxParametersvalue(ip, r));
			return 0;
		}

		if(N > 1)
		{
			allocateDifxAntennaFiles(da, N);
		}

		for(i = 0; i < N; i++)
		{
			r = DifxParametersfind2(ip, r, "FILE %d/%d", a, i);
			if(r < 0)
			{
				fprintf(stderr, "FILE %d/%d not found\n", a, i);
				return 0;
			}

			value = DifxParametersvalue(ip, r);
			if(N == 1 && strlen(value) == 8 && value[0] != '/')
			{
				strncpy(da->vsn, value, 8);
				da->vsn[8] = 0;
			}
			else
			{
				if(N == 1)
				{
					allocateDifxAntennaFiles(da, 1);
				}
				da->file[i] = strdup(value);
			}
		}
	}

	return D;
}

static DifxInput *deriveDifxInputValues(DifxInput *D)
{
	int a, b, c, e, qb, nChan = 0, nc;
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
	for(b = 0; b < D->nFreq; b++)
	{
		if(D->freq[b].freq < D->refFreq || D->refFreq <= 0.0)
		{
			D->refFreq = D->freq[b].freq;
		}
	}

	for(c = 0; c < D->nConfig; c++)
	{
		/* determine number of bits, or zero if different among
		 * antennas */
		D->config[c].quantBits = -1;
		qb = 0;

		/* here "a" is "datastream # within conf", not "antenna" */
		for(a = 0; a < D->config[c].nDatastream; a++)
		{
			e = D->config[c].datastreamId[a];
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

		if(nChan == 0)
		{
			nChan = D->config[c].nChan;
		}
		else if(nChan != D->config[c].nChan)
		{
			nChan = -1;
		}
	}

	for(c = 0; c < D->nConfig; c++)
	{
		makeFreqId2IFmap(D, c);
		makeBaselineFreq2IF(D, c);
	}

	if(nChan == -1)
	{
		fprintf(stderr, "deriveDifxInputValues: nChan changes "
			"between configs\n");
		return 0;
	}
	else
	{
		nc = nChan - D->startChan;
		if(D->nOutChan <= 0 || D->nOutChan > nc/D->specAvg)
		{
			D->nOutChan = nc/D->specAvg;
		}
		D->nFFT = nChan*2;
		D->nInChan = nChan;
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

	return D;
}

static DifxInput *populateUVW(DifxInput *D, DifxParameters *up)
{
	int a, i, c, p, r = 0, v, N, nTel;
	int nPoint, startPoint;
	int rows[20];
	double mjdStop;
	int *antMap;

	const char initKeys[][MAX_DIFX_KEY_LEN] = 
	{
		"INCREMENT (SECS)",
		"NUM SCANS"
	};
	const int N_INIT_ROWS = sizeof(initKeys)/sizeof(initKeys[0]);
	
	const char antKeys[][MAX_DIFX_KEY_LEN] =
	{
		"TELESCOPE %d MOUNT",
		"TELESCOPE %d X (m)",
		"TELESCOPE %d Y (m)",
		"TELESCOPE %d Z (m)"
	};
	const int N_ANT_ROWS = sizeof(antKeys)/sizeof(antKeys[0]);

	const char scanKeys[][MAX_DIFX_KEY_LEN] =
	{
		"SCAN %d POINTS",
		"SCAN %d START PT",
		"SCAN %d SRC NAME",
		"SCAN %d SRC RA",
		"SCAN %d SRC DEC"
	};
	const int N_SCAN_ROWS = sizeof(scanKeys)/sizeof(scanKeys[0]);
	
	if(!D)
	{
		return 0;
	}

	antMap = deriveAntMap(D, up, &nTel);
	if(antMap == 0)
	{
		fprintf(stderr, "populateUVW: deriveAntMap failed\n");
		return 0;
	}

	D->nEOP = 0;

	N = DifxParametersbatchfind(up, 0, initKeys, N_INIT_ROWS, rows);
	if(N < N_INIT_ROWS)
	{
		return 0;
	}

	D->job->modelInc = atof(DifxParametersvalue(up, rows[0]));
	D->nScan         = atoi(DifxParametersvalue(up, rows[1]));

	D->scan  = newDifxScanArray(D->nScan);
	
	rows[N_ANT_ROWS-1] = 0;		/* initialize start */
	for(i = 0; i < D->nAntenna; i++)
	{
		N = DifxParametersbatchfind1(up, rows[N_ANT_ROWS-1], antKeys,
			i, N_ANT_ROWS, rows);
		if(N < N_ANT_ROWS)
		{
			free(antMap);
			return 0;
		}
		a = antMap[i];
		if(a < 0)
		{
			printf("populateUVW: ignoring TELESCOPE %d\n", i);
			continue;
		}
		strcpy(D->antenna[a].mount, DifxParametersvalue(up, rows[0]));
		if(strcasecmp(D->antenna[a].mount, "azel"))
		{
			strcpy(D->antenna[a].mount, "altz");
		}
		D->antenna[a].offset[0]= 0.0;	/* Default */
		D->antenna[a].offset[1]= 0.0;	
		D->antenna[a].offset[2]= 0.0;	
		D->antenna[a].X        = atof(DifxParametersvalue(up, rows[1]));
		D->antenna[a].Y        = atof(DifxParametersvalue(up, rows[2]));
		D->antenna[a].Z        = atof(DifxParametersvalue(up, rows[3]));
		D->antenna[a].dX       = 0.0;
		D->antenna[a].dY       = 0.0;
		D->antenna[a].dZ       = 0.0;
	}

	mjdStop = D->job->mjdStart + D->job->duration/86400.0;
	
	rows[N_SCAN_ROWS-1] = 0;
	for(i = 0; i < D->nScan; i++)
	{
		N = DifxParametersbatchfind1(up, rows[N_SCAN_ROWS-1], scanKeys, 
			i, N_SCAN_ROWS, rows);
		if(N < N_SCAN_ROWS)
		{
			free(antMap);
			return 0;
		}
		nPoint               = atoi(DifxParametersvalue(up, rows[0]));
		startPoint           = atoi(DifxParametersvalue(up, rows[1]));
		D->scan[i].nPoint    = nPoint;
		D->scan[i].startPoint= startPoint;
		D->scan[i].mjdStart  = D->job->mjdStart + 
			startPoint*D->job->modelInc/86400.0;
		D->scan[i].mjdEnd    = D->job->mjdStart + 
			(startPoint+nPoint)*D->job->modelInc/86400.0;
		if(D->scan[i].mjdEnd > mjdStop)
		{
			D->scan[i].mjdEnd = mjdStop;
		}
		D->scan[i].nPoint    = nPoint;
		strncpy(D->scan[i].name, 
			DifxParametersvalue(up, rows[2]), 31);
		D->scan[i].name[31]  = 0;
		D->scan[i].model     = newDifxModelArray(D->nAntenna, nPoint);
		D->scan[i].nAntenna  = D->nAntenna;
		
		D->scan[i].ra        = atof(DifxParametersvalue(up, rows[3]));
		D->scan[i].dec       = atof(DifxParametersvalue(up, rows[4]));
		D->scan[i].calCode[0]= 0;
		D->scan[i].qual      = 0;	/* Default */

		for(c = 0; c < D->nConfig; c++)
		{
			if(strcmp(D->scan[i].name, D->config[c].name) == 0)
			{
				D->scan[i].configId = c;
			}
		}

		if(D->scan[i].configId == -1)
		{
			for(c = 0; c < D->nConfig; c++)
			{
				if(strcmp("DEFAULT", D->config[c].name) == 0)
				{
					D->scan[i].configId = c;
				}
			}
		}

		for(p = -1; p <= D->scan[i].nPoint+1; p++)
		{
			r = DifxParametersfind1(up, r+1, "RELATIVE INC %d", p);
			if(r < 0)
			{
				fprintf(stderr, "UVW row not found : %d %d\n",
					i, p);
				free(antMap);
				return 0;
			}
			v = parseUVWs(D->scan[i].model, nTel, antMap, p,
				DifxParametersvalue(up, r));
			if(v < 0)
			{
				fprintf(stderr, "UVW parse error\n");
				free(antMap);
				return 0;
			}
		}
	}

	free(antMap);

	return D;
}

static DifxInput *populateDelay(DifxInput *D, DifxParameters *dp)
{
	int p, r = 0, s, v;
	int nTel;
	int *antMap;
	
	if(!D)
	{
		return 0;
	}

	antMap = deriveAntMap(D, dp, &nTel);
	if(antMap == 0)
	{
		fprintf(stderr, "populateDelay: deriveAntMap failed\n");
		return 0;
	}

	for(s = 0; s < D->nScan; s++)
	{
		for(p = -1; p <= D->scan[s].nPoint+1; p++)
		{
			r = DifxParametersfind1(dp, r+1, "RELATIVE INC %d", p);
			if(r < 0)
			{
				fprintf(stderr, "Delay row not found : %d %d\n",
					s, p);
				free(antMap);
				return 0;
			}
			v = parseDelays(D->scan[s].model, nTel, antMap, p,
				DifxParametersvalue(dp, r));
			if(v < 0)
			{
				fprintf(stderr, "Delay parse error\n");
				free(antMap);
				return 0;
			}
		}
	}

	free(antMap);

	return D;
}

static DifxInput *populateCalc(DifxInput *D, DifxParameters *cp)
{
	const char initKeys[][MAX_DIFX_KEY_LEN] = 
	{
		"JOB ID",
		"OBSCODE",
		"NUM SCANS",
		"NUM EOP"
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

	const char eopKeys[][MAX_DIFX_KEY_LEN] =
	{
		"EOP %d TIME (mjd)",
		"EOP %d TAI_UTC (sec)",
		"EOP %d UT1_UTC (sec)",
		"EOP %d XPOLE (arcsec)",
		"EOP %d YPOLE (arcsec)"
	};
	const int N_EOP_ROWS = sizeof(eopKeys)/sizeof(eopKeys[0]);
	
	const char scanKeys[][MAX_DIFX_KEY_LEN] =
	{
		"SCAN %d POINTS",
		"SCAN %d START PT",
		"SCAN %d SRC NAME",
		"SCAN %d REAL NAME",
		"SCAN %d SRC RA",
		"SCAN %d SRC DEC",
		"SCAN %d CALCODE",
		"SCAN %d QUAL"
	};
	const int N_SCAN_ROWS = sizeof(scanKeys)/sizeof(scanKeys[0]);
	
	const char scanKeys2[][MAX_DIFX_KEY_LEN] =
	{
		"SCAN %d REAL NAME %d",
		"SCAN %d ANTLIST %d",
		"SCAN %d SRC RA %d",
		"SCAN %d SRC DEC %d",
		"SCAN %d CALCODE %d",
		"SCAN %d QUAL %d"
	};
	const int N_SCAN2_ROWS = sizeof(scanKeys)/sizeof(scanKeys[0]);

	const char spacecraftKeys[][MAX_DIFX_KEY_LEN] =
	{
		"SPACECRAFT %d NAME",
		"SPACECRAFT %d ROWS"
	};
	const int N_SPACECRAFT_ROWS = 
		sizeof(spacecraftKeys)/sizeof(spacecraftKeys[0]);
	
	int rows[20];
	int a, i, j, k, c, s, N, row, n, p;
	const char *cname;
	const char *str;
	const char *antlist;
	int findconfig = 0;
	float nch;
	double time;
	int nSubScan = 0;
	int nSubarray;
	int nPoint, startPoint;
	double modelInc;
	DifxScan *old_scan;
	int old_nScan;
	int nFound, nTel;

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
	strcpy(D->job->obsCode, DifxParametersvalue(cp, rows[1]));
	D->nEOP          = atoi(DifxParametersvalue(cp, rows[3]));

	if(D->nAntenna == 0)
	{
		fprintf(stderr, "Error: populateCalc: D->nAntenna == 0\n");
		return 0;
	}

	if(D->nScan == 0)
	{
		D->nScan = atoi(DifxParametersvalue(cp, rows[2]));
		D->scan = newDifxScanArray(D->nScan);
	}

	if(D->nScan != atoi(DifxParametersvalue(cp, rows[2])))
	{
		fprintf(stderr, ".calc NUM SCANS = %d; .delay NUM SCANS = %d\n",
			atoi(DifxParametersvalue(cp, rows[2])), D->nScan);
		return 0;
	}

	if(D->nEOP > 0)
	{
		D->eop = newDifxEOPArray(D->nEOP);
	}

	row = DifxParametersfind(cp, 0, "DIFX VERSION");
	if(row > 0)
	{
		strncpy(D->job->difxVersion, DifxParametersvalue(cp, row), 63);
		D->job->difxVersion[63] = 0;
	}

	row = DifxParametersfind(cp, 0, "NUM SUBSCANS");
	if(row >= 0)
	{
		nSubScan = atoi(DifxParametersvalue(cp, row));
	}

	row = DifxParametersfind(cp, 0, "SESSION");
	if(row >= 0)
	{
		strncpy(D->job->obsSession, DifxParametersvalue(cp, row), 7);
		D->job->obsSession[7] = 0;
	}
	row = DifxParametersfind(cp, 0, "TAPER FUNCTION");
	if(row >= 0)
	{
		strncpy(D->job->taperFunction, DifxParametersvalue(cp, row), 7);
		D->job->taperFunction[7] = 0;
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
	row = DifxParametersfind(cp, 0, "INCREMENT (SECS)");
	if(row >= 0)
	{
		modelInc = atof(DifxParametersvalue(cp, row));
		if(D->job->modelInc > 0.0 && D->job->modelInc != modelInc)
		{
			fprintf(stderr, "SEVERE : modelInc disagrees!\n");
			return 0;
		}
		D->job->modelInc = modelInc;
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
	row = DifxParametersfind(cp, 0, "OUTPUT CHANNELS");
	if(row >= 0)
	{
		nch = atof(DifxParametersvalue(cp, row));
		if(nch >= 1)
		{
			D->nOutChan = nch;
		}
		else
		{
			D->nOutChan = D->config[0].nChan*nch/D->specAvg;
		}
	}
	row = DifxParametersfind(cp, 0, "START CHANNEL");
	if(row >= 0)
	{
		nch = atof(DifxParametersvalue(cp, row));
		if(nch >= 1)
		{
			D->startChan = nch;
		}
		else
		{
			D->startChan = D->config[0].nChan*nch;
		}
	}

	row = DifxParametersfind(cp, 0, "NUM TELESCOPES");
	if(row >= 0)
	{
		nTel = atoi(DifxParametersvalue(cp, row));
	}
	else
	{
		fprintf(stderr, "populateCalc: NUM TELESCOPES not defined\n");
		return 0;
	}
	
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
				fprintf(stderr, "Warning -- no antenna axis "
					"offsets available\n");
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
		strcpy(D->antenna[a].mount, DifxParametersvalue(cp, rows[1]));
		if(strcasecmp(D->antenna[a].mount, "azel"))
		{
			strcpy(D->antenna[a].mount, "altz");
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
			strcpy(D->antenna[a].shelf, 
				DifxParametersvalue(cp, row));
		}
	}
	
	if(nFound < D->nAntenna)
	{
		fprintf(stderr, "populateCalc: too few antenna matches\n");
		return 0;
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

	if(nSubScan == 0)
	{
	    k = 0;
	    rows[N_SCAN_ROWS-1] = 0;
	    for(i = 0; i < D->nScan; i++)
	    {
		N = DifxParametersbatchfind1(cp, rows[N_SCAN_ROWS-1], scanKeys, 
			i, N_SCAN_ROWS, rows);
		if(N < N_SCAN_ROWS)
		{
			if(i == 0)
			{
				fprintf(stderr, "Warning -- no scan attributes "
					"available\n");
				findconfig = 1;
				break;
			}
			else
			{
				return 0;
			}
		}
		nPoint               = atoi(DifxParametersvalue(cp, rows[0]));
		startPoint           = atoi(DifxParametersvalue(cp, rows[1]));
		D->scan[i].nPoint    = nPoint;
		D->scan[i].startPoint= startPoint;
		D->scan[i].mjdStart  = D->job->mjdStart + 
			startPoint*D->job->modelInc/86400.0;
		D->scan[i].mjdEnd    = D->job->mjdStart + 
			(startPoint+nPoint)*D->job->modelInc/86400.0;
		strncpy(D->scan[k].name, DifxParametersvalue(cp, rows[3]), 31);
		D->scan[k].name[31] = 0;
		D->scan[k].ra = atof(DifxParametersvalue(cp, rows[4]));
		D->scan[k].dec = atof(DifxParametersvalue(cp, rows[5]));
		strncpy(D->scan[k].calCode, 
			DifxParametersvalue(cp, rows[6]), 3);
		D->scan[k].calCode[3] = 0;
		D->scan[k].qual = atoi(DifxParametersvalue(cp, rows[7]));

		cname = DifxParametersvalue(cp, rows[2]);
		for(c = 0; c < D->nConfig; c++)
		{
			if(strcmp(cname, D->config[c].name) == 0)
			{
				D->scan[k].configId = c;
				break;
			}
		}
		if(c == D->nConfig)
		{
			D->scan[k].configId = -1;
			if(strcmp(cname, "SCAN_GAP") != 0)
			{
				fprintf(stderr, "Warning: ingoring source without "
					"config! id=%d  name=%s  realname=%s\n",
					i, cname, D->scan[k].name);
			}
		}
		k++;
	    }
	    D->nScan = k;
	}
	else
	{
	    old_nScan = D->nScan;
	    old_scan = D->scan;
	    D->nScan = nSubScan;
	    D->scan = newDifxScanArray(D->nScan);
	    k = 0;

	    rows[N_SCAN2_ROWS-1] = 0;
	    for(i = 0; i < old_nScan; i++)
	    {
		row = DifxParametersfind1(cp, rows[N_SCAN2_ROWS-1], 
		    "SCAN %d SUBARRAYS", i);
		if(row < 0)
		{
		    if(i == 0)
		    {
			fprintf(stderr, "Warning -- no scan attributes "
				"available\n");
			findconfig = 1;
			break;
		    }
		    else
		    {
			fprintf(stderr, "SCAN %d SUBARRAYS not found\n", i);
			return 0;
		    }
		}

		row = DifxParametersfind1(cp, row, "SCAN %d SRC NAME", i);
		if(row < 0)
		{
		    fprintf(stderr, "SCAN %d SRC NAME not found\n", i);
		    return 0;
		}
		cname = DifxParametersvalue(cp, row);

		nSubarray = atoi(DifxParametersvalue(cp, row));
		for(j = 0; j < nSubarray; j++)
		{
		    copyDifxScan(D->scan + k, old_scan + i, 0, 0, 0);
		    memcpy(D->scan + k, old_scan + i, sizeof(DifxScan));
		    D->scan[k].model = (DifxModel **)calloc(
			D->scan[k].nAntenna, sizeof(DifxModel *));
	    	    rows[N_SCAN2_ROWS-1] = row;
		    N = DifxParametersbatchfind2(cp, rows[N_SCAN2_ROWS-1], 
			scanKeys2, i, j, N_SCAN2_ROWS, rows);
		    if(N < N_SCAN2_ROWS)
		    {
			fprintf(stderr, "Scan %d subarray %d: data not found\n",
				i, j);
			return 0;
		    }
		    strncpy(D->scan[i].name, DifxParametersvalue(cp, rows[0]), 
			31);
		    D->scan[i].name[31]  = 0;
		    strncpy(D->scan[i].calCode, 
			DifxParametersvalue(cp, rows[2]), 3);
		    D->scan[i].calCode[3]= 0;
		    D->scan[i].qual = atoi(DifxParametersvalue(cp, rows[3]));

		    antlist = DifxParametersvalue(cp, rows[1]);
		    while(sscanf(antlist, "%d%n", &a, &p) > 0)
		    {
			if(a < 0 || a > D->scan[k].nAntenna)
			{
			    fprintf(stderr, "Ant num out of range : %s\n",
				DifxParametersvalue(cp, rows[1]));
			    return 0;
			}
			antlist += p;
			/* move the model column over */
			if(old_scan[i].model[a] == 0)
			{
			    fprintf(stderr, "Ant %d in > 1 subarray : %s\n",
				a, DifxParametersvalue(cp, rows[1]));
			    return 0;
			}

			D->scan[k].model[a] = old_scan[i].model[a];
			old_scan[i].model[a] = 0;
		    }

		    for(c = 0; c < D->nConfig; c++)
		    {
			if(strcmp(cname, D->config[c].name) == 0)
			{
				D->scan[i].configId = c;
				break;
			}
		    }
		    if(c == D->nConfig)
		    {
			fprintf(stderr, "Error -- source without config! "
				"id=%d  name=%s  realname=%s\n",
				i, cname, D->scan[i].name);
			return 0;
		    }
		
		    k++;
		}
	    }

	    deleteDifxScanArray(old_scan, old_nScan);
	}

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
		strcpy(D->spacecraft[s].name, DifxParametersvalue(cp, rows[0]));
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

	if(findconfig)
	{
		for(i = 0; i < D->nScan; i++)
		{
			for(c = 0; c < D->nConfig; c++)
			{
				if(strcmp(D->scan[i].name,
					D->config[c].name) == 0)
				{
					D->scan[i].configId = c;
					break;
				}
			}
			if(c == D->nConfig) for(c = 0; c < D->nConfig; c++)
			{
				if(strcmp("DEFAULT", D->config[c].name) == 0)
				{
					D->scan[i].configId = c;
					break;
				}
			}
			if(c == D->nConfig)
			{
				fprintf(stderr, "Error -- source without "
					"config! id=%d  name=%s\n",
					i, D->scan[i].name);
				return 0;
			}
		}
	}


	return D;
}

static void estimateRate(DifxInput *D)
{
	int s, a, p;
	double f;

	if(!D)
	{
		return;
	}

	f = 0.5/D->job->modelInc;
	for(s = 0; s < D->nScan; s++)
	{
		for(a = 0; a < D->nAntenna; a++)
		{
			for(p = 0; p <= D->scan[s].nPoint; p++)
			{
				D->scan[s].model[a][p].dt = 
					f*(D->scan[s].model[a][p+1].t - 
					   D->scan[s].model[a][p-1].t);
			}
		}
	}
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
		strncpy(D->job->calcServer, DifxParametersvalue(p, r), 32);
		D->job->calcServer[31] = 0;
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

int parsePoly1(DifxParameters *p, int r, char *key, int i1,
	double *array, int n)
{
	const char *v;
	int i, l, m;
	double d;

	if(r < 0)
	{
		return -1;
	}

	r = DifxParametersfind1(p, r, key, i1);
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
	int a, t, p, r, s, nScan, nTel;
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
		scan->im = newDifxPolyModelArray(scan->nAntenna, scan->nPoly);

		for(p = 0; p < scan->nPoly; p++)
		{
			r = DifxParametersfind2(mp, r, "SCAN %d POLY %d MJD",
				s, p);
			if(r < 0)
			{
				free(antMap);
				return 0;
			}
			mjd = atoi(DifxParametersvalue(mp, r));
			r = DifxParametersfind2(mp, r, "SCAN %d POLY %d SEC",
				s, p);
			if(r < 0)
			{
				free(antMap);
				return 0;
			}
			sec = atoi(DifxParametersvalue(mp, r));

			for(t = 0; t < nTel; t++)
			{
				a = antMap[t];
				if(a < 0)
				{
					continue;
				}
				scan->im[a][p].mjd = mjd;
				scan->im[a][p].sec = sec;
				scan->im[a][p].order = order;
				scan->im[a][p].validDuration = interval;
				r = parsePoly1(mp, r, "ANT %d DELAY (us)", t,
					scan->im[a][p].delay, order+1);
				r = parsePoly1(mp, r, "ANT %d DRY (us)", t,
					scan->im[a][p].dry, order+1);
				r = parsePoly1(mp, r, "ANT %d WET (us)", t,
					scan->im[a][p].wet, order+1);
				r = parsePoly1(mp, r, "ANT %d U (m)", t,
					scan->im[a][p].u, order+1);
				r = parsePoly1(mp, r, "ANT %d V (m)", t,
					scan->im[a][p].v, order+1);
				r = parsePoly1(mp, r, "ANT %d W (m)", t,
					scan->im[a][p].w, order+1);
				if(r < 0)
				{
					return 0;
				}
			}
			//printf("XX %e %e %e %e\n", scan->im[0][p].delay[0], scan->im[0][p].delay[1], scan->im[0][p].delay[2], scan->im[a][p].delay[3]);
		}
	}

	free(antMap);

	return D;
}

static DifxInput *populateRate(DifxInput *D, DifxParameters *rp)
{
	int a, p, r = 0, s, v;
	double f;
	int nTel;
	int *antMap;
	
	D = parseCalcServerInfo(D, rp);

	if(!D)
	{
		return 0;
	}

	antMap = deriveAntMap(D, rp, &nTel);
	if(antMap == 0)
	{
		fprintf(stderr, "populateRate: deriveAntMap failed\n");
		return 0;
	}

	for(s = 0; s < D->nScan; s++)
	{
		for(p = -1; p <= D->scan[s].nPoint+1; p++)
		{
			r = DifxParametersfind1(rp, r+1, "RELATIVE INC %d", p);
			if(r < 0)
			{
				fprintf(stderr, "Rate row not found : %d %d\n",
					s, p);
				free(antMap);
				return 0;
			}
			v = parseRates(D->scan[s].model, nTel, antMap, p,
				DifxParametersvalue(rp, r));
			if(v < 0)
			{
				fprintf(stderr, "Rate parse error\n");
				free(antMap);
				return 0;
			}
		}

		/* compute atm rates based on atm delays */
		f = 0.5/D->job->modelInc;
		for(a = 0; a < D->nAntenna; a++)
		{
			for(p = 0; p < D->scan[s].nPoint+1; p++)
			{
				D->scan[s].model[a][p].ddry = 
				       f*( D->scan[s].model[a][p+1].dry -
					   D->scan[s].model[a][p-1].dry);
				D->scan[s].model[a][p].dwet = 
				       f*( D->scan[s].model[a][p+1].wet -
					   D->scan[s].model[a][p-1].wet);
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
	int i, n=0, a, p;

	in = fopen(flagfile, "r");
	if(!in)
	{
		return 0;
	}

	p = fscanf(in, "%d", &n);
	if(p == 1 && n > 0 && n < 10000)
	{
		D->nFlag = n;
		D->flag = newDifxAntennaFlagArray(D->nFlag);
		for(i = 0; i < n; i++)
		{
			p = fscanf(in, "%lf%lf%d", &mjd1, &mjd2, &a);
			if(p == 3)
			{
				D->flag[i].mjd1  = mjd1;
				D->flag[i].mjd2  = mjd2;
				D->flag[i].antennaId = a;
			}
		}
	}
	else if(n > 0)
	{
		fprintf(stderr, "populateFlags : unreasonable "
			"number of flags : %d\n", n);
	}

	fclose(in);

	return n;
}

/* take DifxInput structure and derive the source table.
 */
DifxInput *deriveSourceTable(DifxInput *D)
{
	int i, n=0, s, sc;

	if(!D)
	{
		return 0;
	}

	if(D->nScan < 1 || D->scan == 0)
	{
		fprintf(stderr, "No scans to work with!\n");
		return 0;
	}

	/* for now be wasteful and allocate enough memory for each
	 * scan to be its own source 
	 */
	D->source = newDifxSourceArray(D->nScan);
	D->nSource = 0;

	for(s = 0; s < D->nScan; s++)
	{
		for(i = 0; i < n; i++)
		{
			if(D->source[i].ra       == D->scan[s].ra  &&
			   D->source[i].dec      == D->scan[s].dec &&
			   D->source[i].qual     == D->scan[s].qual &&
			   D->source[i].configId == D->scan[s].configId &&
			   strcmp(D->source[i].calCode, D->scan[s].calCode) 
			   	== 0 &&
			   strcmp(D->source[i].name, D->scan[s].name) == 0)
			{
				break;
			}
		}
		
		if(i >= n)
		{
			strcpy(D->source[n].name, D->scan[s].name);
			strcpy(D->source[i].calCode, D->scan[s].calCode);
			D->source[n].ra       = D->scan[s].ra;
			D->source[n].dec      = D->scan[s].dec;
			D->source[n].qual     = D->scan[s].qual;
			D->source[i].configId = D->scan[s].configId;
			n++;
		}

		D->scan[s].sourceId = i;
	}

	D->nSource = n;

	/* Look for spacecraft */
	if(D->nSpacecraft > 0 && D->nSource > 0)
	{
		for(s = 0; s < D->nSource; s++)
		{
			for(sc = 0; sc < D->nSpacecraft; sc++)
			{
				if(strcmp(D->spacecraft[sc].name,
				          D->source[s].name) == 0)
				{
					D->source[s].spacecraftId = sc;
					break;
				}
			}
		}
	}

	return D;
}

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
	int a, i, j, match, n=0, ci, cj;
	int *fs;

	if(!D)
	{
		return 0;
	}

	if(D->nSource < 1 || D->source == 0)
	{
		fprintf(stderr, "No sources to work with!\n");
		return 0;
	}

	fs = (int *)calloc(D->nSource, sizeof(int));

	for(i = 0; i < D->nSource; i++)
	{
		ci = D->source[i].configId;
		if(ci < 0)
		{
			D->source[i].fitsSourceId = -1;
			continue;
		}
		match = -1;
		if(n > 0) for(a = 0; a < n; a++)
		{
			j = fs[a];
			cj = D->source[j].configId;
			if(D->source[i].ra       == D->source[j].ra  &&
			   D->source[i].dec      == D->source[j].dec &&
			   D->source[i].qual     == D->source[j].qual &&
			   D->config[ci].freqId  == D->config[cj].freqId &&
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
			D->source[i].fitsSourceId = n;
			fs[n] = i;
			n++;
		}
		else
		{
			D->source[i].fitsSourceId = match;
		}
	}

	free(fs);
	
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
	
	D->config[0].freqId = nFQ;
	nFQ++;
	
	if(D->nConfig < 2)
	{
		return 0;
	}

	for(c = 1; c < D->nConfig; c++)
	{
		D->config[c].freqId = -1;
		for(d = 0; d < c; d++)
		{
			if(sameFQ(&(D->config[c]), &(D->config[d])))
			{
				D->config[c].freqId = D->config[d].freqId;
				d = c; /* terminate inner loop */
			}
		}
		if(D->config[c].freqId == -1)
		{
			D->config[c].freqId = nFQ;
			nFQ++;
		}
	}

	return nFQ;
}

DifxInput *updateDifxInput(DifxInput *D)
{
	D = deriveDifxInputValues(D);
	D = deriveSourceTable(D);
	D = deriveFitsSourceIds(D);
	//D = setFitsSourceIds(D);
	setGlobalValues(D);
	calcFreqIds(D);
	setOrbitingAntennas(D);
	
	return D;
}

DifxInput *loadDifxInput(const char *filePrefix)
{
	DifxParameters *ip, *up, *dp, *rp, *cp, *mp;
	DifxInput *D, *DSave;
	char inputFile[256];
	char uvwFile[256];
	char delayFile[256];
	char rateFile[256];
	char calcFile[256];
	char flagFile[256];
	char modelFile[256];
	int c;

	sprintf(inputFile, "%s.input", filePrefix);
	sprintf(uvwFile,   "%s.uvw",   filePrefix);
	sprintf(delayFile, "%s.delay", filePrefix);
	sprintf(rateFile,  "%s.rate",  filePrefix);
	sprintf(calcFile,  "%s.calc",  filePrefix);
	sprintf(flagFile,  "%s.flag",  filePrefix);
	sprintf(modelFile, "%s.im",   filePrefix);

	ip = newDifxParametersfromfile(inputFile);
	up = newDifxParametersfromfile(uvwFile);
	dp = newDifxParametersfromfile(delayFile);
	rp = newDifxParametersfromfile(rateFile);
	cp = newDifxParametersfromfile(calcFile);
	mp = newDifxParametersfromfile(modelFile);

	if(!ip || !up || !dp)
	{
		deleteDifxParameters(ip);
		deleteDifxParameters(up);
		deleteDifxParameters(dp);
		deleteDifxParameters(rp);
		deleteDifxParameters(cp);
		
		return 0;
	}

	D = DSave = newDifxInput();

	/* When creating a DifxInput via this function, there will always
	 * be a single DifxJob
	 */
	D->job = newDifxJobArray(1);
	D->nJob = 1;
	strcpy(D->job->fileBase, filePrefix);
	D = populateInput(D, ip);
	D = populateUVW(D, up);
	D = populateDelay(D, dp);
	if(cp)
	{
		D = populateCalc(D, cp);
	}
	else
	{	
		fprintf(stderr, "Warning -- no file called %s found.  Continuing anyways\n",
			calcFile);
		fprintf(stderr, "  Defaults being used for many parameters\n");
	}
	if(rp)
	{
		D = populateRate(D, rp);
	}
	else
	{
		fprintf(stderr, "Warning -- no file called %s found.  "
				"Continuing anyways\n", rateFile);
		fprintf(stderr, "  Model rates will be approximate\n");
		fprintf(stderr, "  Atmosphere values will be absent\n");

		estimateRate(D);
	}
	if(mp)
	{
		D = populateIM(D, mp);
	}

	if(!D)
	{
		deleteDifxInput(DSave);
	}
	
	deleteDifxParameters(ip);
	deleteDifxParameters(up);
	deleteDifxParameters(dp);
	deleteDifxParameters(cp);
	deleteDifxParameters(mp);

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
	char inputFile[256];
	char calcFile[256];
	int c;

	sprintf(inputFile, "%s.input", filePrefix);
	sprintf(calcFile,  "%s.calc",  filePrefix);

	ip = newDifxParametersfromfile(inputFile);
	cp = newDifxParametersfromfile(calcFile);

	if(!ip)
	{
		deleteDifxParameters(ip);
		deleteDifxParameters(cp);
		
		return 0;
	}

	D = DSave = newDifxInput();

	/* When creating a DifxInput via this function, there will always
	 * be a single DifxJob
	 */
	D->job = newDifxJobArray(1);
	D->nJob = 1;
	strcpy(D->job->fileBase, filePrefix);
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
		if(mjd < D->scan[scanId].mjdEnd && 
		   D->scan[scanId].jobId == jobId)
		{
			return scanId;
		}
	}

	return -1;
}

/* return -1 if no suitable scan found */
int DifxInputGetScanIdByAntennaId(const DifxInput *D, double mjd, 
	int antennaId)
{
	int scanId;
	int hasModel;

	if(!D)
	{
		return -1;
	}

	for(scanId = 0; scanId < D->nScan; scanId++)
	{
		/* see if this scan contains the antenna.  If so, one of
		 * the model tables should exist */
		if(D->scan[scanId].nAntenna <= antennaId)
		{
			continue;
		}
		hasModel = 0;
		if(D->scan[scanId].model)
		{
			if(D->scan[scanId].model[antennaId])
			{
				hasModel = 1;
			}
		}
		if(D->scan[scanId].im)
		{
			if(D->scan[scanId].im[antennaId])
			{
				hasModel = 1;
			}
		}
		if(!hasModel)
		{
			continue;
		}
		
		if(mjd <  D->scan[scanId].mjdEnd   &&
		   mjd >= D->scan[scanId].mjdStart)
		{
			return scanId;
		}
	}

	return -1;
}

int DifxInputGetSourceIdByJobId(const DifxInput *D, double mjd, int jobId)
{
	int scanId;

	scanId = DifxInputGetScanIdByJobId(D, mjd, jobId);
	if(scanId < 0 || scanId >= D->nScan)
	{
		return -1;
	}
	else
	{
		return D->scan[scanId].sourceId;
	}
}

int DifxInputGetSourceIdByAntennaId(const DifxInput *D, double mjd, 
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
		return D->scan[scanId].sourceId;
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
	int a, a2, d, f, s;
	int *old2new;
	int changed = 0;
	DifxModel **m2;
	DifxPolyModel **p2;

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

	/* 1. Datastream table -- must be working */
	for(d = 0; d < D->nDatastream; d++)
	{
		D->datastream[d].antennaId =
			old2new[D->datastream[d].antennaId];
	}

	/* 2. Flags -- tested and confirmed to work */
	for(f = 0; f < D->nFlag; f++)
	{
		D->flag[f].antennaId = old2new[D->flag[f].antennaId];
	}

	/* 3. The model tables for each scah */
	for(s = 0; s < D->nScan; s++)
	{
		/* correct the tabulated model table */
		if(D->scan[s].model)
		{
			m2 = (DifxModel **)calloc(D->nAntenna, sizeof(DifxModel *));

			for(a = 0; a < D->scan[s].nAntenna; a++)
			{
				a2 = old2new[a];
				if(a2 < 0 || a2 >= D->nAntenna)
				{
					fprintf(stderr, "Error: DifxInputSortAntennas: "
						"old2new[%d] = %d; nAnt = %d\n",
						a, a2, D->scan[s].nAntenna);
					continue;
				}
				m2[a2] = D->scan[s].model[a];
			}

			free(D->scan[s].model);
			D->scan[s].model = m2;
		}

		/* correct the polynomial model table */
		if(D->scan[s].model)
		{
			p2 = (DifxPolyModel **)calloc(D->nAntenna, sizeof(DifxPolyModel *));

			for(a = 0; a < D->scan[s].nAntenna; a++)
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
	int a, c, d, n, mjd;
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
		if(dd->nRecChan < 1)
		{
			fprintf(stderr, "Error: datastream %d nRecChan=%d\n", d, dd->nRecChan);
			continue;
		}
		
		nBitstream = dd->dataFrameSize/20000;
		nBitstream *= 8;
		fanout = nBitstream/(dd->quantBits*dd->nRecChan);
		sampRate = (int)(D->freq[dd->freqId[0]].bw*2.0+0.00001);
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

	speedUp = 4;

	/* the quantum of integration time */
	quantum = 0.131072*speedUp;

	for(c = 0; c < D->nConfig; c++)
	{
		dc = D->config + c;
		n = (int)(dc->tInt/quantum + 0.5);
		dc->tInt = n * quantum;
	}

	/* here use the first config's tInt to derive the start time of 
	 * the job
	 */
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

	/* The reference for the shift is the start of the .calc file which
	 * starts at a the truncated second
	 */
	deltasec = (int)deltasec;

	printf("                  delayed clock reference by %8.6f seconds\n",
		deltasec);

	for(a = 0; a < D->nAntenna; a++)
	{
		D->antenna[a].delay += deltasec*D->antenna[a].rate;
		printf("  Antenna %s clock shift = %e us = %f deg at 8.4 GHz\n",
			D->antenna[a].name, deltasec*D->antenna[a].rate,
			deltasec*D->antenna[a].rate*8400.0*360.0);
	}

	/* FIXME -- reset BLOCKSPERSEND here? */

	return 0;
}

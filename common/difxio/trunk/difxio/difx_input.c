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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "difxio/difx_input.h"
#include "difxio/parsedifx.h"

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

void printDifxIF(const DifxIF *di)
{
	printf("    Difx IF : %p\n", di);
	printf("      Freq = %f MHz\n", di->freq);
	printf("      Bandwidth = %f MHz\n", di->bw);
	printf("      Sideband = %c\n", di->sideband);
	if(di->nPol == 1)
	{
		printf("      Pol = %c\n", di->pol[0]);
	}
	else if(di->nPol == 2)
	{
		printf("      Pols = %c, %c\n", di->pol[0], di->pol[1]);
	}
	else
	{
		printf("      nPol = %d\n", di->nPol);
	}
}


DifxConfig *newDifxConfigArray(int nConfig)
{
	DifxConfig *dc;
	int c;

	dc = (DifxConfig *)calloc(nConfig, sizeof(DifxConfig));
	for(c = 0; c < nConfig; c++)
	{
		dc[c].doPolar = -1;
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
		free(dc);
	}
}

void printDifxConfig(const DifxConfig *dc)
{
	int i;
	printf("  Difx Config [%s] : %p\n", dc->name, dc);
	printf("    tInt  = %f sec\n", dc->tInt);
	printf("    nChan = %d\n", dc->nChan);
	printf("    postFFringe = %d\n", dc->postFFringe);
	printf("    quadDelayInterp = %d\n", dc->quadDelayInterp);
	printf("    pulsarBinning = %d\n", dc->pulsarBinning);
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
}


DifxDSEntry *newDifxDSEntryArray(int nDSEntry)
{
	DifxDSEntry *ds;

	ds = (DifxDSEntry *)calloc(nDSEntry, sizeof(DifxDSEntry));

	return ds;
}

void deleteDifxDSEntryArray(DifxDSEntry *ds, int nDSEntry)
{
	int e;

	if(ds)
	{
		for(e = 0; e < nDSEntry; e++)
		{
			if(ds[e].nPol)
			{
				free(ds[e].nPol);
			}
			if(ds[e].freqId)
			{
				free(ds[e].freqId);
			}
			if(ds[e].clockOffset)
			{
				free(ds[e].clockOffset);
			}
			if(ds[e].RCfreqId)
			{
				free(ds[e].RCfreqId);
			}
			if(ds[e].RCpolName)
			{
				free(ds[e].RCpolName);
			}
		}
		free(ds);
	}
}

void printDifxDSEntry(const DifxDSEntry *ds)
{
	int f;
	printf("  Difx Datastream Entry[antId=%d] : %p\n", ds->antId, ds);
	printf("    format = %s\n", ds->dataFormat);
	printf("    quantization bits = %d\n", ds->quantBits);
	printf("    nFreq = %d\n", ds->nFreq);
	printf("    nRecChan = %d\n", ds->nRecChan);
	printf("    (freqId, nPol)[freq] =");
	for(f = 0; f < ds->nFreq; f++)
	{
		printf(" (%d, %d)", ds->freqId[f], ds->nPol[f]);
	}
	printf("\n");
	printf("    (freqId, pol)[recchan] =");
	for(f = 0; f < ds->nRecChan; f++)
	{
		printf(" (%d, %c)", ds->RCfreqId[f], ds->RCpolName[f]);
	}
	printf("\n");
}


DifxBaseline *newDifxBaselineArray(int nBaseline)
{
	DifxBaseline *db;

	db = (DifxBaseline *)calloc(nBaseline, sizeof(DifxBaseline));

	return db;
}

void deleteDifxBaselineArray(DifxBaseline *db, int nBaseline)
{
	int b, f;
	
	if(db)
	{
		for(b = 0; b < nBaseline; b++)
		{
			if(db[b].nPolProd)
			{
				free(db[b].nPolProd);
			}
			if(db[b].recChanA)
			{
				for(f = 0; f < db[b].nFreq; f++)
				{
					if(db[b].recChanA[f])
					{
						free(db[b].recChanA[f]);
					}
				}
				free(db[b].recChanA);
			}
			if(db[b].recChanB)
			{
				for(f = 0; f < db[b].nFreq; f++)
				{
					if(db[b].recChanB[f])
					{
						free(db[b].recChanB[f]);
					}
				}
				free(db[b].recChanB);
			}
		}
		free(db);
	}
}

void printDifxBaseline(const DifxBaseline *db)
{
	int f;
	
	printf("  Difx Baseline : %p\n", db);
	printf("    datastream indices = %d %d\n", db->dsA, db->dsB);
	printf("    nFreq = %d\n", db->nFreq);
	if(db->nPolProd)
	{
		printf("    nPolProd[freq] =");
		for(f = 0; f < db->nFreq; f++)
		{
			printf(" %d", db->nPolProd[f]);
		}
		printf("\n");
	}
}


DifxFreq *newDifxFreqArray(int nFreq)
{
	DifxFreq *df;

	df = (DifxFreq *)calloc(nFreq, sizeof(DifxFreq));

	return df;
}

void deleteDifxFreqArray(DifxFreq *df)
{
	if(df)
	{
		free(df);
	}
}

void printDifxFreq(const DifxFreq *df)
{
	printf("  Difx Freq : %p\n", df);
	printf("    Freq = %f MHz\n", df->freq);
	printf("    Bandwidth = %f MHz\n", df->bw);
	printf("    Sideband = %c\n", df->sideband);
}


DifxAntenna *newDifxAntennaArray(int nAntenna)
{
	DifxAntenna* da;

	da = (DifxAntenna *)calloc(nAntenna, sizeof(DifxAntenna));
	
	return da;
}

void deleteDifxAntennaArray(DifxAntenna *da)
{
	if(da)
	{
		free(da);
	}
}

void printDifxAntenna(const DifxAntenna *da)
{
	printf("  DifxAntenna [%s] : %p\n", da->name, da);
	printf("    Delay = %f us\n", da->delay);
	printf("    Rate = %e us/s\n", da->rate);
	printf("    Mount = %s\n", da->mount);
	printf("    Offset = %f, %f, %f m\n", 
		da->offset[0], da->offset[1], da->offset[2]);
	printf("    X, Y, Z = %f, %f, %f m\n", da->X, da->Y, da->Z);
	printf("    VSN = %s\n", da->vsn);
}


DifxSource *newDifxSourceArray(int nSource)
{
	DifxSource *ds;
	int s;

	ds = (DifxSource *)calloc(nSource, sizeof(DifxSource));
	for(s = 0; s < nSource; s++)
	{
		ds[s].configId = -1;
	}
	
	return ds;
}

void deleteDifxSourceArray(DifxSource *ds)
{
	if(ds)
	{
		free(ds);
	}
}

void printDifxSource(const DifxSource *ds)
{
	printf("  DifxSource [%s] : %p\n", ds->name, ds);
	printf("    RA  =  %10.7f\n", ds->ra);
	printf("    Dec = %+11.7f\n", ds->dec);
	printf("    Calcode = %s\n", ds->calcode);
	printf("    Qualifier = %d\n", ds->qual);
	printf("    ConfigId = %d\n", ds->configId);
}


DifxModel **newDifxModelArray(int nAntenna, int nPoint)
{
	DifxModel **dm;
	int a, N;

	dm = (DifxModel **)malloc(nAntenna*sizeof(DifxModel *));

	N = nPoint + 3;

	for(a = 0; a < nAntenna; a++)
	{
		dm[a] = (DifxModel *)calloc(N, sizeof(DifxModel));

		/* offset the array so we can index -1 */
		dm[a]++;
	}

	return dm;
}

void deleteDifxModelArray(DifxModel **dm, int nAntenna)
{
	int a;
	
	if(dm)
	{
		for(a = 0; a < nAntenna; a++)
		{
			/* free memory from actual start of array */
			dm[a]--;

			free(dm[a]);
		}
		free(dm);
	}
}

void printDifxModel(const DifxModel *dm)
{
	printf("    DifxModel : %p\n", dm);
	printf("      U, V, W = %f %f %f m\n", dm->u, dm->v, dm->w);
	printf("      Delay   = %f us\n", dm->t);
}


DifxScan *newDifxScanArray(int nScan)
{
	DifxScan *ds;
	int s;

	ds = (DifxScan *)calloc(nScan, sizeof(DifxScan));
	for(s = 0; s < nScan; s++)
	{
		ds[s].configId = -1;
		ds[s].sourceId = -1;
	}
	
	return ds;
}

void deleteDifxScanArray(DifxScan *ds, int nScan)
{
	int s;
	if(ds)
	{
		for(s = 0; s < nScan; s++)
		{
			if(ds[s].model)
			{
				deleteDifxModelArray(ds[s].model, 
					ds[s].nAntenna);
			}
		}
		free(ds);
	}
}

void printDifxScan(const DifxScan *ds)
{
	printf("  DifxScan [%s] : %p\n", ds->name, ds);
	printf("    Start = MJD %12.6f\n", ds->mjdStart);
	printf("    End   = MJD %12.6f\n", ds->mjdEnd);
	printf("    Calcode = %s\n", ds->calcode);
	printf("    Qualifier = %d\n", ds->qual);
	printf("    nPoint = %d\n", ds->nPoint);
	printf("    nAntenna %d\n", ds->nAntenna);
	printf("    Source ID = %d\n", ds->sourceId);
	printf("    Config ID = %d\n", ds->configId);
	if(ds->nPoint > 1 && ds->nAntenna > 1)
	{
		printDifxModel(ds->model[0] - 1);
		printDifxModel(ds->model[0]);
		printDifxModel(ds->model[1] - 1);
		printDifxModel(ds->model[1]);
	}
}


DifxEOP *newDifxEOPArray(int nEOP)
{
	DifxEOP *de;

	de = (DifxEOP *)calloc(nEOP, sizeof(DifxEOP));

	return de;
}

void deleteDifxEOPArray(DifxEOP *de)
{
	if(de)
	{
		free(de);
	}
}

void printDifxEOP(const DifxEOP *de)
{
	printf("  DifxEOP [%d] : %p\n", (int)(de->mjd + 0.5), de);
	printf("    TAI - UTC = %d sec\n", de->tai_utc);
	printf("    UT1 - UTC = %9.6f sec\n", de->ut1_utc);
	printf("    Pole X, Y = %8.6f, %8.6f arcsec\n", de->xPole, de->yPole);
}


/* allocate empty structure, do minimal initialization */
DifxInput *newDifxInput()
{
	DifxInput *D;

	D = (DifxInput *)calloc(1, sizeof(DifxInput));
	D->specAvg = 1;
	strcpy(D->obsCode, "DIFX");
	strcpy(D->taperFunction, "UNIFORM");

	return D;
}

void deleteDifxInput(DifxInput *D)
{
	if(D)
	{
		if(D->config)
		{
			deleteDifxConfigArray(D->config);
		}
		if(D->dsentry)
		{
			deleteDifxDSEntryArray(D->dsentry, D->nDSEntry);
		}
		if(D->freq)
		{
			deleteDifxFreqArray(D->freq);
		}
		if(D->antenna)
		{
			deleteDifxAntennaArray(D->antenna);
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
			free(D->flag);
		}
		free(D);
	}
}

void printDifxInput(const DifxInput *D)
{
	int i;

	printf("DifxInput : %p\n", D);
	if(!D)
	{
		return;
	}
	printf("  Job ID = %d\n", D->jobId);
	printf("  Project = %s\n", D->obsCode);
	if(D->obsSession[0])
	{
		printf("  Session = %s\n", D->obsSession);
	}
	printf("  Start = MJD %12.6f\n", D->mjdStart);
	printf("  Duration = %f sec\n", D->duration);
	printf("  Model Inc = %f sec\n", D->modelInc);
	printf("  Ref. Freq. = %f MHz\n", D->refFreq);

	printf("  nConfig = %d\n", D->nConfig);
	for(i = 0; i < D->nConfig; i++)
	{
		printDifxConfig(D->config + i);
	}

	printf("  nFreq = %d\n", D->nFreq);
	for(i = 0; i < D->nFreq; i++)
	{
		printDifxFreq(D->freq + i);
	}

	printf("  nAntenna = %d\n", D->nAntenna);
	for(i = 0; i < D->nAntenna; i++)
	{
		printDifxAntenna(D->antenna + i);
	}

	printf("  nSource = %d\n", D->nSource);
	for(i = 0; i < D->nSource; i++)
	{
		printDifxSource(D->source + i);
	}

	printf("  nScan = %d\n", D->nScan);
	for(i = 0; i < D->nScan; i++)
	{
		printDifxScan(D->scan + i);
	}

	printf("  nEOP = %d\n", D->nEOP);
	for(i = 0; i < D->nEOP; i++)
	{
		printDifxEOP(D->eop + i);
	}

	printf("  nDataStreamEntries = %d\n", D->nDSEntry);
	for(i = 0; i < D->nDSEntry; i++)
	{
		printDifxDSEntry(D->dsentry + i);
	}

	if(D->nBaseline > 1)
	{
		printDifxBaseline(D->baseline + 0);
		printDifxBaseline(D->baseline + 1);
	}
	
	printf("\n");
}


static int parseUVWs(DifxModel **model, int nAntenna, int row, 
	const char *str)
{
	int a;
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
		model[a][row].u = u;
		model[a][row].v = v;
		model[a][row].w = w;
	}

	return 0;
}

static int parseDelays(DifxModel **model, int nAntenna, int row, 
	const char *str)
{
	int a;
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
		model[a][row].t = t;
	}
	
	return a;
}

static int parseRates(DifxModel **model, int nAntenna, int row, 
	const char *str)
{
	int a;
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
		model[a][row].dt = rate;
		model[a][row].a  = dry + wet;	/* add wet and dry */
	}
	
	return a;
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
	DifxDSEntry *ds;
	int *freqIds;
	int p, a, f, c, i;
	int maxFreqId=0;
	int nPol = 0;
	int haspol[4] = {0, 0, 0, 0};	/* indices are: 0-R, 1-L, 2-X, 3-Y */

	dc = D->config + configId;
	dc->nIF = 0;

	freqIds = (int *)malloc(sizeof(int)*D->nFreq);

	/* go through datastreams associates with this config and collect all
	 * distinct Freq table ids
	 */
	for(a = 0; dc->indexDS[a] >= 0; a++)
	{
		ds = D->dsentry + dc->indexDS[a];
		for(f = 0; f < ds->nFreq; f++)
		{
			dc->nIF = addtolist(freqIds, ds->freqId[f], dc->nIF);
			if(ds->freqId[f] > maxFreqId)
			{
				maxFreqId = ds->freqId[f];
			}
		}
	}
	dc->freqId2IF = (int *)calloc((maxFreqId+2), sizeof(int));
	dc->freqId2IF[maxFreqId+1] = -1;

	/* determine which polarizations are present.  All IFs in this 
	 * config will maintain slots for all polariazations, even if
	 * this ends up making wasted space in FITS files
	 */
	for(a = 0; dc->indexDS[a] >= 0; a++)
	{
		ds = D->dsentry + dc->indexDS[a];
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
					fprintf(stderr, "Unknown pol %c\n",
						ds->RCpolName[c]);
					return -1;
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

	return 0;
}

static DifxInput *populateInput(DifxInput *D, const DifxParameters *ip)
{
	const char initKeys[][MAX_DIFX_KEY_LEN] =
	{
		"EXECUTE TIME (SEC)",
		"START MJD",
		"START SECONDS",
		"ACTIVE DATASTREAMS",
		"ACTIVE BASELINES",
		"NUM CONFIGURATIONS",
		"FREQ ENTRIES",
		"TELESCOPE ENTRIES"
	};
	const int N_INIT_ROWS = sizeof(initKeys)/sizeof(initKeys[0]);
	
	const char configKeys[][MAX_DIFX_KEY_LEN] =
	{
		"CONFIG SOURCE",
		"INT TIME (SEC)",
		"NUM CHANNELS",
		"POST-F FRINGE ROT",
		"QUAD DELAY INTERP",
		"PULSAR BINNING"
	};
	const int N_CONFIG_ROWS = sizeof(configKeys)/sizeof(configKeys[0]);
	
	const char freqKeys[][MAX_DIFX_KEY_LEN] =
	{
		"FREQ (MHZ) %d",
		"BW (MHZ) %d",
		"SIDEBAND %d"
	};
	const int N_FREQ_ROWS = sizeof(freqKeys)/sizeof(freqKeys[0]);
	
	const char antKeys[][MAX_DIFX_KEY_LEN] =
	{
		"TELESCOPE NAME %d",
		"CLOCK DELAY (us) %d",
		"CLOCK RATE(us/s) %d"
	};
	const int N_ANT_ROWS = sizeof(antKeys)/sizeof(antKeys[0]);

	const char vsnKeys[][MAX_DIFX_KEY_LEN] = 
	{
		"FILE %d/0"
	};
	const int N_VSN_ROWS = sizeof(vsnKeys)/sizeof(vsnKeys[0]);
	
	int rows[20];	/* must be long enough for biggest of above */
	int e, a, f, p, i, l, b, c, r=0, N;
	int qb = 0;
	int nRecChan;
	DifxDSEntry *ds;
	
	if(!D)
	{
		fprintf(stderr, "populateInput: D = 0\n");
		return 0;
	}

	/* COMMON SETTINGS */
	N = DifxParametersbatchfind(ip, 0, initKeys, N_INIT_ROWS, rows);
	if(N < N_INIT_ROWS)
	{
		fprintf(stderr, "populateInput: N < N_INIT_ROWS %d < %d\n",
			N, N_INIT_ROWS);
		return 0;
	}

	/* Initialize some of the structures */
	D->duration = atoi(DifxParametersvalue(ip, rows[0]));
	D->mjdStart = atoi(DifxParametersvalue(ip, rows[1])) +
		      atof(DifxParametersvalue(ip, rows[2]))/86400.0;
	D->activeDatastreams =
		      atoi(DifxParametersvalue(ip, rows[3]));
	D->activeBaselines =
		      atoi(DifxParametersvalue(ip, rows[4]));

	D->nConfig  = atoi(DifxParametersvalue(ip, rows[5]));
	D->nFreq    = atoi(DifxParametersvalue(ip, rows[6]));
	D->nAntenna = atoi(DifxParametersvalue(ip, rows[7]));

	D->config   = newDifxConfigArray(D->nConfig);
	D->freq     = newDifxFreqArray(D->nFreq);
	D->antenna  = newDifxAntennaArray(D->nAntenna);

	/* CONFIGURATIONS */
	rows[N_CONFIG_ROWS-1] = 0;	/* initialize start */
	for(c = 0; c < D->nConfig; c++)
	{
		N = DifxParametersbatchfind(ip, rows[N_CONFIG_ROWS-1], 
			configKeys, N_CONFIG_ROWS, rows);
		if(N < N_CONFIG_ROWS)
		{
			fprintf(stderr, "populateInput: N < N_CONFIG_ROWS %d "
				"< %d\n", N, N_CONFIG_ROWS);
			return 0;
		}
		strcpy(D->config[c].name,   DifxParametersvalue(ip, rows[0]));
		D->config[c].tInt  =   atof(DifxParametersvalue(ip, rows[1]));
		D->config[c].nChan =   atoi(DifxParametersvalue(ip, rows[2]));
		D->config[c].postFFringe = 
			abs(strcmp("FALSE", DifxParametersvalue(ip, rows[3])));
		D->config[c].quadDelayInterp = 
			abs(strcmp("FALSE", DifxParametersvalue(ip, rows[4])));
		D->config[c].pulsarBinning = 
			abs(strcmp("FALSE", DifxParametersvalue(ip, rows[5])));
		N = strlen(D->config[c].name);
		if(D->config[c].name[N-1] == '1')
		{
			D->config[c].doPolar = 1;
		}
		else
		{
			D->config[c].doPolar = 0;
		}

		/* initialize datastream index array */
		D->config[c].indexDS = 
			(int *)malloc(sizeof(int)*(D->activeDatastreams + 1));
		for(a = 0; a <= D->activeDatastreams; a++)
		{
			D->config[c].indexDS[a] = -1;
		}

		/* populate datastream index array */
		for(a = 0; a < D->activeDatastreams; a++)
		{
			r = DifxParametersfind1(ip, r+1, 
				"DATASTREAM %d INDEX", a);
			if(r < 0)
			{
				fprintf(stderr, 
					"DATASTREAM %d INDEX not found\n", a);
				return 0;
			}
			D->config[c].indexDS[a] = 
				atoi(DifxParametersvalue(ip, r));
		}

		/* initialize baseline index array */
		D->config[c].indexBL =
			(int *)malloc(sizeof(int)*(D->activeBaselines+1));
		for(b = 0; b <= D->activeBaselines; b++)
		{
			D->config[c].indexBL[b] = -1;
		}

		/* populate baseline index array */
		for(b = 0; b < D->activeBaselines; b++)
		{
			r = DifxParametersfind1(ip, r+1, 
				"BASELINE %d INDEX", b);
			if(r < 0)
			{
				fprintf(stderr, 
					"BASELINE %d INDEX not found\n", b);
				return 0;
			}
			D->config[c].indexBL[b] = 
				atoi(DifxParametersvalue(ip, r));
		}
	}

	D->nOutChan = D->config[0].nChan;
	
	/* FREQ TABLE */
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

		if(D->freq[b].freq < D->refFreq || D->refFreq <= 0.0)
		{
			D->refFreq = D->freq[b].freq;
		}
	}

	/* TELESCOPE TABLE */
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

	/* DATA TABLE */
	rows[N_VSN_ROWS-1] = 0;		/* initialize start */
	for(a = 0; a < D->nAntenna; a++)
	{
		N = DifxParametersbatchfind1(ip, rows[N_VSN_ROWS-1], vsnKeys,
			a, N_VSN_ROWS, rows);
		if(N < N_VSN_ROWS)
		{
			fprintf(stderr, "populateInput: N < N_VSN_ROWS %d "
				"< %d\n", N, N_VSN_ROWS);
			return 0;
		}
		strncpy(D->antenna[a].vsn, DifxParametersvalue(ip, rows[0]), 8);
		D->antenna[a].vsn[8] = 0;
	}

	/* DATASTREAM TABLE */
	r = DifxParametersfind(ip, 0, "DATASTREAM ENTRIES");
	D->nDSEntry = atoi(DifxParametersvalue(ip, r));
	D->dsentry = newDifxDSEntryArray(D->nDSEntry);

	for(e = 0; e < D->nDSEntry; e++)
	{
		r = DifxParametersfind(ip, r+1, "TELESCOPE INDEX");
		if(r < 0)
		{
			fprintf(stderr, "TELESCOPE INDEX not found\n");
			return 0;
		}
		D->dsentry[e].antId = atoi(DifxParametersvalue(ip, r));
		
		r = DifxParametersfind(ip, r+1, "DATA FORMAT");
		if(r < 0)
		{
			fprintf(stderr, "DATA FORMAT not found\n");
			return 0;
		}
		strncpy(D->dsentry[e].dataFormat,
			DifxParametersvalue(ip, r), 31);
	
		D->dsentry[e].quantBits = -1;
		l = strlen(D->dsentry[e].dataFormat);
		if(l > 8)
		{
			/* do this more formally someday... */
			if(strcmp(D->dsentry[e].dataFormat+l-2, "-1") == 0)
			{
				D->dsentry[e].quantBits = 1;
			}
			else if(strcmp(D->dsentry[e].dataFormat+l-2, "-2") == 0)
			{
				D->dsentry[e].quantBits = 2;
			}
			else
			{
				D->dsentry[e].quantBits = 0;
			}
		}
		if(D->dsentry[e].quantBits < 0)
		{
			r = DifxParametersfind(ip, r+1, "QUANTISATION BITS");
			if(r < 0)
			{
				fprintf(stderr, 
					"Cannot determine quantization bits\n");
				return 0;
			}
			D->dsentry[e].quantBits = 
				atoi(DifxParametersvalue(ip, r));
		}
		

		r = DifxParametersfind(ip, r+1, "NUM FREQS");
		if(r < 0)
		{
			fprintf(stderr, "NUM FREQS not found\n");
			return 0;
		}
		D->dsentry[e].nFreq = atoi(DifxParametersvalue(ip, r));
		D->dsentry[e].nPol = (int *)calloc(D->dsentry[e].nFreq,
			sizeof(int));
		D->dsentry[e].freqId = (int *)calloc(D->dsentry[e].nFreq,
			sizeof(int));
		D->dsentry[e].clockOffset = (double *)calloc(
			D->dsentry[e].nFreq, sizeof(double));

		nRecChan = 0;
		for(i = 0; i < D->dsentry[e].nFreq; i++)
		{
			r = DifxParametersfind1(ip, r+1, 
				"FREQ TABLE INDEX %d", i);
			D->dsentry[e].freqId[i] = 
				atoi(DifxParametersvalue(ip, r));
			r = DifxParametersfind1(ip, r+1, 
				"CLK OFFSET %d (us)", i);
			D->dsentry[e].clockOffset[i] =
				atof(DifxParametersvalue(ip, r));
			r = DifxParametersfind1(ip, r+1, 
				"NUM POLS %d", i);
			D->dsentry[e].nPol[i] = 
				atoi(DifxParametersvalue(ip, r));
			nRecChan += D->dsentry[e].nPol[i];
		}
		D->dsentry[e].nRecChan = nRecChan;
		D->dsentry[e].RCfreqId = (int *)calloc(D->dsentry[e].nRecChan,
			sizeof(int));
		D->dsentry[e].RCpolName = (char *)calloc(D->dsentry[e].nRecChan,
			sizeof(char));

		for(i = 0; i < nRecChan; i++)
		{
			r = DifxParametersfind1(ip, r+1,
				"INPUT BAND %d POL", i);
			D->dsentry[e].RCpolName[i] = 
				DifxParametersvalue(ip, r)[0];
			r = DifxParametersfind1(ip, r+1,
				"INPUT BAND %d INDEX", i);
			a = atoi(DifxParametersvalue(ip, r));
			D->dsentry[e].RCfreqId[i] = D->dsentry[e].freqId[a];
		}
	}

	/* BASELINE TABLE */
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
		D->baseline[b].dsB = atoi(DifxParametersvalue(ip, r));
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
		D->baseline[b].nFreq = atoi(DifxParametersvalue(ip, r));
		D->baseline[b].nPolProd = (int *)calloc(D->baseline[b].nFreq,
			sizeof(int));
		D->baseline[b].recChanA = (int **)calloc(D->baseline[b].nFreq,
			sizeof(int *));
		D->baseline[b].recChanB = (int **)calloc(D->baseline[b].nFreq,
			sizeof(int *));
		
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
			D->baseline[b].nPolProd[f] =
				atoi(DifxParametersvalue(ip, r));
			D->baseline[b].recChanA[f] = (int *)calloc(
				D->baseline[b].nPolProd[f], sizeof(int));
			D->baseline[b].recChanB[f] = (int *)calloc(
				D->baseline[b].nPolProd[f], sizeof(int));
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


	/* Derive some useful tables and values */
	for(c = 0; c < D->nConfig; c++)
	{
		/* determine number of bits, or zero if different among
		 * antennas */
		D->config[c].quantBits = -1;
		for(a = 0; a < D->activeDatastreams; a++)
		{
			qb = 0;
			e = D->config[c].indexDS[a];
			if(e < 0)
			{
				continue;
			}
			ds = D->dsentry + e;
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
		D->config[c].quantBits = qb;

		makeFreqId2IFmap(D, c);
	}

	return D;
}

static DifxInput *populateUVW(DifxInput *D, DifxParameters *up)
{
	int i, c, p, r = 0, v, N;
	int nPoint, startPoint;
	int rows[20];
	
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

	D->nEOP = 0;

	N = DifxParametersbatchfind(up, 0, initKeys, N_INIT_ROWS, rows);
	if(N < N_INIT_ROWS)
	{
		return 0;
	}

	D->modelInc = atof(DifxParametersvalue(up, rows[0]));
	D->nScan    = atoi(DifxParametersvalue(up, rows[1]));

	D->scan  = newDifxScanArray(D->nScan);
	
	rows[N_ANT_ROWS-1] = 0;		/* initialize start */
	for(i = 0; i < D->nAntenna; i++)
	{
		N = DifxParametersbatchfind1(up, rows[N_ANT_ROWS-1], antKeys,
			i, N_ANT_ROWS, rows);
		if(N < N_ANT_ROWS)
		{
			return 0;
		}
		strcpy(D->antenna[i].mount, DifxParametersvalue(up, rows[0]));
		D->antenna[i].offset[0]= 0.0;	/* Default */
		D->antenna[i].offset[1]= 0.0;	
		D->antenna[i].offset[2]= 0.0;	
		D->antenna[i].X        = atof(DifxParametersvalue(up, rows[1]));
		D->antenna[i].Y        = atof(DifxParametersvalue(up, rows[2]));
		D->antenna[i].Z        = atof(DifxParametersvalue(up, rows[3]));
		D->antenna[i].dX       = 0.0;
		D->antenna[i].dY       = 0.0;
		D->antenna[i].dZ       = 0.0;
	}
	
	rows[N_SCAN_ROWS-1] = 0;
	for(i = 0; i < D->nScan; i++)
	{
		N = DifxParametersbatchfind1(up, rows[N_SCAN_ROWS-1], scanKeys, 
			i, N_SCAN_ROWS, rows);
		if(N < N_SCAN_ROWS)
		{
			return 0;
		}
		nPoint               = atoi(DifxParametersvalue(up, rows[0]));
		startPoint           = atoi(DifxParametersvalue(up, rows[1]));
		D->scan[i].mjdStart  = D->mjdStart + 
			startPoint*D->modelInc/86400.0;
		D->scan[i].mjdEnd    = D->mjdStart + 
			(startPoint+nPoint)*D->modelInc/86400.0;
		D->scan[i].nPoint    = nPoint;
		strncpy(D->scan[i].name, 
			DifxParametersvalue(up, rows[2]), 31);
		D->scan[i].name[31]  = 0;
		D->scan[i].model     = newDifxModelArray(D->nAntenna, nPoint);
		D->scan[i].nAntenna  = D->nAntenna;
		
		D->scan[i].ra        = atof(DifxParametersvalue(up, rows[3]));
		D->scan[i].dec       = atof(DifxParametersvalue(up, rows[4]));
		D->scan[i].calcode[0]= 0;
		D->scan[i].qual      = 0;	/* Default */

		for(c = 0; c < D->nConfig; c++)
		{
			if(strcmp(D->scan[i].name, D->config[c].name) == 0)
			{
				D->scan[i].configId = c;
			}
		}

		for(p = -1; p <= D->scan[i].nPoint+1; p++)
		{
			r = DifxParametersfind1(up, r+1, "RELATIVE INC %d", p);
			if(r < 0)
			{
				fprintf(stderr, "UVW row not found : %d %d\n",
					i, p);
				return 0;
			}
			v = parseUVWs(D->scan[i].model, D->nAntenna, p,
				DifxParametersvalue(up, r));
			if(v < 0)
			{
				fprintf(stderr, "UVW parse error\n");
				return 0;
			}
		}
	}

	return D;
}

static DifxInput *populateDelay(DifxInput *D, DifxParameters *dp)
{
	int p, r = 0, s, v;
	
	if(!D)
	{
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
				return 0;
			}
			v = parseDelays(D->scan[s].model, D->nAntenna, p,
				DifxParametersvalue(dp, r));
			if(v < 0)
			{
				fprintf(stderr, "Delay parse error\n");
				return 0;
			}
		}
	}

	return D;
}

static DifxInput *populateCalc(DifxInput *D, DifxParameters *cp)
{
	const char initKeys[][MAX_DIFX_KEY_LEN] = 
	{
		"JOB ID",
		"OBSCODE",
		"NUM EOP"
	};
	const int N_INIT_ROWS = sizeof(initKeys)/sizeof(initKeys[0]);
	
	const char antKeys[][MAX_DIFX_KEY_LEN] =
	{
		"TELESCOPE %d OFFSET (m)"
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
		"SCAN %d SRC NAME",
		"SCAN %d REAL NAME",
		"SCAN %d CALCODE",
		"SCAN %d QUAL"
	};
	const int N_SCAN_ROWS = sizeof(scanKeys)/sizeof(scanKeys[0]);
	
	int rows[20];
	int i, c, N, row;
	const char *cname;
	int findconfig = 0;

	if(!D)
	{
		return 0;
	}

	N = DifxParametersbatchfind(cp, 0, initKeys, N_INIT_ROWS, rows);
	if(N < N_INIT_ROWS)
	{
		return 0;
	}

	D->jobId    = atoi(DifxParametersvalue(cp, rows[0]));
	strcpy(D->obsCode, DifxParametersvalue(cp, rows[1]));
	D->nEOP     = atoi(DifxParametersvalue(cp, rows[2]));

	D->eop   = newDifxEOPArray(D->nEOP);

	row = DifxParametersfind(cp, 0, "SESSION");
	if(row >= 0)
	{
		strncpy(D->obsSession, DifxParametersvalue(cp, row), 7);
		D->obsSession[7] = 0;
	}
	row = DifxParametersfind(cp, 0, "TAPER FUNCTION");
	if(row >= 0)
	{
		strncpy(D->taperFunction, DifxParametersvalue(cp, row), 7);
		D->taperFunction[7] = 0;
	}
	row = DifxParametersfind(cp, 0, "JOB START TIME");
	if(row >= 0)
	{
		D->jobStart = atof(DifxParametersvalue(cp, row));
	}
	row = DifxParametersfind(cp, 0, "JOB STOP TIME");
	if(row >= 0)
	{
		D->jobStop = atof(DifxParametersvalue(cp, row));
	}
	row = DifxParametersfind(cp, 0, "SUBJOB ID");
	if(row >= 0)
	{
		D->subjobId = atoi(DifxParametersvalue(cp, row));
	}
	row = DifxParametersfind(cp, 0, "SUBARRAY ID");
	if(row >= 0)
	{
		D->subarrayId = atoi(DifxParametersvalue(cp, row));
	}
	row = DifxParametersfind(cp, 0, "SPECTRAL AVG");
	if(row >= 0)
	{
		D->specAvg = atoi(DifxParametersvalue(cp, row));
	}

	D->nOutChan = D->config[0].nChan/D->specAvg;

	rows[N_ANT_ROWS-1] = 0;		/* initialize start */
	for(i = 0; i < D->nAntenna; i++)
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
		D->antenna[i].offset[0]= atof(DifxParametersvalue(cp, rows[0]));
		D->antenna[i].offset[1]= 0.0;	/* FIXME */
		D->antenna[i].offset[2]= 0.0;	/* FIXME */
	}
	
	rows[N_EOP_ROWS-1] = 0;		/* initialize start */
	for(i = 0; i < D->nEOP; i++)
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
		strncpy(D->scan[i].name, DifxParametersvalue(cp, rows[1]), 31);
		D->scan[i].name[31]  = 0;
		strncpy(D->scan[i].calcode, 
			DifxParametersvalue(cp, rows[2]), 3);
		D->scan[i].calcode[3]= 0;
		D->scan[i].qual      = atoi(DifxParametersvalue(cp, rows[3]));

		cname = DifxParametersvalue(cp, rows[0]);
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

	f = 0.5/D->modelInc;
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

static DifxInput *populateRate(DifxInput *D, DifxParameters *rp)
{
	int a, p, r = 0, s, v;
	double f;
	
	if(!D)
	{
		return 0;
	}

	r = DifxParametersfind(rp, 0, "CALC SERVER");
	if(r < 0)
	{
		strcpy(D->calcServer, "unknown");
	}
	else
	{
		strncpy(D->calcServer, DifxParametersvalue(rp, r), 32);
		D->calcServer[31] = 0;
	}

	r = 0;

	for(s = 0; s < D->nScan; s++)
	{
		for(p = -1; p <= D->scan[s].nPoint+1; p++)
		{
			r = DifxParametersfind1(rp, r+1, "RELATIVE INC %d", p);
			if(r < 0)
			{
				fprintf(stderr, "Rate row not found : %d %d\n",
					s, p);
				return 0;
			}
			v = parseRates(D->scan[s].model, D->nAntenna, p,
				DifxParametersvalue(rp, r));
			if(v < 0)
			{
				fprintf(stderr, "Rate parse error\n");
				return 0;
			}
		}

		/* compute atm rate based on atm delay */
		f = 0.5/D->modelInc;
		for(a = 0; a < D->nAntenna; a++)
		{
			for(p = 0; p < D->scan[s].nPoint+1; p++)
			{
				D->scan[s].model[a][p].da = 
				       f*( D->scan[s].model[a][p+1].a -
					   D->scan[s].model[a][p-1].a);
			}
		}
	}

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
		D->flag = (DifxAntennaFlag *)malloc(n*sizeof(DifxAntennaFlag));
		for(i = 0; i < n; i++)
		{
			p = fscanf(in, "%lf%lf%d", &mjd1, &mjd2, &a);
			if(p == 3)
			{
				D->flag[i].mjd1  = mjd1;
				D->flag[i].mjd2  = mjd2;
				D->flag[i].antId = a;
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
 * Also derive sourceId and configId for each scan
 */
static DifxInput *deriveSourceTable(DifxInput *D)
{
	int i, n=0, s;

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
			   strcmp(D->source[i].calcode, D->scan[s].calcode) 
			   	== 0 &&
			   strcmp(D->source[i].name, D->scan[s].name) == 0)
			{
				break;
			}
		}
		
		if(i >= n)
		{
			strcpy(D->source[n].name, D->scan[s].name);
			strcpy(D->source[i].calcode, D->scan[s].calcode);
			D->source[n].ra       = D->scan[s].ra;
			D->source[n].dec      = D->scan[s].dec;
			D->source[n].qual     = D->scan[s].qual;
			D->source[i].configId = D->scan[s].configId;
			n++;
		}

		D->scan[s].sourceId = i;
	}

	D->nSource = n;

	return D;
}

static void setGlobalValues(DifxInput *D)
{
	int i, c, p, n, nIF, nPol;
	int doPolar, qb;
	double bw;
	int hasR = 0;
	int hasL = 0;
	char pol[2];

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

	for(c = 0; c < D->nConfig; c++)
	{
		nIF = D->config[c].nIF;
		qb  = D->config[c].quantBits;
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
		doPolar = D->config[c].doPolar;
		if(D->doPolar < doPolar)
		{
			D->doPolar = doPolar;
		}
		for(i = 0; i < nIF; i++)
		{
			nPol   = D->config[c].IF[i].nPol;
			bw     = D->config[c].IF[i].bw;
			pol[0] = D->config[c].IF[i].pol[0];
			pol[1] = D->config[c].IF[i].pol[1];
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
				D->quantBits = D->nIF = D->nPolar = 0;
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

DifxInput *loadDifxInput(const char *fileprefix)
{
	DifxParameters *ip, *up, *dp, *rp, *cp;
	DifxInput *D, *DSave;
	char inputfile[256];
	char uvwfile[256];
	char delayfile[256];
	char ratefile[256];
	char calcfile[256];
	char flagfile[256];

	sprintf(inputfile, "%s.input", fileprefix);
	sprintf(uvwfile,   "%s.uvw",   fileprefix);
	sprintf(delayfile, "%s.delay", fileprefix);
	sprintf(ratefile,  "%s.rate",  fileprefix);
	sprintf(calcfile,  "%s.calc",  fileprefix);
	sprintf(flagfile,  "%s.flag",  fileprefix);

	ip = newDifxParametersfromfile(inputfile);
	up = newDifxParametersfromfile(uvwfile);
	dp = newDifxParametersfromfile(delayfile);
	rp = newDifxParametersfromfile(ratefile);
	cp = newDifxParametersfromfile(calcfile);

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
			calcfile);
		fprintf(stderr, "  Defaults being used for many parameters\n");
	}
	if(rp)
	{
		D = populateRate(D, rp);
	}
	else
	{
		fprintf(stderr, "Warning -- no file called %s found.  Continuing anyways\n",
			ratefile);
		fprintf(stderr, "  Model rates will be approximate\n");
		fprintf(stderr, "  Atmosphere values will be absent\n");

		estimateRate(D);
	}
	D = deriveSourceTable(D);

	setGlobalValues(D);
	calcFreqIds(D);

	if(!D)
	{
		deleteDifxInput(DSave);
	}
	
	deleteDifxParameters(ip);
	deleteDifxParameters(up);
	deleteDifxParameters(dp);
	deleteDifxParameters(cp);

	populateFlags(D, flagfile);
	
	return D;
}

int DifxInputGetSourceId(const DifxInput *D, double mjd)
{
	int s;

	if(!D)
	{
		return -1;
	}

	if(mjd <= D->mjdStart)  /* return first source */
	{
		return D->scan[0].sourceId;
	}

	for(s = 0; s < D->nScan; s++)
	{
		if(mjd < D->scan[s].mjdEnd)
		{
			return D->scan[s].sourceId;
		}
	}

	/* return last scan as last resort */
	return D->scan[D->nScan-1].sourceId;
}

/* return 0-based index of antName, or -1 if not in array */
int DifxInputGetAntennaId(const DifxInput *D, const char *antName)
{
	int a;
	
	if(!D)
	{
		return -1;
	}

	for(a = 0; a < D->nAntenna; a++)
	{
		if(strcmp(D->antenna[a].name, antName) == 0)
		{
			return a;
		}
	}

	return -1;
}

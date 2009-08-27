/***************************************************************************
 *   Copyright (C) 2008, 2009 by Walter Brisken                            *
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
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id$
 * $HeadURL$
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include "fits.h"

#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <assert.h>
#include "config.h"
#include "fitsUV.h"
#include "jobmatrix.h"
#ifdef HAVE_FFTW
#include "sniffer.h"
#endif

#define HEADER_READ_ERROR	-1
#define NEXT_FILE_ERROR		-2
#define DATA_READ_ERROR		-3
#define SKIPPED_RECORD 		-4
#define CONFIG_CHANGE_ERROR	-5
#define BAND_ID_ERROR		-6
#define POL_ID_ERROR		-7
#define NFLOAT_ERROR		-8

static int DifxVisInitData(DifxVis *dv)
{
	int n;
	
	if(!dv)
	{
		return -1;
	}

	if(dv->record)
	{
		free(dv->record);
	}

	/* size of output data record (in floats) */
	dv->nData = dv->nComplex*dv->nFreq*dv->D->nPolar*dv->D->nOutChan;
	if(dv->nData <= 0)
	{
		return -1;
	}
	n = dv->nFreq*dv->D->nPolar + dv->nData;
	dv->record = calloc(n*sizeof(float) + sizeof(struct UVrow), 1);
	if(dv->record == 0)
	{
		assert(dv->record);
		exit(0);
	}
	dv->weight = dv->record->data;
	/* FIXME -- here add a similar thing for gateId? */
	dv->data = dv->weight + (dv->nFreq*dv->D->nPolar);
	dv->sourceId = -1;
	dv->scanId = -1;

	return 0;
}

static void DifxVisStartGlob(DifxVis *dv)
{
	char *globstr;
	const char suffix[] = ".difx/DIFX*";

	globstr = calloc(strlen(dv->D->job[dv->jobId].fileBase)+
		strlen(suffix)+8, 1);
	if(globstr == 0)
	{
		assert(globstr);
		exit(0);
	}

	sprintf(globstr, "%s%s", dv->D->job[dv->jobId].fileBase, suffix);

	glob(globstr, 0, 0, &dv->globbuf);
	dv->nFile = dv->globbuf.gl_pathc;
	
	if(dv->nFile == 0)
	{
		fprintf(stderr, "Error: no data files for job %s\n",
			dv->D->job[dv->jobId].fileBase);
		exit(0);
	}

	dv->globbuf.gl_offs = 0;

	free(globstr);
}

int DifxVisNextFile(DifxVis *dv)
{
	if(dv->in)
	{
		fclose(dv->in);
		dv->in = 0;
	}
	dv->curFile++;
	if(dv->curFile >= dv->nFile)
	{
		printf("    JobId %d done.\n", dv->jobId);
		return -1;
	}
	printf("    JobId %d File %d/%d : %s\n", 
		dv->jobId,
		dv->curFile+1, dv->nFile,
		dv->globbuf.gl_pathv[dv->curFile]);
	dv->in = fopen(dv->globbuf.gl_pathv[dv->curFile], "r");
	if(!dv->in)
	{
		fprintf(stderr, "Error opening file : %s\n", 
			dv->globbuf.gl_pathv[dv->curFile]);
		return -1;
	}

	return 0;
}

DifxVis *newDifxVis(const DifxInput *D, int jobId)
{
	DifxVis *dv;
	int i, c, v;
	int polMask = 0;

	dv = (DifxVis *)calloc(1, sizeof(DifxVis));

	if(!dv)
	{
		fprintf(stderr, "Error: newDifxVis: dv=calloc failed, size=%d\n",
			sizeof(DifxVis));
		assert(dv);
		exit(0);
	}

	if(jobId < 0 || jobId >= D->nJob)
	{
		fprintf(stderr, "Error: newDifxVis: jobId = %d, nJob = %d\n",
			jobId, D->nJob);
		return 0;
	}
	
	dv->jobId = jobId;
	dv->D = D;
	dv->curFile = -1;
	DifxVisStartGlob(dv);
	dv->configId = -1;
	dv->sourceId = -1;
	dv->scanId = -1;
	dv->baseline = -1;
	dv->scale = 1.0;

	/* For now, the difx format only provides 1 weight for the entire
	 * vis record, so we don't need weights per channel */
	dv->nComplex = 2;	/* for now don't write weights */
	dv->first = 1;
	
	for(c = 0; c < D->nConfig; c++)
	{
		if(D->nIF > dv->nFreq)
		{
			dv->nFreq = D->nIF;
		}
		for(i = 0; i < D->nIF; i++)
		{
			if(D->config[c].IF[i].pol[0] == 'R' ||
			   D->config[c].IF[i].pol[1] == 'R')
			{
				polMask |= 0x01;
			}
			if(D->config[c].IF[i].pol[0] == 'L' ||
			   D->config[c].IF[i].pol[1] == 'L')
			{
				polMask |= 0x02;
			}
			if(D->config[c].IF[i].pol[0] == 'X' ||
			   D->config[c].IF[i].pol[1] == 'X')
			{
				polMask |= 0x10;
			}
			if(D->config[c].IF[i].pol[0] == 'Y' ||
			   D->config[c].IF[i].pol[1] == 'Y')
			{
				polMask |= 0x20;
			}
		}
	}

	/* check for polarization confusion */
	if( ((polMask & 0x0F) > 0 && (polMask & 0xF0) > 0) || polMask == 0 )
	{
		fprintf(stderr, "Error: bad polarization combinations : %x\n",
			polMask);
		deleteDifxVis(dv);
		return 0;
	}

	if(polMask & 0x01)
	{
		dv->polStart = -1;
	}
	else if(polMask & 0x02)
	{
		dv->polStart = -2;
	}
	else if(polMask & 0x10)
	{
		dv->polStart = -5;
	}
	else /* must be YY only! who would do that? */
	{
		dv->polStart = -6;
	}

	/* room for input vis record: 3 for real, imag, weight */
	dv->spectrum = (float *)calloc(3*dv->D->nInChan, sizeof(float));
	assert(dv->spectrum);

	dv->dp = newDifxParameters();

	v = DifxVisInitData(dv);
	if(v < 0)
	{
		fprintf(stderr, "Error %d allocating %d + %d bytes\n",
			v, (int)(sizeof(struct UVrow)),
			dv->nComplex*dv->nFreq*dv->D->nPolar*dv->D->nOutChan);
		deleteDifxVis(dv);
		return 0;
	}

	if(DifxVisNextFile(dv) < 0)
	{
		fprintf(stderr, "Info: newDifxVis: DifxVisNextFile(dv) < 0\n");
		deleteDifxVis(dv);
		return 0;
	}

	return dv;
}

void deleteDifxVis(DifxVis *dv)
{
	if(!dv)
	{
		return;
	}

	globfree(&dv->globbuf);
	if(dv->in)
	{
		fclose(dv->in);
	}
	if(dv->spectrum)
	{
		free(dv->spectrum);
	}
	if(dv->dp)
	{
		deleteDifxParameters(dv->dp);
	}
	if(dv->record)
	{
		free(dv->record);
	}
	
	free(dv);
}


static int getPolProdId(const DifxVis *dv, const char *polPair)
{
	const char polSeq[8][4] = 
		{"RR", "LL", "RL", "LR", "XX", "YY", "XY", "YX"};
	int p;

	for(p = 0; p < 8; p++)
	{
		if(strncmp(polPair, polSeq[p], 2) == 0)
		{
			p = (p+1) + dv->polStart;
			
			if(p < 0 || p >= dv->D->nPolar)
			{
				return -1;
			}
			else
			{
				return p;
			}
		}
	}
	
	return -2;
}


/* Use Cramer's rule to evaluate polynomial */
static double evalPoly(const double *p, int n, double x)
{
	double y;
	int i;

	if(n == 1)
	{
		return p[0];
	}

	y = p[n-1];

	for(i = n-2; i >= 0; i--)
	{
		y = x*y + p[i];
	}

	return y;
}

	
int DifxVisNewUVData(DifxVis *dv, int verbose, int pulsarBin)
{
	const char difxKeys[][MAX_DIFX_KEY_LEN] = 
	{
		"BASELINE NUM",
		"MJD",
		"SECONDS",
		"CONFIG INDEX",
		"SOURCE INDEX",
		"FREQ INDEX",
		"POLARISATION PAIR",
		"PULSAR BIN",
		"DATA WEIGHT",
		"U (METRES)",
		"V (METRES)",
		"W (METRES)"
	};

	const char difxKeysOrig[][MAX_DIFX_KEY_LEN] = 
	{
		"BASELINE NUM",
		"MJD",
		"SECONDS",
		"CONFIG INDEX",
		"SOURCE INDEX",
		"FREQ INDEX",
		"POLARISATION PAIR",
		"PULSAR BIN",
		"WEIGHTS WRITTEN",
		"U (METRES)",
		"V (METRES)",
		"W (METRES)"
	};

	const int N_DIFX_ROWS = sizeof(difxKeys)/sizeof(difxKeys[0]);
	int rows[N_DIFX_ROWS];
	int i, i1, v, N, index;
	int a1, a2;
	int bl, scanId;
	double mjd, dt, dt2;
	int changed = 0;
	int nFloat, readSize;
	char line[100];
	int freqNum;
	int configId;
	const DifxConfig *config;
	const DifxScan *scan;
	const DifxPolyModel *im1, *im2;
	int terms1, terms2;
	int d1, d2, aa1, aa2;	/* FIXME -- temporary */
	int bin;

	resetDifxParameters(dv->dp);

	for(i = 0; i < 13; i++)
	{
		fgets(line, 99, dv->in);
		if(feof(dv->in))
		{
			/* EOF should not happen in middle of text */
			if(i != 0)
			{
				return HEADER_READ_ERROR;
			}
			v = DifxVisNextFile(dv);
			if(v < 0)
			{
				return NEXT_FILE_ERROR;
			}
			fgets(line, 99, dv->in);
		}
		DifxParametersaddrow(dv->dp, line);
	}

	/* parse the text header */
	if(dv->D->inputFileVersion == 0)
	{
		N = DifxParametersbatchfind(dv->dp, 0, difxKeys, 
			N_DIFX_ROWS, rows);
	}
	else
	{
		N = DifxParametersbatchfind(dv->dp, 0, difxKeysOrig, 
			N_DIFX_ROWS, rows);
	}
	if(N < N_DIFX_ROWS)
	{
		printf("ERROR: N=%d < N_DIFX_ROWS=%d\n", N, N_DIFX_ROWS);
		return HEADER_READ_ERROR;
	}

	bl           = atoi(DifxParametersvalue(dv->dp, rows[0]));
	mjd          = atoi(DifxParametersvalue(dv->dp, rows[1])) +
	               atof(DifxParametersvalue(dv->dp, rows[2]))/86400.0;
	freqNum      = atoi(DifxParametersvalue(dv->dp, rows[5]));
	bin          = atoi(DifxParametersvalue(dv->dp, rows[7]));

	/* if chan weights are written the data volume is 3/2 as large */
	/* for now, force nFloat = 2 (one weight for entire vis record) */
	nFloat = 2;
	readSize = nFloat * dv->D->nInChan;
	v = fread(dv->spectrum, sizeof(float), readSize, dv->in);
	if(v < readSize)
	{
		return DATA_READ_ERROR;
	}

	/* Drop all records (except autocorrelations) not associated 
	 * with the requested pulsar bin */
	if(bin != pulsarBin && bl % 257 != 0)
	{
		return SKIPPED_RECORD;
	}
	else
	{
		dv->pulsarBin = bin;
	}

	/* FIXME -- look at sourceId in the record as a check */
	/* FIXME -- look at configId in the record as a check */

	/* scanId at middle of integration */
	scanId = DifxInputGetScanIdByJobId(dv->D, mjd, dv->jobId);
	if(scanId < 0)
	{
		return SKIPPED_RECORD;
	}

	scan = dv->D->scan + scanId;
	configId = scan->configId;
	if(configId >= dv->D->nConfig) 
	{
		fprintf(stderr, "Developer error: configId = %d\n", configId);
		fprintf(stderr, "ScanId was %d\n", scanId);
		exit(0);
	}
	if(configId < 0)
	{
		return SKIPPED_RECORD;
	}

	config = dv->D->config + configId;

	/* see if it is still the same scan at the edges of integration */
	dt2 = config->tInt/(86400.0*2.001);  
	if(scan->mjdStart > mjd-dt2 || scan->mjdEnd < mjd+dt2)
	{
		/* Nope! */
		dv->flagTransition = 1;
		return SKIPPED_RECORD;
	}
	else
	{
		dv->flagTransition = 0;
	}

	aa1 = a1 = (bl/256) - 1;
	aa2 = a2 = (bl%256) - 1;

	if(a1 < 0 || a1 >= config->nAntenna ||
	   a2 < 0 || a2 >= config->nAntenna)
	{
		printf("Error: illegal baseline %d -> %d-%d\n", bl, a1, a2);
		return -8;
	}

	/* translate from .input file antId to index for D->antenna via dsId */
	d1 = config->ant2dsId[a1];
	d2 = config->ant2dsId[a2];
	if(d1 < 0 || d1 >= dv->D->nDatastream || 
	   d2 < 0 || d2 >= dv->D->nDatastream)
	{
		printf("Error: baseline %d -> datastreams %d-%d\n",
			bl, d1, d2);
		return -9;
	}
	a1 = dv->D->datastream[d1].antennaId;
	a2 = dv->D->datastream[d2].antennaId;
	bl = (a1+1)*256 + (a2+1);
	
	if(verbose >= 1 && scanId != dv->scanId)
	{
		printf("        MJD=%11.5f jobId=%d scanId=%d Source=%s  FITS SourceId=%d\n", 
			mjd, dv->jobId, scanId, scan->name, dv->D->source[scan->sourceId].fitsSourceId+1);
	}

	dv->scanId = scanId;
	dv->sourceId = scan->sourceId;
	dv->freqId = config->freqId;
	dv->bandId = config->baselineFreq2IF[aa1][aa2][freqNum];
	dv->polId  = getPolProdId(dv, DifxParametersvalue(dv->dp, rows[6]));

	/* stash the weight for later incorporation into a record */
	if(dv->D->inputFileVersion == 0)
	{
		dv->recweight = atof(DifxParametersvalue(dv->dp, rows[8]));
	}
	else
	{
		dv->recweight = 1.0;
	}

	if(bl != dv->baseline || fabs(mjd - dv->mjd) > 1.0/86400000.0)
	{
		changed = 1;
		dv->baseline = bl;
		dv->mjd = mjd;

		index = dv->freqId + dv->nFreq*dv->polId;

		/* swap phase/uvw for FITS-IDI conformance */
		dv->U = -atof(DifxParametersvalue(dv->dp, rows[9]));
		dv->V = -atof(DifxParametersvalue(dv->dp, rows[10]));
		dv->W = -atof(DifxParametersvalue(dv->dp, rows[11]));

		/* recompute from polynomials if possible */
		if(scan->im)
		{
			double u,v,w;
			int n;

			n = getDifxScanIMIndex(scan, mjd, &dt);

			u = dv->U;
			v = dv->V;
			w = dv->W;

			if(a1 >= scan->nAntenna || a2 > scan->nAntenna)
			{
				fprintf(stderr, "Error: DifxVisNewUVData: a1=%d a2=%d nAnt=%d\n",
					a1, a2, scan->nAntenna);
				goto End_UVW_Fix;
			}

			/* use .difx/ antenna indices for model tables */
			im1 = scan->im[a1];
			im2 = scan->im[a2];
			if(im1 && im2)
			{
				if(n < 0)
				{
					fprintf(stderr, "Error: interferometer model index out of range: scanId=%d mjd=%12.6f\n",
					scanId, mjd);
				}
				else
				{
					terms1 = im1->order + 1;
					terms2 = im2->order + 1;
					dv->U = evalPoly(im2[n].u, terms2, dt) 
					       -evalPoly(im1[n].u, terms1, dt);
					dv->V = evalPoly(im2[n].v, terms2, dt) 
					       -evalPoly(im1[n].v, terms1, dt);
					dv->W = evalPoly(im2[n].w, terms2, dt) 
					       -evalPoly(im1[n].w, terms1, dt);
				}
			}
			else
			{
				printf("Badness: %d %d %d-%d\n", scanId, n, aa1, aa2);
			}

		End_UVW_Fix:

			if((fabs(u - dv->U) > 10.0 ||
			    fabs(v - dv->V) > 10.0 ||
			    fabs(w - dv->W) > 10.0) && 
			    !dv->flagTransition) 
			{
				printf("Warning: UVW diff: %d %d %d-%d %f %f  %f %f  %f %f  %f %f\n", 
					scanId, n, aa1, aa2, mjd, dt, u, dv->U, v, dv->V, w, dv->W);
			}
		}
	}

	if(configId != dv->configId)
	{
		if(!changed)	/* cannot change config within integration */
		{
			return CONFIG_CHANGE_ERROR;
		}
		dv->configId = configId;
		dv->tInt = config->tInt;
	}

	/* don't get all excited and signify a change on first rec */
	if(changed && dv->first)
	{
		dv->first = 0;
		changed = 0;
		DifxVisCollectRandomParams(dv);
	}

	if(dv->bandId <  0 || dv->bandId >= dv->nFreq)
	{
		fprintf(stderr, "Parameter problem: bandId should be in "
				"[0, %d), was %d\n", 
				dv->nFreq, dv->bandId);
		
		return BAND_ID_ERROR;
	}
	
	if(dv->polId  <  0 || dv->polId  >= dv->D->nPolar)
	{
		fprintf(stderr, "Parameter problem: polId should be in "
				"[0, %d), was %d\n",
				dv->D->nPolar, dv->polId);

		return POL_ID_ERROR;
	}

	/* don't read weighted data into unweighted slot */
	if(nFloat > dv->nComplex)
	{
		printf("nFloat > dv->nComplex\n");
		return NFLOAT_ERROR;
	}
	
	/* reorder data and set weights if weights not provided */
	if(nFloat < dv->nComplex)
	{
		for(i = 3*dv->D->nInChan-3; i > 0; i -= 3)
		{
			i1 = i*2/3;
			dv->spectrum[i+2] = 1.0;                 /* weight */
			dv->spectrum[i+1] = dv->spectrum[i1+1];  /* imag */
			dv->spectrum[i]   = dv->spectrum[i1];    /* real */
		}
		/* The first element needs no reordering, but needs weight */
		dv->spectrum[2] = 1.0;
	}

	/* scale data by weight */
	for(i = 0; i < dv->D->nInChan; i++)
	{
		dv->spectrum[i*dv->nComplex] *= dv->recweight;
		dv->spectrum[i*dv->nComplex+1] *= dv->recweight;
	}

	return changed;
}

int DifxVisCollectRandomParams(const DifxVis *dv)
{
	const double cLight=2.99792458e8;	/* speed of light in m/s */
	
	dv->record->U		= dv->U/cLight;
	dv->record->V		= dv->V/cLight;
	dv->record->W		= dv->W/cLight;

	dv->record->jd		= 2400000.5 + (int)dv->mjd;
	dv->record->iat		= dv->mjd - (int)dv->mjd;

	/* reminder: antennaIds, sourceId, freqId are 1-based in FITS */
	dv->record->baseline	= dv->baseline;
	dv->record->filter	= 0;
	dv->record->sourceId1	= dv->D->source[dv->sourceId].fitsSourceId + 1;
	dv->record->freqId1	= dv->freqId + 1;
	dv->record->intTime	= dv->tInt;

	return 0;
}

static int RecordIsInvalid(const DifxVis *dv)
{
	int i, n;
	const float *d;

	d = dv->data;
	n = dv->nData;

	for(i = 0; i < n; i++)
	{
		if(isnan(d[i]) || isinf(d[i]))
		{

			printf("Warning -- record with !finite value: ");
			printf("a1=%d a1=%d mjd=%13.7f\n",
				(dv->record->baseline/256) - 1,
				(dv->record->baseline%256) - 1,
				dv->mjd);
			return 1;
		}
	}
	for(i = 0; i < n; i++)
	{
		if(d[i] > 1.0e10 || d[i] < -1.0e10)
		{
			printf("Warning -- record with extreme value: ");
			printf("a1=%d a1=%d mjd=%13.7f value=%e\n",
				(dv->record->baseline/256) - 1,
				(dv->record->baseline%256) - 1,
				dv->mjd,
				d[i]);
			return 1;
		}
	}
	
	return 0;
}

static int RecordIsZero(const DifxVis *dv)
{
	int i, n;
	const float *d;

	d = dv->data;
	n = dv->nData;

	for(i = 0; i < n; i++)
	{
		/* don't look at weight in deciding whether data is valid */
		if((d[i] != 0.0) && (i % dv->nComplex != 2))
		{
			return 0;
		}
	}
	
	// haven't found a non-zero value :(
	return 1;
}

static int RecordIsTransitioning(const DifxVis *dv)
{
	return dv->flagTransition;
}

static int RecordIsFlagged(const DifxVis *dv)
{
	DifxJob *job;
	double mjd;
	int a1, a2;
	int i;

	job = dv->D->job + dv->jobId;

	if(job->nFlag <= 0)
	{
		return 0;
	}

	mjd = (int)(dv->record->jd - 2400000.0) + dv->record->iat;
	a1  = (dv->record->baseline/256) - 1;
	a2  = (dv->record->baseline%256) - 1;

	for(i = 0; i < job->nFlag; i++)
	{
		if(job->flag[i].mjd1 <= mjd &&
		   job->flag[i].mjd2 >= mjd)
		{
			if(job->flag[i].antennaId == a1 ||
			   job->flag[i].antennaId == a2)
			{
				return 1;
			}
		}
	}

	return 0;
}

static double getDifxScaleFactor(const DifxInput *D, double s, int verbose)
{
	double scale;

	if(D->inputFileVersion == 0) /* Perth merge and after */
	{
		scale = 4.576*D->nOutChan/D->nInChan;
	}
	else
	{
		scale = 7.15/(D->chanBW*6.25e6*D->config[0].tInt*D->specAvg);
	}
	if(D->quantBits == 2)
	{
		scale /= (3.3359*3.3359);
	}

	if(s > 0.0)
	{
		if(verbose > 0)
		{
			printf("      Overriding scale factor %e with %e\n",
				scale, s);
		}
		scale = s;
	}

	return scale;
}

static int storevis(DifxVis *dv)
{
	const DifxInput *D;
	int isLSB;
	int startChan;
	int stopChan;
	int i, j, k, index;

	D = dv->D;

	if(dv->configId < 0)
	{
		return -1;
	}
	
	isLSB = D->config[dv->configId].IF[dv->bandId].sideband == 'L';
	startChan = D->startChan;
	stopChan = startChan + D->nOutChan*D->specAvg;

	dv->weight[D->nPolar*dv->bandId + dv->polId] = 
		dv->recweight;
	
	for(i = startChan; i < stopChan; i++)
	{
		if(isLSB)
		{
			j = stopChan - 1 - i;
			index = ((dv->bandId*D->nOutChan + 
				j/D->specAvg)*
				D->nPolar+dv->polId)*dv->nComplex;
		}
		else
		{
			index = ((dv->bandId*D->nOutChan + 
				(i-startChan)/D->specAvg)*
				D->nPolar+dv->polId)*dv->nComplex;
		}
		for(k = 0; k < dv->nComplex; k++)
		{
			/* swap phase/uvw for FITS-IDI conformance */
			if(k % 3 == 1 && !isLSB)
			{
				dv->data[index+k] -= dv->scale*
					dv->spectrum[dv->nComplex*i+k];
			}
			else
			{
				dv->data[index+k] += dv->scale*
					dv->spectrum[dv->nComplex*i+k];
			}
		}
	}

	return 0;
}

static int readvisrecord(DifxVis *dv, int verbose, int pulsarBin)
{
	/* blank array */
	memset(dv->data, 0, dv->nData*sizeof(float));

	dv->changed = 0;

	while(dv->changed == 0 || dv->changed == SKIPPED_RECORD)
	{
		if(!dv->first && dv->changed >= 0)
		{
			storevis(dv);
		}
		dv->changed = DifxVisNewUVData(dv, verbose, pulsarBin);
	}

	return 0;
}

static int DifxVisConvert(const DifxInput *D, 
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out, 
	struct CommandLineOptions *opts)
{
	int i, j, l, v;
	float visScale = 1.0;
	char fileBase[200];
	char dateStr[12];
	char fluxFormFloat[8];
	char gateFormInt[8];
	char weightFormFloat[8];
	int nRowBytes;
	int nColumn;
	int nWeight;
	int nJob, bestj;
	int nInvalid = 0;
	int nFlagged = 0;
	int nZero = 0;
	int nTrans = 0;
	int nWritten = 0;
	double mjd, bestmjd;
	double scale;
	DifxVis **dvs;
	DifxVis *dv;
	JobMatrix *jobMatrix = 0;
#ifdef HAVE_FFTW
	Sniffer *S = 0;
#endif

	/* define the columns in the UV data FITS Table */
	struct fitsBinTableColumn columns[] =
	{
		{"UU--SIN", "1E", "u", "SECONDS"},
		{"VV--SIN", "1E", "v", "SECONDS"},
		{"WW--SIN", "1E", "w", "SECONDS"},
		{"DATE", "1D", "Julian day at 0 hr current day", "DAYS"},
		{"TIME", "1D", "IAT time", "DAYS"},
		{"BASELINE", "1J", "baseline: ant1*256 + ant2", 0},
		{"FILTER", "1J", "filter id number", 0},
		{"SOURCE", "1J", "source id number from source tbl", 0},
		{"FREQID", "1J", "freq id number from frequency tbl", 0},
		{"INTTIM", "1E", "time span of datum", "SECONDS"},
		{"WEIGHT", weightFormFloat, "weights proportional to time", 0},
		{"GATEID", gateFormInt, "gate id from gate model table", 0},
		{"FLUX", fluxFormFloat, "data matrix", "UNCALIB"}
	};

	/* allocate one DifxVis per job */

	scale = getDifxScaleFactor(D, opts->scale, opts->verbose);

	dvs = (DifxVis **)calloc(D->nJob, sizeof(DifxVis *));
	assert(dvs);
	for(j = 0; j < D->nJob; j++)
	{
		dvs[j] = newDifxVis(D, j);
		assert(dvs[j]);
		if(!dvs[j])
		{
			fprintf(stderr, "Error allocating DifxVis[%d/%d]\n",
				j, D->nJob);
			return 0;
		}
		dvs[j]->scale = scale;
	}

	/* for now set dv to the first job's structure */
	dv = dvs[0];

	nWeight = dv->nFreq*D->nPolar;

	/* Start up sniffer */
	if(opts->sniffTime > 0.0)
	{
		strcpy(fileBase, out->filename);
		l = strlen(fileBase);
		for(i = l-1; i > 0; i--)
		{
			if(fileBase[i] == '.')
			{
				fileBase[i] = 0;
				break;
			}
		}
#ifdef HAVE_FFTW
		S = newSniffer(D, dv->nComplex, fileBase, opts->sniffTime);
#endif
	}

	/* Start up jobmatrix */
	if(opts->jobMatrixDeltaT > 0)
	{
		jobMatrix = newJobMatrix(D, fileBase, opts->jobMatrixDeltaT);
	}

	/* set the number of weight and flux values*/
	sprintf(weightFormFloat, "%dE", nWeight);
	sprintf(gateFormInt, "%dJ", 0);
	sprintf(fluxFormFloat, "%dE", dv->nData);

	nColumn = NELEMENTS(columns);
	nRowBytes = FitsBinTableSize(columns, nColumn);

	fitsWriteBinTable(out, nColumn, columns, nRowBytes, "UV_DATA");
	fitsWriteInteger(out, "NMATRIX", 1, "");

	/* get the job ref_date from the fits_keyword struct, convert it into
	   a FITS string and save it in the FITS header */
	mjd2fits((int)D->mjdStart, dateStr);
	fitsWriteString(out, "DATE-OBS", dateStr, "");

// Soon! fitsWriteString(out, "EQUINOX", "J2000", "");
	fitsWriteString(out, "WEIGHTYP", "CORRELAT", "");

	fitsWriteString(out, "TELESCOP", "VLBA", "");
	fitsWriteString(out, "OBSERVER", "PLUTO", "");
	
	arrayWriteKeys(p_fits_keys, out);
	
	fitsWriteInteger(out, "TABREV", 2, "ARRAY changed to FILTER");
	fitsWriteFloat(out, "VIS_SCAL", visScale, "");

	fitsWriteString(out, "SORT", "T*", "");

	/* define the data matrix columns */
	fitsWriteInteger(out, "MAXIS", 6, "");
	fitsWriteInteger(out, "MAXIS1", dv->nComplex, "");

	fitsWriteString(out, "CTYPE1", "COMPLEX", "");
	fitsWriteFloat(out, "CDELT1", 1.0, "");
	fitsWriteFloat(out, "CRPIX1", 1.0, "");
	fitsWriteFloat(out, "CRVAL1", 1.0, "");
	fitsWriteInteger(out, "MAXIS2", D->nPolar, "");
	fitsWriteString(out, "CTYPE2", "STOKES", "");
	fitsWriteFloat(out, "CDELT2", -1.0, "");
	fitsWriteFloat(out, "CRPIX2", 1.0, "");
	fitsWriteFloat(out, "CRVAL2", (float)dv->polStart, "");
	fitsWriteInteger(out, "MAXIS3", D->nOutChan, "");
	fitsWriteString(out, "CTYPE3", "FREQ", "");
	fitsWriteFloat(out, "CDELT3", 
		D->chanBW*D->specAvg*1.0e6/D->nInChan, "");
	fitsWriteFloat(out, "CRPIX3", p_fits_keys->ref_pixel, "");
	fitsWriteFloat(out, "CRVAL3", D->refFreq*1000000.0, "");
	fitsWriteInteger(out, "MAXIS4", dv->nFreq, "");
	fitsWriteString(out, "CTYPE4", "BAND", "");
	fitsWriteFloat(out, "CDELT4", 1.0, "");
	fitsWriteFloat(out, "CRPIX4", 1.0, "");
	fitsWriteFloat(out, "CRVAL4", 1.0, "");
	fitsWriteInteger(out, "MAXIS5", 1, "");
	fitsWriteString(out, "CTYPE5", "RA", "");
	fitsWriteFloat(out, "CDELT5", 0.0, "");
	fitsWriteFloat(out, "CRPIX5", 1.0, "");
	fitsWriteFloat(out, "CRVAL5", 0.0, "");
	fitsWriteInteger(out, "MAXIS6", 1, "");
	fitsWriteString(out, "CTYPE6", "DEC", "");
	fitsWriteFloat(out, "CDELT6", 0.0, "");
	fitsWriteFloat(out, "CRPIX6", 1.0, "");
	fitsWriteFloat(out, "CRVAL6", 0.0, "");
	fitsWriteLogical(out, "TMATX11", 1, "");
	
	fitsWriteEnd(out);


	nJob = D->nJob;

	/* First prime each structure with some data */
	for(j = 0; j < nJob; j++)
	{
		readvisrecord(dvs[j], opts->verbose, opts->pulsarBin);
	}

	/* Now loop until done, looking at */
	while(nJob > 0)
	{
		bestmjd = 1.0e9;
		bestj = 0;
		for(j = 0; j < nJob; j++)
		{
			dv = dvs[j];
			mjd = (int)(dv->record->jd - 2400000.0) + 
				dv->record->iat;
			if(mjd < bestmjd)
			{
				bestmjd = mjd;
				bestj = j;
			}
		}
		dv = dvs[bestj];

		/* dv now points to earliest data. */

		
		if(RecordIsInvalid(dv))
		{
			nInvalid++;
		}
		else if(RecordIsFlagged(dv))
		{
			nFlagged++;
		}
		else if(RecordIsZero(dv))
		{
			nZero++;
		}
		else if(RecordIsTransitioning(dv))
		{
			nTrans++;
		}
		else
		{
#ifdef HAVE_FFTW
			feedSnifferFITS(S, dv->record);
#endif
			if(dv->record->baseline % 257 == 0)
			{
				feedJobMatrix(jobMatrix, dv->record, dv->jobId);
			}
#ifndef WORDS_BIGENDIAN
			FitsBinRowByteSwap(columns, nColumn, 
				dv->record);
#endif

			fitsWriteBinRow(out, (char *)dv->record);
			nWritten++;
		}
		if(dv->changed < 0)
		{
			deleteDifxVis(dv);
			nJob--;
			dvs[bestj] = dvs[nJob];
		}
		else
		{
			v = DifxVisCollectRandomParams(dv);
			if(v < 0)
			{
				fprintf(stderr, "Error in "
					"DifxVisCollectRandomParams : "
					"return value = %d\n", v);
#ifdef HAVE_FFTW
				deleteSniffer(S);
#endif
				return -3;
			}

			readvisrecord(dv, opts->verbose, opts->pulsarBin);
		}
	}

	printf("      %d invalid records dropped\n", nInvalid);
	printf("      %d flagged records dropped\n", nFlagged);
	printf("      %d all zero records dropped\n", nZero);
	printf("      %d scan boundary records dropped\n", nTrans);
	printf("      %d records written\n", nWritten);
	if(opts->verbose > 1)
	{
		printf("        Note : 1 record is all data from 1 baseline\n");
		printf("        for 1 timestamp\n");
	}

	free(dvs);

#ifdef HAVE_FFTW
	deleteSniffer(S);
#endif
	if(jobMatrix)
	{
		writeJobMatrix(jobMatrix);
		deleteJobMatrix(jobMatrix);
	}

	return 0;
}

/* FIXME -- merge this function with DifxVisConvert */
const DifxInput *DifxInput2FitsUV(const DifxInput *D,
	struct fits_keywords *p_fits_keys,
	struct fitsPrivate *out, struct CommandLineOptions *opts)
{
	if(D == 0)
	{
		return 0;
	}

	printf("\n");
	DifxVisConvert(D, p_fits_keys, out, opts);
	printf("                            ");

	return D;
}


/***************************************************************************
 *   Copyright (C) 2008-2022 by Walter Brisken & Adam Deller               *
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
// $Id: fitsUV.c 10998 2023-06-21 18:29:27Z JanWagner $
// $HeadURL: https://svn.atnf.csiro.au/difx/master_tags/DiFX-2.8.1/applications/difx2fits/src/fitsUV.c $
// $LastChangedRevision: 10998 $
// $Author: JanWagner $
// $LastChangedDate: 2023-06-22 02:29:27 +0800 (四, 2023-06-22) $
//
//============================================================================
#include "fits.h"

#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <assert.h>
#include <difxio/parsevis.h>
#include <regex.h>
#include <limits.h>
#include "config.h"
#include "fitsUV.h"
#include "jobmatrix.h"
#include "util.h"
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

		exit(EXIT_FAILURE);
	}
	dv->weight = dv->record->data;
	dv->data = dv->weight + (dv->nFreq*dv->D->nPolar);
	dv->sourceId = -1;
	dv->scanId = -1;
#warning "FIXME: here add a similar thing for gateId?"

	return 0;
}

static void DifxVisStartGlob(DifxVis *dv)
{
	char *globPattern;
	const char suffix[] = "/DIFX*";
	int v;

	globPattern = calloc(strlen(dv->D->job[dv->jobId].outputFile) + strlen(suffix) + 8, 1);
	if(globPattern == 0)
	{
		assert(globPattern);

		exit(EXIT_FAILURE);
	}

	sprintf(globPattern, "%s%s", dv->D->job[dv->jobId].outputFile, suffix);

	v = glob2(__FUNCTION__, globPattern, 0, 0, &dv->globbuf);	/* see util.c */
	dv->nFile = dv->globbuf.gl_pathc;

	if(dv->nFile == 0 || v == GLOB_NOMATCH)
	{
		fprintf(stderr, "Error: no data files in %s\n", dv->D->job[dv->jobId].outputFile);

		exit(EXIT_FAILURE);
	}	

	dv->globbuf.gl_offs = 0;

	free(globPattern);
}

static int re2int(const char *str, const regmatch_t *subexpression)
{
	const int MaxNumLen = 16;
	char tmp[MaxNumLen+1];
	int n;
	n = subexpression->rm_eo - subexpression->rm_so;

	if(n > MaxNumLen)
	{
		fprintf(stderr, "Error: re2int: integer too long (%d characters)\n", n);

		exit(EXIT_FAILURE);
	}

	strncpy(tmp, str + subexpression->rm_so, n);
	tmp[n] = 0;

	return atoi(tmp);
}

int DifxVisNextFile(DifxVis *dv)
{
	int antennaId;
	int v, bin, pc;
	regex_t fileMatch;
	regmatch_t fileSubexpressions[5];

	/* FIXME: someday: move this compilation to top level? */
	v = regcomp(&fileMatch, "\\S*/DIFX_([0-9]+)_([0-9]+)\\.s([0-9]+)\\.b([0-9]+)", REG_EXTENDED);
	if(v != 0)
	{
		fprintf(stderr, "Developer error: DifxVisNextFile: regular expresson won't compile\n");

		exit(EXIT_FAILURE);
	}

	if(dv->in)
	{
		fclose(dv->in);
		dv->in = 0;
		dv->nRec = 0;
	}

	while(dv->in == 0)
	{
		++dv->curFile;
		if(dv->curFile >= dv->nFile)
		{
			if(dv->keepAC == 0)
			{
				printf("    JobId %d XC done.\n", dv->jobId);
			}
			else if(dv->keepXC == 0)
			{
				printf("    JobId %d AC done.\n", dv->jobId);
			}
			else
			{
				printf("    JobId %d done.\n", dv->jobId);
			}
			regfree(&fileMatch);

			return -2;
		}
		if(regexec(&fileMatch, dv->globbuf.gl_pathv[dv->curFile], 5, fileSubexpressions, 0) != 0)
		{
			printf("\nWarning: File %s found which will be ignored because it does not match the expected filename convention for DiFX output files.\n\n", dv->globbuf.gl_pathv[dv->curFile]);

			continue;
		}

		/* The filename matched the template expression */

		/* slot 3 of the re match should be phase center number */
		pc  = re2int(dv->globbuf.gl_pathv[dv->curFile], fileSubexpressions+3);
		/* slot 4 of the re match should be pulsar bin number */
		bin = re2int(dv->globbuf.gl_pathv[dv->curFile], fileSubexpressions+4);

		if(bin == dv->pulsarBin && pc == dv->phaseCentre)
		{
			printf("    JobId %d file %d/%d : %s\n", dv->jobId, dv->curFile+1, dv->nFile, dv->globbuf.gl_pathv[dv->curFile]);
			dv->in = fopen(dv->globbuf.gl_pathv[dv->curFile], "r");
			if(dv->in == 0)
			{
				/* should not have gotten here */
				fprintf(stderr, "Error: cannot open visibility file for read: %s\n", dv->globbuf.gl_pathv[dv->curFile]);
				fprintf(stderr, "Check permissions.  Failure _could_ be related to exhausting file descriptors\n");

				exit(EXIT_FAILURE);
			}
		}
	}

	regfree(&fileMatch);

	if(!dv->in)
	{
		fprintf(stderr, "Error opening file %s\n", dv->globbuf.gl_pathv[dv->curFile]);

		return -1;
	}

	/* Inc the last timestamp by epsilon */
	for(antennaId = 0; antennaId < dv->D->nAntenna; ++antennaId)
	{
		dv->mjdLastRecord[antennaId] += 0.05/86400.0;
	}

	return 0;
}

struct vvDatum
{
	int b1, b2;		/* quantization bits for antennas 1 and 2 */
	double factor;		/* van Vleck correction factor (near zero correlation) */
};
static double vanVleck(int b1, int b2)
{
	const struct vvDatum vvData[] = 
	{
		{1, 1, 2.0/M_PI},	/* analytic value */
		{1, 2, 0.752},
		{1, 4, 0.794},
		{1, 8, 0.795},
		{2, 2, 0.882518},	/* exact value for continuity with AIPS */
		{2, 4, 0.934},
		{2, 8, 0.934},
		{4, 4, 0.988},
		{4, 8, 0.989},
		{8, 8, 0.991},

		{0, 0, 0}		/* list terminator */
	};
	int i;

	for(i = 0; vvData[i].b1 > 0; ++i)
	{
		if((vvData[i].b1 == b1 && vvData[i].b2 == b2) || (vvData[i].b1 == b2 && vvData[i].b2 == b1))
		{
			return vvData[i].factor;
		}
	}

	return 0.0;	/* effectively an error code */
}

/* returns an overall polarization- and baseline-based scale factor */
static double ***getDifxScaleFactor(const DifxInput *D, const struct CommandLineOptions *opts)
{
	double *antScale;
	int *bits;
	double ***scale;
	int polId, antennaId, a1, a2;
	
	antScale = (double *)calloc(D->nAntenna, sizeof(double));
	bits = (int *)calloc(D->nAntenna, sizeof(int));
	if(!antScale || !bits)
	{
		fprintf(stderr, "\nError: getDifxScaleFactor: cannot allocate %d doubles for antScale factor and/or %d ints for bits\n", D->nAntenna, D->nAntenna);

		exit(EXIT_FAILURE);
	}

	scale = (double ***)calloc(4, sizeof(double **));
	if(!scale)
	{
		fprintf(stderr, "\nError: getDifxScaleFactor: cannot allocate 4 **double for scale factor\n");

		exit(EXIT_FAILURE);
	}
	for(polId = 0; polId < 4; ++polId)
	{
		scale[polId] = (double **)calloc(D->nAntenna, sizeof(double *));
		if(!scale[polId])
		{
			fprintf(stderr, "\nError: getDifxScaleFactor: cannot allocate %d *double for scale factor\n", D->nAntenna);

			exit(EXIT_FAILURE);
		}
		for(antennaId = 0; antennaId < D->nAntenna; ++antennaId)
		{
			scale[polId][antennaId] = (double *)calloc(D->nAntenna, sizeof(double));
			if(!scale[polId][antennaId])
			{
				fprintf(stderr, "\nError: getDifxScaleFactor: cannot allocate %d doubles for scale factor\n", D->nAntenna);

				exit(EXIT_FAILURE);
			}
		}
	}

	for(antennaId = 0; antennaId < D->nAntenna; ++antennaId)
	{
		/* Now all scale factors here are antenna-based voltage scalings -WFB 20120312 */

		const int maxDatastreams = 8;
		int dsIds[maxDatastreams];
		int n;
		int quantBits;

		n = DifxInputGetDatastreamIdsByAntennaId(dsIds, D, antennaId, maxDatastreams);

		if(n < 1)	/* should never happen */
		{
			quantBits = D->quantBits;	/* a fallback in case of oddness */
		}
		else
		{
			quantBits = D->datastream[dsIds[0]].quantBits;
		}
		bits[antennaId] = quantBits;

		/* An empirical fudge factor used to get scaling same as for VLBA hardware correlator */
		antScale[antennaId] = 2.1392;

		if(quantBits == 2)
		{
			antScale[antennaId] /= 3.3359;
		}
		else if(quantBits == 4 || quantBits == 8)
		{
			antScale[antennaId] /= 7.5;		/* FIXME: this will need some tuning */
		}
		// Add other options below here...

		if(opts->scale > 0.0)
		{
			static int first = 1;

			if(opts->doVanVleck)
			{
				fprintf(stderr, "\nError: getDifxScaleFactor: cannot both apply provided scale factor and van Vleck correction\n");

				exit(EXIT_FAILURE);
			}

			if(first)
			{
				first = 0;
				printf("      Overriding antenna %d scale factor %e with %e\n", antennaId, antScale[antennaId], opts->scale);
			}
			antScale[antennaId] = opts->scale;
		}
	}

	for(a2 = 0; a2 < D->nAntenna; ++a2)
	{
		for(a1 = 0; a1 < D->nAntenna; ++a1)
		{
			for(polId = 0; polId < 4; ++polId)
			{
				scale[polId][a2][a1] = antScale[a2]*antScale[a1];
			}
			if(opts->doVanVleck)
			{
				double vv;

				vv = vanVleck(bits[a1], bits[a2]);
				if(vv <= 0.0)
				{
					fprintf(stderr, "\nError: getDifxScaleFactor: van Vleck correction not programmed in for %d bits by %d bits\n", bits[a1], bits[a2]);

					exit(EXIT_FAILURE);
				}
				for(polId = 0; polId < 2; ++polId)	/* polId = 0, 1 are parallel hands */
				{
					if(a1 != a2)	/* don't correct parallel handed autocorrs */
					{
						scale[polId][a2][a1] /= vv;
					}
				}
				for(polId = 2; polId < 4; ++polId)	/* polId = 2, 3 are cross hands */
				{
					scale[polId][a2][a1] /= vv;
				}
			}
		}
	}

	free(antScale);
	free(bits);

	return scale;
}

DifxVis *newDifxVis(const DifxInput *D, int jobId, const struct CommandLineOptions *opts, int pulsarBin, int phaseCentre)
{
	DifxVis *dv;
	int freqSetId;
	int v;
	int polMask = 0;

	if(jobId < 0 || jobId >= D->nJob)
	{
		fprintf(stderr, "Error: newDifxVis: jobId = %d, nJob = %d\n", jobId, D->nJob);
		
		return 0;
	}

	dv = (DifxVis *)calloc(1, sizeof(DifxVis));
	if(!dv)
	{
		fprintf(stderr, "Error: newDifxVis: dv=calloc failed, size=%d\n", (int)(sizeof(DifxVis)));
		assert(dv);

		exit(EXIT_FAILURE);
	}
	
	dv->jobId = jobId;
	dv->D = D;
	dv->curFile = -1;
	DifxVisStartGlob(dv);
	dv->configId = -1;
	dv->sourceId = -1;
	dv->scanId = -1;
	dv->baseline = -1;
	dv->scale = getDifxScaleFactor(D, opts);
	dv->pulsarBin = pulsarBin;
	dv->phaseCentre = phaseCentre;
	dv->keepAC = 1;
	dv->keepXC = 1;
	dv->maxRec = INT_MAX;

	/* For now, the difx format only provides 1 weight for the entire
	 * vis record, so we don't need weights per channel */
	dv->nComplex = 2;	/* for now don't write weights */
	dv->first = 1;
	
	for(freqSetId = 0; freqSetId < D->nFreqSet; ++freqSetId)
	{
		int i;

		if(D->nIF > dv->nFreq)
		{
			dv->nFreq = D->nIF;
		}
		for(i = 0; i < D->freqSet[freqSetId].nIF; ++i)
		{
			int p;

			for(p = 0; p < D->freqSet[freqSetId].IF[i].nPol; ++p)
			{
				polMask |= polMaskValue(D->freqSet[freqSetId].IF[i].pol[p]);
			}
		}
	}

	dv->mjdLastRecord = (double *)calloc(D->nAntenna, sizeof(double));

	/* check for polarization confusion */
	if(!opts->antpol)
	{
		if(isMixedPolMask(polMask) ||
			(polMask & DIFXIO_POL_ERROR) != 0 ||
			(polMask == 0))
		{
			fprintf(stderr, "Error: bad polarization combinations : %x\n", polMask);
			deleteDifxVis(dv);

			return 0;
		}

		if(polMask & DIFXIO_POL_R)
		{
			dv->polStart = -1;
		}
		else if(polMask & DIFXIO_POL_L)
		{
			dv->polStart = -2;
		}
		else if(polMask & DIFXIO_POL_X)
		{
			dv->polStart = -5;
		}
		else /* must be YY only! who would do that? */
		{
			dv->polStart = -6;
		}
	}

	/* room for input vis record: 3 for real, imag, weight */
	dv->spectrum = (float *)calloc(3*dv->D->nInChan, sizeof(float));
	assert(dv->spectrum);

	dv->dp = newDifxParameters();

	v = DifxVisInitData(dv);
	if(v < 0)
	{
		fprintf(stderr, "Error %d allocating %d + %d bytes\n", v, (int)(sizeof(struct UVrow)), dv->nComplex*dv->nFreq*dv->D->nPolar*dv->D->nOutChan);
		deleteDifxVis(dv);
		
		return 0;
	}

	v = DifxVisNextFile(dv);
	if(v < 0)
	{
		deleteDifxVis(dv);
		if(v == -2)	/* could not open file */
		{
			fprintf(stderr, "Error: zero files openable for jobId = %d!\n", jobId);
			fprintf(stderr, "Fits creation failed; partial FITS file left hanging around\n");

			exit(EXIT_FAILURE);
		}
		else
		{
			fprintf(stderr, "Info: newDifxVis: DifxVisNextFile(dv) < 0\n");
		}
		
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
		dv->in = 0;
	}
	if(dv->spectrum)
	{
		free(dv->spectrum);
		dv->spectrum = 0;
	}
	if(dv->dp)
	{
		deleteDifxParameters(dv->dp);
		dv->dp = 0;
	}
	if(dv->record)
	{
		free(dv->record);
		dv->record = 0;
	}
	if(dv->mjdLastRecord)
	{
		free(dv->mjdLastRecord);
		dv->mjdLastRecord = 0;
	}
	if(dv->scale)
	{
		int a, p;

		for(p = 0; p < 4; ++p)
		{
			for(a = 0; a < dv->D->nAntenna; ++a)
			{
				free(dv->scale[p][a]);
			}
			free(dv->scale[p]);
		}
		free(dv->scale);
		dv->scale = 0;
	}
	
	free(dv);
}


static int getPolProdId(const DifxVis *dv, const char *polPair, const struct CommandLineOptions *opts)
{
	const char polSeq[8][4] = {"RR", "LL", "RL", "LR", "XX", "YY", "XY", "YX"};
	const char pol1st[3] = {'R', 'X', 'H'};
	const char pol2nd[3] = {'L', 'Y', 'V'};
	int p, ind_1st, ind_2nd;

	if(!opts->antpol)
	{
		for(p = 0; p < 8; ++p)
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
	}
	else
	{
//
// --------- Case of antenna-based polarizaiton (opts->antpol == 0)
// --------- Polarization order: A1A2  B1B2  A1B2  B1A2, where
// --------- 1,2 are antenna indices and 
// --------- A is the 1st polarization, B is the 2nd polarization
//
		ind_1st = 0;
		ind_2nd = 0;
		p = -2; /* intialize with the error code */
		for(p = 0; p < 3; ++p)
		{
			if(polPair[0] == pol1st[p])
			{
				ind_1st = 1;
			}
			if(polPair[1] == pol1st[p])
			{
				ind_2nd = 1;
			}
			if(polPair[0] == pol2nd[p])
			{
				ind_1st = 2;
			}
			if(polPair[1] == pol2nd[p])
			{
				ind_2nd = 2;
			}
		}
		if(ind_1st == 1 && ind_2nd == 1)
		{
			p = 0;
		}
		if(ind_1st == 2 && ind_2nd == 2)
		{
			p = 1;
		}
		if(ind_1st == 1 && ind_2nd == 2)
		{
			p = 2;
		}
		if(ind_1st == 2 && ind_2nd == 1)
		{
			p = 3;
		}

		return p;
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

	for(i = n-2; i >= 0; --i)
	{
		y = x*y + p[i];
	}

	return y;
}
	
/* Auxilliary debugging routine for printing the contents of visilibity record */
static void UVfitsDump(const DifxVis *dv, double utcmin, double utcmax)
{
	int a1, a2, i,j,k,m;
	int startChan;
	const double eps = 0.01;
	const DifxInput *D;

	D = dv->D;
	startChan = D->startChan;

	a1 = dv->record->baseline % 256 - 1;
	a2 = dv->record->baseline / 256 - 1;

	m=dv->nFreq*dv->D->nPolar;

	if(dv->record->utc*86400.0 < utcmax + eps  && dv->record->utc*86400.0 > utcmin - eps)
	{
		printf("UV head utc: %9.7f ista %1d %1d fi %3d pol %1d start: %d stop: %d nfreq: %3d npol: %1d ncha: %4d nd: %8d wei %f scale: %g\n",
			dv->record->utc*86400.0, a1, a2, dv->record->freqId1, dv->polId,
			startChan, startChan, dv->nFreq, dv->D->nPolar, dv->D->nOutChan,
			dv->nData, dv->record->data[0], dv->scale[dv->polId][a1][a2]);
		printf("UV head2 dv->D->nInChan %d dv->nComplex: %d dv->baseline: %d dv->record->baseline: %d m= %3d \n", dv->D->nInChan, dv->nComplex, dv->baseline, dv->record->baseline, m);
		for(i = 0; i < dv->nFreq; ++i)
		{
			for(j = 0; j < dv->D->nOutChan; ++j)
			{
				for(k = 0; k < dv->D->nPolar; ++k)
				{
					printf("UV MJD: %5.0f utc: %9.3f sta: %1d %1d If: %2d Ic: %3d Ip: %1d vis: %14.7e, %14.7e \n",
						dv->record->jd - 2400000.5, dv->record->utc*86400.0, a1, a2, i, j, k,
						dv->record->data[m], dv->record->data[m+1]);
					m=m+2;
				}
			}
		}
	}
}

int DifxVisNewUVData(DifxVis *dv, const struct CommandLineOptions *opts, const DelayCal *DC, DelayCalCache *DCC, const Bandpass *B)
{
	int i, i1, v;
	int antId1, antId2;	/* These reference the DifxInput Antenna */
	int bl;			/* bl number computed from antId1 and antId2 */
	int dsId1, dsId2;	/* This refers to DifxInput Datastream table */
	int scanId, binHeaderVersion, headerConfigIndex, intmjd;
	double mjd, utc, dt, dt2, weight;
	double uvw[3];
	char polPair[3];
	int changed = 0;
	int nFloat, readSize;
	int freqId;
	int configId;
	const DifxConfig *config;
	const DifxFreqSet *dfs;
	const DifxScan *scan;
	const DifxPolyModel *im1, *im2;
	int terms1, terms2;
	int configBaselineId, configAntennaId1, configAntennaId2;	/* These are local to the config */
	int bin, sourceId, sync;

	resetDifxParameters(dv->dp);

	//first of all, figure out what kind of header we are dealing with
	v = fread(&sync, sizeof(int), 1, dv->in);
	if(v != 1 || dv->nRec >= dv->maxRec)
	{
		v = DifxVisNextFile(dv);
		if(v < 0)
		{
			return NEXT_FILE_ERROR;
		}
		v = fread(&(sync), sizeof(int), 1, dv->in);
	}
	++dv->nRec;

	if(sync == VISRECORD_SYNC_WORD_DIFX1) //old style ascii header
	{
		fprintf(stderr, "Error: This version of difx2fits will not work with DiFX 1.x data\n");

		return HEADER_READ_ERROR;
	}
	else if(sync == VISRECORD_SYNC_WORD_DIFX2) //new style binary header
	{
		v = fread(&binHeaderVersion, sizeof(int), 1, dv->in);
		if(v != 1)
		{
			fprintf(stderr, "Error reading DiFX output header version\n");

			return HEADER_READ_ERROR;
		}
		else if(binHeaderVersion == 1) //new style binary header
		{
			const int headerFields = 13;	/* should equal sum of 3rd argument of following freads */

			v  = fread(&configBaselineId, sizeof(int), 1, dv->in);
			v += fread(&intmjd, sizeof(int), 1, dv->in);
			mjd = intmjd;
			v += fread(&utc, sizeof(double), 1, dv->in);
			utc /= 86400.0;	/* convert seconds to days */
			v += fread(&headerConfigIndex, sizeof(int), 1, dv->in);
			v += fread(&sourceId, sizeof(int), 1, dv->in);
			v += fread(&freqId, sizeof(int), 1, dv->in);
			v += fread(polPair, 1, 2, dv->in);
			polPair[2] = 0;
			v += fread(&bin, sizeof(int), 1, dv->in);
			v += fread(&weight, sizeof(double), 1, dv->in);
			v += fread(uvw, sizeof(double), 3, dv->in);
			if(v != headerFields)
			{
				fprintf(stderr, "Error parsing header: %d fields read; %d expected.\n", v, headerFields);

				return HEADER_READ_ERROR;
			}
			if(opts->verbose > 3)
			{
				fprintf(stdout, "Read a vis from baseline %d\n", configBaselineId);
			}
		}
		else //dunno what to do
		{
			fprintf(stderr, "Error parsing header: got a sync of %x and version of %d\n", sync, binHeaderVersion);

			return HEADER_READ_ERROR;
		}
	}
	else
	{
		fprintf(stderr, "Error parsing header: got an unrecognized sync of %xd\n", sync);

		return HEADER_READ_ERROR;
	}

	/* adjust freqId through remapping if needed */
	if(dv->D->job[dv->jobId].freqIdRemap)
	{
		freqId = dv->D->job[dv->jobId].freqIdRemap[freqId];
	}

	/* if chan weights are written the data volume is 3/2 as large */
	/* for now, force nFloat = 2 (one weight for entire vis record) */
	nFloat = 2;
	readSize = nFloat * dv->D->nOutChan;
	v = fread(dv->spectrum, sizeof(float), readSize, dv->in);
	if(v < readSize)
	{
		return DATA_READ_ERROR;
	}

	if( (configBaselineId % 257 == 0 && !dv->keepAC) ||
	    (configBaselineId % 257 != 0 && !dv->keepXC) )
	{
		return SKIPPED_RECORD;
	}
//	dv->pulsarBin = bin;

#warning "FIXME: look at sourceId in the record as a check"
#warning "FIXME: look at configId in the record as a check"

	/* scanId at middle of integration */
	scanId = DifxInputGetScanIdByJobId(dv->D, mjd+utc, dv->jobId);
	if(scanId < 0)
	{
		if(opts->verbose > 2)
		{
			printf("ScanID < 0 (= %d); skipping record\n", scanId);
		}

		return SKIPPED_RECORD;
	}

	scan = dv->D->scan + scanId;
	configId = scan->configId;
	if(configId >= dv->D->nConfig) 
	{
		fprintf(stderr, "Developer error: DifxVisNewUVData: configId=%d  nConfig=%d  scanId=%d", configId, dv->D->nConfig, scanId);

		exit(EXIT_FAILURE);
	}
	if(configId < 0)
	{
		fprintf(stderr, "configId doesn't match: skipping!\n");
		
		return SKIPPED_RECORD;
	}
	if(dv->phaseCentre >= scan->nPhaseCentres)
	{
		return SKIPPED_RECORD;
	}
	if(sourceId != scan->orgjobPhsCentreSrcs[dv->phaseCentre] && (configBaselineId % 257 != 0) ) //don't skip autocorrelations
	{
		if(opts->verbose > 2)
		{
			printf("Skipping record with sourceId %d because orgjobphaseCentresrc[%d] is %d\n", sourceId, dv->phaseCentre, scan->orgjobPhsCentreSrcs[dv->phaseCentre]);
		}

		return SKIPPED_RECORD;
	}

	config = dv->D->config + configId;
	dfs = dv->D->freqSet + config->freqSetId;

	/* see if it is still the same scan at the edges of integration */
	dt2 = config->tInt/(86400.0*2.0001);  
	if(scan->mjdStart > mjd+utc-dt2 || scan->mjdEnd < mjd+utc+dt2)
	{
		/* Nope! */
		dv->flagTransition = 1;
		if(opts->verbose > 2)
		{
			printf("Flag transition: mjd range=%12.6f to %12.6f\n", mjd+utc-dt2, mjd+utc+dt2);
		}

		return SKIPPED_RECORD;
	}
	else
	{
		dv->flagTransition = 0;
	}

	if(dfs->freqId2IF[freqId] < 0)
	{
		if(opts->verbose > 2)
		{
			printf("Freq not used: freqId= %d freqSetId= %d configId= %d, ind= %d.  Skipping record.\n", freqId, config->freqSetId, configId, dfs->freqId2IF[freqId]);
		}

		return SKIPPED_RECORD;
	}

	configAntennaId1 = (configBaselineId/256) - 1;
	configAntennaId2 = (configBaselineId%256) - 1;

	if(configAntennaId1 < 0 || configAntennaId1 >= config->nAntenna ||
	   configAntennaId2 < 0 || configAntennaId2 >= config->nAntenna)
	{
		printf("Error: illegal config[%d] baseline %d -> %d-%d\n", configId, configBaselineId, configAntennaId1, configAntennaId2);
		
		return -8;
	}

	/* translate from .input file antId to index for D->antenna via dsId */
	dsId1 = config->ant2dsId[configAntennaId1];
	dsId2 = config->ant2dsId[configAntennaId2];
	if(dsId1 < 0 || dsId1 >= dv->D->nDatastream || 
	   dsId2 < 0 || dsId2 >= dv->D->nDatastream)
	{
		printf("Error: baseline %d -> datastreams %d-%d\n", configBaselineId, dsId1, dsId2);

		return -9;
	}

	/* These are DifxInput-wide antenna and baseline ids */
	antId1 = dv->D->datastream[dsId1].antennaId;
	antId2 = dv->D->datastream[dsId2].antennaId;
	bl = (antId1+1)*256 + (antId2+1);
	
	if(opts->verbose >= 1 && scanId != dv->scanId)
	{
		printf("        MJD=%11.5f jobId=%d scanId=%d dv->scanId=%d Source=%s  FITS SourceId=%d\n", 
			mjd+utc, dv->jobId, scanId, dv->scanId, 
			dv->D->source[scan->phsCentreSrcs[dv->phaseCentre]].name, 
			dv->D->source[scan->phsCentreSrcs[dv->phaseCentre]].fitsSourceIds[config->freqSetId]+1);
	}

	dv->scanId   = scanId;
	dv->sourceId = scan->phsCentreSrcs[dv->phaseCentre];
	dv->freqId   = config->freqSetId;
	dv->bandId   = dfs->freqId2IF[freqId];
	dv->polId    = getPolProdId(dv, polPair, opts);

	/* freqId should correspond to the freqId table for the actual sideband produced in the 
	 * case of mixed-sideband correlation.  Check with Adam that this is true! */
	dv->sideband = dv->D->freq[freqId].sideband;

	/* stash the weight for later incorporation into a record */
	dv->recweight = weight;

	if(bl != dv->baseline || fabs((mjd-dv->mjd) + (utc-dv->utc))  > 1.0/86400000.0)
	{
		changed = 1;
		dv->baseline = bl;
		dv->mjd = mjd;
		dv->utc = utc;

		/* swap phase/uvw for FITS-IDI conformance */
		dv->U = -uvw[0];
		dv->V = -uvw[1];
		dv->W = -uvw[2];

		/* recompute from polynomials if possible */
		if(scan->im)
		{
			double u, v, w;
			int n;

			n = getDifxScanIMIndex(scan, mjd, utc, &dt);

			u = dv->U;
			v = dv->V;
			w = dv->W;

			/* use .difx/ antenna indices for model tables */
			if(scan->im[antId1] && scan->im[antId2])
			{
				//printf("About to look at the actual im object\n");
				im1 = scan->im[antId1][dv->phaseCentre + 1];
				im2 = scan->im[antId2][dv->phaseCentre + 1];
				//printf("Got the im structures - they are %p and %p\n", im1, im2);
				if(!(im1 && im2))
				{
					fprintf(stderr, "Warning: one or the other antenna models is missing (im1=%p, im2=%p)\n", im1, im2);
				}
				else if(n < 0)
				{
					fprintf(stderr, "Error: interferometer model index out of range: scanId=%d mjd=%12.6f\n", scanId, mjd+utc);
				}
				else
				{
					terms1 = im1->order + 1;
					terms2 = im2->order + 1;
					dv->U = evalPoly(im2[n].u, terms2, dt) - evalPoly(im1[n].u, terms1, dt);
					dv->V = evalPoly(im2[n].v, terms2, dt) - evalPoly(im1[n].v, terms1, dt);
					dv->W = evalPoly(im2[n].w, terms2, dt) - evalPoly(im1[n].w, terms1, dt);
				}
			}
			else
			{
				printf("Cannot check UVW values!\n");
			}

			if((fabs(u - dv->U) > 10.0 ||
			    fabs(v - dv->V) > 10.0 ||
			    fabs(w - dv->W) > 10.0) && 
			    !dv->flagTransition) 
			{
				printf("Warning: UVW diff: %d %d %d-%d %f %f  %f %f  %f %f  %f %f\n", scanId, n, antId1, antId2, mjd+utc, dt, u, dv->U, v, dv->V, w, dv->W);
			}
		}
	}

	if(configId != dv->configId)
	{
		if(!changed)	/* cannot change config within integration */
		{
			fprintf(stderr, "configId changes within integration: skipping!\n");

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

	if(dv->bandId < 0 || dv->bandId >= dv->D->freqSet[dv->freqId].nIF)
	{
		fprintf(stderr, "Baseline %d: Parameter problem: bandId should be in [0, %d), was %d\n", dv->baseline, dv->D->freqSet[dv->freqId].nIF, dv->bandId);
		
		return BAND_ID_ERROR;
	}
	
	if(dv->polId < 0 || dv->polId >= dv->D->nPolar)
	{
		if(configBaselineId % 257 == 0 && opts->skipExtraAutocorrs)
		{
			// Silently ignore e.g. LL autocorrelations in an RR-only correlation
			return SKIPPED_RECORD;
		}
		else
		{
			fprintf(stderr, "Baseline %d: Parameter problem: polId should be in [0, %d), was %d; polPair was '%s'\n", dv->baseline, dv->D->nPolar, dv->polId, polPair);
			if(configBaselineId % 257 == 0) //Tell the user how to get around this problem
			{
				fprintf(stderr, "This polarisation error was generated by an autocorrelation.  To skip autocorrelations which don't correspond to any computed cross-correlations, re-run difx2fits with --skip-extra-autocorrs\n");
			}

			return POL_ID_ERROR;
		}
	}

	/* apply delay calibration and/or bandpass if provided */
	if(nFloat == 2)	/* currently this is all that is supported */
	{
		if(DC)
		{
			double df;

			df = dv->D->freq[freqId].bw / dv->D->nOutChan;
			if(dv->D->freq[freqId].sideband == 'L')
			{
				df = -df;
			}
			applyDelayCal((float complex *)(dv->spectrum), dv->D->nOutChan, df, freqId, antId1, polPair[0], antId2, polPair[1], mjd+utc, DC, DCC);
		}
		if(B)
		{
			applyBandpass((float complex *)(dv->spectrum), dv->D->nOutChan, freqId, antId1, polPair[0], antId2, polPair[1], B);
		}
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
		for(i = 3*dv->D->nOutChan-3; i > 0; i -= 3)
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
	for(i = 0; i < dv->D->nOutChan; ++i)
	{
		dv->spectrum[i*dv->nComplex] *= dv->recweight;
		dv->spectrum[i*dv->nComplex+1] *= dv->recweight;
	}

	return changed;
}

int DifxVisCollectRandomParams(const DifxVis *dv)
{
	const double cLight=2.99792458e8;	/* speed of light in m/s */
	int freqSetId;

	freqSetId = dv->D->config[dv->configId].freqSetId;
	
	dv->record->U		= dv->U/cLight;
	dv->record->V		= dv->V/cLight;
	dv->record->W		= dv->W/cLight;

	dv->record->jd		= 2400000.5 + dv->mjd;
	dv->record->utc		= dv->utc;

	/* reminder: antennaIds, sourceId, freqId are 1-based in FITS */
	dv->record->baseline	= dv->baseline;
	dv->record->filter	= 0;
	dv->record->sourceId1	= dv->D->source[dv->sourceId].fitsSourceIds[freqSetId] + 1;
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

	for(i = 0; i < n; ++i)
	{
		if(isnan(d[i]) || isinf(d[i]))
		{
			printf("Warning: record with !finite value: a1=%d a2=%d mjd=%13.7f\n",
				(dv->record->baseline/256) - 1,
				(dv->record->baseline%256) - 1,
				dv->mjd + dv->utc);

			return 1;
		}
	}
	for(i = 0; i < n; ++i)
	{
		if(d[i] > 1.0e10 || d[i] < -1.0e10)
		{
			printf("Warning: record with extreme value: a1=%d a2=%d mjd=%13.7f value=%e\n",
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

	for(i = 0; i < n; ++i)
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

static int RecordIsOld(DifxVis *dv)
{
	int antennaId1, antennaId2;
	double mjd;

	mjd = dv->mjd + dv->utc;
	antennaId1 = (dv->record->baseline/256) - 1;
	antennaId2 = (dv->record->baseline%256) - 1;
	
	if(mjd < dv->mjdLastRecord[antennaId1] || mjd < dv->mjdLastRecord[antennaId2])
	{
		return 1;
	}
	else
	{
		dv->mjdLastRecord[antennaId1] = mjd;
		dv->mjdLastRecord[antennaId2] = mjd;
	}

	return 0;
}

static int RecordHasNegativeWeight(const DifxVis *dv)
{
	return (dv->recweight < 0.0);
}

static int RecordIsTransitioning(const DifxVis *dv)
{
	return dv->flagTransition;
}

static int RecordIsFlagged(const DifxVis *dv)
{
	DifxJob *job;
	double mjd;
	int antennaId1, antennaId2;
	int flagId;

	job = dv->D->job + dv->jobId;

	if(job->nFlag <= 0)
	{
		return 0;
	}

	mjd = (int)(dv->record->jd - 2400000.0) + dv->record->utc;
	antennaId1 = (dv->record->baseline/256) - 1;
	antennaId2 = (dv->record->baseline%256) - 1;

	for(flagId = 0; flagId < job->nFlag; ++flagId)
	{
		if(job->flag[flagId].mjd1 <= mjd &&
		   job->flag[flagId].mjd2 >= mjd)
		{
			if(job->flag[flagId].antennaId == antennaId1 ||
			   job->flag[flagId].antennaId == antennaId2)
			{
				return 1;
			}
		}
	}

	return 0;
}

static int storevis(DifxVis *dv)
{
	const DifxInput *D;
	int startChan;
	int stopChan;
	int i;
	double scale;

	if(dv->configId < 0)
	{
		return -1;
	}

	if(dv->scale)
	{	
		/* form baseline-based power scale factor from antenna-based voltage scale factors */
		int a1, a2;

		a1 = dv->baseline % 256 - 1;
		a2 = dv->baseline / 256 - 1;
		scale = dv->scale[dv->polId][a1][a2];
	}
	else
	{
		scale = 1.0;
	}
	
	D = dv->D;

	startChan = D->startChan;
	stopChan = startChan + D->nOutChan*D->specAvg;

	dv->weight[D->nPolar*dv->bandId + dv->polId] = dv->recweight;
	
	for(i = startChan; i < stopChan; ++i)
	{
		int k;
		int j;
		int index;

		j = i - startChan;
		
		index = ((dv->bandId*D->nOutChan + j/D->specAvg)*D->nPolar+dv->polId)*dv->nComplex;

		for(k = 0; k < dv->nComplex; ++k)
		{
			/* swap phase/uvw for FITS-IDI conformance */
			if(k % 3 == 1)
			{
				dv->data[index+k] -= scale*dv->spectrum[dv->nComplex*i+k];
			}
			else
			{
				dv->data[index+k] += scale*dv->spectrum[dv->nComplex*i+k];
			}
		}
	}

	return 0;
}

static int readvisrecord(DifxVis *dv, const struct CommandLineOptions *opts, const DelayCal *DC, DelayCalCache *DCC, const Bandpass *B, int *nSkipped_recs)
{
	/* blank array */
	memset(dv->weight, 0, dv->nFreq*dv->D->nPolar*sizeof(float));
	memset(dv->data, 0, dv->nData*sizeof(float));

	dv->changed = 0;

        *nSkipped_recs = 0;

	while(dv->changed == 0 || dv->changed == SKIPPED_RECORD)
	{
		if(!dv->first && dv->changed >= 0)
		{
			storevis(dv);
		}
		dv->changed = DifxVisNewUVData(dv, opts, DC, DCC, B);
		if(opts->verbose > 3)
		{
			printf("readvisrecord: changed is %d\n", dv->changed);
		}
		if(dv->changed == HEADER_READ_ERROR)
		{
			return -1;
		}
		if(dv->changed == SKIPPED_RECORD)
		{
			(*nSkipped_recs)++;
		}
	}

	return 0;
}

static int *generateExcludedSourceList(const char *includeSourceList, const DifxInput *D)
{
	char srcName[32];	/* DIFXIO_NAME_LENGTH = 32 */
	int *l;
	int i, n = 0;
	const char *p;
	int q;

	l = (int *)calloc(D->nSource + 1, sizeof(int));

	p = includeSourceList;
	while(sscanf(p, "%31s%n", srcName, &q) == 1)
	{
		for(i = 0; i < D->nSource; ++i)
		{
			if(strcasecmp(srcName, D->source[i].name) == 0)
			{
				l[n++] = i;
				break;
			}
		}
		
		if(i == D->nSource)
		{
			fprintf(stderr, "Warning: requested source '%s' is not in any of the contributing files\n", srcName);
		}

		p += q;
	}

	l[n] = -1;	/* terminator */

	return l;
}

/* excludeSourceList should be terminated with -1, or can be a null pointer */
static int ExcludeSource(const DifxVis *dv, const int *includeSourceIdList)
{
	if(includeSourceIdList == 0)
	{
		/* No list provided, so no filtering */

		return 0;
	}
	else
	{
		int i;

		for(i = 0; includeSourceIdList[i] >= 0; ++i)
		{
			if(dv->sourceId == includeSourceIdList[i])
			{
				return 0;	/* keep it -- it is on the include list */
			}
		}
	}

	return 1;	/* turf it -- it is not on the provided list */
}

const DifxInput *DifxInput2FitsUV(const DifxInput *D, struct fits_keywords *p_fits_keys, struct fitsPrivate *out, const struct CommandLineOptions *opts, int passNum)
{
	int i, l, v;
	float visScale = 1.0;
	char fileBase[200];
	char dateStr[12];
	char fluxFormFloat[8];
	char gateFormInt[8];
	char weightFormFloat[8];
	char *str;
	int nRowBytes;
	int nColumn;
	int nWeight;
	int bestdvId;
	int nInvalid = 0;
	int nFlagged = 0;
	int nZero = 0;
	int nNegWeight = 0;
	int nTrans = 0;
	int nWritten = 0;
	int nOld = 0;
	int nSourceFiltered = 0;	/* number of records discarded due to being on source exclusion list */
	int nSkipped = 0;
	int nSkipped_recs;
	double mjd, bestmjd;
	DifxVis **dvs;
	DifxVis *dv;
	JobMatrix *jobMatrix = 0;
	int jobId;
	int nDifxVis;
	int dvId;
	double firstMJD = 1.0e7;
	double lastMJD = 0.0;
	int doUVDump = 0;
	double utcmin = 0.0, utcmax = 8640000.0;
#ifdef HAVE_FFTW
	Sniffer *S = 0;
#endif
	DelayCal *DC = 0;
	DelayCalCache DCC;
	Bandpass *B = 0;
	int *includeSourceIdList = 0;

	if(opts->includeSourceList)
	{
		includeSourceIdList = generateExcludedSourceList(opts->includeSourceList, D);
	}

	if(opts->applyDelayCalFile)
	{
		DC = loadDelayCal(opts->applyDelayCalFile, D);
	}
	resetDelayCalCache(&DCC);
	if(opts->applyBandpassFile)
	{
		B = loadBandpass(opts->applyBandpassFile, D);
	}

	str = getenv("DIFX2FITS_UVFITS_DUMP");
	if(str && strcasecmp(str, "yes") == 0)
	{
		doUVDump = 1;

		str = getenv("DIFX2FITS_UVFITS_DUMP_UTCMIN");
		if(str)
		{
			utcmin = atof(str);
		}
		str = getenv("DIFX2FITS_UVFITS_DUMP_UTCMAX");
		if(str)
		{
			utcmax = atof(str);
		}
	}

	/* define the columns in the UV data FITS Table */
	struct fitsBinTableColumn columns[] =
	{
		{"UU---SIN", "1E", "u", "SECONDS"},
		{"VV---SIN", "1E", "v", "SECONDS"},
		{"WW---SIN", "1E", "w", "SECONDS"},
		{"DATE", "1D", "Julian day at 0 hr current day", "DAYS"},
		{"TIME", "1D", "UTC time", "DAYS"},
		{"BASELINE", "1J", "baseline: ant1*256 + ant2", 0},
		{"FILTER", "1J", "filter id number", 0},
		{"SOURCE", "1J", "source id number from source tbl", 0},
		{"FREQID", "1J", "freq id number from frequency tbl", 0},
		{"INTTIM", "1E", "time span of datum", "SECONDS"},
		{"WEIGHT", weightFormFloat, "weights proportional to time", 0},
		{"GATEID", gateFormInt, "gate id from gate model table", 0},
		{"FLUX", fluxFormFloat, "data matrix", "UNCALIB"}
	};

	if(D == 0)
	{
		return 0;
	}

	printf("\n");
	/* allocate one DifxVis per job */


	if( (opts->pulsarBin == 0 && opts->phaseCentre == 0) || opts->alwaysWriteAutocorr == 0)
	{
		nDifxVis = D->nJob;
		if(opts->pulsarBin == 0 && opts->phaseCentre == 0)
		{
			printf("  Visibility files:\n");
		}
		else
		{
			if(opts->profileMode == 1)
			{
				printf("  Pulsar auto-correlation files\n");	
			}
			else
			{
				printf("  Cross-correlation files:\n");
			}
		}
	}
	else
	{
		/* factor of two allows one DifxVis for bin!=0 or phasecenter!=0 and the other for autocorrs from bin=0,pc=0 */
		nDifxVis = 2*D->nJob;
		printf("  Cross-correlation files:\n");
	}

	dvs = (DifxVis **)calloc(nDifxVis, sizeof(DifxVis *));	
	assert(dvs);

	/* here allocate all the "normal" DifxVis objects */
	for(jobId = 0; jobId < D->nJob; ++jobId)
	{
		dvs[jobId] = newDifxVis(D, jobId, opts, opts->pulsarBin, opts->phaseCentre);
		if(!dvs[jobId])
		{
			fprintf(stderr, "Error allocating DifxVis[%d/%d]\n", jobId, nDifxVis);

			return 0;
		}
		if((opts->pulsarBin > 0 || opts->phaseCentre > 0) && opts->profileMode == 0)
		{
			dvs[jobId]->keepAC = 0;	/* Really a no-op since these data don't come with ACs */
		}
		if(opts->dontIncludeVisibilities)
		{
			dvs[jobId]->maxRec = 2;
		}
	}

	if(nDifxVis > D->nJob)
	{
		printf("  Auto-correlation donor files:\n");
		/* here allocate special AC-only DifxVis objects */
		for(jobId = 0; jobId < D->nJob; ++jobId)
		{
			dvs[jobId + D->nJob] = newDifxVis(D, jobId, opts, 0, 0);
			if(!dvs[jobId + D->nJob])
			{
				fprintf(stderr, "Error allocating DifxVis[%d/%d]\n", jobId + D->nJob, nDifxVis);

				return 0;
			}
			dvs[jobId + D->nJob]->keepXC = 0; /* this is for AC only */
			if(opts->dontIncludeVisibilities)
			{
				dvs[jobId + D->nJob]->maxRec = 2;
			}
		}
	}
	printf("\n");

	/* for now set dv to the first job's structure */
	dv = dvs[0];

	nWeight = dv->nFreq*D->nPolar;

	strcpy(fileBase, out->filename);
	l = strlen(fileBase);
	for(i = l-1; i > 0; --i)
	{
		if(fileBase[i] == '.')
		{
			fileBase[i] = 0;
			break;
		}
	}

	/* Start up sniffer */
#ifdef HAVE_FFTW
	if( (opts->pulsarBin == 0 || opts->sniffAllBins) &&
	    (opts->phaseCentre == 0 || opts->sniffAllPhaseCentres) &&
	    opts->sniffTime > 0.0 )
	{
		S = newSniffer(D, dv->nComplex, fileBase, opts->sniffTime);
		if(S && opts->verbose > 1)
		{
			printf("    Sniffer memory usage ~= %lldMB\n", getSnifferMemoryUsage(S)/1000000);
		}
	}
#endif

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

	fitsWriteString(out, "EQUINOX", "J2000", "");
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
	fitsWriteFloat(out, "CDELT3", D->chanBW*D->specAvg*1.0e6/D->nOutChan, "");
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


	/* First prime each structure with some data */
	for(dvId = 0; dvId < nDifxVis; ++dvId)
	{
		if(opts->verbose > 3)
		{
			fprintf(stdout, "Priming, dv=%d/%d\n", dvId, nDifxVis);
		}
		readvisrecord(dvs[dvId], opts, DC, &DCC, B, &nSkipped_recs);
		if(opts->verbose > 3)
		{
			fprintf(stdout, "Done priming DifxVis objects\n");
		}
	}

	/* Now loop until done, looking at */
	while(nDifxVis > 0)
	{
		bestmjd = 1.0e9;
		bestdvId = 0;
		for(dvId = 0; dvId < nDifxVis; ++dvId)
		{
			dv = dvs[dvId];
			mjd = (int)(dv->record->jd - 2400000.0) + dv->record->utc;
			if(mjd < bestmjd)
			{
				bestmjd = mjd;
				bestdvId = dvId;
			}
		}
		dv = dvs[bestdvId];

		/* dv now points to earliest data. */

		if(RecordIsInvalid(dv))
		{
			++nInvalid;
		}
		else if(RecordIsFlagged(dv))
		{
			++nFlagged;
		}
		else if(RecordIsZero(dv))
		{
			if(opts->verbose > 0)
			{
				fprintf(stdout, "Found a zero record at mjd=%12.6f baseline=%d sourceId=%d\n", dv->mjd+dv->utc, dv->baseline, dv->sourceId);
			}
			++nZero;
		}
		else if(RecordHasNegativeWeight(dv))
		{
			if(opts->verbose > 0)
			{
				fprintf(stdout, "Found a negative weight recorrd at mjd=%12.6f baseline=%d sourceId=%d\n", dv->mjd+dv->utc, dv->baseline, dv->sourceId);
			}
			++nNegWeight;
		}
		else if(RecordIsTransitioning(dv))
		{
			++nTrans;
		}
		else if(RecordIsOld(dv))
		{
			++nOld;
		}
		else if(ExcludeSource(dv, includeSourceIdList))
		{
			++nSourceFiltered;
		}
		else
		{
#ifdef HAVE_FFTW
			if(S)
			{
				feedSnifferFITS(S, dv);
			}
#endif
			if(dv->record->baseline % 257 == 0)
			{
				feedJobMatrix(jobMatrix, dv->record, dv->jobId);
			}

			if(doUVDump)	/* diagnostic; set by environment variables (see above in this function) */
			{
				UVfitsDump(dv, utcmin, utcmax);
			}
#ifndef WORDS_BIGENDIAN
			FitsBinRowByteSwap(columns, nColumn, dv->record);
#endif

			if(dv->mjd+dv->utc < firstMJD)
			{
				firstMJD = dv->mjd+dv->utc;
			}
			if(dv->mjd+dv->utc > lastMJD)
			{
				lastMJD = dv->mjd+dv->utc;
			}

			fitsWriteBinRow(out, (char *)dv->record);
			++nWritten;
		}
		if(dv->changed == SKIPPED_RECORD)
		{
			++nSkipped;
		}
		if(dv->changed < 0)
		{
			deleteDifxVis(dv);
			--nDifxVis;
			dvs[bestdvId] = dvs[nDifxVis];
		}
		else
		{
			v = DifxVisCollectRandomParams(dv);
			if(v < 0)
			{
				fprintf(stderr, "Error in DifxVisCollectRandomParams : return value = %d\n", v);
#ifdef HAVE_FFTW
				if(S)
				{
					deleteSniffer(S);
					fftw_cleanup();
					S = 0;
				}
#endif
				return 0;
			}

			readvisrecord(dv, opts, DC, &DCC, B, &nSkipped_recs);
			nSkipped = nSkipped + nSkipped_recs;
		}
	}
	if(opts->verbose > 2)
	{
		printf("      dv->D->nConfig= %d\n", dv->D->nConfig);
	}

	printf("      %d invalid records dropped\n", nInvalid);
	printf("      %d flagged records dropped\n", nFlagged);
	printf("      %d all zero records dropped\n", nZero);
	printf("      %d negative weight records\n", nNegWeight);
	printf("      %d scan boundary records dropped\n", nTrans);
	printf("      %d out-of-time-range records dropped\n", nOld);
	printf("      %d records from excluded sources dropped\n", nSourceFiltered);
	printf("      %d records skipped\n", nSkipped);
	printf("      %d records written\n", nWritten);
	if(nWritten > 0)
	{
		printf("      FITS MJD range: %12.6f to %12.6f\n", firstMJD, lastMJD);
	}
	if(opts->verbose > 1)
	{
		printf("        Note : 1 record is all data from 1 baseline for 1 timestamp\n");
	}

	free(dvs);

#ifdef HAVE_FFTW
	if(S)
	{
		deleteSniffer(S);
		fftw_cleanup();
		S = 0;
	}
#endif
	if(B)
	{
		deleteBandpass(B);
		B = 0;
	}

	if(jobMatrix)
	{
		writeJobMatrix(jobMatrix, passNum);
		deleteJobMatrix(jobMatrix);
	}

	if(includeSourceIdList)
	{
		free(includeSourceIdList);
		includeSourceIdList = 0;
	}

	if(nNegWeight > 0)
	{
		printf("\n\n*** The presence of negative weight scans indicates a real problem with the correlator and should be reported immediately! ***\n\n");
	}

	printf("                            ");

	return D;
}

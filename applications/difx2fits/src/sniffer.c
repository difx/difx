/***************************************************************************
 *   Copyright (C) 2008-2017 by Walter Brisken & Adam Deller               *
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
// $Id: sniffer.c 10605 2022-08-22 22:53:15Z WalterBrisken $
// $HeadURL: https://svn.atnf.csiro.au/difx/master_tags/DiFX-2.8.1/applications/difx2fits/src/sniffer.c $
// $LastChangedRevision: 10605 $
// $Author: WalterBrisken $
// $LastChangedDate: 2022-08-22 16:53:15 -0600 (Mon, 22 Aug 2022) $
//
//============================================================================

#include <stdlib.h>
#include <string.h>
#include "sniffer.h"

typedef struct
{
	fftw_complex ***spectrum;	/* [BBC][Time][Chan] */
	fftw_complex ****ifSpectrum;	/* [IF][Stokes][Time][Chan]; note that Stokes=0,1 just point to data in spectrum, or zero if single pol */
	int a1, a2, sourceId;
	double mjdStart, mjdMax;
	double mjdSum;
	int mjdCount;
	int nTime, nChan;
	int nBBC, nIF;
	double *weightSum;
	double *weightMin;
	double *weightMax;
	double **ifWeightSum;
	double *lastDump;
	int **if2bbc;			/* [ifNum][pol] */
	int *nRec;
	int *isLSB;
} Accumulator;

struct _Sniffer
{
	FILE *apd;	/* amp, phase, dalay (, rate) file */
	FILE *apc;      /* amp, phase, chan (, rate) file */
	FILE *wts;
	FILE *acb;
	FILE *xcb;
	FILE *cpol;	/* stores cross polarization results: |RL/sqrt(RR*LL)|, and |LR/sqrt(RR*LL)| */
	double solInt;			/* (sec) FFT interval */
	double bw;			/* (MHz) IF bandwidth */
	double deltaT;			/* (sec) grid spacing */
	double deltaF;			/* (MHz) grid spacing */
	const DifxInput *D;
	int nRec;			/* total records sniffed */
	int nPol, nStokes, nIF, nTime, nChan, nAntenna;
	long long memoryNeed;
	int nComplex;
	int minInt;
	int configId;
	int fftOversample;
	fftw_plan plan1;
	fftw_plan plan2;
	fftw_complex *fftbuffer;
	int fft_nx, fft_ny;
	Accumulator **accum;
	int *fitsSourceId2SourceId;
};

static void resetAccumulator(Accumulator *A)
{
	int i, t, p;

	for(i = 0; i < A->nBBC; ++i)
	{
		for(t = 0; t < A->nTime; ++t)
		{
			memset(A->spectrum[i][t], 0, A->nChan*sizeof(fftw_complex));
		}
		A->weightMin[i] = 1000.0;
		A->weightMax[i] = 0.0;
		A->weightSum[i] = 0.0;
		A->nRec[i] = 0;
	}
	for(i = 0; i < A->nIF; ++i)
	{
		A->ifSpectrum[i][0] = 0;
		A->ifSpectrum[i][1] = 0;
		for(t = 0; t < A->nTime; ++t)
		{
			memset(A->ifSpectrum[i][2][t], 0, A->nChan*sizeof(fftw_complex));
			memset(A->ifSpectrum[i][3][t], 0, A->nChan*sizeof(fftw_complex));
		}
		A->if2bbc[i][0] = A->if2bbc[i][1] = 0;
		for(p = 0; p < 4; ++p)
		{
			A->ifWeightSum[i][p] = 0.0;
		}
	}
	A->mjdStart = 0;
	A->mjdSum = 0.0;
	A->mjdCount = 0;
}

static Accumulator *newAccumulatorArray(Sniffer *S, int n)
{
	Accumulator *A;
	int a;
	int nBBC;

	nBBC = S->nIF*S->nPol;

	A = (Accumulator *)calloc(n, sizeof(Accumulator));
	for(a = 0; a < n; ++a)
	{
		int i;

		A[a].nBBC = nBBC;
		A[a].nIF = S->nIF;
		A[a].nChan = S->nChan;
		A[a].nTime = S->nTime;
		A[a].spectrum = (fftw_complex ***)malloc(nBBC*sizeof(fftw_complex **));
		for(i = 0; i < nBBC; ++i)
		{
			int t;
			
			A[a].spectrum[i] = (fftw_complex **)malloc(S->nTime*sizeof(fftw_complex *));
			for(t = 0; t < A[a].nTime; ++t)
			{
				A[a].spectrum[i][t] = (fftw_complex *)calloc(S->nChan, sizeof(fftw_complex));
			}
		}
		A[a].ifSpectrum = (fftw_complex ****)malloc(S->nIF*sizeof(fftw_complex **));
		for(i = 0; i < S->nIF; ++i)
		{
			int t;
			A[a].ifSpectrum[i] = (fftw_complex ***)malloc(4*sizeof(fftw_complex **));
			A[a].ifSpectrum[i][0] = 0;
			A[a].ifSpectrum[i][1] = 0;
			A[a].ifSpectrum[i][2] = (fftw_complex **)malloc(S->nTime*sizeof(fftw_complex *));
			A[a].ifSpectrum[i][3] = (fftw_complex **)malloc(S->nTime*sizeof(fftw_complex *));
			for(t = 0; t < A[a].nTime; ++t)
			{
				A[a].ifSpectrum[i][2][t] = (fftw_complex *)calloc(S->nChan, sizeof(fftw_complex));
				A[a].ifSpectrum[i][3][t] = (fftw_complex *)calloc(S->nChan, sizeof(fftw_complex));
			}
		}
		A[a].nRec = (int *)calloc(nBBC, sizeof(int));
		A[a].isLSB = (int *)calloc(nBBC, sizeof(int));
		A[a].weightSum = (double *)calloc(nBBC, sizeof(double));
		A[a].weightMin = (double *)calloc(nBBC, sizeof(double));
		A[a].weightMax = (double *)calloc(nBBC, sizeof(double));
		A[a].lastDump = (double *)calloc(S->D->nSource, sizeof(double));
		A[a].sourceId = -1;

		A[a].ifWeightSum = (double **)malloc(A[a].nIF*sizeof(double *));
		A[a].if2bbc = (int **)malloc(A[a].nIF*sizeof(int *));
		for(i = 0; i < S->nIF; ++i)
		{
			A[a].ifWeightSum[i] = (double *)calloc(4, sizeof(double));
			A[a].if2bbc[i] = (int *)calloc(2, sizeof(int));
		}
	}

	return A;
}

static void deleteAccumulatorArray(Accumulator *A, int n)
{
	int a;

	if(!A)
	{
		return;
	}

	for(a = 0; a < n; ++a)
	{
		if(A[a].spectrum)
		{
			int i;

			for(i = 0; i < A[a].nBBC; ++i)
			{
				int t;

				for(t = 0; t < A[a].nTime; ++t)
				{
					free(A[a].spectrum[i][t]);
				}
				free(A[a].spectrum[i]);
			}
			for(i = 0; i < A[a].nIF; ++i)
			{
				int t;

				for(t = 0; t < A[a].nTime; ++t)
				{
					free(A[a].ifSpectrum[i][2][t]);
					free(A[a].ifSpectrum[i][3][t]);
				}
				free(A[a].ifSpectrum[i][2]);
				free(A[a].ifSpectrum[i][3]);
				free(A[a].ifSpectrum[i]);
			}
			free(A[a].spectrum);
			free(A[a].ifSpectrum);
			free(A[a].nRec);
			free(A[a].isLSB);
			free(A[a].weightSum);
			free(A[a].weightMin);
			free(A[a].weightMax);
			free(A[a].lastDump);

			for(i = 0; i < A->nIF; ++i)
			{
				free(A[a].ifWeightSum[i]);
				free(A[a].if2bbc[i]);
			}
			free(A[a].ifWeightSum);
			free(A[a].if2bbc);
		}
	}

	free(A);
}

Sniffer *newSniffer(const DifxInput *D, int nComplex, const char *filebase, double solInt)
{
	Sniffer *S;
	char filename[DIFXIO_FILENAME_LENGTH];
	int a1, c;
	double tMax = 0.0;
	FILE *log;
	int i, m, v;
	long long int maxSnifferMemory;
	const char *e;

	maxSnifferMemory = DEFAULT_MAX_SNIFFER_MEMORY;
	e = getenv("DIFX_MAX_SNIFFER_MEMORY");
	if(e)
	{
		maxSnifferMemory = atoll(e);
	}

	/* write summary to log file */
	v = snprintf(filename, DIFXIO_FILENAME_LENGTH, "%s.log", filebase);
	if(v >= DIFXIO_FILENAME_LENGTH)
	{
		fprintf(stderr, "\nError: sniffer log filename too long.  No sniffing today\n");

		return 0;
	}
	log = fopen(filename, "w");
	fprintDifxInputSummary(log, D);
	fclose(log);

	for(c = 0; c < D->nConfig; ++c)
	{
		if(D->config[c].tInt > tMax)
		{
			tMax = D->config[c].tInt;
		}
	}

	S = (Sniffer *)calloc(1, sizeof(Sniffer));

	m = 1;
	for(i = 0; i < D->nSource; ++i)
	{
		int freqSetId;

		for(freqSetId = 0; freqSetId < D->nFreqSet; ++freqSetId)
		{
			if(D->source[i].fitsSourceIds[freqSetId] > m)
			{
				m = D->source[i].fitsSourceIds[freqSetId];
			}
		}
	}
	S->fitsSourceId2SourceId = (int *)malloc((m+1)*sizeof(int));
	for(i = 0; i <= m; ++i)
	{
		S->fitsSourceId2SourceId[i] = -1;
	}
	for(i = 0; i < D->nSource; ++i)
	{
		int freqSetId;

		for(freqSetId = 0; freqSetId < D->nFreqSet; ++freqSetId)
		{
			if(D->source[i].fitsSourceIds[freqSetId] >= 0)
			{
				S->fitsSourceId2SourceId[D->source[i].fitsSourceIds[freqSetId]] = i;
			}
		}
	}

	S->deltaT = tMax;
	S->deltaF = D->chanBW;
	S->bw = D->chanBW;
	S->fftOversample = 3;

	S->nAntenna = D->nAntenna;
	S->D = D;
	S->nIF = D->nIF;
	S->nPol = D->nPol;
	S->nStokes = D->nPolar;
	S->nComplex = nComplex;
	S->configId = -1;
	S->nChan = D->nOutChan;
	S->nTime = solInt/tMax;
	if(S->nTime <= 1)
	{
		S->nTime = 1;
		fprintf(stderr, "\nWarning: sniffer interval is not long compared to integration time.\n");
		fprintf(stderr, "Changing to %f seconds.\n\n",
		tMax * S->nTime);
	}
	S->solInt = tMax * S->nTime;

	S->memoryNeed = (long long int)(S->nTime)*S->nChan*S->nIF*S->nPol*S->nAntenna*S->nAntenna*sizeof(fftw_complex);
	if(S->memoryNeed > maxSnifferMemory)
	{
		if(maxSnifferMemory > 0)
		{
			fprintf(stderr, "    ** DISABLING SNIFFER AS THE MEMORY REQUIREMENTS ARE EXCESSIVE (%lldMB > %lldMB) **\n", S->memoryNeed/1000000, maxSnifferMemory/1000000);
		}
		deleteSniffer(S);

		return 0;
	}
	
	/* Open fringe fit files */
	v = snprintf(filename, DIFXIO_FILENAME_LENGTH, "%s.apd", filebase);
	if(v >= DIFXIO_FILENAME_LENGTH)
	{
		fprintf(stderr, "\nError: sniffer apd filename too long.  No sniffing today\n");
		deleteSniffer(S);

		return 0;
	}
	S->apd = fopen(filename, "w");
	if(!S->apd)
	{
		fprintf(stderr, "Cannot open %s for write\n", filename);
		deleteSniffer(S);

		return 0;
	}
	fprintf(S->apd, "obscode:  %s\n", D->job->obsCode);

	v = snprintf(filename, DIFXIO_FILENAME_LENGTH, "%s.apc", filebase);
	if(v >= DIFXIO_FILENAME_LENGTH)
	{
		fprintf(stderr, "\nError: sniffer apc filename too long.  No sniffing today\n");
		deleteSniffer(S);

		return 0;
	}
	S->apc = fopen(filename, "w");
	if(!S->apc)
	{
		fprintf(stderr, "Cannot open %s for write\n", filename);
		deleteSniffer(S);
		
		return 0;
	}
	fprintf(S->apc, "obscode:  %s\n", D->job->obsCode);

	if(S->nStokes == 4)
	{
		v = snprintf(filename, DIFXIO_FILENAME_LENGTH, "%s.cpol", filebase);
		if(v >= DIFXIO_FILENAME_LENGTH)
		{
			fprintf(stderr, "\nError: sniffer cpol filename too long.  No sniffing today\n");
			deleteSniffer(S);

			return 0;
		}
		S->cpol = fopen(filename, "w");
		if(!S->cpol)
		{
			fprintf(stderr, "Cannot open %s for write\n", filename);
			deleteSniffer(S);

			return 0;
		}
		fprintf(S->cpol, "obscode:  %s\n", D->job->obsCode);
	}
	else
	{
		S->cpol = 0;
	}


	/* Open weights file */
	v = snprintf(filename, DIFXIO_FILENAME_LENGTH, "%s.wts", filebase);
	if(v >= DIFXIO_FILENAME_LENGTH)
	{
		fprintf(stderr, "\nError: sniffer wts filename too long.  No sniffing today\n");
		deleteSniffer(S);

		return 0;
	}
	S->wts = fopen(filename, "w");
	if(!S->wts)
	{
		fprintf(stderr, "Cannot open %s for write\n", filename);
		deleteSniffer(S);
		
		return 0;
	}
	fprintf(S->wts, "PLOTWT summary: %s\n", D->job->obsCode);

	/* Open acband file */
	v = snprintf(filename, DIFXIO_FILENAME_LENGTH, "%s.acb", filebase);
	if(v >= DIFXIO_FILENAME_LENGTH)
	{
		fprintf(stderr, "\nError: sniffer acb filename too long.  No sniffing today\n");
		deleteSniffer(S);

		return 0;
	}
	S->acb = fopen(filename, "w");
	if(!S->acb)
	{
		fprintf(stderr, "\nCannot open %s for write\n", filename);
		deleteSniffer(S);
		
		return 0;
	}

	/* Open xcband file */
	v = snprintf(filename, DIFXIO_FILENAME_LENGTH, "%s.xcb", filebase);
	if(v >= DIFXIO_FILENAME_LENGTH)
	{
		fprintf(stderr, "\nError: sniffer xcb filename too long.  No sniffing today\n");
		deleteSniffer(S);

		return 0;
	}
	S->xcb = fopen(filename, "w");
	if(!S->xcb)
	{
		fprintf(stderr, "\nCannot open %s for write\n", filename);
		deleteSniffer(S);
		
		return 0;
	}
	
#warning "FIXME: I think almost twice as much memory is being allocated as is needed -WFB"
	S->accum = (Accumulator **)malloc(S->nAntenna*sizeof(Accumulator *));
	for(a1 = 0; a1 < S->nAntenna; ++a1)
	{
		int a2;

		S->accum[a1] = newAccumulatorArray(S, S->nAntenna);
		for(a2 = 0; a2 < S->nAntenna; ++a2)
		{
			S->accum[a1][a2].a1 = a1;
			S->accum[a1][a2].a2 = a2;
		}
	}

	/* Prepare FFT stuff */

	S->fft_nx = S->fftOversample*S->nChan;
	S->fft_ny = S->fftOversample*S->nTime;
	S->fftbuffer = (fftw_complex*)fftw_malloc(S->fft_nx*S->fft_ny*sizeof(fftw_complex));
	S->plan1 = fftw_plan_many_dft(1, &(S->fft_ny), S->fft_nx, S->fftbuffer, 0, S->fft_nx, 1, S->fftbuffer, 0, S->fft_nx, 1, FFTW_FORWARD, FFTW_MEASURE);
	S->plan2 = fftw_plan_many_dft(1, &(S->fft_nx), S->fft_ny, S->fftbuffer, 0, 1, S->fft_nx, S->fftbuffer, 0, 1, S->fft_nx, FFTW_FORWARD, FFTW_MEASURE);

	return S;
}

long long getSnifferMemoryUsage(const Sniffer *S)
{
	if(S)
	{
		return S->memoryNeed;
	}
	else
	{
		return 0LL;
	}
}

void deleteSniffer(Sniffer *S)
{
	if(S)
	{
		if(S->fitsSourceId2SourceId)
		{
			free(S->fitsSourceId2SourceId);
			S->fitsSourceId2SourceId = 0;
		}
		if(S->apd)
		{
			fclose(S->apd);
			S->apd = 0;
		}
		if(S->apc)
		{
			fclose(S->apc);
			S->apc = 0;
		}
		if(S->cpol)
		{
			fclose(S->cpol);
			S->cpol = 0;
		}
		if(S->wts)
		{
			fclose(S->wts);
			S->wts = 0;
		}
		if(S->acb)
		{
			fclose(S->acb);
			S->acb = 0;
		}
		if(S->xcb)
		{
			fclose(S->xcb);
			S->xcb = 0;
		}
		if(S->accum)
		{
			int a;

			for(a = 0; a < S->nAntenna; ++a)
			{
				deleteAccumulatorArray(S->accum[a], S->nAntenna);
			}
			free(S->accum);
		}
		if(S->plan1)
		{
			fftw_destroy_plan(S->plan1);
		}
		if(S->plan2)
		{
			fftw_destroy_plan(S->plan2);
		}
		if(S->fftbuffer)
		{
			fftw_free(S->fftbuffer);
		}
		free(S);
	}
}

static double peakup(double peak[3], int i, int n, double w)
{
	double d, f;

	if(i >= n/2)
	{
		i -= n;
	}

	d = 2.0*peak[1]-peak[0]-peak[2];
	if(d <= 0.0)
	{
		f = i;
	}
	else
	{
		f = i + (peak[2]-peak[0])/(2.0*d);
	}

	return f/w;
}

static int dump(Sniffer *S, Accumulator *A)
{
	int b, j, p, a1, a2, besti, bestj;
	fftw_complex **array;
	double amp2, max2;
	double phase, delay, rate;
	double *amp;
	double specAmp, specPhase, specRate;
	int specChan;
	double peak[3];
	double w;
	char startStr[32], stopStr[32];
	FILE *fp;
	const DifxConfig *config;
	const DifxFreqSet *dfs;
	const DifxIF *IF;
	char pol, side;
	double freq;
	int chan=1;
	int maxNRec = 0;
	double mjd;

	if(A->sourceId < 0 || S->configId < 0 || A->mjdCount < 0)
	{
		return 0;
	}

	amp = (double *)malloc(A->nBBC*sizeof(double));

	mjd = A->mjdSum / A->mjdCount;

	config = S->D->config + S->configId;
	dfs = S->D->freqSet + config->freqSetId;

	a1 = A->a1;
	a2 = A->a2;

	for(b = 0; b < A->nBBC; ++b)
	{
		if(A->nRec[b] > maxNRec)
		{
			maxNRec = A->nRec[b];
		}
	}

	/* dump XC/AC bandpass at most every 15 minutes each source,
	   and only if at least 1 IF has >= 75% valid records */
	if(A->mjdStart > A->lastDump[A->sourceId] + 15.0/1440.0 && maxNRec >= A->nTime*3/4)
	{
		int i;

		A->lastDump[A->sourceId] = A->mjdStart;

		srvMjd2str(A->mjdStart, startStr);
		srvMjd2str(A->mjdMax, stopStr);

		if(a1 == a2)	/* Autocorrelation? */
		{
			fp = S->acb;
		}
		else		/* Cross corr? */
		{
			fp = S->xcb;
		}

		fprintf(fp, "timerange: %s %s obscode: %s chans: %d x %d\n", startStr, stopStr, S->D->job->obsCode, S->D->nOutChan, A->nBBC);
		fprintf(fp, "source: %s bandw: %6.3f MHz\n", S->D->source[A->sourceId].name, S->bw);
		for(i = 0; i < S->nIF; ++i)
		{
			IF = dfs->IF + i;
			freq = IF->freq/1000.0;	/* freq in GHz */
			side = IF->sideband;
			for(p = 0; p < S->nPol; ++p)
			{
				pol = IF->pol[p];
				fprintf(fp, "bandfreq: %9.6f GHz polar: %c%c side: %c bbchan: 0\n", freq, pol, pol, side);
			}
		}
		
		if(a1 == a2)	/* Autocorrelation? */
		{
			for(b = 0; b < A->nBBC; ++b)
			{
				int f;

				for(f = 0; f < A->nChan; ++f)
				{
					fftw_complex z;

					z = 0.0;
					if(A->weightSum[b] > 0.0)
					{
						int t;
						
						for(t = 0; t < A->nTime; ++t)
						{
							z += A->spectrum[b][t][f];
						}
						z /= A->weightSum[b];
					}
					fprintf(fp, "%2d %-3s %5d %7.5f\n", a1+1, S->D->antenna[a1].name, chan, creal(z));
					++chan;
				}
			}
		}
		else		/* Cross corr? */
		{
			for(b = 0; b < A->nBBC; ++b)
			{
				int f;

				for(f = 0; f < A->nChan; ++f)
				{
					float x, y;

					if(A->weightSum[b] > 0.0)
					{
						int t;
						
						fftw_complex z = 0.0;
						for(t = 0; t < A->nTime; ++t)
						{
							z += A->spectrum[b][t][f];
						}
						z /= A->weightSum[b];
						x = creal(z);
						y = -cimag(z);	/* to match an equivalent in AIPS */
					}
					else
					{
						x = y = 0.0;
					}
					fprintf(fp, "%2d %2d %-3s %-3s %5d %7.5f %8.3f\n",
						a1+1, a2+1, 
						S->D->antenna[a1].name,
						S->D->antenna[a2].name,
						chan, sqrt(x*x+y*y), 
						atan2(y, x)*180.0/M_PI);
					++chan;
				}
			}
		}
	}

	if(a1 == a2) /* Autocorrelation? */
	{
		int bbc;

		/* weights file */
		fprintf(S->wts, "%5d %8.5f %2d %-3s %2d", (int)mjd, 24.0*(mjd-(int)mjd), A->a1+1, S->D->antenna[A->a1].name, A->nBBC);

		for(bbc = 0; bbc < A->nBBC; ++bbc)
		{
			if(A->nRec[bbc] == 0)
			{
				w = 0.0;
			}
			else
			{
				w = A->weightSum[bbc]/A->nRec[bbc];
			}
			fprintf(S->wts, " %5.3f", w);
		}
		for(bbc = 0; bbc < A->nBBC; ++bbc)
		{
			if(A->nRec[bbc] == 0)
			{
				w = 0.0;
			}
			else
			{
				w = A->weightMin[bbc];
			}
			fprintf(S->wts, " %5.3f", w);
		}
		for(bbc = 0; bbc < A->nBBC; ++bbc)
		{
			if(A->nRec[bbc] == 0)
			{
				w = 0.0;
			}
			else
			{
				w = A->weightMax[bbc];
			}
			fprintf(S->wts, " %5.3f", w);
		}
		fprintf(S->wts, "\n");
	}

	else
	{
		int bbc;
		int ifNum;

		/* fringe fit */

		fprintf(S->apd, "%5d %10.7f %2d %-10s %2d %2d %-3s %-3s %2d",
			(int)mjd, 24.0*(mjd-(int)mjd), A->sourceId+1,
			S->D->source[A->sourceId].name, a1+1, a2+1,
			S->D->antenna[a1].name,
			S->D->antenna[a2].name,
			A->nBBC);

		fprintf(S->apc, "%5d %10.7f %2d %-10s %2d %2d %-3s %-3s %2d",
			(int)mjd, 24.0*(mjd-(int)mjd), A->sourceId+1,
			S->D->source[A->sourceId].name, a1+1, a2+1,
			S->D->antenna[a1].name,
			S->D->antenna[a2].name,
			A->nBBC);

		if(S->cpol)
		{
			fprintf(S->cpol, "%5d %10.7f %2d %-10s %2d %2d %-3s %-3s %2d",
				(int)mjd, 24.0*(mjd-(int)mjd), A->sourceId+1,
				S->D->source[A->sourceId].name, a1+1, a2+1,
				S->D->antenna[a1].name,
				S->D->antenna[a2].name,
				S->nIF);
		}

		for(bbc = 0; bbc < A->nBBC; ++bbc)
		{
			fftw_complex z;

			if(A->nRec[bbc] < S->nTime/2 || A->weightSum[bbc] == 0.0)
			{
				fprintf(S->apd, " 0 0 0 0");
				fprintf(S->apc, " 0 0 0 0");
				continue;
			}
			array = A->spectrum[bbc];
			memset(S->fftbuffer, 0, S->fft_nx*S->fft_ny*sizeof(fftw_complex));
			for(j = 0; j < A->nTime; ++j)
			{
				int i;

				for(i = 0; i < A->nChan; ++i)
				{
					S->fftbuffer[j*S->fft_nx + i] = array[j][i];
				}
			}

			/* First transform in time to form rates.  Here we do
                         * the spectral line sniffing to look for peak in
                         * rate/chan space*/
                        fftw_execute(S->plan1);
                        max2 = 0.0;
                        besti = bestj = 0;
                        for(j = 0; j < S->fft_ny; ++j)
                        {
				int i;

                                for(i = 0; i < S->fft_nx; ++i)
                                {
                                        z = S->fftbuffer[j*S->fft_nx + i];
                                        amp2 = z*~z;
                                        if(amp2 > max2)
                                        {
                                                besti = i;
                                                bestj = j;
                                                max2 = amp2;
                                        }
                                }
                        }
                        z = S->fftbuffer[bestj*S->fft_nx + besti];
                        specAmp = sqrt(max2);
                        specChan = besti;
                        specPhase = (180.0/M_PI)*atan2(cimag(z), creal(z));

                        peak[1] = specAmp;
                        if(bestj == 0)
                        {
                                z = S->fftbuffer[(S->fft_ny-1)*S->fft_nx+besti];
                        }
			else
                        {
                                z = S->fftbuffer[(bestj-1)*S->fft_nx + besti];
                        }
                        peak[0] = sqrt(z*~z);
                        if(bestj == S->fft_ny-1)
                        {
                                z = S->fftbuffer[besti];
                        }
                        else
                        {
                                z = S->fftbuffer[(bestj+1)*S->fft_nx + besti];
                        }
                        peak[2] = sqrt(z*~z);
                        specRate = peakup(peak, bestj, S->fft_ny, S->solInt*S->fftOversample);

                        /* Now do second axis of FFT (frequency) to look for a peak in rate/delay space */
			fftw_execute(S->plan2);

			max2 = 0.0;
			besti = bestj = 0;
			for(j = 0; j < S->fft_ny; ++j)
			{
				int i;

				for(i = 0; i < S->fft_nx; ++i)
				{
					z = S->fftbuffer[j*S->fft_nx + i];
					amp2 = creal(z*~z);
					if(amp2 > max2)
					{
						besti = i;
						bestj = j;
						max2 = amp2;
					}
				}
			}
			z = S->fftbuffer[bestj*S->fft_nx + besti];
			phase = (180.0/M_PI)*atan2(cimag(z), creal(z));
			amp[bbc] = sqrt(max2);
			peak[1] = amp[bbc];
			if(besti == 0)
			{
				z = S->fftbuffer[(bestj+1)*S->fft_nx - 1];
			}
			else
			{
				z = S->fftbuffer[bestj*S->fft_nx + besti - 1];
			}
			peak[0] = sqrt(creal(z*~z));
			if(besti == S->fft_nx-1)
			{
				z = S->fftbuffer[bestj*S->fft_nx];
			}
			else
			{
				z = S->fftbuffer[bestj*S->fft_nx + besti + 1];
			}
			peak[2] = sqrt(creal(z*~z));
			delay = peakup(peak, besti, S->fft_nx, S->bw*S->fftOversample/1000.0);
			if(bestj == 0)
			{
				z = S->fftbuffer[(S->fft_ny-1)*S->fft_nx+besti];
			}
			else
			{
				z = S->fftbuffer[(bestj-1)*S->fft_nx + besti];
			}
			peak[0] = sqrt(creal(z*~z));
			if(bestj == S->fft_ny-1)
			{
				z = S->fftbuffer[besti];
			}
			else
			{
				z = S->fftbuffer[(bestj+1)*S->fft_nx + besti];
			}
			peak[2] = sqrt(creal(z*~z));
			rate = peakup(peak, bestj, S->fft_ny, S->solInt*S->fftOversample);

			/* correct for negative frequency axis if LSB */
			if(A->isLSB[bbc])
			{
				phase = -phase;
				rate = -rate;
				specPhase = -specPhase;
				specChan = S->fft_nx-1-specChan;
				specRate = -specRate;
			}

			fprintf(S->apd, " %10.4f %7.5f %10.4f %10.6f", 
				delay, 
				2.0*amp[bbc]/(A->weightSum[bbc]*S->nChan), 
				phase, 
				rate);

			fprintf(S->apc, " %4d %7.5f %10.4f %10.6f",
				specChan+1,
				2.0*specAmp/(A->weightSum[bbc]*S->nChan), 
				specPhase,
				specRate);
		}

		if(S->cpol)
		{
			for(ifNum = 0; ifNum < S->nIF; ++ifNum)
			{
				fftw_complex z;
				int bbc0, bbc1;
				int pol;
				double stokesAmp[4];
				double norm;

				bbc0 = A->if2bbc[ifNum][0];
				bbc1 = A->if2bbc[ifNum][1];

				if(A->nRec[bbc0] < S->nTime/2 || A->nRec[bbc1] < S->nTime/2 || A->weightSum[bbc0] == 0.0 || A->weightSum[bbc1] == 0.0 || A->ifWeightSum[ifNum][2] == 0.0 || A->ifWeightSum[ifNum][3] == 0.0)
				{
					fprintf(S->cpol, " %d %d", -100-bbc0, -100-bbc1);
					continue;
				}

				for(pol = 2; pol < 4; ++pol)	/* start at 2 because parallel hands are already done */
				{
					array = A->ifSpectrum[ifNum][pol];
					memset(S->fftbuffer, 0, S->fft_nx*S->fft_ny*sizeof(fftw_complex));
					for(j = 0; j < A->nTime; ++j)
					{
						int i;

						for(i = 0; i < A->nChan; ++i)
						{
							S->fftbuffer[j*S->fft_nx + i] = array[j][i];
						}
					}

					/* First transform in time to form rates.  Here we do
					 * the spectral line sniffing to look for peak in
					 * rate/chan space*/
					fftw_execute(S->plan1);

					/* Now do second axis of FFT (frequency) to look for a peak in rate/delay space */
					fftw_execute(S->plan2);

					max2 = 0.0;
					besti = bestj = 0;
					for(j = 0; j < S->fft_ny; ++j)
					{
						int i;

						for(i = 0; i < S->fft_nx; ++i)
						{
							z = S->fftbuffer[j*S->fft_nx + i];
							amp2 = creal(z*~z);
							if(amp2 > max2)
							{
								besti = i;
								bestj = j;
								max2 = amp2;
							}
						}
					}
					z = S->fftbuffer[bestj*S->fft_nx + besti];
					phase = (180.0/M_PI)*atan2(cimag(z), creal(z));
					stokesAmp[pol] = sqrt(max2);
				}
				norm = sqrt(amp[bbc0]*amp[bbc1]);
				if(norm == 0.0)
				{
					fprintf(S->cpol, " -1 -1");
				}
				else
				{
					fprintf(S->cpol, " %5.3f %5.3f", stokesAmp[2]/norm, stokesAmp[3]/norm);
				}
			}
			fprintf(S->cpol, "\n");
		}
		fprintf(S->apd, "\n");
		fprintf(S->apc, "\n");
	}

	free(amp);

	return 0;
}

static void add(Accumulator *A, int bbc, int index, float weight, const float *data, int stride, int isLSB, double mjd)
{
	fftw_complex *array;
	int c;

	array = A->spectrum[bbc][index];
	for(c = 0; c < A->nChan; ++c)
	{
		const complex float *z;
		
		z = (complex float *)(data + c*stride);
		array[c] += (*z)*weight;
	}

	++A->nRec[bbc];
	A->isLSB[bbc] = isLSB;
	A->weightSum[bbc] += weight;
	if(weight > A->weightMax[bbc])
	{
		A->weightMax[bbc] = weight;
	}
	if(weight < A->weightMin[bbc])
	{
		A->weightMin[bbc] = weight;
	}
	++A->mjdCount;
	A->mjdSum += mjd;
}

static void addIF(Accumulator *A, int ifNum, int pol, int index, float weight, const float *data, int stride)
{
	fftw_complex *array;
	int c;

	array = A->ifSpectrum[ifNum][pol][index];
	for(c = 0; c < A->nChan; ++c)
	{
		const complex float *z;
		
		z = (complex float *)(data + c*stride);
		array[c] += (*z)*weight;
	}

	A->ifWeightSum[ifNum][pol] += weight;
}

int feedSnifferFITS(Sniffer *S, const DifxVis *dv)
{
	const struct UVrow *data;
	const DifxConfig *dc;
	const DifxFreqSet *dfs;
	double mjd;
	Accumulator *A;
	int a1, a2;
	int i;
	int configId, sourceId, scanId, scanId2;
	int stride, index;

	if(!S)
	{
		return 0;
	}

	data = dv->record;

	if(data->sourceId1 < 1)
	{
		return 0;
	}

	sourceId = S->fitsSourceId2SourceId[data->sourceId1-1];
	if(sourceId < 0 || sourceId >= S->D->nSource)
	{
		return 0;
	}

	mjd = data->jd - 2400000.5;
	mjd += data->utc;
	a1 = data->baseline/256 - 1;
	a2 = data->baseline%256 - 1;
	scanId = DifxInputGetScanIdByAntennaId(S->D, mjd, a1);
	scanId2 = DifxInputGetScanIdByAntennaId(S->D, mjd, a2);
	if(scanId < 0 || scanId > S->D->nScan || scanId2 < 0 || scanId2 > S->D->nScan)
	{
		return 0;
	}
	if(scanId != scanId2)
	{
		fprintf(stderr, "Warning: feedSnifferFITS: antenna1=%d and antenna2=%d refer to different scans (%d and %d)\n", a1, a2, scanId, scanId2);
	}

	configId = S->D->scan[scanId].configId;
	if(configId < 0 || configId >= S->D->nConfig)
	{
		return 0;
	}
	dc = S->D->config + configId;
	dfs = S->D->freqSet + dc->freqSetId;

	if(configId != S->configId)
	{
		S->nIF = dfs->nIF;
		S->nPol = dc->nPol;
		S->configId = configId;
	}


	A = &(S->accum[a1][a2]);

	if(mjd > A->mjdMax || A->sourceId != sourceId)
	{
		dump(S, A);
		resetAccumulator(A);
		A->sourceId = sourceId;
	}
	if(A->mjdStart < 50000.0)
	{
		A->mjdStart = mjd - 0.5*S->deltaT/86400.0;
		A->mjdMax = A->mjdStart + S->solInt/86400.0;
	}

	index = (mjd - A->mjdStart)/(S->deltaT/86400.0);

	if(index < 0 || index > A->nTime)
	{
		fprintf(stderr, "Developer Error: Sniffer: bad time slot for mjd=%14.6f index=%d (max index expected=%d).  This is not a critical problem and should not have any impact on correctness or completeness of data, but should be reported.\n", mjd, index, A->nTime-1);

		return -2;
	}

	if(index == A->nTime)
	{
		/* This is a rare case where the number of intervals containing data is greater than expected by one.  Not serious.  Don't raise a stink, just move on... */

		return -1;
	}

	stride = S->nComplex*S->nStokes;

	for(i = 0; i < S->nIF; ++i)
	{
		int isLSB;
		int p;
		
		isLSB = dv->sideband == 'L';

		for(p = 0; p < S->nPol; ++p)
		{
			int bbc, offset;
			float weight;

			bbc = i*S->nPol + p;
			weight = data->data[p + S->nStokes*i];
			offset = S->nStokes*S->nIF + stride*S->nChan*i + p*S->nComplex;
			add(A, bbc, index, weight, data->data+offset, stride, isLSB, mjd);
		}
		if(S->nStokes == 4 && data->data[2 + S->nStokes*i] > 0)	/* check for existence of cross-polar data */
		{
			int offset;

			/* RR */
			A->ifSpectrum[i][0] = A->spectrum[i*S->nPol + 0];
			A->if2bbc[i][0] = i*S->nPol + 0;
			/* LL */
			A->ifSpectrum[i][1] = A->spectrum[i*S->nPol + 1];
			A->if2bbc[i][1] = i*S->nPol + 1;
			/* RL */
			offset = S->nStokes*S->nIF + stride*S->nChan*i + 2*S->nComplex;
			addIF(A, i, 2, index, data->data[2 + S->nStokes*i], data->data+offset, stride);
			/* LR */
			offset = S->nStokes*S->nIF + stride*S->nChan*i + 3*S->nComplex;
			addIF(A, i, 3, index, data->data[3 + S->nStokes*i], data->data+offset, stride);
		}
	}

	++S->nRec;

	return S->nRec;
}

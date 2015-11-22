/***************************************************************************
 *   Copyright (C) 2014 by Walter Brisken                                  *
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
 * $Id: difxcalculator.c 1419 2009-08-29 21:37:08Z WalterBrisken $
 * $HeadURL: https://svn.atnf.csiro.au/difx/libraries/difxio/branches/difx-1.5/utils/difxcalculator.c $
 * $LastChangedRevision: 1419 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2009-08-29 15:37:08 -0600 (Sat, 29 Aug 2009) $
 *
 *==========================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <complex.h>
#include <fftw3.h>
#include <time.h>
#include "difx_input.h"

const char program[] = "psrflag";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.1";
const char verdate[] = "20140211";

void usage()
{
	printf("%s  ver. %s  %s  %s\n\n", program, version, author, verdate);
	printf("Usage : %s [options] <inputfilebase1> [ <inputfilebase2> [...] ]\n\n", program);
	printf("options can include:\n");
	printf("--verbose\n");
	printf("-v         be a bit more verbose\n\n");
	printf("--help\n");
	printf("-h         print help information and quit\n\n");
	printf("<inputfilebaseN> is the base name of a difx fileset.\n\n");
	printf("The output is a text file that can be read by AIPS UVFLG called <exp>.<psr>.flag\n\n");
}

double interpolate(double x0, double y0, double x1, double y1, double x)
{
	if(x1 < x0)
	{
		x1 += 1.0;
	}
	if(x < x0)
	{
		x += 1.0;
	}
	return y0 + (y1-y0)*(x-x0)/(x1-x0);
}

double getRate(const DifxInput *D, int antennaId, int scanId, int pc, int mjd, int sec)
{
	int p;
	DifxPolyModel *im;
	int dt;

	im = D->scan[scanId].im[antennaId][pc];

	for(p = 0; p < D->scan[scanId].nPoly; ++p)
	{
		if(im[p].mjd == mjd && im[p].sec <= sec && im[p].sec+im[p].validDuration >= sec)
		{
			break;
		}
	}

	if(p == D->scan[scanId].nPoly)
	{
		return -1e9;
	}

	dt = sec - im[p].sec;

	//printf("ANT %d  mjd %d sec %d -> %10.6f   im.sec = %d  dt=%d d[0]=%10.8e d[1]=%10.8e\n", antennaId, mjd, sec, mjd+sec/86400.0, im[p].sec, dt, im[p].delay[0], im[p].delay[1]);

	return im[p].delay[1] + 2.0*im[p].delay[2]*dt + 6.0*im[p].delay[3]*dt*dt;
}

double getPulsarSpinRate(const DifxPulsar *P, double mjd)
{
	int p;
	double dt;

	for(p = 0; p < P->nPolyco; ++p)
	{
		if(fabs(mjd - P->polyco[p].mjd) <= P->polyco[p].nBlk/1440.0)
		{
			break;
		}
	}

	if(p >= P->nPolyco)
	{
		return -1;
	}

	dt = (mjd - P->polyco[p].mjd)*86400.0;

	/* FIXME: evaluate full polynomial */
	return P->polyco[p].f0 + P->polyco[p].coef[1]/60.0 + dt*P->polyco[p].coef[2]/3600.0 + dt*dt*P->polyco[p].coef[3]/216000.0;
}

void mjd2keyin(char *str, double mjd)
{
	time_t t;
	struct tm brokenTime;

	t = (mjd - 40587.0)*86400.0;
	gmtime_r(&t, &brokenTime);
	strftime(str, 20, "%j,%H,%M,%S", &brokenTime);
}

int runPulsar(const DifxInput *D, int configId, const char *pulsarName, double thresh)
{
	//const double thresh = 0.02;
	int pulsarId;
	int scanId;
	const DifxConfig *dc;
	const int dataSize=80000;
	const int fftSize=560000;
	char fileName[DIFXIO_FILENAME_LENGTH];
	double gate[fftSize];
	double tInt;
	double f0;	/* reference spin period, seconds */
	fftw_complex Fgate[fftSize/2+1];
	fftw_plan plan;
	int i, j;
	int nBin, bin;
	double m;
	double binEnd0, binEnd1, binWeight1;
	FILE *out;
	FILE *flagOut;
	char T1[20], T2[20];

	dc = D->config + configId;
	pulsarId = dc->pulsarId;

	for(i = 0; i < fftSize; ++i)
	{
		gate[i] = 0;
	}

	tInt = D->config[configId].tInt;
	f0 = D->pulsar[pulsarId].polyco[0].f0;

	printf("  tInt = %f sec, pulsar f0 = %f Hz\n", tInt, f0);

	nBin = D->pulsar[pulsarId].nBin;
	
	for(i = 0; i < dataSize; ++i)
	{
		double phi;	/* turns of pulsar phase */

		phi = f0*tInt*i/dataSize;
		phi -= (int)phi;
		
		for(bin = 0; bin < nBin; ++bin)
		{
			if(D->pulsar[pulsarId].binEnd[bin] > phi)
			{
				break;
			}
		}
		if(bin == nBin)
		{
			bin = 0;
		}

		binEnd0 = D->pulsar[pulsarId].binEnd[(bin+nBin-1) % nBin];
		binEnd1 = D->pulsar[pulsarId].binEnd[bin];
		binWeight1 = D->pulsar[pulsarId].binWeight[bin];

		gate[i] = binWeight1;
	}
	
	snprintf(fileName, DIFXIO_FILENAME_LENGTH, "%s.bin", pulsarName);
	out = fopen(fileName, "w");
	if(!out)
	{
		fprintf(stderr, "Error: cannot open %s for write\n", fileName);
	}
	else
	{
		for(i = 0; i < dataSize; ++i)
		{
			double phi;	/* turns of pulsar phase */

			phi = f0*tInt*i/dataSize;

			fprintf(out, "%d %f %f %f %f\n", i, tInt*i/fftSize, phi, phi-(int)phi, gate[i]);
		}
		fclose(out);
	}

	plan = fftw_plan_dft_r2c_1d(fftSize, gate, Fgate, FFTW_ESTIMATE);
	fftw_execute(plan);
	fftw_destroy_plan(plan);

	m = Fgate[0];
	for(i = 0; i <= fftSize/2; ++i)
	{
		Fgate[i] /= m;
	}

	snprintf(fileName, DIFXIO_FILENAME_LENGTH, "%s.fft", pulsarName);
	out = fopen(fileName, "w");
	if(!out)
	{
		fprintf(stderr, "Error: cannot open %s for write\n", fileName);
	}
	else
	{
		for(i = 0; i <= dataSize/2; ++i)
		{
			fprintf(out, "%d %f %f\n", i, (1.0/tInt)*i*dataSize/fftSize, creal(Fgate[i]*~Fgate[i]));
		}
		fclose(out);
	}

	snprintf(fileName, DIFXIO_FILENAME_LENGTH, "%s%s.%s.flag", D->job->obsCode, D->job->obsSession, pulsarName);
	flagOut = fopen(fileName, "w");
	if(!flagOut)
	{
		fprintf(stderr, "Error: cannot open %s for write\n", fileName);
	}

	for(i = 1; i < D->nAntenna; ++i)
	{
		for(j = 0; j < i; ++j)
		{
			double flagOn[16];	// mjd onset of fringy flag; 0 for not set
			int k, ii;
			int mjd, sec;

			for(k = 0; k < 16; ++k)
			{
				flagOn[k] = 0;
			}

			// printf("%s -- %s\n", D->antenna[i].name, D->antenna[j].name);

			snprintf(fileName, DIFXIO_FILENAME_LENGTH, "%s.%s-%s.rates", pulsarName, D->antenna[i].name, D->antenna[j].name);
			out = fopen(fileName, "w");

			for(scanId = 0; scanId < D->nScan; ++scanId)
			{
				int pc;	/* phase center */
				DifxScan *scan;

				scan = D->scan + scanId;
				dc = D->config + scan->configId;

				if(strcmp(pulsarName, D->source[scan->phsCentreSrcs[scan->nPhaseCentres-1]].name) != 0)
				{
					continue;
				}

				fprintf(out, "\n");

				/* pc = 0 is pointing center; there are a total of nPhaseCentres+1 models; use the last one. */
				pc = scan->nPhaseCentres;

				mjd = (int)(scan->mjdStart);
				sec = (int)((scan->mjdStart-mjd)*86400.0);
	
				while(mjd + sec/86400.0 < scan->mjdEnd)
				{
					double R1, R2, f;
					fftw_complex M;
					int index;
					double fq; /* MHz */

					R1 = getRate(D, i, scanId, pc, mjd, sec);
					R2 = getRate(D, j, scanId, pc, mjd, sec);
					f = getPulsarSpinRate(D->pulsar + pulsarId, mjd+sec/86400.0);

					fprintf(out, "%14.7f %d  %f %f  %f", mjd+sec/86400.0, scanId, R1, R2, f);
					
					for(ii = 0; ii < dc->nIF; ++ii)
					{
						const int nSpecPts = 100;
						int l;
						double spbw;
						fq = dc->IF[ii].freq;
						spbw = dc->IF[ii].bw/nSpecPts;
						if(dc->IF[ii].sideband == 'L')
						{
							spbw = -spbw;
						}
						M = 0.0;

						for(l = 0; l < nSpecPts; ++l)
						{
							double fqm;

							fqm = fq + (0.5 + l)*spbw;

							index = (int)(fabs((R1-R2)*fqm)*(f/f0)*tInt*fftSize/dataSize +0.5);
							if(index < fftSize)
							{
								M += Fgate[index];
							}
						}
						M /= nSpecPts;

						fprintf(out, "  %f %f", fabs((R1-R2)*fq), cabs(M));

						if(cabs(M) > thresh)
						{
							if(flagOn[ii] < 1.0)
							{
								flagOn[ii] = mjd+sec/86400.0;
							}
						}
						else
						{
							if(flagOn[ii] > 1.0)
							{
								double m1, m2, mAdd;

								m1 = flagOn[ii];
								m2 = mjd+sec/86400.0;

								mAdd = 0.5*(m2-m1);

								m1 -= mAdd;
								m2 += mAdd;

								mjd2keyin(T1, m1);
								mjd2keyin(T2, m2);

								fprintf(flagOut, "ant_name='%s' bas_name='%s' timerang=%s,%s bif=%d eif=%d sources='%s' Reason='pulsar/fringe' /\n", D->antenna[i].name, D->antenna[j].name, T1, T2, ii+1, ii+1, pulsarName);
								flagOn[ii] = 0;
							}
						}
					}
					fprintf(out, "\n");

					++sec;
					if(sec >= 86400)
					{
						sec = 0;
						++mjd;
					}
				}
			}
			//close off any open flags
			for(ii = 0; ii < dc->nIF; ++ii)
			{
				if(flagOn[ii] > 1.0)
				{
					double m1, m2, mAdd;
					m1 = flagOn[ii];
					m2 = mjd+sec/86400.0;
					mAdd = 0.5*(m2-m1);
					m1 -= mAdd;
					m2 += mAdd;
					mjd2keyin(T1, m1);
					mjd2keyin(T2, m2);
					fprintf(flagOut, "ant_name='%s' bas_name='%s' timerang=%s,%s bif=%d eif=%d sources='%s' Reason='pulsar/fringe' /\n", D->antenna[i].name, D->antenna[j].name, T1, T2, ii+1, ii+1, pulsarName);
					flagOn[ii] = 0;
				}
			}
			fclose(out);
		}
		
	}
	fclose(flagOut);

	return 0;
}

int main(int argc, char **argv)
{
	DifxInput *D = 0;
	double threshold = 0.02;
	int a;
	int verbose = 0;
	int mergable, compatible;
	int nJob = 0;
	int pulsarId;
	
	for(a = 1; a < argc; ++a)
	{
		if(argv[a][0] == '-')
		{
			if(strcmp(argv[a], "-v") == 0 ||
			   strcmp(argv[a], "--verbose") == 0)
			{
				++verbose;
				continue;
			}
			else if(strcmp(argv[a], "-h") == 0 ||
			   strcmp(argv[a], "--help") == 0)
			{
				usage();

				exit(EXIT_SUCCESS);
			}
			else if(strcmp(argv[a], "-t") == 0)
			{
				if(a+1 < argc)
				{
					threshold = atof(argv[a+1]);
					++a;
				}
				else
				{
					usage();

					exit(EXIT_FAILURE);
				}
			}
			else
			{
				fprintf(stderr, "Unknown option %s\n", argv[a]);

				exit(EXIT_FAILURE);
			}
		}
		else if(D == 0)
		{
			D = loadDifxInput(argv[a]);
		}
		else
		{
			DifxInput *D1, *D2;

			D1 = D;
			D2 = loadDifxInput(argv[a]);
			if(D2)
			{
				mergable = areDifxInputsMergable(D1, D2);
				compatible = areDifxInputsCompatible(D1, D2);
				if(mergable && compatible)
				{
					D = mergeDifxInputs(D1, D2, verbose);
					deleteDifxInput(D1);
					deleteDifxInput(D2);
				}
				else
				{
					printf("cannot merge job %s: mergable=%d compatible=%d\n", argv[a], mergable, compatible);
					deleteDifxInput(D1);
					deleteDifxInput(D2);
					D = 0;
				}
			}
			else
			{
				deleteDifxInput(D);
				D = 0;
			}
		}
		if(!D)
		{
			fprintf(stderr, "File %s -> D == 0.  Quitting\n", argv[a]);

			return EXIT_FAILURE;
		}
		else
		{
			++nJob;
		}
	}

	if(nJob == 0)
	{
		printf("Nothing to do!  Quitting.  Run with -h for help information\n");

		return EXIT_SUCCESS;
	}

	D = updateDifxInput(D);
	if(!D)
	{
		fprintf(stderr, "Update failed: D == 0.  Quitting\n");
		
		return EXIT_FAILURE;
	}

	//printDifxInput(D);

	for(pulsarId = 0; pulsarId < D->nPulsar; ++pulsarId)
	{
		int sourceId;
		int scanId;
		int configId;
		const char *pulsarName;

		for(configId = 0; configId < D->nConfig; ++configId)
		{
			if(D->config[configId].pulsarId == pulsarId)
			{
				break;
			}
		}
		if(configId >= D->nConfig)
		{
			printf("Skipping pulsarId=%d because no config uses it\n", pulsarId);

			continue;
		}

		for(scanId = 0; scanId < D->nScan; ++scanId)
		{
			if(D->scan[scanId].configId == configId)
			{
				break;
			}
		}
		if(scanId >= D->nScan)
		{
			printf("Skipping pulsarId=%d because no scan uses it\n", pulsarId);

			continue;
		}

		sourceId = D->scan[scanId].phsCentreSrcs[D->scan[scanId].nPhaseCentres-1];

		if(sourceId < 0 || sourceId >= D->nSource)
		{
			printf("Skipping pulsarId=%d because sourceId = %d and nSource = %d\n", pulsarId, sourceId, D->nSource);

			continue;
		}

		pulsarName = D->source[sourceId].name;

		printf("Processing pulsar %s:\n", pulsarName);

		runPulsar(D, configId, pulsarName, threshold);
	}

	deleteDifxInput(D);

	return 0;
}

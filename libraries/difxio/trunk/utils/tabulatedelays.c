/***************************************************************************
 *   Copyright (C) 2015-2018 by Walter Brisken                             *
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
 * $HeadURL: $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "difx_input.h"

const char program[] = "tabulatedelays";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.4";
const char verdate[] = "20191110";

void usage()
{
	printf("\n%s  ver. %s  %s  %s\n\n", program, version, author, verdate);
	printf("Usage : %s [options] <inputfilebase1> [ <inputfilebase2> [...] ]\n\n", program);
	printf("options can include:\n");
	printf("--help\n");
	printf("-h         print help information and quit\n\n");
	printf("--az       print azimuth [deg], rate [deg/s] instead of delay, rate\n\n");
	printf("--el       print elevation [deg], rate [deg/s] instead of delay, rate\n\n");
	printf("--dry      print dry atmosphere delay [us]\n\n");
	printf("--wet      print wet atmosphere delay [us]\n\n");
	printf("--uvw      print antenna u,v,w [m] instead of delay, rate\n\n");
	printf("--clock    print clock offset and rate instead of delay, rate\n\n");
	printf("--perint   print values at the center of every integration rather than every 8s\n\n");
	printf("--addclock include clock offset/rate in delay/rate values\n\n");
	printf("<inputfilebaseN> is the base name of a difx fileset.\n\n");
	printf("All normal program output goes to stdout.\n\n");
	printf("This program reads through one or more difx datasets and evaluates\n");
	printf("delay polynomials in the .im files on a regular time grid (every\n");
	printf("8 seconds).  Delays and rates are both calculated.  Output should\n");
	printf("be self explanatory.  Plotting utilities such as gnuplot can be used\n");
	printf("directly on the output.\n\n");
	printf("When operating without --perint, the entirety of the delay polynomials\n");
	printf("are plotted, even exceeding the time range of the scans to which they\n");
	printf("belong.  Comments in the output separate scans cleanly.  When --perint\n");
	printf("is used, only the time covered by the scans is output.\n\n");
	printf("Sign conventions:\n");
	printf("  Delay: a positive delay indicates wavefront arrival at the station\n");
	printf("        before wavefront arrival at earth center.  The delay includes\n");
	printf("        contribution from wet and dry atmosphere components.\n");
	printf("  Rate: simply the time derivative of Delay.\n");
	printf("  Clock Offset: sign convention is opposite that of .vex \"clock_early\"\n");
	printf("        parameter; a positive clock offset indicates slow station clock.\n");
	printf("        The sum of Clock Offset and Delay is the total correlator delay.\n");
	printf("  Clock Rate: simply the time derivative of Clock Offset.\n\n");
}

enum Item
{
	ItemDelay = 0,
	ItemAz,
	ItemEl,
	ItemDry,
	ItemWet,
	ItemUVW,
	ItemClock
};

/* Use Cramer's rule to evaluate polynomial */
double evaluatePoly(const double *p, int n, double x)
{
	long double y;
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

double evaluatePolyDeriv(const double *p, int n, double x)
{
	long double y;
	int i;

	if(n == 1)
	{
		return 0;
	}

	if(n == 2)
	{
		return p[1];
	}

	y = (n-1)*p[n-1];

	for(i = n-2; i >= 1; --i)
	{
		y = x*y + i*p[i];
	}

	return y;
}

/* returns -1 if not found */
int getPolyIndex(const DifxScan *ds, int antennaId, int mjd, double sec)
{
	int p;

	if(ds->im[antennaId] == 0)
	{
		return -1;
	}

	/* get polynomial */
	for(p = 0; p < ds->nPoly; ++p)
	{
		if(ds->im[antennaId][0][p].mjd == mjd && ds->im[antennaId][0][p].sec <= sec)
		{
			return p;
		}
	}
	
	return -1;
}

int main(int argc, char **argv)
{
	DifxInput *D = 0;
	int a, s;
	int item = ItemDelay;
	int perint = 0;
	int addClock = 0;
	int nNoIm = 0;	/* count of scans w/o model */
	DifxMergeOptions mergeOptions;

	resetDifxMergeOptions(&mergeOptions);
	mergeOptions.eopMergeMode = EOPMergeModeRelaxed;
	mergeOptions.freqMergeMode = FreqMergeModeUnion;
	
	for(a = 1; a < argc; ++a)
	{
		if(argv[a][0] == '-')
		{
			if(strcmp(argv[a], "-h") == 0 ||
			   strcmp(argv[a], "--help") == 0)
			{
				usage();

				exit(EXIT_SUCCESS);
			}
			else if(strcmp(argv[a], "--az") == 0)
			{
				item = ItemAz;
			}
			else if(strcmp(argv[a], "--el") == 0)
			{
				item = ItemEl;
			}
			else if(strcmp(argv[a], "--dry") == 0)
			{
				item = ItemDry;
			}
			else if(strcmp(argv[a], "--wet") == 0)
			{
				item = ItemWet;
			}
			else if(strcmp(argv[a], "--uvw") == 0)
			{
				item = ItemUVW;
			}
			else if(strcmp(argv[a], "--clock") == 0)
			{
				item = ItemClock;
			}
			else if(strcmp(argv[a], "--perint") == 0)
			{
				perint = 1;
			}
			else if(strcmp(argv[a], "--addclock") == 0)
			{
				addClock = 1;
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
				if(areDifxInputsCompatible(D1, D2, &mergeOptions))
				{
					D = mergeDifxInputs(D1, D2, 0);
					deleteDifxInput(D1);
					deleteDifxInput(D2);
				}
				else
				{
					fprintf(stderr, "cannot merge job %s\n", argv[a]);
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
	}

	if(addClock && item != ItemDelay)
	{
		fprintf(stderr, "Error: The --addclock option only works when tabulating delays and rates\n");
		
		exit(EXIT_FAILURE);
	}

	if(!D)
	{
		fprintf(stderr, "Nothing to do!  Quitting.  Run with -h for help information\n");

		return EXIT_SUCCESS;
	}

	D = updateDifxInput(D, 0);
	if(!D)
	{
		fprintf(stderr, "Update failed: D == 0.  Quitting\n");
		
		return EXIT_FAILURE;
	}

	DifxInputSortAntennas(D, 0);

	printf("# produced by program %s ver. %s\n\n", program, version);
	printf("# Columns are:\n");
	printf("# 1. mjd [day]\n");
	for(a = 0; a < D->nAntenna; ++a)
	{
		switch(item)
		{
		case ItemDelay:
			if(addClock)
			{
				printf("# %d. Antenna %d (%s) delay including clock offset [us]\n", 2+2*a, a, D->antenna[a].name);
				printf("# %d. Antenna %d (%s) rate including clock rate [us/s]\n", 3+2*a, a, D->antenna[a].name);
			}
			else
			{
				printf("# %d. Antenna %d (%s) delay not including clock offset [us]\n", 2+2*a, a, D->antenna[a].name);
				printf("# %d. Antenna %d (%s) rate not including clock rate [us/s]\n", 3+2*a, a, D->antenna[a].name);
			}
			break;
		case ItemAz:
			printf("# %d. Antenna %d (%s) azimuth [deg]\n", 2+2*a, a, D->antenna[a].name);
			printf("# %d. Antenna %d (%s) azimuth rate [deg/s]\n", 3+2*a, a, D->antenna[a].name);
			break;
		case ItemEl:
			printf("# %d. Antenna %d (%s) geometric elevation [deg]\n", 2+2*a, a, D->antenna[a].name);
			printf("# %d. Antenna %d (%s) geometric elevation rate [deg/s]\n", 3+2*a, a, D->antenna[a].name);
			break;
		case ItemDry:
			printf("# %d. Antenna %d (%s) dry atmosphere delay [us]\n", 2+2*a, a, D->antenna[a].name);
			printf("# %d. Antenna %d (%s) dry atmosphere rate [us/s]\n", 3+2*a, a, D->antenna[a].name);
			break;
		case ItemWet:
			printf("# %d. Antenna %d (%s) wet atmosphere delay [us]\n", 2+2*a, a, D->antenna[a].name);
			printf("# %d. Antenna %d (%s) wet atmosphere rate [us/s]\n", 3+2*a, a, D->antenna[a].name);
			break;
		case ItemUVW:
			printf("# %d. Antenna %d (%s) baseline U [m]\n", 2+3*a, a, D->antenna[a].name);
			printf("# %d. Antenna %d (%s) baseline V [m]\n", 3+3*a, a, D->antenna[a].name);
			printf("# %d. Antenna %d (%s) baseline W [m]\n", 4+3*a, a, D->antenna[a].name);
		case ItemClock:
			printf("# %d. Antenna %d (%s) clock offset [us]\n", 2+2*a, a, D->antenna[a].name);
			printf("# %d. Antenna %d (%s) clock rate [us/s]\n", 3+2*a, a, D->antenna[a].name);
			break;
		}
	}

	for(s = 0; s < D->nScan; ++s)
	{
		const DifxScan *ds;
		const DifxConfig *dc;
		int refAnt;	/* points to a valid antenna in this poly */
		int p, i;

		ds = D->scan + s;
		dc = D->config + ds->configId;

		printf("\n# scan %d of %d: source = %s\n", s+1, D->nScan, D->source[ds->phsCentreSrcs[0]].name);

		if(!ds->im)
		{
			printf("#   No IM table for this scan\n");
			++nNoIm;

			continue;
		}

		for(refAnt = 0; refAnt < D->nAntenna; ++refAnt)
		{
			if(ds->im[refAnt])
			{
				break;
			}
		}
		if(refAnt >= D->nAntenna)
		{
			/* Huh, no delays for any antennas...? */

			printf("#   No delays\n");

			continue;
		}

		if(perint)
		{
			double t;	/* time into scan, in seconds, initialized at tInt/2 */

			for(t = 0.5*dc->tInt; t < ds->durSeconds; t += dc->tInt)
			{
				double mjd;
				int intmjd;
				double sec;

				mjd = ds->mjdStart + t/86400.0;
				intmjd = (int)(mjd);
				sec = (mjd - intmjd)*86400.0;

				printf("%14.8f", mjd);

				if(item == ItemClock)
				{
					for(a = 0; a < D->nAntenna; ++a)
					{
						printf("   %12.8f %12.9f", 
							evaluateDifxAntennaClock(D->antenna+a, mjd),
							evaluateDifxAntennaClockRate(D->antenna+a, mjd));
					}
				}
				else if(item == ItemUVW)
				{
					for(a = 0; a < D->nAntenna; ++a)
					{
						p = getPolyIndex(ds, a, intmjd, sec);
						if(p < 0)
						{
							printf("   %12.3f %12.3f %12.3f", 0.0, 0.0, 0.0);
						}
						else
						{
							double u, v, w;

							if(ds->im[a] == 0)
							{
								u = v = w = 0.0;
							}
							else
							{
								u = evaluatePoly(ds->im[a][0][p].u, ds->im[a][0][p].order+1, sec - ds->im[a][0][p].sec);
								v = evaluatePoly(ds->im[a][0][p].v, ds->im[a][0][p].order+1, sec - ds->im[a][0][p].sec);
								w = evaluatePoly(ds->im[a][0][p].w, ds->im[a][0][p].order+1, sec - ds->im[a][0][p].sec);
							}

							/* print to mm precision */
							printf("   %12.3f %12.3f %12.3f", u, v, w); 
						}
					}
				}
				else
				{
					for(a = 0; a < D->nAntenna; ++a)
					{
						p = getPolyIndex(ds, a, intmjd, sec);
						if(p < 0)
						{
							printf("   %12.6f %12.9f", 0.0, 0.0);
						}
						else
						{
							double v1, v2;

							if(a >= ds->nAntenna)
							{
								v1 = v2 = -1.0;
							}
							else if(ds->im[a] == 0)
							{
								/* print zeros in cases where there is no data */
								v1 = v2 = 0.0;
							}
							else
							{
								const double *poly;

								switch(item)
								{
								case ItemDelay:
									poly = ds->im[a][0][p].delay;
									break;
								case ItemAz:
									poly = ds->im[a][0][p].az;
									break;
								case ItemEl:
									poly = ds->im[a][0][p].elgeom;
									break;
								case ItemDry:
									poly = ds->im[a][0][p].dry;
									break;
								case ItemWet:
									poly = ds->im[a][0][p].wet;
									break;
								default:
									fprintf(stderr, "Weird!\n");

									exit(EXIT_FAILURE);
								}

								v1 = evaluatePoly(poly, ds->im[a][0][p].order+1, sec - ds->im[a][0][p].sec);
								v2 = evaluatePolyDeriv(poly, ds->im[a][0][p].order+1, sec - ds->im[a][0][p].sec);

								if(addClock && item == ItemDelay)
								{
									v1 += evaluateDifxAntennaClock(D->antenna+a, mjd);
									v2 += evaluateDifxAntennaClockRate(D->antenna+a, mjd);
								}
							}

							/* print to picosecond and femtosecond/sec precision */
							printf("   %12.6f %12.9f", v1, v2);
						}
					}
					
					printf("\n");
				}
			}
		}
		else
		{
			for(p = 0; p < ds->nPoly; ++p)
			{
				const int N = (p == ds->nPoly-1) ? 16 : 15;

				for(i = 0; i < N; ++i)
				{
					double mjd;

					mjd = ds->im[refAnt][0][p].mjd + (ds->im[refAnt][0][p].sec + i*8)/86400.0;

					printf("%14.8f", mjd);

					if(item == ItemClock)
					{
						for(a = 0; a < D->nAntenna; ++a)
						{
							printf("   %12.8f %12.9f", 
								evaluateDifxAntennaClock(D->antenna+a, mjd),
								evaluateDifxAntennaClockRate(D->antenna+a, mjd));
						}
					}
					else if(item == ItemUVW)
					{
						for(a = 0; a < D->nAntenna; ++a)
						{
							double u, v, w;

							if(ds->im[a] == 0)
							{
								u = v = w = 0.0;
							}
							else
							{
								u = evaluatePoly(ds->im[a][0][p].u, ds->im[a][0][p].order+1, 8*i);
								v = evaluatePoly(ds->im[a][0][p].v, ds->im[a][0][p].order+1, 8*i);
								w = evaluatePoly(ds->im[a][0][p].w, ds->im[a][0][p].order+1, 8*i);
							}

							/* print to mm precision */
							printf("   %12.3f %12.3f %12.3f", u, v, w); 
						}
					}
					else
					{
						for(a = 0; a < D->nAntenna; ++a)
						{
							double v1, v2;

							if(a >= ds->nAntenna)
							{
								v1 = v2 = -1.0;
							}
							else if(ds->im[a] == 0)
							{
								/* print zeros in cases where there is no data */
								v1 = v2 = 0.0;
							}
							else
							{
								const double *poly;

								switch(item)
								{
								case ItemDelay:
									poly = ds->im[a][0][p].delay;
									break;
								case ItemAz:
									poly = ds->im[a][0][p].az;
									break;
								case ItemEl:
									poly = ds->im[a][0][p].elgeom;
									break;
								case ItemDry:
									poly = ds->im[a][0][p].dry;
									break;
								case ItemWet:
									poly = ds->im[a][0][p].wet;
									break;
								default:
									fprintf(stderr, "Weird!\n");

									exit(EXIT_FAILURE);
								}

								v1 = evaluatePoly(poly, ds->im[a][0][p].order+1, 8*i);
								v2 = evaluatePolyDeriv(poly, ds->im[a][0][p].order+1, 8*i);

								if(addClock && item == ItemDelay)
								{
									v1 += evaluateDifxAntennaClock(D->antenna+a, mjd);
									v2 += evaluateDifxAntennaClockRate(D->antenna+a, mjd);
								}
							}

							/* print to picosecond and femtosecond/sec precision */
							printf("   %12.6f %12.9f", v1, v2);
						}
					}
					printf("\n");
				}
			}
		}
	}

	if(nNoIm > 0)
	{
		fprintf(stderr, "\nNote: %d scans were encountered that did not have associated delay models.  Likely this is because no .im files is present.  Use difxcalc or calcif2 to create.\n", nNoIm);
	}

	deleteDifxInput(D);

	return EXIT_SUCCESS;
}

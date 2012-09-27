/***************************************************************************
 *   Copyright (C) 2008-2011 by Walter Brisken & John Morgan               *
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
 * $HeadURL: https://svn.atnf.csiro.au/difx/libraries/mark5access/trunk/mark5access/mark5_stream.c $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "difxio/difx_input.h"
#include "pbgen.h"
const double pi = 3.141592653589793;
const char program[] = "pbgen";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>, John Morgan <john.morgan@icrar.org>";
const char version[] = "0.1";
const char verdate[] = "20120927";

void usage()
{
	printf("%s  ver. %s  %s  %s\n\n", program, version, author, verdate);
	printf("NB proof of concept only!!\n");
	printf("Offset of one source in primary beam is calculated at 30 second intervals\n");
	printf("Selected phase centre can only be set at compile time\n");
	printf("Usage : %s <inputfilebase> ...\n\n", program);
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

double haversine(double az1, double el1, double az2, double el2)
{
	/*  
	 * Determine the great circle angular distance between two points
	 * in spherical polar coordinates
	 *
	 * This should give the distance from the pointing centre (az1, el1) of a 2nd point (az2, el2)
	 * all must be in radians
	 */
	double a;
	a = pow(sin((el1 - el2)/2), 2) + (pow(sin((az1 - az2)/2), 2) * pow(cos(el1), 2));
	return 2 * atan2(sqrt(a), sqrt(1-a));
}

double azbearing(double az1, double el1, double az2, double el2)
{
	/*  Initial bearing from 'North' of point 1 from point 2
	 *  For an az/el mounted telescope, 'North' is always 'up' so this should
	 *  give the bearing within the primary beam of (az2, el2) from a 
	 *  pointing centre (az1, el1).
	 * 
	 *  The exact form is such that if the two points share the same RA, and the decl(2) > decl(1),
	 *  The angle returned is the Parallactic Angle
	 */
	double se1, ce1, se2, ce2, sa21, ca21;
	sincos(el1, &se1, &ce1);
	sincos(el2, &se2, &ce2);
	sincos(az2-az1, &sa21, &ca21);

	return -atan2(sa21*ce1, (ce2*se1) - (se2*ce1*ca21));
}
int getMergedDifxInput(int argc, char **argv, DifxInput *D)
{
}

int main(int argc, char **argv)
{
	int error;
	DifxInput *D = 0;
	DifxAntenna *da = 0;
	//FIXME need to choose phase centre and time increment at command line!!
	int n, s, a, np, dsId, antId;
	int phaseCentre = 3;//FIXME get from co
	int tInc = 30; //seconds
	int first_line= 1;
	double distance, pangle; 
	int arg;
	int verbose = 0;
	int mergable, compatible;
	int nJob = 0;
	
	for(arg = 1; arg < argc; ++arg)
	{
		if(argv[arg][0] == '-')
		{
			if(strcmp(argv[arg], "-v") == 0 ||
			   strcmp(argv[arg], "--verbose") == 0)
			{
				++verbose;
				continue;
			}
			else if(strcmp(argv[arg], "-h") == 0 ||
			   strcmp(argv[arg], "--help") == 0)
			{
				usage();

				exit(2);
			}
			else
			{
				fprintf(stderr, "Unknown option %s\n", argv[arg]);

				exit(EXIT_FAILURE);
			}
		}
		else if(D == 0)
		{
			D = loadDifxInput(argv[arg]);
		}
		else
		{
			DifxInput *D1, *D2;

			D1 = D;
			D2 = loadDifxInput(argv[arg]);
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
					printf("cannot merge job %s: mergable=%d compatible=%d\n", argv[arg], mergable, compatible);
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
			fprintf(stderr, "File %s -> D == 0.  Quitting\n", argv[arg]);

			return EXIT_FAILURE;
		}
		else
		{
			++nJob;
		}
	}

	if(nJob == 0)
	{
		printf("No Jobs!  Quitting.\n");

		return 2;
	}

	D = updateDifxInput(D);
	if(!D)
	{
		fprintf(stderr, "Update failed: D == 0.  Quitting\n");
		
		return EXIT_FAILURE;
	}
	printDifxInput(D);

	error = getMergedDifxInput(argc, argv, D);
	printDifxInput(D);

	for(s = 0; s < D->nScan; ++s)
	{
		const DifxScan *scan;
		const DifxJob *job;
		const DifxConfig *config;
		const DifxPolyModel *im_pointing, *im_source;
		int configId, jobId, terms;
		double mjd, dt;
		double az_pointing, el_pointing, az_source, el_source;
		scan = D->scan + s;
		jobId = scan->jobId;
		job = D->job + jobId;
		configId = scan->configId;

		if(configId < 0)
		{
			continue;
		}

		if(phaseCentre >= scan->nPhaseCentres)
		{
			continue;
		}

		config = D->config + configId;

		if(scan->im)
		{
			np = scan->nPoly;
		}
		else
		{
			fprintf(stderr, "No IM info available for scan %d: skipping generation of ML table\n", s);
			continue;
		}

		//increment by tInt, check for which poly is valid
		//evaluate polynomial, work out offset, 
		//FIXME write out Antenna name, important that these are matched correctly!!

		/*this may not be super-accurate (would be better to use integer seconds etc.) but should be good enough*/
		mjd = scan->mjdStart;
		while (mjd < scan->mjdEnd)
		{
			n = getDifxScanIMIndex(scan, (int) floor(mjd), mjd - floor(mjd), &dt);
			if(first_line)
			{
				printf("# first number is offset in arcminutes, 2nd number is parallactic angle in degrees\n");
				printf("# mjd        ");
				for(a = 0; a < config->nAntenna; ++a)
				{
					dsId = config->ant2dsId[a];
					antId = D->datastream[dsId].antennaId;
					printf("%s            ", da = D->antenna[antId].name);
				}
				printf("\n");
			}
			printf("%11.6f ", mjd);
			for(a = 0; a < config->nAntenna; ++a)
			//for(a = 0; a < 1; ++a)
			{
				const DifxAntenna *da;
				const DifxPolyModel *P;
				int dsId, antId;

				dsId = config->ant2dsId[a];
				if(dsId < 0 || dsId >= D->nDatastream)
				{
					continue;
				}
				/* convert to D->antenna[] index ... */
				antId = D->datastream[dsId].antennaId;

				if(antId < 0 || antId >= scan->nAntenna)
				{
					continue;
				}

				da = D->antenna + antId;

				if(scan->im[antId] == 0)
				{
					//FIXME what does this do??
					//if(skip[antId] == 0)
					//{
					//	printf("\n    Polynomial model error: skipping antId %d = %s", antId, da->name);
					//}
					continue;
				}


				/* use .difx/ antenna indices for model tables */
				if(scan->im[antId])
				{
					im_pointing = scan->im[antId][0];
					im_source = scan->im[antId][phaseCentre];
					if(!(im_pointing))
					{
						fprintf(stderr, "Warning: the antenna model is missing for the pointing centre(im=%p)\n", im_pointing);
					}
					if(!(im_source))
					{
						fprintf(stderr, "Warning: the antenna model is missing (im=%p)\n", im_source);
					}
					else if(n < 0)
					{
						fprintf(stderr, "Error: interferometer model index out of range: scanId=%d mjd=%12.6f\n", s, mjd);
					}
					else
					{
						terms = im_pointing->order + 1;//FIXME is this OK?
						az_pointing =     evalPoly(im_pointing[n].az, terms, dt)*pi/180.;
						el_pointing = evalPoly(im_pointing[n].elgeom, terms, dt)*pi/180.;
						az_source = evalPoly(im_source[n].az, terms, dt)*pi/180.;
						el_source = evalPoly(im_source[n].elgeom, terms, dt)*pi/180.;
						distance = haversine(az_pointing, el_pointing, az_source, el_source);
						pangle = azbearing(az_pointing, el_pointing, az_source, el_source);
						//printf("%04.1f %04.1f , ", az_pointing*180./pi, el_pointing*180./pi);
						//printf("%04.1f %04.1f , ", az_source*180./pi, el_source*180./pi);
						printf("%07.4f %05.1f ", distance*180.*60./pi, pangle*180./pi);
					}
				}
			} /* end antenna loop */
		mjd += tInc/86400.;
		first_line = 0;
		printf("\n");
		} /* time integration loop */
	} /* Scan loop */
	return D;
}

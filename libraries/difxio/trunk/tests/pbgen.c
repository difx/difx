/***************************************************************************
 *   Copyright (C) 2012 by John Morgan                                     *
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

#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <glob.h>
#include <util.h>
#include <math.h>
#include <errno.h>
#include <sys/resource.h>
#include "difxio/difx_input.h"
#include "pbgen.h"
const double pi = 3.141592653589793;
const char program[] = "pbgen";
const char author[]  = "John Morgan <john.morgan@icrar.org>";
const char version[] = "0.1";
const char verdate[] = "20120927";

#define MAX_INPUT_FILES		4096
const double DefaultTInc= 30;

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
	a = pow(sin((el1 - el2)/2), 2.) + (pow(sin((az1 - az2)/2), 2.) * pow(cos(el1), 2.));
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
#ifdef USESINCOS
	sincos(el1, &se1, &ce1);
	sincos(el2, &se2, &ce2);
	sincos(az2-az1, &sa21, &ca21);
#else 
	se1 = sin(el1);
	ce1 = cos(el1);
	se2 = sin(el2);
	ce2 = cos(el2);
	sa21 = sin(az2-az1);
	ca21 = cos(az2-az1);
#endif
	return -atan2(sa21*ce1, (ce2*se1) - (se2*ce1*ca21));
}

int exceedOpenFileLimit(int numFiles)
{
        struct rlimit limit;
	//
	// Get max number of open files that the OS allows
	if (getrlimit(RLIMIT_NOFILE, &limit) != 0) 
	{
	    printf("Cannot determine user file open limit (errno=%d)\n", errno);
	    return(1);
	}
	//
	// Check if the number of DIFX files (plus a buffer of 20) exceed OS limit
	if (numFiles + 20 >= limit.rlim_cur)
		return(1);

	return(0);
}
int getMergedDifxInput(int argc, char **argv, DifxInput *D)
{
	return 0;
}

int main(int argc, char **argv)
{
	int error;
	DifxInput *D = 0;
	DifxInput *D1 = 0;
	DifxInput *D2 = 0;
	DifxAntenna *da = 0;
	//FIXME need to choose phase centre and time increment at command line!!
	int n, s, a, np, dsId, antId;
	int phaseCentre = 0;
	int tInc;
	int new_scan= 1;
	int header= 1;
	double distance, pangle; 
	int i, l;
	int verbose = 0;
	int mergable, compatible;
	int nJob = 0;

	char *baseFile[MAX_INPUT_FILES];
	int nBaseFile = 0;
	int nWithoutPhaseCentre = 0;
	int doalldifx = 0;
	glob_t globBuffer;

	
	tInc= DefaultTInc;

	for(i = 1; i < argc; ++i)
	{
		if(argv[i][0] == '-')
		{
			if(strcmp(argv[i], "-v") == 0 ||
			   strcmp(argv[i], "--verbose") == 0)
			{
				++verbose;
				continue;
			}
			else if(strcmp(argv[i], "-h") == 0 ||
			   strcmp(argv[i], "--help") == 0)
			{
				usage();

				exit(2);
			}
			else if(strcmp(argv[i], "--difx") == 0 ||
			        strcmp(argv[i], "-d") == 0)
			{
				++doalldifx;
			}
			else if(i+1 < argc) /* one parameter arguments */
			{
				if(strcmp(argv[i], "-a") == 0 ||
				   strcmp(argv[i], "--avgseconds") == 0)
				{
					++i;
					tInc = atof(argv[i]);
				}
				else if(strcasecmp(argv[i], "-p") == 0 ||
					strcasecmp(argv[i], "--phaseCentre") == 0 ||
					strcasecmp(argv[i], "--phasecenter") == 0)
				{
					++i;
					phaseCentre = atoi(argv[i]);
				}
			}
			else
			{
				fprintf(stderr, "Unknown option %s\n", argv[i]);

				exit(EXIT_FAILURE);
			}
		}
		else
		{
	
			if (exceedOpenFileLimit(nBaseFile))
			{
				printf("Error: The number of input files exceeds the OS limit of allowed open files!\n");
				printf("Run ulimit -n to increase that number.\n");
				printf("Note: This might require increasing the hard limit in /etc/security/limits.conf\n");

				return 0;
			}
	
			if(nBaseFile >= MAX_INPUT_FILES)
			{
				printf("Error: too many input files!\n");
				printf("Max = %d\n", MAX_INPUT_FILES);

				return 0;
			}
			baseFile[nBaseFile] = strdup(argv[i]);
			++nBaseFile;
		}
	}
	if((nBaseFile >  0 && doalldifx >  0) ||
	   (nBaseFile == 0 && doalldifx == 0))
	{
		return 0;
	}
	if(doalldifx)
	{
		glob2(__FUNCTION__, "*.im", 0, 0, &globBuffer);
		if(exceedOpenFileLimit(globBuffer.gl_pathc))
		{
			printf("Error: The number of input files exceeds the OS limit of allowed open files!\n");
			printf("Run ulimit -n to increase that number.\n");
			printf("Note: This might require increasing the hard limit in /etc/security/limits.conf\n");

			return 0;
		}

		if(globBuffer.gl_pathc > MAX_INPUT_FILES)
		{
			printf("Error: too many input files!\n");
			printf("Max = %d\n", MAX_INPUT_FILES);
			
			return 0;
		}
		nBaseFile = globBuffer.gl_pathc;
		for(i = 0; i < nBaseFile; ++i)
		{
			baseFile[i] = strdup(globBuffer.gl_pathv[i]);
		}
		globfree(&globBuffer);
	}
	/* if input file ends in .im, trim it */
	for(i = 0; i < nBaseFile; ++i)
	{
		l = strlen(baseFile[i]);
		if(l < 4)
		{
			continue;
		}
		if(strcmp(baseFile[i]+l-3, ".im") == 0)
		{
			baseFile[i][l-3] = 0;
		}
	}

	for(i = 0; i < nBaseFile; ++i)
	{
		if(baseFile[i] == 0)
		{
			continue;
		}

		if(verbose > 1)
		{
			printf("Loading %s\n", baseFile[i]);
		}
		D2 = loadDifxInput(baseFile[i]);
		if(!D2)
		{
			fprintf(stderr, "loadDifxInput failed on <%s>.\n", baseFile[i]);

			return 0;
		}
		if(DifxInputGetMaxPhaseCentres(D2) <= phaseCentre)
		{
			if(verbose > 0)
			{
				printf("Skipping %s because it doesn't contain phase centre %d\n", baseFile[i], phaseCentre);
			}

			deleteDifxInput(D2);
			free(baseFile[i]);
			baseFile[i] = 0;

			++nWithoutPhaseCentre;

			continue;
		}

		if(D)
		{
			D1 = D;

			if(!areDifxInputsMergable(D1, D2) ||
			   !areDifxInputsCompatible(D1, D2))
			{
				deleteDifxInput(D2);

				continue;
			}
			else if(verbose > 1)
			{
				printf("Merging %s\n", baseFile[i]);
			}

			D = mergeDifxInputs(D1, D2, verbose);

			deleteDifxInput(D1);
			deleteDifxInput(D2);

			if(!D)
			{
				fprintf(stderr, "Merging failed on <%s>.\n", baseFile[i]);

				return 0;
			}
		}
		else
		{
			D = D2;
		}
		if(baseFile[i])
		{
			free(baseFile[i]);
			baseFile[i] = 0;
		}
	}

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
		//FIXME should check if this changes (at least check if nAnt changes)
		//maybe just rewrite participating antennas at the start of each scan.

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
			if(new_scan)
			{
				if(header)
				{
					printf("# first number is offset in arcminutes, 2nd number is parallactic angle in degrees\n");
				}
				printf("# mjd        ");
				for (a = 0; a < config->nAntenna; ++a)
				{
					dsId = config->ant2dsId[a];
					antId = D->datastream[dsId].antennaId;
					printf("%s             ", D->antenna[antId].name);
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
						printf("%07.4f %+06.1f ", distance*180.*60./pi, pangle*180./pi);
					}
				}
			} /* end antenna loop */
		mjd += tInc/86400.;
		new_scan= 0;
		header = 0;
		printf("\n");
		} /* time integration loop */
	new_scan= 1;
	} /* Scan loop */ 

	return EXIT_SUCCESS;
}

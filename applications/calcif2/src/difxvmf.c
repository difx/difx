/***************************************************************************
 *   Copyright (C) 2019 by Walter Brisken                                  *
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
 * $Id: difxvmf.c 10847 2022-12-04 22:42:48Z WalterBrisken $
 * $HeadURL: $
 * $LastChangedRevision: 10847 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2022-12-05 06:42:48 +0800 (一, 2022-12-05) $
 *
 *==========================================================================*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <difxio/difx_input.h>
#include "vmf.h"
#include "wxdata.h"

const char program[] = "difxvmf";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.2";
const char verdate[] = "20190821";

void usage()
{
	printf("%s  ver. %s  %s  %s\n\n", program, version, author, verdate);
	printf("Usage : %s [options] <inputfilebase1> [ <inputfilebase2> [...] ]\n\n", program);
	printf("options can include:\n");
	printf("--verbose\n");
	printf("-v         be a bit more verbose\n\n");
	printf("--help\n");
	printf("-h         print help information and quit\n\n");
	printf("--usewx\n");
	printf("-w         use measured .weather files if available\n\n");
	printf("Environment variables used:\n");
	printf("  DIFX_VERSION : to enforce DiFX version compatibility\n");
	printf("  DIFX_VMF_DATA : writable directory for caching downloaded VMF coefficients\n\n");
	printf("This program takes a DiFX fileset (including the .im file) and modifies the wet\n");
	printf("and dry troposphere values based on the Vienna Mapping Functions.  This program\n");
	printf("retrieves the needed external data from http://vmf.geo.tuwien.ac.at .  The .im\n");
	printf("file will be replaced with an updated version.\n\n");
	printf("If --usewx is specified, files of the form <project>.<stn>.weather will be looked\n");
	printf("for in the local directory and used to supply metrology data, overriding defaults.\n\n");
}

int processFile(const char *inputFile, const DifxMergeOptions *mergeOptions, const char *difxVersion, int overrideVersion, int useWx, int verbose)
{
	const int MaxVMFData = 8192;
	DifxInput *D;
	int status = 0;
	VMFData vmfData[MaxVMFData];
	int vmfRows;
	WXData *wxData;

	if(verbose > 0)
	{
		printf("Processing %s\n", inputFile);
	}

	D = loadDifxInput(inputFile);
	if(!D)
	{
		fprintf(stderr, "Error: %s: loading of %s failed.\n", program, inputFile);

		return -2;
	}

	vmfRows = loadVMFData(vmfData, MaxVMFData, (int)(D->job->mjdStart), 2, verbose);
	if(vmfRows <= 0)
	{
		fprintf(stderr, "Error: %s: no VMF data loaded\n", program);

		return -1;
	}
	
	if(difxVersion && D->job->difxVersion[0])
	{
		if(strncmp(difxVersion, D->job->difxVersion, DIFXIO_VERSION_LENGTH-1))
		{
			printf("Attempting to run calcif2 from version %s on a job make for version %s\n", difxVersion, D->job->difxVersion);
			if(overrideVersion)
			{
				fprintf(stderr, "Warning: %s: continuing despite version mismatch because of --override-version\n", program);
			}
			else
			{
				fprintf(stderr, "Error: %s: calcif2 won't run on mismatched version without --override-version.\n", program);
				deleteDifxInput(D);

				return -3;
			}
		}
	}
	else if(!D->job->difxVersion[0])
	{
		printf("Warning: calcif2: working on unversioned job\n");
	}

	D = updateDifxInput(D, mergeOptions);
	if(!D)
	{
		fprintf(stderr, "Error: %s updateDifxInput(): update of %s failed.\n", program, inputFile);

		return -4;
	}

	if(useWx)
	{
		wxData = loadWeatherForProject(D);
	}
	else
	{
		wxData = 0;
	}
	
	status = calculateVMFDifxInput(D, vmfData, vmfRows, wxData, verbose);

	if(wxData)
	{
		deleteWXDataArray(wxData, D->nAntenna);
	}

	if(verbose > 0)
	{
		printf("%d records were updated\n", status);
	}

	if(status > 0)
	{
		/* write data back out */
		writeDifxIM(D);

		printf("Wrote new version of %s with VMF.\n", D->job->imFile);
	}
	else
	{
		printf("Warning: %s calculateVMFDifxInput(): No output produced\n", program);
	}


	deleteDifxInput(D);
	
	return status;
}

int main(int argc, char **argv)
{
	int nTry = 0;
	int nGood = 0;
	int nRecord = 0;
	int a;
	int verbose = 0;
	int versionOverride = 0;
	DifxMergeOptions mergeOptions;
	const char *difxVersion;
	int useWx = 0;

	difxVersion = getenv("DIFX_VERSION");
	if(difxVersion == 0)
	{
		fprintf(stderr, "Error: %s: environment variable DIFX_VERSION must be set.\n", program);

		exit(EXIT_FAILURE);
	}

	if(getenv("DIFX_VMF_DATA") == 0)
	{
		fprintf(stderr, "Error: %s: environment variable DIFX_VMF_DATA must be set to a writable directory.\n", program);

		exit(EXIT_FAILURE);
	}

	resetDifxMergeOptions(&mergeOptions);
	resetDifxInputCompatibilityStatistics();
	mergeOptions.eopMergeMode = EOPMergeModeRelaxed;

	for(a = 1; a < argc; ++a)
	{
		int n;

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
			else if(strcmp(argv[a], "-w") == 0 ||
			   strcmp(argv[a], "--usewx") == 0)
			{
				useWx = 1;
			}
			else if(strcmp(argv[a], "--override-version") == 0)
			{
				versionOverride = 1;
			}
			else
			{
				fprintf(stderr, "Error: %s: unknown argument: %s\n", program, argv[a]);

				exit(EXIT_FAILURE);
			}
		}
		else
		{
			++nTry;
			n = processFile(argv[a], &mergeOptions, difxVersion, versionOverride, useWx, verbose);
			if(n > 0)
			{
				++nGood;
				nRecord += n;
			}
		}
	}

	if(nTry > 1 || verbose > 0)
	{
		printf("%d/%d file sets converted (%d total records).\n", nGood, nTry, nRecord);
	}

	return EXIT_SUCCESS;
}

/***************************************************************************
 *   Copyright (C) 2008-2010 by Walter Brisken                             *
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
#include "difx_input.h"

int main(int argc, char **argv)
{
	DifxInput *D = 0;
	int a;
	int verbose = 0;
	int mergable, compatible;
	
	if(argc < 2)
	{
		printf("Usage : %s <inputfilebase> ...\n", argv[0]);
		return 0;
	}
	
	for(a = 1; a < argc; a++)
	{
		if(strcmp(argv[a], "-v") == 0 ||
		   strcmp(argv[a], "--verbose") == 0)
		{
			verbose++;
			continue;
		}
		if(D == 0)
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
					printf("cannot merge jobs: mergable=%d compatible=%d\n",
						mergable, compatible);
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
			fprintf(stderr, "D == 0.  quitting\n");
			return 0;
		}
	}

	D = updateDifxInput(D);
	if(!D)
	{
		fprintf(stderr, "update failed: D == 0.  quitting\n");
		return 0;
	}

	strcpy(D->job->inputFile, "input.test");
	strcpy(D->job->calcFile, "calc.test");
	strcpy(D->job->threadsFile, "threads.test");
	strcpy(D->job->imFile, "im.test");
	strcpy(D->job->outputFile, "output.test");

	printDifxInput(D);

	writeDifxCalc(D);
	writeDifxInput(D);

	deleteDifxInput(D);

	return 0;
}

/***************************************************************************
 *   Copyright (C) 2009-2011 by Walter Brisken                             *
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
#include "difx_input.h"
#include "difx_calculator.h"

const char program[] = "difxcalculator";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.2";
const char verdate[] = "20110730";

const int nNode = 10;
const int nCore = 7;

static void usage()
{
	fprintf(stderr, "\n%s ver. %s  %s  %s\n\n", program, version, author, verdate);
	fprintf(stderr, "A program to calculate software correlator resource usage.\n");
	fprintf(stderr, "This is based on Adam Deller's difx_calculator.xls .\n");
	fprintf(stderr, "\nUsage: %s <input file base name> [<speedUp factor>]\n", program);
	fprintf(stderr, "\n<input file base name> is the prefix of the difx .input file\n");
	fprintf(stderr, "        to study.  Files ending in .input and .calc are needed.\n");
	fprintf(stderr, "\n<speedUp factor> is a floating point number which is the ratio\n");
	fprintf(stderr, "        of correlation speed to observation speed.\n\n");
}

int main(int argc, char **argv)
{
	DifxInput *D;
	DifxCalculator *C;

	if(argc < 2)
	{
		usage();

		return EXIT_FAILURE;
	}
	
	D = loadDifxCalc(argv[1]);
	if(!D)
	{
		fprintf(stderr, "D == 0.  quitting\n");
		
		return EXIT_FAILURE;
	}

	D = updateDifxInput(D);
	if(!D)
	{
		fprintf(stderr, "update failed: D == 0.  quitting\n");
		
		return EXIT_FAILURE;
	}

	C = newDifxCalculator();

	if(argc > 2)
	{
		C->speedUp = atof(argv[2]);
	}

	populateDifxCalculator(C, D);

	printDifxCalculator(C);

	deleteDifxCalculator(C);

	deleteDifxInput(D);

	return EXIT_SUCCESS;
}

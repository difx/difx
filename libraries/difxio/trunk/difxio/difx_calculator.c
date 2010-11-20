/***************************************************************************
 *   Copyright (C) 2010 by Walter Brisken                                  *
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
// $Id$
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include <stdlib.h>
#include "difx_calculator.h"

const char printFormat[] = "%-24s%-15s%s\n";

DifxCalculator *newDifxCalculator()
{
	DifxCalculator *C;

	C = (DifxCalculator *)calloc(1, sizeof(DifxCalculator));
	
	if(C)
	{
		C->speedUp = 1.0;
	}

	return C;
}

void deleteDifxCalculator(DifxCalculator *C)
{
	if(C)
	{
		if(C->config)
		{
			deleteDifxCalculatorConfigArray(C->config, C->nConfig);
		}
		free(C);
	}
}

DifxCalculatorConfig *newDifxCalculatorConfigArray(int n)
{
	DifxCalculatorConfig *c;

	c = (DifxCalculatorConfig *)calloc(n, sizeof(DifxCalculatorConfig));

	return c;
}

void deleteDifxCalculatorConfigArray(DifxCalculatorConfig *c, int n)
{
	if(c)
	{
		free(c);
	}
}

int populateDifxCalculator(DifxCalculator *C, const DifxInput *D)
{
	DifxCalculatorConfig *c;
	DifxConfig *d;
	int i;

	if(!D || !C)
	{
		return -1;
	}
	
	if(C->config)
	{
		deleteDifxCalculatorConfigArray(C->config, C->nConfig);
	}

	C->nConfig = D->nConfig;
	C->config = newDifxCalculatorConfigArray(C->nConfig);

	for(i = 0; i < D->nConfig; i++)
	{
		c = &C->config[i];
		d = &D->config[i];
		c->nAntenna = d->nAntenna;
		c->nBaseline = d->nBaseline;
	}

	return 0;
}

static void printString(const char *p, const char *v, const char *n)
{
	char nullstring[] = "";
	if(n == 0) 
	{
		n = nullstring;
	}

	printf(printFormat, p, v, n);
}

static void printInt(const char *p, int v, const char *n)
{
	const int MaxLen = 16;
	char nullstring[] = "";
	char tmp[MaxLen];

	if(n == 0) 
	{
		n = nullstring;
	}

	snprintf(tmp, MaxLen, "%d", v);

	printf(printFormat, p, tmp, n);
}

static void printDouble(const char *p, double v, const char *n)
{
	const int MaxLen = 16;
	char nullstring[] = "";
	char tmp[MaxLen];

	if(n == 0) 
	{
		n = nullstring;
	}

	snprintf(tmp, MaxLen, "%f", v);

	printf(printFormat, p, tmp, n);
}

void printDifxCalculator(const DifxCalculator *C)
{
	int i;
	const DifxCalculatorConfig *c;

	printf("\nNumber of Configurations = %d\n\n", C->nConfig);

	for(i = 0; i < C->nConfig; i++)
	{
		c = &C->config[i];
		printf("CONFIG %d\n", i);
		printString("PARAMETER", "VALUE", "NOTE");
		printInt("Number of telescopes", c->nAntenna,  0);
		printInt("Number of baselines",  c->nBaseline, 0);
	}
}

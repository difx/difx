/***************************************************************************
 *   Copyright (C) 2024 by Walter Brisken                                  *
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

#include <stdio.h>
#include <stdlib.h>
#include "snifferoptions.h"

const long long int DefaultSnifferMaxMemory = 2000000000LL;	/* [bytes] */
const double DefaultSnifferSolutionInterval = 30.0;		/* sec */
const double DefaultSnifferBandpassInterval = 15.0;		/* min */


SnifferOptions *newSnifferOptions()
{
	SnifferOptions *sOpts;

	sOpts = (SnifferOptions *)calloc(1, sizeof(SnifferOptions));
	resetSnifferOptions(sOpts);

	return sOpts;
}

void resetSnifferOptions(SnifferOptions *sOpts)
{
	const char *e;
	
	e = getenv("DIFX_MAX_SNIFFER_MEMORY");
	if(e)
	{
		sOpts->maxMemory = atoll(e);
	}
	else
	{
		sOpts->maxMemory = DefaultSnifferMaxMemory;
	}
	sOpts->solutionInterval = DefaultSnifferSolutionInterval;
	sOpts->bandpassInterval = DefaultSnifferBandpassInterval;
}

void deleteSnifferOptions(SnifferOptions *sOpts)
{
	if(sOpts)
	{
		free(sOpts);
	}
}

void printSnifferOptions(const SnifferOptions *sOpts)
{
	printf("SnifferOptions\n");
	printf("  solutionInterval = %f sec\n", sOpts->solutionInterval);
	printf("  bandpassInterval = %f min\n", sOpts->bandpassInterval);
	printf("  maxMemory = %lld bytes\n", sOpts->maxMemory);
	printf("  writeBandpass = %d\n", sOpts->writeBandpass);
}


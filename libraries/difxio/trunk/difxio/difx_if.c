/***************************************************************************
 *   Copyright (C) 2007-2010 by Walter Brisken                             *
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "difxio/difx_input.h"

int isSameDifxIF(const DifxIF *di1, const DifxIF *di2)
{
	int i;

	if(di1 == 0 && di2 == 0)
	{
		return 1;
	}
	else if(di1 == 0 || di2 == 0)
	{
		return 0;
	}

	if(di1->freq != di2->freq ||
	   di1->bw != di2->bw ||
	   di1->sideband != di2->sideband ||
	   di1->nPol != di2->nPol)
	{
		return 0;
	}

	for(i = 0; i < di1->nPol; i++)
	{
		if(di1->pol[i] != di2->pol[i])
		{
			return 0;
		}
	}

	return 1;
}

DifxIF *newDifxIFArray(int nIF)
{
	DifxIF *di;

	di = (DifxIF *)calloc(nIF, sizeof(DifxIF));
	
	return di;
}

void deleteDifxIFArray(DifxIF *di)
{
	if(di)
	{
		free(di);
	}
}

void fprintDifxIF(FILE *fp, const DifxIF *di)
{
	fprintf(fp, "    Difx IF : %p\n", di);
	if(!di)
	{
		return;
	}
	fprintf(fp, "      Freq = %f MHz\n", di->freq);
	fprintf(fp, "      Bandwidth = %f MHz\n", di->bw);
	fprintf(fp, "      Sideband = %c\n", di->sideband);
	if(di->nPol == 1)
	{
		fprintf(fp, "      Pol = %c\n", di->pol[0]);
	}
	else if(di->nPol == 2)
	{
		fprintf(fp, "      Pols = %c, %c\n", di->pol[0], di->pol[1]);
	}
	else
	{
		fprintf(fp, "      nPol = %d\n", di->nPol);
	}
}

void printDifxIF(const DifxIF *di)
{
	fprintDifxIF(stdout, di);
}

void fprintDifxIFSummary(FILE *fp, const DifxIF *di)
{
	const int PolStringLength = 8;
	char pols[PolStringLength];
	int v;

	if(!di)
	{
		return;
	}

	if(di->nPol == 1)
	{
		v = snprintf(pols, PolStringLength, "(%c)", di->pol[0]);
	}
	else if(di->nPol == 2)
	{
		v = snprintf(pols, PolStringLength, "(%c,%c)", di->pol[0], di->pol[1]);
	}
	else
	{
		v = snprintf(pols, PolStringLength, "(%d)", di->nPol);
	}

	fprintf(fp, "    Freq=%f MHz  BW=%f MHz Sideband=%c Pols=%s\n",
		di->freq, di->bw, di->sideband, pols);

	if(v >= PolStringLength)
	{
		fprintf(stderr, "Developer error: fprintDifxIFSummary: PolStringLength=%d is too short.  Needs to be at least %d\n", PolStringLength, v);
	}
}

void printDifxIFSummary(const DifxIF *di)
{
	fprintDifxIFSummary(stdout, di);
}

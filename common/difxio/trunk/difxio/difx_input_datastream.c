/***************************************************************************
 *   Copyright (C) 2008 by Walter Brisken                                  *
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
#include <string.h>
#include "difxio/difx_input.h"


DifxDatastream *newDifxDatastreamArray(int nDatastream)
{
	DifxDatastream *ds;

	ds = (DifxDatastream *)calloc(nDatastream, sizeof(DifxDatastream));

	return ds;
}

void deleteDifxDatastreamArray(DifxDatastream *ds, int nDatastream)
{
	int e;

	if(ds)
	{
		for(e = 0; e < nDatastream; e++)
		{
			if(ds[e].nPol)
			{
				free(ds[e].nPol);
			}
			if(ds[e].freqId)
			{
				free(ds[e].freqId);
			}
			if(ds[e].clockOffset)
			{
				free(ds[e].clockOffset);
			}
			if(ds[e].RCfreqId)
			{
				free(ds[e].RCfreqId);
			}
			if(ds[e].RCpolName)
			{
				free(ds[e].RCpolName);
			}
		}
		free(ds);
	}
}

void printDifxDatastream(const DifxDatastream *ds)
{
	int f;
	printf("  Difx Datastream Entry[antId=%d] : %p\n", ds->antId, ds);
	printf("    format = %s\n", ds->dataFormat);
	printf("    quantization bits = %d\n", ds->quantBits);
	printf("    nFreq = %d\n", ds->nFreq);
	printf("    nRecChan = %d\n", ds->nRecChan);
	printf("    (freqId, nPol)[freq] =");
	for(f = 0; f < ds->nFreq; f++)
	{
		printf(" (%d, %d)", ds->freqId[f], ds->nPol[f]);
	}
	printf("\n");
	printf("    (freqId, pol)[recchan] =");
	for(f = 0; f < ds->nRecChan; f++)
	{
		printf(" (%d, %c)", ds->RCfreqId[f], ds->RCpolName[f]);
	}
	printf("\n");
}

int isSameDifxDatastream(const DifxDatastream *dd1, const DifxDatastream *dd2)
{
	return 0;
}

void copyDifxDatastream(DifxDatastream *dest, const DifxDatastream *src)
{
}

DifxDatastream *mergeDifxDatastreamArrays(const DifxDatastream *dd1, int ndd1,
	const DifxDatastream *dd2, int ndd2, int *datastreamIdRemap,
	const int *freqIdRemap, const int *antIdRemap)
{
	return 0;
}

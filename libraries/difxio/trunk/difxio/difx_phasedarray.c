/***************************************************************************
 *   Copyright (C) 2008-2015 by Walter Brisken                             *
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
// $Id:  $
// $HeadURL:  $
// $LastChangedRevision:  $
// $Author:  $
// $LastChangedDate:  $
//
//============================================================================

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "difxio/difx_input.h"


const char phasedArrayOutputTypeNames[][MAX_PHASED_ARRAY_TYPE_STRING_LENGTH] =
{
	"FILTERBANK",
	"TIMESERIES",
	"UNKNOWN"
};

const char phasedArrayOutputFormatNames[][MAX_PHASED_ARRAY_FORMAT_STRING_LENGTH] =
{
	"DIFX",
	"VDIF",
	"UNKNOWN"
};

enum PhasedArrayOutputType stringToPhasedArrayOutputType(const char *str)
{
	enum PhasedArrayOutputType t;

	for(t = 0; t < NumPhasedArrayOutputTypes; ++t)
	{
		if(strcmp(str, phasedArrayOutputTypeNames[t]) == 0)
		{
			break;
		}
	}

	return t;
}

enum PhasedArrayOutputFormat stringToPhasedArrayOutputFormat(const char *str)
{
	enum PhasedArrayOutputFormat f;

	for(f = 0; f < NumPhasedArrayOutputFormats; ++f)
	{
		if(strcmp(str, phasedArrayOutputFormatNames[f]) == 0)
		{
			break;
		}
	}

	return f;
}

DifxPhasedArray *newDifxPhasedarrayArray(int nPhasedArray)
{
	DifxPhasedArray *dpa;

	dpa = (DifxPhasedArray *)calloc(nPhasedArray, sizeof(DifxPhasedArray));

	return dpa;
}

/* grow the size of a phasedarray array by 1 element */
DifxPhasedArray *growDifxPhasedarrayArray(DifxPhasedArray *dpa, int origSize)
{
	dpa = (DifxPhasedArray *)realloc(dpa, (origSize+1)*sizeof(DifxPhasedArray));
	memset(dpa+origSize, 0, sizeof(DifxPhasedArray));

	return dpa;
}

void deleteDifxPhasedarrayArray(DifxPhasedArray *dpa, int nPhasedArray)
{
	if(!dpa)
	{
		return;
	}
	free(dpa);
}

void fprintDifxPhasedArray(FILE *fp, const DifxPhasedArray *dpa)
{
	fprintf(fp, "  Difx Phased Array : %p\n", dpa);
	if(dpa)
	{
		fprintf(fp, "    Filename = %s\n", dpa->fileName);
		fprintf(fp, "    Output type = %s\n", phasedArrayOutputTypeNames[dpa->outputType]);
		fprintf(fp, "    Output format = %s\n", phasedArrayOutputFormatNames[dpa->outputFormat]);
		fprintf(fp, "    Accumulation time (ns) = %f\n", dpa->accTime);
		fprintf(fp, "    Complex output: %d\n", dpa->complexOutput);
		fprintf(fp, "    Output quantisation bits: %d\n", dpa->quantBits);
	}
}

void printDifxPhasedArray(const DifxPhasedArray *dpa)
{
	fprintDifxPhasedArray(stdout, dpa);
}

int isSameDifxPhasedArray(const DifxPhasedArray *dpa1, const DifxPhasedArray *dpa2)
{
	if(strcmp(dpa1->fileName, dpa2->fileName) == 0)
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

void copyDifxPhasedArray(DifxPhasedArray *dest, const DifxPhasedArray *src)
{
	snprintf(dest->fileName, DIFXIO_NAME_LENGTH, "%s", src->fileName);
	dest->outputType = src->outputType;
	dest->outputFormat = src->outputFormat;
	dest->accTime = src->accTime;
	dest->complexOutput = src->complexOutput;
	dest->quantBits = src->quantBits;
}

DifxPhasedArray *dupDifxPhasedarrayArray(const DifxPhasedArray *src, int nPhasedArray)
{
	DifxPhasedArray *dpa;
	int p;

	dpa = newDifxPhasedarrayArray(nPhasedArray);
	for(p = 0; p < nPhasedArray; p++)
	{
		copyDifxPhasedArray(dpa + p, src + p);
	}

	return dpa;
}

/* merge two DifxPhasedArray tables into an new one.  phasedArrayIdRemap will 
 * contain the mapping from dpa2's old indices to that of the merged set
 */
DifxPhasedArray *mergeDifxPhasedarrayArrays(const DifxPhasedArray *dpa1, int ndpa1,
	const DifxPhasedArray *dpa2, int ndpa2, int *phasedArrayIdRemap, int *ndpa)
{
	DifxPhasedArray *dpa = 0;
	int i, j;

	/* check for trivial cases */
	if(ndpa1 <= 0 && ndpa2 <= 0)
	{
		*ndpa = 0;
		return 0;
	}
	if(ndpa2 <= 0)
	{
		*ndpa = ndpa1;
		return dupDifxPhasedarrayArray(dpa1, ndpa1);
	}
	if(ndpa1 <= 0)
	{
		*ndpa = ndpa2;
		for(i = 0; i < ndpa2; i++)
		{
			phasedArrayIdRemap[i] = i;
		}
		return dupDifxPhasedarrayArray(dpa2, ndpa2);
	}

	/* both input arrays are non-trivial -- merge them */
	*ndpa = ndpa1;
	for(j = 0; j < ndpa2; j++)
	{
		for(i = 0; i < ndpa1; i++)
		{
			if(isSameDifxPhasedArray(dpa1 + i, dpa2 + j))
			{
				phasedArrayIdRemap[j] = i;
				break;
			}
		}
		if(i == ndpa1)
		{
			phasedArrayIdRemap[j] = *ndpa;
			(*ndpa)++;
		}
	}

	/* Allocate and copy */
	dpa = newDifxPhasedarrayArray(*ndpa);
	for(i = 0; i < ndpa1; i++)
	{
		copyDifxPhasedArray(dpa + i, dpa1 + i);
	}
	for(j = 0; j < ndpa2; j++)
	{
		i = phasedArrayIdRemap[j];
		if(i >= ndpa1)
		{
			copyDifxPhasedArray(dpa + i, dpa2 + j);
		}
	}

	return dpa;
}


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
#include "difxio/difx_write.h"


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

void fprintDifxDatastream(FILE *fp, const DifxDatastream *ds)
{
	int f;
	fprintf(fp, "  Difx Datastream Entry[antennaId=%d] : %p\n", 
		ds->antennaId, ds);
	fprintf(fp, "    format = %s\n", ds->dataFormat);
	fprintf(fp, "    quantization bits = %d\n", ds->quantBits);
	fprintf(fp, "    nFreq = %d\n", ds->nFreq);
	fprintf(fp, "    nRecChan = %d\n", ds->nRecChan);
	fprintf(fp, "    (freqId, nPol)[freq] =");
	for(f = 0; f < ds->nFreq; f++)
	{
		fprintf(fp, " (%d, %d)", ds->freqId[f], ds->nPol[f]);
	}
	fprintf(fp, "\n");
	fprintf(fp, "    (freq(index to above), pol)[recchan] =");
	for(f = 0; f < ds->nRecChan; f++)
	{
		fprintf(fp, " (%d, %c)", ds->RCfreqId[f], ds->RCpolName[f]);
	}
	fprintf(fp, "\n");
}

void printDifxDatastream(const DifxDatastream *ds)
{
	fprintDifxDatastream(stdout, ds);
}

int isSameDifxDatastream(const DifxDatastream *dd1, const DifxDatastream *dd2,
	const int *freqIdRemap, const int *antennaIdRemap)
{
	int f, c;
	
	if(dd1->antennaId != antennaIdRemap[dd2->antennaId] ||
	   strcmp(dd1->dataFormat, dd2->dataFormat) != 0 ||
	   dd1->nFreq != dd2->nFreq ||
	   dd1->nRecChan != dd2->nRecChan)
	{
		return 0;
	}
	for(f = 0; f < dd1->nFreq; f++)
	{
		if(dd1->nPol[f] != dd2->nPol[f] ||
		   dd1->freqId[f] != freqIdRemap[dd2->freqId[f]])
		{
			return 0;
		}
	}
	for(c = 0; c < dd1->nRecChan; c++)
	{
		if(dd1->RCfreqId[c]  != freqIdRemap[dd2->RCfreqId[c]] ||
		   dd1->RCpolName[c] != dd2->RCpolName[c])
		{
			return 0;
		}
	}

	return 1;
}

void DifxDatastreamAllocFreqs(DifxDatastream *dd, int nFreq)
{
	if(dd->freqId)
	{
		free(dd->freqId);
	}
	if(dd->nPol)
	{
		free(dd->nPol);
	}
	if(dd->clockOffset)
	{
		free(dd->clockOffset);
	}
	dd->nFreq = nFreq;
	dd->freqId = (int *)calloc(nFreq, sizeof(int));
	dd->nPol = (int *)calloc(nFreq, sizeof(int));
	dd->clockOffset = (double *)calloc(nFreq, sizeof(double));
}

void DifxDatastreamAllocRecChans(DifxDatastream *dd, int nRecChan)
{
	if(dd->RCfreqId)
	{
		free(dd->RCfreqId);
	}
	if(dd->RCpolName)
	{
		free(dd->RCpolName);
	}
	dd->nRecChan = nRecChan;
	dd->RCfreqId = (int *)calloc(nRecChan, sizeof(int));
	dd->RCpolName = (char *)calloc(nRecChan, sizeof(char));
}

void copyDifxDatastream(DifxDatastream *dest, const DifxDatastream *src,
	const int *freqIdRemap, const int *antennaIdRemap)
{
	int f, c;
	
	if(antennaIdRemap != 0)
	{
		dest->antennaId = antennaIdRemap[src->antennaId];
	}
	else
	{
		dest->antennaId = src->antennaId;
	}
	strcpy(dest->dataFormat, src->dataFormat);
	dest->quantBits = src->quantBits;

	DifxDatastreamAllocFreqs(dest, src->nFreq);
	DifxDatastreamAllocRecChans(dest, src->nRecChan);
	for(f = 0; f < dest->nFreq; f++)
	{
		dest->nPol[f] = src->nPol[f];
		if(freqIdRemap)
		{
			dest->freqId[f] = freqIdRemap[src->freqId[f]];
		}
		else
		{
			dest->freqId[f] = src->freqId[f];
		}
		dest->clockOffset[f] = src->clockOffset[f];
	}
	for(c = 0; c < dest->nRecChan; c++)
	{
		dest->RCfreqId[c]  = src->RCfreqId[c];
		dest->RCpolName[c] = src->RCpolName[c];
	}
}

DifxDatastream *mergeDifxDatastreamArrays(const DifxDatastream *dd1, int ndd1,
	const DifxDatastream *dd2, int ndd2, int *datastreamIdRemap,
	const int *freqIdRemap, const int *antennaIdRemap, int *ndd)
{
	int i, j;
	DifxDatastream *dd;

	*ndd = ndd1;

	/* first identify entries that differ and assign new datastreamIds */
	for(j = 0; j < ndd2; j++)
	{
		for(i = 0; i < ndd1; i++)
		{
			if(isSameDifxDatastream(dd1 + i, dd2 + j,
				freqIdRemap, antennaIdRemap))
			{
				datastreamIdRemap[j] = i;
				break;
			}
		}
		if(i == ndd1)
		{
			datastreamIdRemap[j] = *ndd;
			(*ndd)++;
		}
	}

	dd = newDifxDatastreamArray(*ndd);
	
	/* now copy dd1 */
	for(i = 0; i < ndd1; i++)
	{
		copyDifxDatastream(dd + i, dd1 + i, 0, 0);
	}

	/* now copy unique members of dd2 */
	for(j = 0; j < ndd2; j++)
	{
		if(datastreamIdRemap[j] >= ndd1)
		{
			copyDifxDatastream(dd + datastreamIdRemap[j], dd2 + j,
				freqIdRemap, antennaIdRemap);
		}
	}

	return dd;
}

int writeDifxDatastream(FILE *out, const DifxDatastream *dd)
{
	int i;
	char pol[2];

	pol[1] = 0;

	writeDifxLineInt(out, "TELESCOPE INDEX", dd->antennaId);
	writeDifxLineDouble(out, "TSYS", "%4.2f", dd->tSys);
	writeDifxLine(out, "DATA FORMAT", dd->dataFormat);
	writeDifxLineInt(out, "QUANTISATION BITS", dd->quantBits);
	writeDifxLineInt(out, "DATA FRAME SIZE", dd->dataFrameSize);
	writeDifxLine(out, "DATA SOURCE", dd->dataSource);
	writeDifxLine(out, "FILTERBANK USED", "FALSE");
	writeDifxLineInt(out, "NUM FREQS", dd->nFreq);
	for(i = 0; i < dd->nFreq; i++)
	{
		writeDifxLineInt1(out, "FREQ TABLE INDEX %d", i, dd->freqId[i]);
		writeDifxLineDouble1(out, "CLK OFFSET %d (us)", i, 
			"%8.6f", dd->clockOffset[i]);
		writeDifxLineInt1(out, "NUM POLS %d", i, dd->nPol[i]);
	}
	for(i = 0; i < dd->nRecChan; i++)
	{
		pol[0] = dd->RCpolName[i];
		writeDifxLine1(out, "INPUT BAND %d POL", i, pol);
		writeDifxLineInt1(out, "INPUT BAND %d INDEX", i, 
			dd->RCfreqId[i]);
	}

	return 8 + 3*dd->nFreq + 2*dd->nRecChan;
}

int DifxDatastreamGetRecChans(DifxDatastream *ds, int freqId, char *pols, int *recChans)
{
	int r;
	int n=0;

	for(r = 0; r < ds->nRecChan; r++)
	{
		if(ds->RCfreqId[r] == freqId)
		{
			if(ds->RCpolName[r] <= ' ')
			{
				continue;
			}
			if(n >= 2)
			{
				fprintf(stderr, "Warning: skipping dup rechan\n");
			}
			else if(n == 1 && ds->RCpolName[r] == pols[0])
			{
				fprintf(stderr, "Warning: skipping dup rechan\n");
			}
			else
			{
				pols[n] = ds->RCpolName[r];
				recChans[n] = r;
				n++;
			}
		}
	}

	if(n == 2)
	{
		if(pols[0] == 'L' && pols[1] == 'R')
		{
			r = recChans[0];
			recChans[0] = recChans[1];
			recChans[1] = r;
			pols[0] = 'R';
			pols[1] = 'L';
		}
		if(pols[0] == 'Y' && pols[1] == 'X')
		{
			r = recChans[0];
			recChans[0] = recChans[1];
			recChans[1] = r;
			pols[0] = 'X';
			pols[1] = 'Y';
		}
	}

	return n;
}

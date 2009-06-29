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
	DifxDatastream *dd;

	dd = (DifxDatastream *)calloc(nDatastream, sizeof(DifxDatastream));

	return dd;
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

void deleteDifxDatastreamInternals(DifxDatastream *dd)
{
	if(dd->nPol)
	{
		free(dd->nPol);
		dd->nPol = 0;
	}
	if(dd->freqId)
	{
		free(dd->freqId);
		dd->freqId = 0;
	}
	if(dd->clockOffset)
	{
		free(dd->clockOffset);
		dd->clockOffset = 0;
	}
	if(dd->RCfreqId)
	{
		free(dd->RCfreqId);
		dd->RCfreqId = 0;
	}
	if(dd->RCpolName)
	{
		free(dd->RCpolName);
		dd->RCpolName = 0;
	}
}

void deleteDifxDatastreamArray(DifxDatastream *dd, int nDatastream)
{
	int e;

	if(dd)
	{
		for(e = 0; e < nDatastream; e++)
		{
			deleteDifxDatastreamInternals(dd + e);
		}
		free(dd);
	}
}

void fprintDifxDatastream(FILE *fp, const DifxDatastream *dd)
{
	int f;
	fprintf(fp, "  Difx Datastream Entry[antennaId=%d] : %p\n", 
		dd->antennaId, dd);
	fprintf(fp, "    format = %s\n", dd->dataFormat);
	fprintf(fp, "    quantization bits = %d\n", dd->quantBits);
	fprintf(fp, "    nFreq = %d\n", dd->nFreq);
	fprintf(fp, "    nRecChan = %d\n", dd->nRecChan);
	fprintf(fp, "    (freqId, nPol)[freq] =");
	for(f = 0; f < dd->nFreq; f++)
	{
		fprintf(fp, " (%d, %d)", dd->freqId[f], dd->nPol[f]);
	}
	fprintf(fp, "\n");
	fprintf(fp, "    (freq(index to above), pol)[recchan] =");
	for(f = 0; f < dd->nRecChan; f++)
	{
		fprintf(fp, " (%d, %c)", dd->RCfreqId[f], dd->RCpolName[f]);
	}
	fprintf(fp, "\n");
}

void printDifxDatastream(const DifxDatastream *dd)
{
	fprintDifxDatastream(stdout, dd);
}

int isSameDifxDatastream(const DifxDatastream *dd1, const DifxDatastream *dd2,
	const int *freqIdRemap, const int *antennaIdRemap)
{
	int f, c, antennaId2, freqId2;

	if(antennaIdRemap)
	{
		antennaId2 = antennaIdRemap[dd2->antennaId];
	}
	else
	{
		antennaId2 = dd2->antennaId;
	}
	
	if(dd1->antennaId != antennaId2 ||
	   strcmp(dd1->dataFormat, dd2->dataFormat) != 0 ||
	   dd1->nFreq != dd2->nFreq ||
	   dd1->nRecChan != dd2->nRecChan)
	{
		return 0;
	}
	for(f = 0; f < dd1->nFreq; f++)
	{
		if(freqIdRemap)
		{
			freqId2 = freqIdRemap[dd2->freqId[f]];
		}
		else
		{
			freqId2 = dd2->freqId[f];
		}
		if(dd1->nPol[f] != dd2->nPol[f] ||
		   dd1->freqId[f] != freqId2 ||
		   dd1->clockOffset[f] != dd2->clockOffset[f])
		{
			return 0;
		}
	}
	for(c = 0; c < dd1->nRecChan; c++)
	{
		if(dd1->RCfreqId[c]  != dd2->RCfreqId[c] ||
		   dd1->RCpolName[c] != dd2->RCpolName[c])
		{
			return 0;
		}
	}

	return 1;
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

/* don't re-allocate internal structures */
void moveDifxDatastream(DifxDatastream *dest, DifxDatastream *src)
{
	dest->antennaId = src->antennaId;
	strcpy(dest->dataFormat, src->dataFormat);
	dest->quantBits = src->quantBits;
	dest->freqId = src->freqId;
	dest->nPol = src->nPol;
	dest->clockOffset = src->clockOffset;
	dest->RCfreqId = src->RCfreqId;
	dest->RCpolName = src->RCpolName;

	/* unlink src data structures */
	src->freqId = 0;
	src->nPol = 0;
	src->clockOffset = 0;
	src->RCfreqId = 0;
	src->RCpolName = 0;
}

int simplifyDifxDatastreams(DifxInput *D)
{
	int d, d1;
	int d0;
	int b, c, cd;
	int n0;

	n0 = D->nDatastream;

	if(n0 < 2)
	{
		return 0;
	}

	for(d = 1;;)
	{
		if(d >= D->nDatastream)
		{
			break;
		}

		for(d1 = 0; d1 < d; d1++)
		{
			if(isSameDifxDatastream(D->datastream+d, D->datastream+d1, 0, 0))
			{
				break;
			}
		}
		if(d == d1)	/* no match found */
		{
			d++;	/* advance to next datastream */
		}
		else		/* found match */
		{
			/* 1. Renumber this and all higher datastreams */
			for(b = 0; b < D->nBaseline; b++)
			{
				d0 = D->baseline[b].dsA;
				if(d0 == d)
				{
					d0 = d1;
				}
				else if(d0 > d)
				{
					d0--;
				}
				D->baseline[b].dsA = d0;

				d0 = D->baseline[b].dsB;
				if(d0 == d)
				{
					d0 = d1;
				}
				else if(d0 > d)
				{
					d0--;
				}
				D->baseline[b].dsB = d0;
			}

			for(c = 0; c < D->nConfig; c++)
			{
				for(cd = 0; cd < D->config[c].nDatastream; cd++)
				{
					d0 = D->config[c].datastreamId[cd];
					if(d0 == d)
					{
						d0 = d1;
					}
					else if(d0 > d)
					{
						d0--;
					}
					D->config[c].datastreamId[cd] = d0;
				}
			}

			/* 2. reduce number of datastreams */
			D->nDatastream--;

			/* 3. Delete this datastream and bump up higher ones */
			deleteDifxDatastreamInternals(D->datastream + d);
			for(d1 = d; d1 < D->nDatastream; d1++)
			{
				moveDifxDatastream(D->datastream+d1, D->datastream+d1+1);
			}
		}
	}

	return n0 - D->nDatastream;
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
	writeDifxLineDouble(out, "TSYS", "%f", dd->tSys);
	writeDifxLine(out, "DATA FORMAT", dd->dataFormat);
	writeDifxLineInt(out, "QUANTISATION BITS", dd->quantBits);
	writeDifxLineInt(out, "DATA FRAME SIZE", dd->dataFrameSize);
	writeDifxLine(out, "DATA SOURCE", dd->dataSource);
	writeDifxLineBoolean(out, "FILTERBANK USED", 0);
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

int DifxDatastreamGetRecChans(DifxDatastream *dd, int freqId, char *pols, int *recChans)
{
	int r;
	int n=0;

	for(r = 0; r < dd->nRecChan; r++)
	{
		if(dd->RCfreqId[r] == freqId)
		{
			if(dd->RCpolName[r] <= ' ')
			{
				continue;
			}
			if(n >= 2)
			{
				fprintf(stderr, "Warning: skipping dup rechan\n");
			}
			else if(n == 1 && dd->RCpolName[r] == pols[0])
			{
				fprintf(stderr, "Warning: skipping dup rechan\n");
			}
			else
			{
				pols[n] = dd->RCpolName[r];
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

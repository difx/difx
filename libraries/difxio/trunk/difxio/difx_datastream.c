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

void DifxDatastreamAllocFreqs(DifxDatastream *dd, int nRecFreq)
{
	if(dd->recFreqId)
	{
		free(dd->recFreqId);
	}
	if(dd->nRecPol)
	{
		free(dd->nRecPol);
	}
	if(dd->clockOffset)
	{
		free(dd->clockOffset);
	}
	if(dd->freqOffset)
	{
		free(dd->freqOffset);
	}
	dd->nRecFreq = nRecFreq;
	dd->recFreqId = (int *)calloc(nRecFreq, sizeof(int));
	dd->nRecPol = (int *)calloc(nRecFreq, sizeof(int));
	dd->clockOffset = (double *)calloc(nRecFreq, sizeof(double));
	dd->freqOffset = (double *)calloc(nRecFreq, sizeof(double));
        if(dd->zoomFreqId)
        {
                free(dd->zoomFreqId);
        }
        if(dd->nZoomPol)
        {
                free(dd->nZoomPol);
        }
        dd->nZoomFreq = 0;
        dd->zoomFreqId = 0;
        dd->nZoomPol = 0;
}

void DifxDatastreamAllocBands(DifxDatastream *dd, int nRecBand)
{
	if(dd->recBandFreqId)
	{
		free(dd->recBandFreqId);
	}
	if(dd->recBandPolName)
	{
		free(dd->recBandPolName);
	}
	dd->nRecBand = nRecBand;
	dd->recBandFreqId = (int *)calloc(nRecBand, sizeof(int));
	dd->recBandPolName = (char *)calloc(nRecBand, sizeof(char));
	if(dd->zoomBandFreqId)
	{
		free(dd->zoomBandFreqId);
	}
	if(dd->zoomBandPolName)
	{
		free(dd->zoomBandPolName);
	}
	dd->nZoomBand = 0; //for now
	dd->zoomBandFreqId = 0;
	dd->zoomBandPolName = 0;
}

void DifxDatastreamAllocZoomFreqs(DifxDatastream *dd, int nZoomFreq)
{
        if(dd->zoomFreqId)
        {
                free(dd->zoomFreqId);
        }
        if(dd->nZoomPol)
        {
                free(dd->nZoomPol);
        }
	dd->nZoomFreq = nZoomFreq;
        dd->zoomFreqId = (int *)calloc(nZoomFreq, sizeof(int));
        dd->nZoomPol = (int *)calloc(nZoomFreq, sizeof(int));
}

void DifxDatastreamAllocZoomBands(DifxDatastream *dd, int nZoomBand)
{
        if(dd->zoomBandFreqId)
        {
                free(dd->zoomBandFreqId);
        }
        if(dd->zoomBandPolName)
        {
                free(dd->zoomBandPolName);
        }
        dd->nZoomBand = nZoomBand;
        dd->zoomBandFreqId = (int *)calloc(nZoomBand, sizeof(int));
        dd->zoomBandPolName = (char *)calloc(nZoomBand, sizeof(char));
}

void deleteDifxDatastreamInternals(DifxDatastream *dd)
{
	if(dd->nRecPol)
	{
		free(dd->nRecPol);
		dd->nRecPol = 0;
	}
	if(dd->recFreqId)
	{
		free(dd->recFreqId);
		dd->recFreqId = 0;
	}
	if(dd->clockOffset)
	{
		free(dd->clockOffset);
		dd->clockOffset = 0;
	}
	if(dd->freqOffset)
	{
		free(dd->freqOffset);
		dd->freqOffset = 0;
	}
	if(dd->recBandFreqId)
	{
		free(dd->recBandFreqId);
		dd->recBandFreqId = 0;
	}
	if(dd->recBandPolName)
	{
		free(dd->recBandPolName);
		dd->recBandPolName = 0;
	}
	if(dd->nZoomPol)
	{
		free(dd->nZoomPol);
		dd->nZoomPol = 0;
	}
	if(dd->zoomFreqId)
	{
		free(dd->zoomFreqId);
		dd->zoomFreqId = 0;
	}
	if(dd->zoomBandFreqId)
	{
		free(dd->zoomBandFreqId);
		dd->zoomBandFreqId = 0;
	}
	if(dd->zoomBandPolName)
	{
		free(dd->zoomBandPolName);
		dd->zoomBandPolName = 0;
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
	fprintf(fp, "    nRecFreq = %d\n", dd->nRecFreq);
	fprintf(fp, "    nRecBand = %d\n", dd->nRecBand);
	fprintf(fp, "    (RecFreqId, nRecPol)[freq] =");
	for(f = 0; f < dd->nRecFreq; f++)
	{
		fprintf(fp, " (%d, %d)", dd->recFreqId[f], dd->nRecPol[f]);
	}
	fprintf(fp, "\n");
	fprintf(fp, "    (freq(index to above), pol)[recchan] =");
	for(f = 0; f < dd->nRecBand; f++)
	{
		fprintf(fp, " (%d, %c)", dd->recBandFreqId[f], dd->recBandPolName[f]);
	}
	fprintf(fp, "    nZoomFreq = %d\n", dd->nZoomFreq);
	fprintf(fp, "    nZoomBand = %d\n", dd->nZoomBand);
	fprintf(fp, "    (ZoomFreqId, nZoomPol)[freq] =");
	for(f = 0; f < dd->nZoomFreq; f++)
	{
		fprintf(fp, " (%d, %d)", dd->zoomFreqId[f], dd->nZoomPol[f]);
	}
	fprintf(fp, "\n");
	fprintf(fp, "    (freq(index to above), pol)[recchan] =");
	for(f = 0; f < dd->nZoomBand; f++)
	{
		fprintf(fp, " (%d, %c)", dd->zoomBandFreqId[f], dd->zoomBandPolName[f]);
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
	   dd1->nRecFreq != dd2->nRecFreq ||
	   dd1->nRecBand != dd2->nRecBand ||
	   dd1->nZoomFreq != dd2->nZoomFreq ||
	   dd1->nZoomBand != dd2->nZoomBand)
	{
		return 0;
	}
	for(f = 0; f < dd1->nRecFreq; f++)
	{
		if(freqIdRemap)
		{
			freqId2 = freqIdRemap[dd2->recFreqId[f]];
		}
		else
		{
			freqId2 = dd2->recFreqId[f];
		}
		if(dd1->nRecPol[f] != dd2->nRecPol[f] ||
		   dd1->recFreqId[f] != freqId2 ||
		   dd1->clockOffset[f] != dd2->clockOffset[f] ||
		   dd1->freqOffset[f] != dd2->freqOffset[f])
		{
			return 0;
		}
	}
	for(c = 0; c < dd1->nRecBand; c++)
	{
		if(dd1->recBandFreqId[c]  != dd2->recBandFreqId[c] ||
		   dd1->recBandPolName[c] != dd2->recBandPolName[c])
		{
			return 0;
		}
	}
	for(f = 0; f < dd1->nZoomFreq; f++)
	{
		if(freqIdRemap)
		{
			freqId2 = freqIdRemap[dd2->zoomFreqId[f]];
		}
		else
		{
			freqId2 = dd2->zoomFreqId[f];
		}
		if(dd1->nZoomPol[f] != dd2->nZoomPol[f] ||
		   dd1->zoomFreqId[f] != freqId2)
		{
			return 0;
		}
	}
	for(c = 0; c < dd1->nZoomBand; c++)
	{
		if(dd1->zoomBandFreqId[c]  != dd2->zoomBandFreqId[c] ||
		   dd1->zoomBandPolName[c] != dd2->zoomBandPolName[c])
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
	dest->phaseCalIntervalMHz = src->phaseCalIntervalMHz;

	DifxDatastreamAllocFreqs(dest, src->nRecFreq);
	DifxDatastreamAllocBands(dest, src->nRecBand);
	for(f = 0; f < dest->nRecFreq; f++)
	{
		dest->nRecPol[f] = src->nRecPol[f];
		if(freqIdRemap)
		{
			dest->recFreqId[f] = freqIdRemap[src->recFreqId[f]];
		}
		else
		{
			dest->recFreqId[f] = src->recFreqId[f];
		}
		dest->clockOffset[f] = src->clockOffset[f];
		dest->freqOffset[f]  = src->freqOffset[f];
	}
	for(c = 0; c < dest->nRecBand; c++)
	{
		dest->recBandFreqId[c]  = src->recBandFreqId[c];
		dest->recBandPolName[c] = src->recBandPolName[c];
	}
	for(f = 0; f < dest->nZoomFreq; f++)
	{
		dest->nZoomPol[f] = src->nZoomPol[f];
		if(freqIdRemap)
		{
			dest->zoomFreqId[f] = freqIdRemap[src->zoomFreqId[f]];
		}
		else
		{
			dest->zoomFreqId[f] = src->zoomFreqId[f];
		}
	}
	for(c = 0; c < dest->nZoomBand; c++)
	{
		dest->zoomBandFreqId[c]  = src->zoomBandFreqId[c];
		dest->zoomBandPolName[c] = src->zoomBandPolName[c];
	}
}

/* don't re-allocate internal structures */
void moveDifxDatastream(DifxDatastream *dest, DifxDatastream *src)
{
	dest->antennaId = src->antennaId;
	strcpy(dest->dataFormat, src->dataFormat);
	dest->quantBits = src->quantBits;
	dest->recFreqId = src->recFreqId;
	dest->nRecPol = src->nRecPol;
	dest->clockOffset = src->clockOffset;
	dest->freqOffset = src->freqOffset;
	dest->recBandFreqId = src->recBandFreqId;
	dest->recBandPolName = src->recBandPolName;
	dest->zoomFreqId = src->zoomFreqId;
	dest->nZoomPol = src->nZoomPol;
	dest->zoomBandFreqId = src->zoomBandFreqId;
	dest->zoomBandPolName = src->zoomBandPolName;

	/* unlink src data structures */
	src->recFreqId = 0;
	src->nRecPol = 0;
	src->clockOffset = 0;
	src->freqOffset = 0;
	src->recBandFreqId = 0;
	src->recBandPolName = 0;
	src->zoomFreqId = 0;
	src->nZoomPol = 0;
	src->zoomBandFreqId = 0;
	src->zoomBandPolName = 0;
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
	writeDifxLine(out, "FILTERBANK USED", "FALSE");
	writeDifxLineInt(out, "PHASE CAL INT (MHZ)", dd->phaseCalIntervalMHz);
	writeDifxLineInt(out, "NUM RECORDED FREQS", dd->nRecFreq);
	for(i = 0; i < dd->nRecFreq; i++)
	{
		writeDifxLineInt1(out, "REC FREQ INDEX %d", i, dd->recFreqId[i]);
		writeDifxLineDouble1(out, "CLK OFFSET %d (us)", i, 
			"%8.6f", dd->clockOffset[i]);
		writeDifxLineDouble1(out, "FREQ OFFSET %d (Hz)", i,
			"%8.6f", dd->freqOffset[i]);
		writeDifxLineInt1(out, "NUM REC POLS %d", i, dd->nRecPol[i]);
	}
	for(i = 0; i < dd->nRecBand; i++)
	{
		pol[0] = dd->recBandPolName[i];
		writeDifxLine1(out, "REC BAND %d POL", i, pol);
		writeDifxLineInt1(out, "REC BAND %d INDEX", i, 
			dd->recBandFreqId[i]);
	}
	writeDifxLineInt(out, "NUM ZOOM FREQS", dd->nZoomFreq);
	for(i = 0; i < dd->nZoomFreq; i++)
	{
		writeDifxLineInt1(out, "ZOOM FREQ INDEX %d", i, dd->zoomFreqId[i]);
		writeDifxLineInt1(out, "ZOOM REC POLS %d", i, dd->nZoomPol[i]);
	}
	for(i = 0; i < dd->nZoomBand; i++)
	{
		pol[0] = dd->zoomBandPolName[i];
		writeDifxLine1(out, "ZOOM BAND %d POL", i, pol);
		writeDifxLineInt1(out, "ZOOM BAND %d INDEX", i,
			dd->zoomBandFreqId[i]);
	}

	return 8 + 4*dd->nRecFreq + 2*dd->nRecBand + 2*dd->nZoomFreq + 2*dd->nZoomBand;
}

int DifxDatastreamGetRecBands(DifxDatastream *dd, int freqId, char *pols, int *recBands)
{
	int r;
	int n=0;

	for(r = 0; r < dd->nRecBand; r++)
	{
		if(dd->recBandFreqId[r] < 0 || dd->recBandFreqId[r] >= dd->nRecFreq)
		{
			fprintf(stderr, "Error! recBandFreqId[%d] is %d where nRecFreq is %d\n",
			        r, dd->recBandFreqId[r], dd->nRecFreq);
			continue;
		}
		if(dd->recFreqId[dd->recBandFreqId[r]] == freqId)
		{
			if(dd->recBandPolName[r] <= ' ')
			{
				continue;
			}
			if(n >= 2)
			{
				fprintf(stderr, "Warning: skipping dup rechan\n");
			}
			else if(n == 1 && dd->recBandPolName[r] == pols[0])
			{
				fprintf(stderr, "Warning: skipping dup rechan\n");
			}
			else
			{
				pols[n] = dd->recBandPolName[r];
				recBands[n] = r;
				n++;
			}
		}
	}

	if(n == 2)
	{
		if(pols[0] == 'L' && pols[1] == 'R')
		{
			r = recBands[0];
			recBands[0] = recBands[1];
			recBands[1] = r;
			pols[0] = 'R';
			pols[1] = 'L';
		}
		if(pols[0] == 'Y' && pols[1] == 'X')
		{
			r = recBands[0];
			recBands[0] = recBands[1];
			recBands[1] = r;
			pols[0] = 'X';
			pols[1] = 'Y';
		}
	}

	return n;
}

int DifxDatastreamGetZoomBands(DifxDatastream *dd, int freqId, char *pols, int *zoomBands)
{
        int z;
        int n=0;
        for(z = 0; z < dd->nZoomBand; z++)
        {
		if(dd->zoomBandFreqId[z] < 0 || dd->zoomBandFreqId[z] >= dd->nZoomFreq)
		{
			fprintf(stderr, "Error! zoomBandFreqId[%d] is %d where nZoomFreq is %d\n",
				z, dd->zoomBandFreqId[z], dd->nZoomFreq);
				continue;
		}
                if(dd->zoomFreqId[dd->zoomBandFreqId[z]] == freqId)
                {
                        if(dd->zoomBandPolName[z] <= ' ')
                        {
                                continue;
                        }
                        if(n >= 2)
                        {
                                fprintf(stderr, "Warning: skipping dup zoomchan\n");
                        }
                        else if(n == 1 && dd->zoomBandPolName[z] == pols[0])
                        {
                                fprintf(stderr, "Warning: skipping dup zoomchan\n");
                        }
                        else
                        {
                                pols[n] = dd->zoomBandPolName[z];
                                zoomBands[n] = z;
                                n++;
                        }
                }
        }

	if(n == 2)
        {
                if(pols[0] == 'L' && pols[1] == 'R')
                {
                        z = zoomBands[0];
                        zoomBands[0] = zoomBands[1];
                        zoomBands[1] = z;
                        pols[0] = 'R';
                        pols[1] = 'L';
                }
                if(pols[0] == 'Y' && pols[1] == 'X')
                {
                        z = zoomBands[0];
                        zoomBands[0] = zoomBands[1];
                        zoomBands[1] = z;
                        pols[0] = 'X';
                        pols[1] = 'Y';
                }
        }

        return n;
}

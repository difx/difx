/***************************************************************************
 *   Copyright (C) 2008-2016 by Walter Brisken                             *
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
// $Id: difx_datastream.c 10966 2023-05-09 06:52:03Z JanWagner $
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/difxio/trunk/difxio/difx_datastream.c $
// $LastChangedRevision: 10966 $
// $Author: JanWagner $
// $LastChangedDate: 2023-05-09 00:52:03 -0600 (Tue, 09 May 2023) $
//
//============================================================================

#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <math.h>
#include "difxio/difx_write.h"

/* Note: these values cannot have whitespace */
const char dataSourceNames[][MAX_DATA_SOURCE_NAME_LENGTH] = 
{
	"NONE",
	"MODULE",
	"FILE",
	"NETWORK",
	"FAKE",
	"MARK6",
	"SHAREDMEMORY",
	"UNSPECIFIED",	/* should be second to last entry */
	"ERROR"		/* takes place of NumDataSources */
};

/* Note: these values cannot have whitespace */
const char samplingTypeNames[][MAX_SAMPLING_NAME_LENGTH] =
{
	"REAL",
	"COMPLEX",
	"COMPLEX_DSB",
	"ERROR"		/* takes place of NumSamplingTypes */
};

enum DataSource stringToDataSource(const char *str)
{
	enum DataSource type;

	if(strcasecmp(str, "MARK5") == 0)
	{
		type = DataSourceModule;
	}
	else
	{
		for(type = 0; type < NumDataSources; ++type)
		{
			if(strcasecmp(str, dataSourceNames[type]) == 0)
			{
				break;
			}
		}
	}

	return type;	/* value = NumDataSources impies error */
}

enum SamplingType stringToSamplingType(const char *str)
{
	enum SamplingType type;

	for(type = 0; type < NumSamplingTypes; ++type)
	{
		if(strcasecmp(str, samplingTypeNames[type]) == 0)
		{
			break;
		}
	}

	return type;
}

DifxDatastream *newDifxDatastreamArray(int nDatastream)
{
	DifxDatastream *dd;
	int s;

	dd = (DifxDatastream *)calloc(nDatastream, sizeof(DifxDatastream));
	for(s = 0; s < nDatastream; ++s)
	{
		dd[s].dataSource = DataSourceUnspecified;
		dd[s].pol[0] = ' ';
		dd[s].pol[1] = ' ';
	}

	return dd;
}

void DifxDatastreamAllocFiles(DifxDatastream *ds, int nFile)
{
	int i;

	if(ds->file)
	{
		for(i = 0; i < ds->nFile; ++i)
		{
			if(ds->file[i])
			{
				free(ds->file[i]);
				ds->file[i] = 0;
			}
		}
		free(ds->file);
		ds->file = 0;
	}
	
	ds->nFile = nFile;
	if(nFile > 0)
	{
		ds->file = (char **)calloc(nFile, sizeof(char *));
	}
}

void DifxDatastreamAllocFreqs(DifxDatastream *dd, int nRecFreq)
{
	if(dd->recFreqId)
	{
		free(dd->recFreqId);
		dd->recFreqId = 0;
	}
	if(dd->recFreqDestId)
	{
		free(dd->recFreqDestId);
		dd->recFreqDestId = 0;
	}
	if(dd->nRecPol)
	{
		free(dd->nRecPol);
		dd->nRecPol = 0;
	}
	if(dd->clockOffset)
	{
		free(dd->clockOffset);
		dd->clockOffset = 0;
	}
	if(dd->clockOffsetDelta)
	{
		free(dd->clockOffsetDelta);
		dd->clockOffsetDelta = 0;
	}
	if(dd->phaseOffset)
	{
		free(dd->phaseOffset);
		dd->phaseOffset = 0;
	}
	if(dd->freqOffset)
	{
		free(dd->freqOffset);
		dd->freqOffset = 0;
	}

	dd->nRecFreq = nRecFreq;
	if(nRecFreq > 0)
	{
		dd->recFreqId = (int *)calloc(nRecFreq, sizeof(int));
		dd->recFreqDestId = (int *)calloc(nRecFreq, sizeof(int));
		dd->nRecPol = (int *)calloc(nRecFreq, sizeof(int));
		dd->clockOffset = (double *)calloc(nRecFreq, sizeof(double));
		dd->clockOffsetDelta = (double *)calloc(nRecFreq, sizeof(double));
		dd->phaseOffset = (double *)calloc(nRecFreq, sizeof(double));
		dd->freqOffset = (double *)calloc(nRecFreq, sizeof(double));
	}
}

void DifxDatastreamAllocBands(DifxDatastream *dd, int nRecBand)
{
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

	dd->nRecBand = nRecBand;
	if(nRecBand > 0)
	{
		dd->recBandFreqId = (int *)calloc(nRecBand, sizeof(int));
		dd->recBandPolName = (char *)calloc(nRecBand, sizeof(char));
	}
}

void DifxDatastreamAllocZoomFreqs(DifxDatastream *dd, int nZoomFreq)
{
	if(dd->zoomFreqId)
	{
		free(dd->zoomFreqId);
		dd->zoomFreqId = 0;
	}
	if(dd->zoomFreqDestId)
	{
		free(dd->zoomFreqDestId);
		dd->zoomFreqDestId = 0;
	}
	if(dd->nZoomPol)
	{
		free(dd->nZoomPol);
		dd->nZoomPol = 0;
	}
	dd->nZoomFreq = nZoomFreq;
	if(nZoomFreq > 0)
	{
		dd->zoomFreqId = (int *)calloc(nZoomFreq, sizeof(int));
		dd->zoomFreqDestId = (int *)calloc(nZoomFreq, sizeof(int));
		dd->nZoomPol = (int *)calloc(nZoomFreq, sizeof(int));
	}
}

void DifxDatastreamAllocZoomBands(DifxDatastream *dd, int nZoomBand)
{
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
	dd->nZoomBand = nZoomBand;
	if(nZoomBand > 0)
	{
		dd->zoomBandFreqId = (int *)calloc(nZoomBand, sizeof(int));
		dd->zoomBandPolName = (char *)calloc(nZoomBand, sizeof(char));
	}
}

void DifxDatastreamAllocPhasecalTones(DifxDatastream *dd, int nTones)
{
	if(dd->recToneFreq)
	{
		free(dd->recToneFreq);
		dd->recToneFreq = 0;
	}
	if(dd->recToneOut)
	{
		free(dd->recToneOut);
		dd->recToneOut = 0;
	}
	dd->nRecTone = nTones;
	if(nTones > 0)
	{
		dd->recToneFreq = (int *)calloc(nTones, sizeof(int));
		dd->recToneOut = (int *)calloc(nTones, sizeof(int));
	}
}

int DifxDatastreamGetPhasecalRange(const DifxDatastream *dd, const DifxFreq *df, double* lowest, double* highest)
{
	double lowEdge;
	double tonefreq;
	double toneinterval;
	int permitEdgeTone = 0;

	if(lowest)
	{
		*lowest = 0;
	}
	if(highest)
	{
		*highest = 0;
	}

	if(dd->nRecFreq == 0 || dd->phaseCalIntervalMHz == 0)
	{
		/* Can't do anything... */
		return 0;
	}

	/* find bottom end of baseband */
	lowEdge = df->freq;
	if(df->sideband == 'L')
	{
		lowEdge -= df->bw;
		permitEdgeTone = 1; // Nyquist of LSB is allowed
	}

	/* lowest frequency pcal */
	toneinterval = dd->phaseCalIntervalMHz / dd->phaseCalIntervalDivisor; // NB: rounding errors are uncritical here
	tonefreq = ((unsigned long)(lowEdge / toneinterval)) * toneinterval + dd->phaseCalBaseMHz;
	if(tonefreq < lowEdge)
	{
		tonefreq += toneinterval;
	}
	if(!permitEdgeTone && fabs(tonefreq-lowEdge) < 1e-6)
	{
		//skip tone in DC bin
		tonefreq += toneinterval;
	}

	/* calculate number of tones that fit the band */
	int ntones = 0;
	while (tonefreq + ntones * toneinterval < lowEdge + df->bw)
	{
		++ntones;
	}

	/* return the range (tone freqs in MHz) */
	if(lowest && ntones)
	{
		*lowest = tonefreq;
	}
	if(highest && ntones)
	{
		*highest = tonefreq + (ntones - 1) * toneinterval;
	}

	if(0)
	{
		printf("full: band %.5g .. %.5g %cSB, pcal comb N*%.5g + %.5g\n", lowEdge, lowEdge + df->bw, df->sideband, toneinterval, dd->phaseCalBaseMHz);
		printf("      first tone in band is %.5g\n", tonefreq);
		printf("      final tone of band is %.5g\n", tonefreq + (ntones - 1) * toneinterval);
		printf("      ntones is %d\n", ntones);
	}

	return ntones;
}

/* Must have rec band/freq and freq table filled in before calling this function */
void DifxDatastreamCalculatePhasecalTones(DifxDatastream *dd, const DifxFreq *df)
{
	double lowest, highest, toneinterval;
	int i;

	if(dd->nRecFreq == 0 || dd->phaseCalIntervalMHz == 0)
	{
		/* Can't do anything... */
		return;
	}

	/* Get number of tones, and lowest/highest in-band non-DC tone frequencies */
	dd->nRecTone = DifxDatastreamGetPhasecalRange(dd, df, &lowest, &highest);

	/* Allocate the tone frequency and on/off arrays */
	DifxDatastreamAllocPhasecalTones(dd, dd->nRecTone);

	/* Fill in the tone frequencies and on/off values */
	toneinterval = dd->phaseCalIntervalMHz / dd->phaseCalIntervalDivisor; // NB: rounding errors are uncritical here
	for(i = 0; i < dd->nRecTone; ++i)
	{
		int t;

		dd->recToneFreq[i] = lowest + i * toneinterval;
		for(t = 0; t < df->nTone; ++t)
		{
			if(df->tone[t] == i)
			{
				dd->recToneOut[i] = 1;
				break;
			}
		}
	}
}

/* Fills in provided array toneFreq[] (of max length maxCount)
 * with either -1, meaning don't process this tone, or a positive
 * number indicating the pulse cal tone frequency.
 * For LSBs the flags in toneFreq[] are returned descending in frequency.
 * This is the order in which they are written out by mpifxcorr in the pcal file.
 *
 * Return the number of tones extracted by DiFX. Not all these tones may be exporrted to FITS-IDI
 */
int DifxDatastreamGetPhasecalTones(double *toneFreq, const DifxDatastream *dd, const DifxFreq *df, int maxCount, int AllPcalTones)
{
	double lowest, highest, toneinterval;
	int nRecTone=0;
	int i, j, t;

	for(t = 0; t < maxCount; ++t)
	{
		toneFreq[t] = -1.0;	/* flag as not used */
	}

	if(dd->nRecFreq == 0 || dd->phaseCalIntervalMHz == 0)
	{
		/* No tones to extract */
		return 0;
	}

	/* Get number of tones, and lowest/highest in-band non-DC tone frequencies */
	nRecTone = DifxDatastreamGetPhasecalRange(dd, df, &lowest, &highest);

	/* Fill in the tone frequencies and on/off values */
	toneinterval = dd->phaseCalIntervalMHz / dd->phaseCalIntervalDivisor; // NB: rounding errors are uncritical here
	if ( AllPcalTones == 0 ){
	     /* Use the phase cal tones specified in the difx input files */
	     for(t = 0; t < df->nTone; ++t)
	     {
     		i = df->tone[t];
		     if(df->sideband == 'U')
		     {
			     j = i;
		     }
		     else
		     {
			     j = nRecTone - 1 - i;	/* reverse order of LSB tones */
		     }
		     if(j >= 0 && j < maxCount)
		     {
			     toneFreq[j] = lowest + i*toneinterval;
		     }
	     }
        } 
        else
        {
	     /* Use all the phase cal tones regardless what is in the difx input files */
	     for( t = 0; t < nRecTone; ++t)
	     {
		if ( df->sideband == 'U')
		{
		    toneFreq[t] = lowest  + t*toneinterval;
		}
		else
		{
		    toneFreq[t] = highest - t*toneinterval;
		}
	     }
        } 

	return nRecTone;
}

void deleteDifxDatastreamInternals(DifxDatastream *dd)
{
	/* The following will deallocate several arrays */
	DifxDatastreamAllocBands(dd, 0);

	/* The following will deallocate several arrays */
	DifxDatastreamAllocFreqs(dd, 0);

	/* The following will deallocate several arrays */
	DifxDatastreamAllocZoomBands(dd, 0);

	/* The following will deallocate several arrays */
	DifxDatastreamAllocZoomFreqs(dd, 0);

	/* The following will deallocate several arrays */
	DifxDatastreamAllocPhasecalTones(dd, 0);

	/* allocating zero files is equivalent to deleting any existing ones */
	DifxDatastreamAllocFiles(dd, 0);
}

void deleteDifxDatastreamArray(DifxDatastream *dd, int nDatastream)
{
	if(dd)
	{
		int e;

		for(e = 0; e < nDatastream; ++e)
		{
			deleteDifxDatastreamInternals(dd + e);
		}
		free(dd);
	}
}

void fprintDifxDatastream(FILE *fp, const DifxDatastream *dd)
{
	int f;

	fprintf(fp, "  Difx Datastream Entry[antennaId=%d] : %p\n", dd->antennaId, dd);
	fprintf(fp, "    data source = %s\n", dataSourceNames[dd->dataSource]);
	if(dd->nFile > 0)
	{
		for(f = 0; f < dd->nFile; ++f)
		{
			if(dd->file[f])
			{
				fprintf(fp, "      file %d = %s\n", f, dd->file[f]);
			}
			else
			{
				fprintf(fp, "      file %d = <null>\n", f);
			}
		}
	}
	if(dd->dataSource == DataSourceNetwork)
	{
		fprintf(fp, "      network port = %s\n", dd->networkPort);
		fprintf(fp, "      window size = %d\n", dd->windowSize);
	}
	fprintf(fp, "    format = %s\n", dd->dataFormat);
	fprintf(fp, "    quantization bits = %d\n", dd->quantBits);
	fprintf(fp, "    sampling = %s\n", samplingTypeNames[dd->dataSampling]);
	fprintf(fp, "    nRecFreq = %d\n", dd->nRecFreq);
	fprintf(fp, "    nRecBand = %d\n", dd->nRecBand);
	fprintf(fp, "    (RecFreqId, RecFreqDestId, nRecPol)[freq] =");
	for(f = 0; f < dd->nRecFreq; ++f)
	{
		fprintf(fp, " (%d, %d, %d)", dd->recFreqId[f], dd->recFreqDestId[f], dd->nRecPol[f]);
	}
	fprintf(fp, "\n");
	fprintf(fp, "    (freq(index to above), pol)[recBand] =");
	for(f = 0; f < dd->nRecBand; ++f)
	{
		fprintf(fp, " (%d, %c)", dd->recBandFreqId[f], dd->recBandPolName[f]);
	}
	fprintf(fp, "\n");
	fprintf(fp, "    nZoomFreq = %d\n", dd->nZoomFreq);
	fprintf(fp, "    nZoomBand = %d\n", dd->nZoomBand);
	fprintf(fp, "    (ZoomFreqId, ZoomFreqDestId, nZoomPol)[freq] =");
	for(f = 0; f < dd->nZoomFreq; ++f)
	{
		fprintf(fp, " (%d, %d, %d)", dd->zoomFreqId[f], dd->zoomFreqDestId[f], dd->nZoomPol[f]);
	}
	fprintf(fp, "\n");
	fprintf(fp, "    (freq(index to above), pol)[recBand] =");
	for(f = 0; f < dd->nZoomBand; ++f)
	{
		fprintf(fp, " (%d, %c)", dd->zoomBandFreqId[f], dd->zoomBandPolName[f]);
	}
	fprintf(fp, "\n");
	fprintf(fp, "    tcalFrequency = %d\n", dd->tcalFrequency);
	if(fabs(dd->phaseCalIntervalMHz - (int)(dd->phaseCalIntervalMHz + 0.5)) < 0.0001)
	{
		fprintf(fp, "    phaseCalIntMHZ = %d\n", (int)(dd->phaseCalIntervalMHz + 0.5));
	}
	else
	{
		fprintf(fp, "    phaseCalIntMHZ = %7.5f\n", dd->phaseCalIntervalMHz);
	}
	if(dd->phaseCalIntervalDivisor != 1)
	{
		fprintf(fp, "    phaseCalIntDivisor = %ld\n", dd->phaseCalIntervalDivisor);
	}
	if(dd->phaseCalBaseMHz > 0)
	{
		fprintf(fp, "    phaseCalBaseMHZ = %7.5f\n", dd->phaseCalBaseMHz);
	}
	fprintf(fp, "    nRecPhaseCalTones = %d\n", dd->nRecTone);
	for(f = 0; f < dd->nRecTone; ++f)
	{
		fprintf(fp, "     Tone %d freq = %d MHz, written = %d\n", f, dd->recToneFreq[f], dd->recToneOut[f]);
	}
}

void printDifxDatastream(const DifxDatastream *dd)
{
	fprintDifxDatastream(stdout, dd);
}

int isSameDifxDatastream(const DifxDatastream *dd1, const DifxDatastream *dd2, const int *freqIdRemap, const int *antennaIdRemap)
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
	   dd1->dataSampling != dd2->dataSampling ||
	   dd1->nRecFreq != dd2->nRecFreq ||
	   dd1->nRecBand != dd2->nRecBand ||
	   dd1->nZoomFreq != dd2->nZoomFreq ||
	   dd1->nZoomBand != dd2->nZoomBand ||
	   dd1->dataSource != dd2->dataSource ||
	   dd1->tcalFrequency != dd2->tcalFrequency ||
	   dd1->phaseCalIntervalMHz != dd2->phaseCalIntervalMHz ||
	   dd1->phaseCalIntervalDivisor != dd2->phaseCalIntervalDivisor)
	{
		return 0;
	}

	for(f = 0; f < dd1->nRecFreq; ++f)
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
		   dd1->clockOffsetDelta[f] != dd2->clockOffsetDelta[f] ||
		   dd1->phaseOffset[f] != dd2->phaseOffset[f] ||
		   dd1->freqOffset[f] != dd2->freqOffset[f])
		{
			return 0;
		}

		if(freqIdRemap)
		{
			freqId2 = freqIdRemap[dd2->recFreqDestId[f]];
		}
		else
		{
			freqId2 = dd2->recFreqDestId[f];
		}
		if(dd1->recFreqDestId[f] != freqId2)
		{
			return 0;
		}
	}
	for(c = 0; c < dd1->nRecBand; ++c)
	{
		if(dd1->recBandFreqId[c]  != dd2->recBandFreqId[c] ||
		   dd1->recBandPolName[c] != dd2->recBandPolName[c])
		{
			return 0;
		}
	}
	for(f = 0; f < dd1->nZoomFreq; ++f)
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

		if(freqIdRemap)
		{
			freqId2 = freqIdRemap[dd2->zoomFreqDestId[f]];
		}
		else
		{
			freqId2 = dd2->zoomFreqDestId[f];
		}
		if(dd1->zoomFreqDestId[f] != freqId2)
		{
			return 0;
		}
	}
	for(c = 0; c < dd1->nZoomBand; ++c)
	{
		if(dd1->zoomBandFreqId[c]  != dd2->zoomBandFreqId[c] ||
		   dd1->zoomBandPolName[c] != dd2->zoomBandPolName[c])
		{
			return 0;
		}
	}

	return 1;
}

void copyDifxDatastream(DifxDatastream *dest, const DifxDatastream *src, const int *freqIdRemap, const int *antennaIdRemap)
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
	snprintf(dest->dataFormat, DIFXIO_FORMAT_LENGTH, "%s", src->dataFormat);
	dest->dataSampling = src->dataSampling;
	dest->dataSource = src->dataSource;
	dest->quantBits = src->quantBits;
	dest->tcalFrequency = src->tcalFrequency;
	dest->phaseCalIntervalMHz = src->phaseCalIntervalMHz;
	dest->nRecTone = src->nRecTone;

	DifxDatastreamAllocFreqs(dest, src->nRecFreq);
	DifxDatastreamAllocBands(dest, src->nRecBand);
	DifxDatastreamAllocZoomFreqs(dest, src->nZoomFreq);
	DifxDatastreamAllocZoomBands(dest, src->nZoomBand);
	DifxDatastreamAllocPhasecalTones(dest, src->nRecTone);
	for(f = 0; f < dest->nRecFreq; ++f)
	{
		dest->nRecPol[f] = src->nRecPol[f];
		if(freqIdRemap)
		{
			dest->recFreqId[f] = freqIdRemap[src->recFreqId[f]];
			dest->recFreqDestId[f] = freqIdRemap[src->recFreqDestId[f]];
		}
		else
		{
			dest->recFreqId[f] = src->recFreqId[f];
			dest->recFreqDestId[f] = src->recFreqDestId[f];
		}
		dest->clockOffset[f] = src->clockOffset[f];
		dest->clockOffsetDelta[f] = src->clockOffsetDelta[f];
		dest->phaseOffset[f] = src->phaseOffset[f];
		dest->freqOffset[f]  = src->freqOffset[f];
	}
	for(c = 0; c < dest->nRecBand; ++c)
	{
		dest->recBandFreqId[c]  = src->recBandFreqId[c];
		dest->recBandPolName[c] = src->recBandPolName[c];
	}
	for(f = 0; f < dest->nZoomFreq; ++f)
	{
		dest->nZoomPol[f] = src->nZoomPol[f];
		if(freqIdRemap)
		{
			dest->zoomFreqId[f] = freqIdRemap[src->zoomFreqId[f]];
			dest->zoomFreqDestId[f] = freqIdRemap[src->zoomFreqDestId[f]];
		}
		else
		{
			dest->zoomFreqId[f] = src->zoomFreqId[f];
			dest->zoomFreqDestId[f] = src->zoomFreqDestId[f];
		}
	}
	for(c = 0; c < dest->nZoomBand; ++c)
	{
		dest->zoomBandFreqId[c]  = src->zoomBandFreqId[c];
		dest->zoomBandPolName[c] = src->zoomBandPolName[c];
	}
	for(c = 0; c < dest->nRecTone; ++c)
	{
		dest->recToneFreq[c] = src->recToneFreq[c];
		dest->recToneOut[c]  = src->recToneOut[c];
	}
	if(src->nFile > 0)
	{
		DifxDatastreamAllocFiles(dest, src->nFile);
		for(f = 0; f < src->nFile; ++f)
		{
			dest->file[f] = strndup(src->file[f], DIFXIO_FILENAME_LENGTH);
		}
	}
	snprintf(dest->networkPort, DIFXIO_ETH_DEV_SIZE, "%s", src->networkPort);
	dest->windowSize  = src->windowSize;
}

/* don't re-allocate internal structures */
void moveDifxDatastream(DifxDatastream *dest, DifxDatastream *src)
{
	dest->antennaId = src->antennaId;
	dest->tSys = src->tSys;
	snprintf(dest->dataFormat, DIFXIO_FORMAT_LENGTH, "%s", src->dataFormat);
	dest->dataSampling = src->dataSampling;
	snprintf(dest->networkPort, DIFXIO_ETH_DEV_SIZE, "%s", src->networkPort);
	dest->windowSize = src->windowSize;
	dest->quantBits = src->quantBits;
	dest->dataFrameSize = src->dataFrameSize;
	dest->tcalFrequency = src->tcalFrequency;
	dest->phaseCalIntervalMHz = src->phaseCalIntervalMHz;
	dest->nRecTone = src->nRecTone;
	dest->recToneFreq = src->recToneFreq;
	dest->recToneOut = src->recToneOut;
	dest->dataSource = src->dataSource;
	dest->nRecFreq = src->nRecFreq;
	dest->nRecBand = src->nRecBand;
	dest->recFreqId = src->recFreqId;
	dest->recFreqDestId = src->recFreqDestId;
	dest->nRecPol = src->nRecPol;
	dest->clockOffset = src->clockOffset;
	dest->clockOffsetDelta = src->clockOffsetDelta;
	dest->phaseOffset = src->phaseOffset;
	dest->freqOffset = src->freqOffset;
	dest->recBandFreqId = src->recBandFreqId;
	dest->recBandPolName = src->recBandPolName;
	dest->nZoomFreq = src->nZoomFreq;
	dest->nZoomBand = src->nZoomBand;
	dest->zoomFreqId = src->zoomFreqId;
	dest->zoomFreqDestId = src->zoomFreqDestId;
	dest->nZoomPol = src->nZoomPol;
	dest->zoomBandFreqId = src->zoomBandFreqId;
	dest->zoomBandPolName = src->zoomBandPolName;
	dest->nFile = src->nFile;
	dest->file = src->file;

	/* unlink src data structures */
	src->recFreqId = 0;
	src->recFreqDestId = 0;
	src->nRecPol = 0;
	src->clockOffset = 0;
	src->clockOffsetDelta = 0;
	src->phaseOffset = 0;
	src->freqOffset = 0;
	src->recBandFreqId = 0;
	src->recBandPolName = 0;
	src->zoomFreqId = 0;
	src->zoomFreqDestId = 0;
	src->nZoomPol = 0;
	src->zoomBandFreqId = 0;
	src->zoomBandPolName = 0;
	src->file = 0;
	src->recToneFreq = 0;
	src->recToneOut = 0;
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

		for(d1 = 0; d1 < d; ++d1)
		{
			if(isSameDifxDatastream(D->datastream+d, D->datastream+d1, 0, 0))
			{
				break;
			}
		}
		if(d == d1)	/* no match found */
		{
			++d;	/* advance to next datastream */
		}
		else		/* found match */
		{
			/* 1. Renumber this and all higher datastreams */
			for(b = 0; b < D->nBaseline; ++b)
			{
				d0 = D->baseline[b].dsA;
				if(d0 == d)
				{
					d0 = d1;
				}
				else if(d0 > d)
				{
					--d0;
				}
				D->baseline[b].dsA = d0;

				d0 = D->baseline[b].dsB;
				if(d0 == d)
				{
					d0 = d1;
				}
				else if(d0 > d)
				{
					--d0;
				}
				D->baseline[b].dsB = d0;
			}

			for(c = 0; c < D->nConfig; ++c)
			{
				for(cd = 0; cd < D->config[c].nDatastream; ++cd)
				{
					d0 = D->config[c].datastreamId[cd];
					if(d0 == d)
					{
						d0 = d1;
					}
					else if(d0 > d)
					{
						--d0;
					}
					D->config[c].datastreamId[cd] = d0;
				}
			}

			/* 2. reduce number of datastreams */
			--D->nDatastream;

			/* 3. Delete this datastream and bump up higher ones */
			deleteDifxDatastreamInternals(D->datastream + d);
			for(d1 = d; d1 < D->nDatastream; ++d1)
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
	for(j = 0; j < ndd2; ++j)
	{
		for(i = 0; i < ndd1; ++i)
		{
			if(isSameDifxDatastream(dd1 + i, dd2 + j, freqIdRemap, antennaIdRemap))
			{
				datastreamIdRemap[j] = i;
				break;
			}
		}
		if(i == ndd1)	/* no match found.  This must be a new one */
		{
			datastreamIdRemap[j] = *ndd;
			++(*ndd);
		}
	}

	dd = newDifxDatastreamArray(*ndd);
	
	/* now copy dd1 */
	for(i = 0; i < ndd1; ++i)
	{
		copyDifxDatastream(dd + i, dd1 + i, 0, 0);
	}

	/* now copy unique members of dd2 */
	for(j = 0; j < ndd2; ++j)
	{
		if(datastreamIdRemap[j] >= ndd1)
		{
			copyDifxDatastream(dd + datastreamIdRemap[j], dd2 + j, freqIdRemap, antennaIdRemap);
		}
	}

	return dd;
}

int writeDifxDatastream(FILE *out, const DifxDatastream *dd)
{
	const int MaxLineLength = 256;
	int i;
	char pol[2];

	pol[1] = 0;

	writeDifxLineInt(out, "TELESCOPE INDEX", dd->antennaId);
	writeDifxLineDouble(out, "TSYS", "%f", dd->tSys);
	writeDifxLine(out, "DATA FORMAT", dd->dataFormat);
	writeDifxLineInt(out, "QUANTISATION BITS", dd->quantBits);
	writeDifxLineInt(out, "DATA FRAME SIZE", dd->dataFrameSize);
	writeDifxLine(out, "DATA SAMPLING", samplingTypeNames[dd->dataSampling]);
	writeDifxLine(out, "DATA SOURCE", dataSourceNames[dd->dataSource]);
	writeDifxLine(out, "FILTERBANK USED", "FALSE");
	if(dd->tcalFrequency > 0)
	{
		writeDifxLineInt(out, "TCAL FREQUENCY", dd->tcalFrequency);
	}
	writeDifxLineDouble(out, "PHASE CAL INT (MHZ)", "%.5g", dd->phaseCalIntervalMHz);
	if(dd->phaseCalIntervalDivisor != 1)
	{
		writeDifxLineInt(out, "PHASE CAL DIVISOR", dd->phaseCalIntervalDivisor);
	}
	if(dd->phaseCalBaseMHz != 0)
	{
		writeDifxLineDouble(out, "PHASE CAL BASE(MHZ)", "%.5g", dd->phaseCalBaseMHz);
	}
	writeDifxLineInt(out, "NUM RECORDED FREQS", dd->nRecFreq);
	for(i = 0; i < dd->nRecFreq; ++i)
	{
		writeDifxLineInt1(out, "REC FREQ INDEX %d", i, dd->recFreqId[i]);

		if(dd->clockOffsetDelta[i]==0.0 && dd->phaseOffset[i]==0.0)
		{
			writeDifxLineDouble1(out, "CLK OFFSET %d (us)", i, "%8.6f", dd->clockOffset[i]);
		}
		else
		{
			char v[MaxLineLength];
			int r;
		
			if (dd->phaseOffset[i]==0.0) 
			{
				r = snprintf(v, MaxLineLength, "%.6f:%.6f", dd->clockOffset[i], dd->clockOffsetDelta[i]);
			}
		  	else
			{
		    		r = snprintf(v, MaxLineLength, "%.6f:%.6f:%.2f", dd->clockOffset[i], dd->clockOffsetDelta[i], dd->phaseOffset[i]);
			}

			if(r >= MaxLineLength)
			{
				fprintf(stderr, "Developer error: writeDifxDatastream: MaxLineLength = %d is too small, wants to be > %d\n", MaxLineLength, r);

				exit(0);
			}

			writeDifxLine1(out, "CLK OFFSET %d (us)", i, v);
		}

		writeDifxLineDouble1(out, "FREQ OFFSET %d (Hz)", i, "%8.6f", dd->freqOffset[i]);
		writeDifxLineInt1(out, "NUM REC POLS %d", i, dd->nRecPol[i]);
	}
	for(i = 0; i < dd->nRecBand; ++i)
	{
		pol[0] = dd->recBandPolName[i];
		if(pol[0] <= ' ')
		{
			fprintf(stderr, "Error: Unknown polarization for telescope index %d band %d\n", dd->antennaId, i);
		}
		writeDifxLine1(out, "REC BAND %d POL", i, pol);
		writeDifxLineInt1(out, "REC BAND %d INDEX", i, dd->recBandFreqId[i]);
	}
	writeDifxLineInt(out, "NUM ZOOM FREQS", dd->nZoomFreq);
	for(i = 0; i < dd->nZoomFreq; ++i)
	{
		writeDifxLineInt1(out, "ZOOM FREQ INDEX %d", i, dd->zoomFreqId[i]);
		writeDifxLineInt1(out, "NUM ZOOM POLS %d", i, dd->nZoomPol[i]);
	}
	for(i = 0; i < dd->nZoomBand; ++i)
	{
		pol[0] = dd->zoomBandPolName[i];
		writeDifxLine1(out, "ZOOM BAND %d POL", i, pol);
		writeDifxLineInt1(out, "ZOOM BAND %d INDEX", i, dd->zoomBandFreqId[i]);
	}

	return 8 + 4*dd->nRecFreq + 2*dd->nRecBand + 2*dd->nZoomFreq + 2*dd->nZoomBand;
}

int DifxDatastreamGetRecBands(DifxDatastream *dd, int freqId, char *pols, int *recBands)
{
	int r;
	int n=0;
	int localFqId;

	for(r = 0; r < dd->nRecBand; ++r)
	{
		localFqId = dd->recBandFreqId[r];
		if(localFqId < 0 || localFqId >= dd->nRecFreq)
		{
			fprintf(stderr, "Error: DifxDatastreamGetRecBands: localFqId=%d out of range (%d) r=%d\n", localFqId, dd->nRecFreq, r);
		}
		else if(dd->recFreqId[localFqId] == freqId)
		{
			if(dd->recBandPolName[r] <= ' ')
			{
				continue;
			}
			if(n >= 2)
			{
				fprintf(stderr, "Warning: skipping dup rechan 1: r=%d freqId=%d localFqId=%d\n", r, freqId, localFqId);
			}
			else if(n == 1 && dd->recBandPolName[r] == pols[0])
			{
				fprintf(stderr, "Warning: skipping dup rechan 2: r=%d freqId=%d localFqId=%d\n", r, freqId, localFqId);
			}
			else
			{
				pols[n] = dd->recBandPolName[r];
				recBands[n] = r;
				++n;
			}
		}
	}

	/* check to see if a swap to canonical polarization order is due */
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

        for(z = 0; z < dd->nZoomBand; ++z)
        {
		if(dd->zoomBandFreqId[z] < 0 || dd->zoomBandFreqId[z] >= dd->nZoomFreq)
		{
			fprintf(stderr, "Error: zoomBandFreqId[%d] is %d where nZoomFreq is %d\n", z, dd->zoomBandFreqId[z], dd->nZoomFreq);

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
                                fprintf(stderr, "Warning: skipping dup zoomchan 1: z=%d freqId=%d pol=%c,%c ndup=%d\n", z, freqId, dd->zoomBandPolName[z], pols[0], n);
                        }
                        else if(n == 1 && dd->zoomBandPolName[z] == pols[0])
                        {
                                fprintf(stderr, "Warning: skipping dup zoomchan 2: z=%d freqId=%d pol=%c,%c ndup=%d\n", z, freqId, dd->zoomBandPolName[z], pols[0], n);
                        }
                        else
                        {
                                pols[n] = dd->zoomBandPolName[z];
                                zoomBands[n] = z;
                                ++n;
                        }
                }
        }

	/* check to see if a swap to canonical polarization order is due */
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

int getDifxDatastreamBandFreqId(const DifxDatastream *dd, int band)
{
	int localFqId;
	if(band < 0)
	{
		return -1;
	}
	if(band < dd->nRecBand)
	{
		localFqId = dd->recBandFreqId[band];
		return dd->recFreqId[localFqId];
	}
	if(band < dd->nRecBand + dd->nZoomBand)
	{
		localFqId = dd->zoomBandFreqId[band-dd->nRecBand];
		return dd->zoomFreqId[localFqId];
	}

	return -1;
}

char getDifxDatastreamBandPol(const DifxDatastream *dd, int band)
{
	if(band < 0)
	{
		return ' ';
	}
	if(band < dd->nRecBand)
	{
		return dd->recBandPolName[band];
	}
	if(band < dd->nRecBand + dd->nZoomBand)
	{
		return dd->zoomBandPolName[band-dd->nRecBand];
	}
	return ' ';
}

int getDifxDatastreamBandFreqIdAndPol(int *freqId, char *pol, const DifxDatastream *dd, int band)
{
	int localFqId;
	if(band < 0)
	{
		return -1;
	}
	if(band < dd->nRecBand)
	{
		localFqId = dd->recBandFreqId[band];
		*freqId = dd->recFreqId[localFqId];
		*pol = dd->recBandPolName[band];

		return 0;
	}
	if(band < dd->nRecBand + dd->nZoomBand)
	{
		localFqId = dd->zoomBandFreqId[band-dd->nRecBand];
		*freqId = dd->zoomFreqId[localFqId];
		*pol = dd->zoomBandPolName[band-dd->nRecBand];

		return 0;
	}

	return -1;
}

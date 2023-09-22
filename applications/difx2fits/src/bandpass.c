#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "bandpass.h"

/* freq [MHz] */
static float complex getBandpassGain(const BandpassData *d, double freq)
{
	/* FIXME: add interpolation mode(s) in the future */
	const int mode = 1;	/* 0 means no smoothing (nearest channel).  1 means Hanning-like smoothing. */
	int c;

	c = floor((freq - d->freq)*d->nChan/d->bw + 0.5);
	if(c < 0 || c >= d->nChan)
	{
		fprintf(stderr, "Developer error: getBandpassGain() : freq=%f not in range\n", freq);

		exit(0);
	}

	switch(mode)
	{
	case 1:
		if(c == 0)
		{
			return 0.5*(d->gain[0] + d->gain[1]);
		}
		else if(c == d->nChan-1)
		{
			return 0.5*(d->gain[d->nChan-2] + d->gain[d->nChan-1]);
		}
		else
		{
			return 0.25*(d->gain[c-1] + 2.0*d->gain[c] + d->gain[c+1]);
		}
	default:
		return d->gain[c];
	}
}

Bandpass *loadBandpass(const char *fileName, const DifxInput *D)
{
	const int MaxLength = 128;
	Bandpass *B;
	FILE *in;
	int n = 0;
	char str[MaxLength];
	char type[MaxLength];
	int antNum;
	char ant[MaxLength];
	int IFnum;
	int nChan;
	double f, bw;	/* [MHz] */
	char pol[MaxLength];
	char obsCode[MaxLength];
	int index;	/* index to Bandpass.data[] */
	int c = 0;
	double x, y;
	int freqId, polId, antId;

	in = fopen(fileName, "r");
	if(!in)
	{
		fprintf(stderr, "Error: bandpass file %s could not be opened.\n", fileName);

		return 0;
	}

	while(fgets(str, MaxLength, in) != 0)
	{
		if(sscanf(str, "%s%d%s%d%d%lf%lf%s", type, &antNum, ant, &IFnum, &nChan, &f, &bw, pol) == 8 && strcmp(type, "Bandpass") == 0)
		{
			++n;
		}
	}
	rewind(in);

	B = (Bandpass *)calloc(1, sizeof(Bandpass));

	B->D = D;
	B->nData = n;
	B->data = (BandpassData *)calloc(n, sizeof(BandpassData));
	
	index = -1;
	while(fgets(str, MaxLength, in) != 0)
	{
		if(sscanf(str, "%s%d%s%d%d%lf%lf%s", type, &antNum, ant, &IFnum, &nChan, &f, &bw, pol) == 8 && strcmp(type, "Bandpass") == 0)
		{
			++index;
			snprintf(B->data[index].antennaName, MAX_ANTENNA_SITE_NAME_LENGTH, "%s", ant);
			B->data[index].nChan = nChan;
			B->data[index].freq = f;
			B->data[index].bw = bw;
			B->data[index].pol = pol[0];
			B->data[index].gain = (float complex *)calloc(nChan, sizeof(float complex));
			c = 0;
		}
		else if(sscanf(str, "%s%s", type, obsCode) == 2 && strcmp(type, "obscode:") == 0)
		{
			snprintf(B->obsCode, DIFXIO_OBSCODE_LENGTH, "%s", obsCode);
		}
		else if(sscanf(str, "%lf%lf%lf", &f, &x, &y) == 3)
		{
			B->data[index].gain[c] = 1.0 / (x + 1.0I*y);
			++c;
		}
	}
	
	fclose(in);

	B->nPol = D->config->nPol;
	for(polId = 0; polId < D->config->nPol; ++polId)
	{
		B->pol[polId] = D->config->pol[polId];
	}
	B->nAnt = D->nAntenna;
	B->nFreq = D->nFreq;

	/* Now index the data so it can be readily found based on antId, freqId and pol */
	B->bp = (BandpassData ****)calloc(B->nPol, sizeof(BandpassData ***));
	for(polId = 0; polId < B->nPol; ++polId)
	{
		B->bp[polId] = (BandpassData ***)calloc(B->nFreq, sizeof(BandpassData **));
		for(freqId = 0; freqId < B->nFreq; ++freqId)
		{
			double freqLow, freqHi;	/* [MHz] */

			if(D->freq[freqId].sideband == 'U')
			{
				freqLow = D->freq[freqId].freq;
				freqHi = D->freq[freqId].freq + D->freq[freqId].bw;
			}
			else
			{
				freqLow = fabs(D->freq[freqId].freq) - fabs(D->freq[freqId].bw);
				freqHi = fabs(D->freq[freqId].freq);
			}

			B->bp[polId][freqId] = (BandpassData **)calloc(B->nAnt, sizeof(BandpassData *));
			for(antId = 0; antId < B->nAnt; ++antId)
			{
				/* Now search through the data array for the best match and assign */

				/* for now, stop search when a good match is found; probably only one match will work */
				for(index = 0; index < B->nData; ++index)
				{
					BandpassData *d;

					d = B->data + index;

					if(B->pol[polId] == d->pol && 
					   strcasecmp(D->antenna[antId].name, d->antennaName) == 0 &&
					   freqLow >= d->freq &&
					   freqHi <= d->freq + d->bw)
					{
						B->bp[polId][freqId][antId] = d;
//	printf("(%d %d %d) -> %d\n", polId, freqId, antId, index);

						break;
					}
				}
				if(B->bp[polId][freqId][antId] == 0)
				{
//	printf("(%d %d %d) -> x\n", polId, freqId, antId);
				}
			}
		}
	}


	return B;
}

void deleteBandpass(Bandpass *B)
{
	if(B)
	{
		if(B->data)
		{
			free(B->data);
		}

		if(B->bp)
		{
			int polId, freqId;

			for(polId = 0; polId < B->nPol; ++polId)
			{
				for(freqId = 0; freqId < B->nFreq; ++freqId)
				{
					free(B->bp[polId][freqId]);
				}
				free(B->bp[polId]);
			}
			free(B->bp);
		}

		free(B);
	}
}

/* returns:
 * 1 on success
 * 0 on did not try
 * -1 on error
 */
int applyBandpass(float complex *spectrum, int nChan, int freqId, int antId1, char pol1, int antId2, char pol2, const Bandpass *B)
{
	int polId1, polId2;
	const BandpassData *d1, *d2;
	const DifxFreq *fq;
	int c;
	double freq;	/* [MHz] */
	double f0, df;	/* [MHz] DC frequency and freq increment */

	polId1 = pol1 == B->pol[0] ? 0 : 1;
	polId2 = pol2 == B->pol[0] ? 0 : 1;

	d1 = B->bp[polId1][freqId][antId1];
	d2 = B->bp[polId2][freqId][antId2];

	if(d1 == 0 && d2 == 0)
	{
		return 0;
	}

	fq = B->D->freq + freqId;
	df = fabs(fq->bw)/nChan;
	if(fq->sideband == 'U')
	{
		f0 = fq->freq;
	}
	else
	{
		/* FIXME: do we need to shift by df here or not? */
		f0 = fabs(fq->freq) - fabs(fq->bw);
	}

	for(c = 0; c < nChan; ++c)
	{
		freq = f0 + df*c;

		if(d1)
		{
			spectrum[c] *= getBandpassGain(d1, freq);
		}
		if(d2)
		{
			spectrum[c] *= conj(getBandpassGain(d2, freq));
		}
	}

	return 1;
}

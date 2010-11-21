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

const char printFormat[] = "%-26s%-15s%s\n";

const int defaultNCore = 10;
const double defaultNThread = 7;

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
	int i, j, k, nc;

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

	C->nCore = 0;
	C->nThread = 0;

	if(D->nCore > 0 && D->nThread != 0)
	{
		C->nCore = D->nCore;
		for(i = 0; i < C->nCore; i++)
		{
			C->nThread += D->nThread[i];
		}
		C->nThread /= C->nCore;
		C->hasThreadsFile = 1;
	}
	else
	{
		C->hasThreadsFile = 0;
		C->nCore = defaultNCore;
		C->nThread = defaultNThread;
	}

	for(i = 0; i < D->nConfig; i++)
	{
		c = &C->config[i];
		d = &D->config[i];

		c->nBand = 0.0;
		c->nPolPerBand = 0.0;
		c->nPol = 0;
		c->nBit = 0.0;
		c->bandwidth = 0.0;
		c->nChan = 0.0;

		c->nAntenna = d->nAntenna;
		c->nBaseline = d->nBaseline;
		c->nDataSegment = D->nDataSegments;
		c->dataBufferFactor = D->dataBufferFactor;

		for(j = 0; j < D->nFreq; j++)
		{
			if(D->freq[j].nChan > c->nChan)
			{
				c->nChan = D->freq[j].nChan;
			}
		}

		for(j = 0; j < d->nBaseline; j++)
		{
			int bl = d->baselineId[j];
			
			c->nBand += D->baseline[bl].nFreq;
			for(k = 0; k < D->baseline[bl].nFreq; k++)
			{
				if(D->baseline[bl].nPolProd[k] > c->nPol)
				{
					c->nPol = D->baseline[bl].nPolProd[k];
				}
				c->nPolPerBand += D->baseline[bl].nPolProd[k];
			}
		}
		c->nPolPerBand /= c->nBand;
		c->nBand /= c->nBaseline;
		if(c->nPol > 1)
		{
			c->nPol = 2;
		}
		c->nPolPerBand /= c->nPol;

		nc = 0;
		for(j = 0; j < d->nDatastream; j++)
		{
			int ds = d->datastreamId[j];
			
			c->nBit += D->datastream[ds].quantBits;
			for(k = 0; k < D->datastream[ds].nRecBand; k++)
			{
				int f;

				f = D->datastream[ds].recBandFreqId[k];
				f = D->datastream[ds].recFreqId[f];
				if(f < 0 || f > D->nFreq)
				{
					printf("ACK i=%d, j=%d, k = %d -> f=%d\n", i, j, k, f);
				}
				c->bandwidth += D->freq[f].bw;
				nc++;
			}
		}
		c->nBit /= d->nDatastream;
		c->bandwidth /= nc;
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

static void printDouble(const char *p, double v, const char *n, const char *fmt)
{
	const int MaxLen = 16;
	char nullstring[] = "";
	char tmp[MaxLen];

	if(fmt == 0)
	{
		fmt = "%f";
	}

	if(n == 0) 
	{
		n = nullstring;
	}

	snprintf(tmp, MaxLen, fmt, v);

	printf(printFormat, p, tmp, n);
}

void printDifxCalculator(const DifxCalculator *C)
{
	int i;
	const DifxCalculatorConfig *c;

	printf("\n");
	printInt("Number of Configurations", C->nConfig, 0);
	printInt("Number of Cores", C->nCore, C->hasThreadsFile ? "" : "Default since no .threads file");
	printDouble("Number of Threads", C->nThread, C->hasThreadsFile ? "" : "Default since no .threads file", "%3.1f");
	printf("\n");

	for(i = 0; i < C->nConfig; i++)
	{
		c = &C->config[i];
		printf("CONFIG %d\n", i);
		printString("PARAMETER",              "VALUE",             "NOTE");
		printInt("Number of telescopes",      c->nAntenna,         0);
		printInt("Number of baselines",       c->nBaseline,        0);
		printDouble("Number of bands",        c->nBand,            "Averaged over baselines", "%3.1f");
		printDouble("Bandwidth (MHz)",        c->bandwidth,        0, 0);
		printInt("Decimation factor",         c->decimationFactor, 0);
		printInt("Number of polarizations",   c->nPol,             0);
		printDouble("Pol. products per band", c->nPolPerBand,      "Averaged over baselines", "%3.1f");
		printDouble("Bits per sample",        c->nBit,             "Averaged over baselines", "%3.1f");
		printInt("Blocks per send",           c->blocksPerSend,    0);
		printInt("Spectral points per band",  c->nChan,            "As correlated");
		printInt("Data buffer factor",        c->dataBufferFactor, 0);
		printInt("Number of data segments",   c->nDataSegment,     0);
		printf("\n");
	}
}

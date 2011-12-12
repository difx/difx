/***************************************************************************
 *   Copyright (C) 2010-2011 by Walter Brisken                             *
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

const char printFormat[] = "%-28s%-15s%s\n";

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
	C->tObs = 86400.0*(D->mjdStop - D->mjdStart);
	C->visibilityLength = D->visBufferLength;

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
		c->nPhaseCenter = 1;

		for(j = 0; j < D->nScan; j++)
		{
			if(D->scan[j].configId == i)
			{
				if(D->scan[j].nPhaseCentres > c->nPhaseCenter)
				{
					c->nPhaseCenter = D->scan[j].nPhaseCentres;
				}
			}
		}

		for(j = 0; j < D->nFreq; j++)
		{
			if(D->freq[j].nChan > c->nChan)
			{
				c->nChan = D->freq[j].nChan;
			}
		}

		c->specAvg = 0;

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
					printf("ACK! i=%d j=%d k=%d -> f=%d\n", i, j, k, f);
				}
				c->bandwidth += D->freq[f].bw*D->freq[f].decimation*1.0e6;
				c->specAvg += D->freq[f].bw*D->freq[f].specAvg;
				nc++;
			}
		}
		c->nBit /= d->nDatastream;
		c->bandwidth /= nc;
		c->specAvg /= nc;

		c->tSubint = d->subintNS*1.0e-9;
		c->tGuard = d->guardNS*1.0e-9;
		c->tInt = d->tInt;

		/* derived parameters */

#warning "FIXME: consider n phase center"
#warning "FIXME: pulsar phase bins?"
#warning "FIXME: does nChanAvg play a role in the following calculations?"
		c->subintsPerInt = c->tInt/c->tSubint;
		c->visibilitySize = c->nPhaseCenter*(c->nAntenna + c->nBaseline)*(8*c->nChan*c->nBand*c->nPol*c->nPolPerBand/c->specAvg);
		c->recDataRate = c->nBand*c->bandwidth*c->nPol*c->nBit*2.0;
		c->basebandMessageSize = c->tSubint*c->recDataRate/8.0;
		c->blocksPerSend = c->basebandMessageSize/(c->nBit*c->nChan*2.0*c->nBand*c->nPol/8);
		c->basebandReadSize = c->basebandMessageSize*c->dataBufferFactor/c->nDataSegment;
		c->datastreamOutputRate = c->recDataRate * C->speedUp;
		c->coreInputRatio = c->nAntenna/(double)(C->nCore);
		c->coreInputRate = c->recDataRate*c->coreInputRatio*C->speedUp;
		c->coreOutputRatio = c->visibilitySize/(c->nAntenna*c->basebandMessageSize);
		c->coreOutputRate = c->coreInputRate*c->coreOutputRatio;
		c->managerInputRate = c->coreOutputRate*C->nCore;
		c->diskDataRate = c->visibilitySize/c->tInt;
		c->datasetSize = c->diskDataRate*C->tObs;

		c->datastreamBufferSize = c->basebandMessageSize*c->dataBufferFactor;
		c->modeSize = c->basebandMessageSize + ((c->nBand*c->nPol*c->nChan*4)*(2+2+2+1)+c->nChan*4.0*(2+2+2+2+2+2+2+3+5));
		c->coreSize = 4.0*((c->nAntenna*c->modeSize) + (c->nAntenna+c->nBaseline)*c->visibilitySize) + C->nThread*c->visibilitySize;
		c->managerSize = c->visibilitySize*C->visibilityLength*c->nPhaseCenter;

		c->datastreamBufferDur = c->datastreamBufferSize*8/c->recDataRate;
		c->datastreamReadDur = c->datastreamBufferDur/c->nDataSegment;
		c->managerSlack = C->visibilityLength*c->tInt/2.0;
		c->coreBufferDur = C->nCore*4.0*c->tSubint;
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

void printDifxCalculatorConfig(const DifxCalculatorConfig *c)
{
	printString("PARAMETER",               "VALUE",             "NOTE");
	printInt("Number of telescopes",       c->nAntenna,         0);
	printInt("Number of baselines",        c->nBaseline,        0);
	printDouble("Number of bands",         c->nBand,            "Averaged over baselines", "%3.1f");
	printDouble("Bandwidth (MHz)",         c->bandwidth*1.0e-6, 0, 0);
	printInt("Number of polarizations",    c->nPol,             0);
	printDouble("Pol. products per band",  c->nPolPerBand,      "Averaged over baselines", "%3.1f");
	printDouble("Bits per sample",         c->nBit,             "Averaged over baselines", "%3.1f");
	printDouble("Blocks per send",         c->blocksPerSend,    "Has to be integral!", "%3.1f");
	printInt("Spectral points per band",   c->nChan,            "As correlated");
	printDouble("Spectral averaging",      c->specAvg,          "Averaged over bands", "%3.1f");
	printInt("Data buffer factor",         c->dataBufferFactor, 0);
	printInt("Number of data segments",    c->nDataSegment,     0);
	printf("\n");
	printString("NETWORK / DISK USAGE",    "VALUE",             "NOTE");
	printDouble("Record data rate (Mbps)", c->recDataRate*1.0e-6, 0, "%3.1f");
	printDouble("DS output rate (Mbps)",   c->datastreamOutputRate*1.0e-6, 0, 0);
	printDouble("Baseband msg. size (MB)", c->basebandMessageSize*1.0e-6, "Datastream to Core send; want a few MB", 0);
	printDouble("Baseband read size (MB)", c->basebandReadSize*1.0e-6, "Typically want ~10 to 50 MB", 0);
	printDouble("Core input data ratio",   c->coreInputRatio,   0, 0);
	printDouble("Core input rate (Mbps)",  c->coreInputRate*1.0e-6,    0, 0);
	printDouble("Core output data ratio",  c->coreOutputRatio,  "Really should be << 1.0", 0);
	printDouble("Core output rate (Mbps)", c->coreOutputRate*1.0e-6,   0, 0);
	printDouble("Manager input rate (Mbps)", c->managerInputRate*1.0e-6,  0, 0);
	printDouble("Disk output rate (MB/s)", c->diskDataRate*1.0e-6, 0, 0);
	printDouble("Dataset size (MB)",       c->datasetSize*1.0e-6,  0, 0);
	printf("\n");
	printString("MEMORY USAGE",            "VALUE",             "NOTE");
	printDouble("Size of DS buffer (MB)",  c->datastreamBufferSize*1.0e-6,  0, 0);
	printDouble("Size of vis. dump (B)",   c->visibilitySize,  0, "%1.0f");
	printDouble("Size of Mode object (MB)",c->modeSize*1.0e-6, 0, 0);
	printDouble("Core memory usage (MB)",  c->coreSize*1.0e-6, 0, 0);
	printDouble("Manager memory usage (MB)", c->managerSize*1.0e-6, 0, 0);
	printf("\n");
	printString("TIMES",                   "VALUE",             "NOTE");
	printDouble("Integration time (s)",  c->tInt,             0, 0);
	printDouble("Subint time (ms)",        c->tSubint*1000.0,   0, 0);
	printDouble("Subints per int",         c->subintsPerInt,    "If not an integer, expect variable weights", 0);
	printDouble("DS buffer obs. dur. (s)", c->datastreamBufferDur, 0, 0);
	printDouble("DS read obs. dur. (s)",   c->datastreamReadDur, "Must not exceed 2^31 ms (~2 sec)", 0);
	printDouble("Manager slack (sec)",     c->managerSlack, 0, 0);
	printDouble("Core buffer obs. dur. (s)", c->coreBufferDur, 0, 0);
}

void printDifxCalculator(const DifxCalculator *C)
{
	int i;
	const DifxCalculatorConfig *c;

	printf("\n");
	printInt("Number of Configurations", C->nConfig, 0);
	printInt("Number of Core processes", C->nCore, C->hasThreadsFile ? "" : "Default since no .threads file");
	printDouble("Number of Threads per Core", C->nThread, C->hasThreadsFile ? "Should be <= number of CPU cores per node" : "Default since no .threads file", "%3.1f");
	printInt("Vis. buffer length", C->visibilityLength, "");
	printDouble("Speedup factor", C->speedUp, "", 0);
	printDouble("Observe dur. (Hours)", C->tObs/1440.0, 0, 0);
	printDouble("Correlation dur. (Hours)", C->tObs/C->speedUp/1440.0, 0, 0);
	printf("\n");

	for(i = 0; i < C->nConfig; i++)
	{
		c = &C->config[i];
		printf("---- CONFIG %d ----\n", i);
		printDifxCalculatorConfig(c);
		printf("\n");
	}
}

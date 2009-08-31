/***************************************************************************
 *   Copyright (C) 2009 by Walter Brisken                                  *
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
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id: difxcalculator.c 1419 2009-08-29 21:37:08Z WalterBrisken $
 * $HeadURL: https://svn.atnf.csiro.au/difx/libraries/difxio/branches/difx-1.5/utils/difxcalculator.c $
 * $LastChangedRevision: 1419 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2009-08-29 15:37:08 -0600 (Sat, 29 Aug 2009) $
 *
 *==========================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include "difx_input.h"

const int nNode = 10;
const int nCore = 7;

int main(int argc, char **argv)
{
	DifxInput *D;
	const DifxConfig *config;
	int nAnt, nBL;
	double nFreq;
	double nPpB;
	double quantBits;
	double bandwidth;
	int nPol;
	int nc, c, f, i, j, bl, ds;
	int BPS, nChan;
	int nDataSeg, dataBufferFactor;
	double speedUp = 1.0;
	int visLength;
	double tInt, tObs;
	double recDataRate;
	double basebandMessageSize;
	double basebandReadSize, coreInputRatio, coreOutputRatio, coreInputRate;
	double coreOutputRate, manInputRate, diskDataRate, datasetSize;
	double databufferDur, managerSlack, tSubint, corebufferDur, subintsPerInt;
	int visSize;
	int decimation;
	double dsBufferSize;	
	double modeSize, coreSize, manSize;


	if(argc < 2)
	{
		printf("Usage : %s <inputfilebase> [<speedUp>]\n", argv[0]);
		return 0;
	}
	
	if(argc > 2)
	{
		speedUp = atof(argv[2]);
	}

	D = loadDifxCalc(argv[1]);
	if(!D)
	{
		fprintf(stderr, "D == 0.  quitting\n");
		return 0;
	}

	D = updateDifxInput(D);
	if(!D)
	{
		fprintf(stderr, "update failed: D == 0.  quitting\n");
		return 0;
	}

	printf("\nNumber of Configurations = %d\n", D->nConfig);

	nDataSeg = D->nDataSegments;
	dataBufferFactor = D->dataBufferFactor;
	visLength = D->visBufferLength;
	tObs = 24*(D->mjdStop - D->mjdStart);

	for(c = 0; c < D->nConfig; c++)
	{
		printf("\nCONFIG %d\n", c);

		config = D->config + c;
		decimation = config->decimation;
		BPS = config->blocksPerSend;
		nChan = config->nChan;

		nAnt  = config->nAntenna;
		nBL   = config->nBaseline;
		tInt  = config->tInt;
		nFreq = 0.0;
		nPpB = 0.0;
		nPol = 0;
		quantBits = 0.0;
		for(i = 0; i < nBL; i++)
		{
			bl = config->baselineId[i];
			nFreq += D->baseline[bl].nFreq;
			for(j = 0; j < D->baseline[bl].nFreq; j++)
			{
				if(D->baseline[bl].nPolProd[j] > nPol)
				{
					nPol = D->baseline[bl].nPolProd[j];
				}
				nPpB += D->baseline[bl].nPolProd[j];
			}
		}
		nPpB /= nFreq;
		nFreq /= nBL;
		if(nPol > 1)
		{
			nPol = 2;
		}
		nc = 0;
		nPpB /= nPol;
		bandwidth = 0.0;
		for(i = 0; i < config->nDatastream; i++)
		{
			ds = config->datastreamId[i];
			quantBits += D->datastream[ds].quantBits;
			for(j = 0; j < D->datastream[ds].nRecChan; j++)
			{
				f = D->datastream[ds].RCfreqId[j];
				f = D->datastream[ds].freqId[f];
				if(f < 0 || f > D->nFreq)
				{
					printf("ACK i=%d, j=%d -> f=%d\n", i, j, f);
				}
				bandwidth += D->freq[f].bw;
				nc++;
			}
		}
		quantBits /= config->nDatastream;
		bandwidth /= nc;

		printf("PARAMETER                 VALUE        NOTE\n");
		printf("Number of telescopes      %d\n", nAnt);
		printf("Number of baselines       %d\n", nBL);
		printf("Number of IFs             %3.1f\n", nFreq);
		printf("Bandwidth (MHz)           %6.4f\n", bandwidth);
		printf("Decimation factor         %d\n", decimation);
		printf("Number of polarizations   %d\n", nPol);
		printf("Pol. products / band      %3.1f\n", nPpB);
		printf("Bits / sample             %3.1f\n", quantBits);
		printf("Blocks per send           %d\n", BPS);
		printf("Spectral points / band    %d\n", nChan);
		printf("Data buffer factor        %d\n", dataBufferFactor);
		printf("Num. data segments        %d\n", nDataSeg);
		printf("\n");
		printf("Playback speedup ratio    %4.2f\n", speedUp);
		printf("Num. core nodes           %d\n", nNode);
		printf("Num threads / core        %d\n", nCore);
		printf("Visbuffer length          %d\n", visLength);
		printf("Integration time (sec)    %4.2f\n", tInt);
		printf("Obs. duration (hours)     %4.2f\n", tObs);

		visSize = (nAnt+nBL)*(8*nChan*nFreq*nPol*nPpB);

		recDataRate = nFreq*bandwidth*nPol*quantBits*decimation*2.0;
		basebandMessageSize = BPS*nChan*2.0*nFreq*nPol*decimation/(8*1024*1024);
		basebandReadSize = basebandMessageSize*dataBufferFactor/nDataSeg;
		coreInputRatio = nAnt/(float)nNode;
		coreInputRate = recDataRate*coreInputRatio*speedUp;
		coreOutputRatio = visSize/(1024*1024*basebandMessageSize);
		coreOutputRate = coreInputRate*coreOutputRatio;
		manInputRate = coreOutputRate*nNode;
		diskDataRate = visSize/(tInt*1024*1024);
		datasetSize = diskDataRate*tObs*3600/1024;

		printf("\n");
		printf("NETWORK / DISK USAGE      VALUE        NOTE\n");
		printf("Record data rate (Mbps)   %5.3f\n", recDataRate);
		printf("Baseband msg size (MB)    %5.3f\n", basebandMessageSize);
		printf("Baseband read size (MB)   %5.3f\n", basebandReadSize);
		printf("Core input data ratio     %5.3f\n", coreInputRatio);
		printf("Core input rate (Mbps)    %5.3f\n", coreInputRate);
		printf("Core output data ratio    %5.3f\n", coreOutputRatio);
		printf("Core output rate (Mbps)   %5.3f\n", coreOutputRate);
		printf("Manager input rate (Mbps) %5.3f\n", manInputRate);
		printf("Disk output rate (MB/s)   %5.3f\n", diskDataRate);
		printf("Dataset size (GB)         %5.3f\n", datasetSize);

		dsBufferSize = basebandMessageSize*dataBufferFactor;
		modeSize = basebandMessageSize/decimation +((nFreq*nPol*nChan*4)*(2+2+2+1)+nChan*4.0*(2+2+2+2+2+2+2+3+5))/(1024*1024);
		coreSize = ((nAnt*modeSize)+(nAnt+nBL)*visSize/(1024*1024))*4+nCore*visSize/(1024*1024);
		manSize = visSize*visLength/(1024*1024);


		printf("\n");
		printf("MEMORY USAGE              VALUE        NOTE\n");
		printf("Size of DS buffer (MB)    %5.3f\n", dsBufferSize);
		printf("Size of vis. dump (bytes) %d\n", visSize);
		printf("Size of a Mode (MB)       %5.3f\n", modeSize);
		printf("Core memory usage (MB)    %5.3f\n", coreSize);
		printf("Manager mem usage (MB)    %5.3f\n", manSize);

		databufferDur = dsBufferSize*8/recDataRate;
		managerSlack = visLength*tInt/2.0;
		tSubint = 1000.0/(recDataRate/(8.0*basebandMessageSize*1.024*1.024));
		corebufferDur = nCore*4.0*tSubint/1000.0;
		subintsPerInt = tInt/(tSubint/1000.0);

		printf("\n");
		printf("TIMES                     VALUE        NOTE\n");
		printf("DS buffer dur. (sec)      %5.3f\n", databufferDur);
		printf("Manager slack (sec)       %5.3f\n", managerSlack);
		printf("Subint time (ms)          %5.3f\n", tSubint);
		printf("Core buffer dur. (sec)    %5.3f\n", corebufferDur);
		printf("Subints. per int.         %5.3f\n", subintsPerInt);
		printf("\n");
	}

	deleteDifxInput(D);

	return 0;
}

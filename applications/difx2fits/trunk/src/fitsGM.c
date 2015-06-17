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
// $Id$
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================
#include <stdlib.h>
#include <sys/types.h>
#include <strings.h>
#include "config.h"
#include "difx2fits.h"

/* return -1 on out-of-range */
int getGateWindow(const DifxPulsar *dp, int bin,
        double *phaseOpen, double *phaseClose)
{
        if(!dp->scrunch)
        {
                /* First the simple case: no scrunching */
                if(bin < 0 || bin >= dp->nBin)
                {
                        return -1;
                }
                *phaseClose = dp->binEnd[bin];
                bin--;
                if(bin < 0)
                {
                        bin = dp->nBin-1;
                }
                *phaseOpen = dp->binEnd[bin];
        }
        else if(bin == 0)
        {
		/* More complicated: include all phases with weight > 0 */
                /* Note: this will get confused for pulsars with interpulses */
                int b;
                int lastBin = dp->nBin-1;
                int binOpen = -1;
                int binClose = -1;
                for(b = 0; b < dp->nBin; b++)
                {
                        if(dp->binWeight[lastBin] <= 0.0 &&
                           dp->binWeight[b] > 0.0)
                        {
                                binOpen = lastBin;
                        }
                        if(dp->binWeight[lastBin] > 0.0 &&
                           dp->binWeight[b] <= 0.0)
                        {
                                binClose = lastBin;
                        }
                        lastBin = b;
                }
                if(binOpen >= 0 && binClose >= 0)
                {
                        *phaseOpen  = dp->binEnd[binOpen];
                        *phaseClose = dp->binEnd[binClose];
                }
                else
                {
                        *phaseOpen = 0.0;
                        *phaseClose = 1.0;
                }
	}
	else
        {
                /* scrunching and bin > 1 doesn't make sense */
                return -1;
        }

        return 0;
}

const DifxInput *DifxInput2FitsGM(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out,
	const struct CommandLineOptions *opts)
{
	char bandFormFloat[8], polyFormDouble[8];

	struct fitsBinTableColumn columns[] =
	{
		{"GATEID",    "1J", "id of this row", 0},
		{"START",     "1D", "time of model start", "MJD"},
		{"STOP",      "1D", "time of model stop", "MJD"},
		{"SOURCE",    "1J", "sourde id from sources tbl", 0},
		{"FREQID",    "1J", "frequency id from frequency tbl", 0},
		{"DISP",      "1D", "dispersion measure. PC/CM^3", 0},
		{"ON_PHASE",  bandFormFloat, "phase gate opens by band", 0},
		{"OFF_PHASE", bandFormFloat, "phase gate closess by band", 0},
		{"REF_FREQ",  "1D", "reference frequency, HZ", 0},
		{"MODEL",     polyFormDouble, "gate control polynomial, PHASE", 0}
	};

	int nColumn;
	int nRowBytes;
	char *fitsbuf;
	char *p_fitsbuf;
	int nBand, nPoly;
	float *onPhase, *offPhase;
	double *poly;
	int configId, scanId;
	const DifxPulsar *dp;
	const DifxPolyco *pc;
	const DifxConfig *config;
	int32_t gateId1;
	int32_t sourceId1;
	int32_t freqId1;
	double start, stop, dm, refFreq;
	double f;
	double phaseOpen, phaseClose;
	int psr, p, i, c, v;

	if(D == 0)
	{
		return 0;
	}

	if(D->nPulsar == 0 || !D->pulsar)
	{
		return D;
	}

	nBand = p_fits_keys->no_band;
	nPoly = DifxPulsarArrayGetMaxPolyOrder(D->pulsar, D->nPulsar);

	if(nPoly < 2)
	{
		nPoly = 2;
	}

	onPhase  = (float *)calloc(nBand, sizeof(float));
	if(onPhase == 0)
	{
		fprintf(stderr, "Error: DifxInput2FitsGM: cannot allocate onPhase (%d)\n", nBand);

		exit(EXIT_FAILURE);
	}
	offPhase = (float *)calloc(nBand, sizeof(float));
	if(offPhase == 0)
	{
		fprintf(stderr, "Error: DifxInput2FitsGM: cannot allocate offPhase (%d)\n", nBand);
		free(onPhase);

		exit(EXIT_FAILURE);
	}
	poly = (double *)calloc(nPoly, sizeof(double));
	if(poly == 0)
	{
		fprintf(stderr, "Error: DifxInput2FitsGM: cannot allocate poly (%d)\n", nPoly);
		free(onPhase);
		free(offPhase);
		
		exit(EXIT_FAILURE);
	}

	sprintf(bandFormFloat,  "%1dE", nBand);  
	sprintf(polyFormDouble, "%1dD", nPoly);  

	nColumn = NELEMENTS(columns);
	nRowBytes = FitsBinTableSize(columns, nColumn);

	fitsWriteBinTable(out, nColumn, columns, nRowBytes, "GATEMODL");

	/* write standard FITS header keywords and values to output file */
	arrayWriteKeys(p_fits_keys, out);

	/* and some specific to this table */
	fitsWriteInteger(out, "TABREV", 1, "");
	fitsWriteInteger(out, "NO_POLY", nPoly, "number of terms in polynomial");
	fitsWriteInteger(out, "BIN_ID", opts->pulsarBin+1, "pulsar bin number");

	fitsWriteEnd(out);

	fitsbuf = (char *)calloc(nRowBytes, 1);
	
	if(fitsbuf == 0)
	{
		fprintf(stderr, "Error: DifxInput2FitsGM: cannot allocate fitsbuf(%d)\n", nRowBytes);
		free(onPhase);
		free(offPhase);
		free(poly);

		exit(EXIT_FAILURE);
	}
	gateId1 = 0;

	for(psr = 0; psr < D->nPulsar; psr++)
	{
		for(scanId = 0; scanId < D->nScan; scanId++)
		{
			configId = D->scan[scanId].configId;
			config = D->config + configId;
			if(config->pulsarId == psr)
			{
				break;
			}
		}
		if(scanId >= D->nScan)
		{
			fprintf(stderr, "PulsarId %d not linked to any scans!\n",
				 psr);
			continue;
		}
		dp = D->pulsar + psr;

		v = getGateWindow(dp, opts->pulsarBin, &phaseOpen, &phaseClose);
		if(v < 0)
		{
			continue;
		}
		if(dp->nBin > 1) for(i = 0; i < nBand; i++)
		{
			onPhase[i]  = phaseOpen;
			offPhase[i] = phaseClose;
		}

		for(p = 0; p < dp->nPolyco; p++)
		{
			sourceId1 = D->scan[scanId].pointingCentreSrc+1;
			freqId1 = config->fitsFreqId+1;
			gateId1++;
			pc = dp->polyco + p;

			dm = pc->dm;
			refFreq = pc->refFreq*1.0e6;	/* convert to Hz */
			start = pc->mjd - pc->nBlk/2880.0;
			stop  = pc->mjd + pc->nBlk/2880.0;

			/* only write those polynomials that overlap the file */
			if(start > D->mjdStop || stop < D->mjdStart)
			{
				continue;
			}

			f = 1.0;
			for(c = 0; c < pc->nCoef; c++)
			{
				poly[c] = pc->coef[c]*f;
				f /= 60.0;
			}
			if(c < nPoly) for(; c < nPoly; c++)
			{
				poly[c] = 0.0;
			}
			poly[0] += pc->p0;
			poly[1] += pc->f0;

			/* pointer to the buffer for FITS records */
			p_fitsbuf = fitsbuf;

			FITS_WRITE_ITEM (gateId1, p_fitsbuf);
			FITS_WRITE_ITEM (start, p_fitsbuf);
			FITS_WRITE_ITEM (stop, p_fitsbuf);
			FITS_WRITE_ITEM (sourceId1, p_fitsbuf);
			FITS_WRITE_ITEM (freqId1, p_fitsbuf);
			FITS_WRITE_ITEM (dm, p_fitsbuf);
			FITS_WRITE_ARRAY(onPhase, p_fitsbuf, nBand);
			FITS_WRITE_ARRAY(offPhase, p_fitsbuf, nBand);
			FITS_WRITE_ITEM (refFreq, p_fitsbuf);
			FITS_WRITE_ARRAY(poly, p_fitsbuf, nPoly);

			testFitsBufBytes(p_fitsbuf - fitsbuf, nRowBytes, "GM");
#ifndef WORDS_BIGENDIAN
			FitsBinRowByteSwap(columns, nColumn, fitsbuf);
#endif
			fitsWriteBinRow(out, fitsbuf);
		}
	}

	free(fitsbuf);
	free(onPhase);
	free(offPhase);
	free(poly);

	return D;
}	

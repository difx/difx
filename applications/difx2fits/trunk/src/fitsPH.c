/***************************************************************************
 *   Copyright (C) 2008-2011 by Walter Brisken & John Morgan               *
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
#include "other.h"

const float pcaltiny = 1e-10;

#warning "FIXME: even this could be too small!"
const int MaxLineLength = 40000;
const double DefaultDifxCableCalExtrapolate = 2.0; /* The timerange a cable cal measurement 
						    * is valid over is the integration window
						    * multiplied by this factor */


#warning "FIXME: the generation of this FITS table is quite broken with regard to multiple datastreams per antenna."



/* This function is not really well posed and although it looks like it
 * belongs in difxio it should stay here until its fundamental problem is
 * fixed.  That is that it implicitly assumes one datastream per antenna.
 * to get around that, the specific datastream should be passed into this
 * function.  --WFB
 */
int DifxInputGetIFsByRecBand(int *IFs, int *polId, const DifxInput *D, int antId, int configId, int recBand, int maxCount)
{
	DifxConfig *dc;
	DifxDatastream *dd = 0;
	DifxFreq *df;
	int n = 0;
	int i, d, p;
	int dsId, localFreqId;
	char polName;

	if(!D)
	{
		return -1;
	}
	if(recBand < 0 || antId >= D->nAntenna || antId < 0 || configId >= D->nConfig || configId < 0)
	{
		return -2;
	}

	dc = D->config + configId;

	for(d = 0; d < dc->nDatastream; d++)
	{
		dsId = dc->datastreamId[d];
		if(dsId >= 0 && dsId < D->nDatastream)
		{
			if(D->datastream[dsId].antennaId == antId)
			{
				dd = D->datastream + dsId;

				break;
			}
		}
	}

	if(!dd)
	{
		return 0;
	}

	if(recBand >= dd->nRecBand)
	{
		return -3;
	}

	polName = dd->recBandPolName[recBand];

	if(dc->nPol > 0 && dc->pol[0] == polName)
	{
		*polId = 0;
	}
	else if(dc->nPol > 1 && dc->pol[1] == polName)
	{
		*polId = 1;
	}
	else
	{
		fprintf(stderr, "Developer error: DifxInputGetIFsByRecBand: polName=%c not in configId=%d\n", 
			polName, configId);

		return -4;
	}

	localFreqId = dd->recBandFreqId[recBand];
	df = D->freq + dd->recFreqId[localFreqId];

	for(i = 0; i < dc->nIF; i++)
	{
		if(isDifxIFInsideDifxFreq(dc->IF + i, df))
		{
			for(p = 0; p < dc->IF[i].nPol; p++)
			{
				if(dc->IF[i].pol[p] == polName)
				{
					if(n < maxCount)
					{
						IFs[n] = i;
					}
					n++;
				}
			}
		}
	}

	return n;
}

/* This function is not really well posed and although it looks like it
 * belongs in difxio it should stay here until its fundamental problem is
 * fixed.  That is that it implicitly assumes one datastream per antenna.
 * to get around that, the specific datastream should be passed into this
 * function.  --WFB
 */
int DifxInputGetIFsByRecFreq(int *IFs, const DifxInput *D, int dsId, int configId, int recFreq, int polId, int maxCount)
{
	DifxConfig *dc;
	DifxDatastream *dd = 0;
	DifxFreq *df;
	int n = 0;
	int i, p;
	char polName;

	if(!D)
	{
		return -1;
	}
	if(recFreq < 0 || dsId >= D->nDatastream || dsId < 0 || configId >= D->nConfig || configId < 0)
	{
		return -2;
	}

	dc = D->config + configId;

	if(polId >= dc->nPol)
	{
		return -1;
	}

	dd = D->datastream + dsId;

	polName = dc->pol[polId];

	if(recFreq >= dd->nRecFreq)
	{
		return -3;
	}

	df = D->freq + dd->recFreqId[recFreq];

	for(i = 0; i < dc->nIF; i++)
	{
		if(isDifxIFInsideDifxFreq(dc->IF + i, df))
		{
			for(p = 0; p < dc->IF[i].nPol; p++)
			{
				if(dc->IF[i].pol[p] == polName)
				{
					if(n < maxCount)
					{
						IFs[n] = i;
					}
					n++;
				}
			}
		}
	}

	return n;
}

int getDifxPcalFile(const DifxInput *D, int antId, int jobId, FILE **file)
{
	char filename[DIFXIO_FILENAME_LENGTH];
	const char prefix[] = "/PCAL_";

	if(*file)
	{
		fclose(*file);
		*file = 0;
	}

	sprintf(filename, "%s%s%s", D->job[jobId].outputFile,
			 prefix, D->antenna[antId].name);

	*file = fopen(filename, "r");
	if(!*file)
	{
		fprintf(stderr, "\nFile %s does not exist.\n", filename);

		return -1;
	}
	
	return 0;
}
	
/* Go through pcal file, determine maximum number of tones.
 * Note that this one file can contain different numbers of tones at different times,
 * so the output should only be used to set an upper limit on array sizes and
 * things of that ilk.
 */
static int getNTone(const char *filename, double t1, double t2)
{
	FILE *in;
	char line[MaxLineLength+1];
	int n, nTone, maxnTone=0;
	double t;
	char *rv;
	char antName1[DIFXIO_NAME_LENGTH];

	in = fopen(filename, "r");
	if(!in)
	{
		return -1;
	}
	
	for(;;)
	{
		rv = fgets(line, MaxLineLength, in);
		if(!rv)
		{
			break;
		}
		n = sscanf(line, "%s%lf%*f%*f%*d%*d%d", antName1, &t, &nTone);
		if(n != 3)
		{
			continue;
		}
		if(t >= t1 && t <= t2)
		{
			if(nTone > maxnTone)
			{
				maxnTone = nTone;
			}
		}
		
	}
	fclose(in);

	return maxnTone;
}

inline int isUndefinedVLBA(double v)
{
	/* A VLBA specialty! */
	return (v > 999.89 && v < 999.91);
}

/* The following function is for parsing a line of the station-extracted `pcal' file. 
 * Here there is an implicit assumption that there is one datastream per antenna 
 */
static int parsePulseCal(const char *line, 
	int antId,
	int *sourceId,
	double *time, float *timeInt, double *cableCal,
	double freqs[2][array_MAX_TONES], 
	float pulseCalRe[2][array_MAX_TONES], 
	float pulseCalIm[2][array_MAX_TONES], 
	float stateCount[2][array_MAX_STATES*array_MAX_BANDS],
	float pulseCalRate[2][array_MAX_TONES],
	int refDay, const DifxInput *D, int *configId, 
	int phasecentre)
{
	int IFs[array_MAX_BANDS];
	int states[array_MAX_STATES];
	int np, nb, nt, ns, nIF;
	int nRecBand, recBand;
	int n, p, i;
	int polId, tone, s;
	int toneIndex;
	int pol, band;
	int scanId;
	double A;
	float B, C;
	double mjd;
	char antName[DIFXIO_NAME_LENGTH];
	
	/* Note: This is a particular NaN variant the FITS-IDI format/convention 
	 * wants, namely 0xFFFFFFFF */
	union
	{
		int32_t i32;
		float f;
	} nan;
	nan.i32 = -1;
	
	for(pol = 0; pol < 2; pol++)
	{
		for(toneIndex = 0; toneIndex < array_MAX_TONES; toneIndex++)
		{
			freqs[pol][toneIndex] = 0.0;
			pulseCalRe[pol][toneIndex] = nan.f;
			pulseCalIm[pol][toneIndex] = nan.f;
			pulseCalRate[pol][toneIndex] = 0.0;
		}
		for(s = 0; s < array_MAX_STATES*array_MAX_BANDS; s++)
		{
			stateCount[pol][s] = nan.f;
		}
	}

	n = sscanf(line, "%s%lf%f%lf%d%d%d%d%d%n", antName, time, timeInt, 
		cableCal, &np, &nb, &nt, &ns, &nRecBand, &p);
	if(n != 9)
	{
		return -1;
	}
	line += p;

	if(ns > array_MAX_STATES)
	{
		fprintf(stderr, "Developer error: parsePulseCal: array_MAX_STATES=%d is to small (needs to be %d)\n",
			array_MAX_STATES, ns);

		exit(EXIT_FAILURE);
	}

	/* A VLBA specialty! */
	if(isUndefinedVLBA(*cableCal))
	{
		*cableCal = nan.f;
	}

	*time -= refDay;
	mjd = *time + (int)(D->mjdStart);

	if(mjd < D->mjdStart || mjd > D->mjdStop)
	{
		return -1;
	}

	scanId = DifxInputGetScanIdByAntennaId(D, mjd, antId);
	if(scanId < 0)
	{	
		return -3;
	}

        if(phasecentre >= D->scan[scanId].nPhaseCentres)
        {
		return -3;
        }

	*sourceId = D->scan[scanId].phsCentreSrcs[phasecentre];
	*configId = D->scan[scanId].configId;
	if(*sourceId < 0 || *configId < 0)	/* not in scan */
	{
		return -3;
	}

	*cableCal *= 1e-12;	/* convert to s from ps */

	/* Read in pulse cal information */
	for(pol = 0; pol < np; pol++)
	{
		/* Loop over number of recorded bands in .pc file. */
		for(band = 0; band < nb; band++)
		{
			for(tone = 0; tone < nt; tone++)
			{
				n = sscanf(line, "%d%lf%f%f%n", &recBand, &A, &B, &C, &p);
				if(n < 4)
				{
					return -4;
				}
				line += p;
				if(recBand < 0 || recBand >= nRecBand)
				{
					continue;
				}
				if(!isUndefinedVLBA(B) && !isUndefinedVLBA(C))
				{
					/* get a list of IFs (which could be zoom bands) corresponding to a recBand */
					nIF = DifxInputGetIFsByRecBand(IFs, &polId, D, antId, *configId, recBand, array_MAX_BANDS);
					if(nIF > 0 && polId >= 0 && polId < 2)
					{
						for(i = 0; i < nIF; i++)
						{
							toneIndex = tone + IFs[i]*nt;

							/* Subtlety here: in case multiple recBands correspond to the same FITS IF,
							 * let the one corresponding to the first survive; this is the default 
							 * elsewhere in difx2fits and other programs */
							if(freqs[polId][toneIndex] == 0.0)
							{
								freqs[polId][toneIndex] = A*1.0e6;	/* convert to Hz from MHz */
								pulseCalRe[polId][toneIndex] = B*cos(C*M_PI/180.0);
								pulseCalIm[polId][toneIndex] = B*sin(C*M_PI/180.0);
							}
						}
					}
				}
			}
		}
	}
	
	if(ns > 0)
	{
		/* Read in state count information */
		for(pol = 0; pol < np; pol++)
		{
			/* Loop over number of recorded bands in .pc file. */
			for(band = 0; band < nb; band++)
			{
				n = sscanf(line, "%d%n", &recBand, &p);
				line += p;

				for(s = 0; s < ns; s++)
				{
					n = sscanf(line, "%f%n", &B, &p);
					if(n < 1)
					{
						return -5;
					}
					line += p;
					states[s] = B;
				}

				nIF = DifxInputGetIFsByRecBand(IFs, &polId, D, antId, *configId, recBand, array_MAX_BANDS);
				if(nIF > 0 && polId >= 0 && polId < 2)
				{
					for(s = 0; s < ns; s++)
					{
						for(i = 0; i < nIF; i++)
						{
							stateCount[polId][s + i*4] = states[s];
						}
					}
				}
			}
		}
	}

	return 0;
}

/* The following function is for parsing a line of the 
 * station-extracted `pcal' file and extracting only the cable cal
 * value. Any values with their time centroid outside of a scan
 * will be discarded */
static int parsePulseCalCableCal(const char *line, 
	int antId,
	int *sourceId, int *scanId,
	double *time, float *timeInt, double *cableCal,
	int refDay, const DifxInput *D, int *configId, 
	int phasecentre)
{
	int n, p;
	double mjd;
	char antName[DIFXIO_NAME_LENGTH];

	/* Note: This is a particular NaN variant the FITS-IDI format/convention 
	 * wants, namely 0xFFFFFFFF */
	union
	{
		int32_t i32;
		float f;
	} nan;
	nan.i32 = -1;
	
	n = sscanf(line, "%s%lf%f%lf%n", antName, time, timeInt, cableCal, &p);
	if(n != 4)
	{
		return -1;
	}

	*time -= refDay;
	mjd = *time + (int)(D->mjdStart);

	if(mjd < D->mjdStart || mjd > D->mjdStop)
	{
		return -1;
	}

	*scanId = DifxInputGetScanIdByAntennaId(D, mjd, antId);
	if(*scanId < 0)
	{	
		return -3;
	}

        if(phasecentre >= D->scan[*scanId].nPhaseCentres)
        {
		return -3;
        }

	*sourceId = D->scan[*scanId].phsCentreSrcs[phasecentre];
	*configId = D->scan[*scanId].configId;
	if(*sourceId < 0 || *configId < 0)	/* not in scan */
	{
		return -3;
	}
	
	if(isUndefinedVLBA(*cableCal))
	{
		*cableCal = nan.f;
	}
	else
	{
		/* convert from ps to s */
		*cableCal *= 1e-12;
	}

	return 0;
}

/* The following function is for parsing a line of the files containing 
 * The DiFX-extracted pulse cals */
static int parseDifxPulseCal(const char *line, 
	int dsId, int nBand, int nTone,
	int *sourceId, int *scanId, double *time, int jobId,
	double freqs[2][array_MAX_TONES], 
	float pulseCalRe[2][array_MAX_TONES], 
	float pulseCalIm[2][array_MAX_TONES], 
	float stateCount[2][array_MAX_STATES*array_MAX_BANDS],
	float pulseCalRate[2][array_MAX_TONES],
	int refDay, const DifxInput *D, int *configId, 
	int phasecentre)
{
	static int tooMany[2][array_MAX_BANDS] = { {0} };	/* zeros the values */
	static int tooFew[2][array_MAX_BANDS] = { {0} };	/* zeros the values */
	static int lastDsId = -1;
	const DifxFreq *df;
	const DifxDatastream *dd;
	int np, nb, nt, ns;
	int nRecBand, nIF, difxPol, nRecTone;
	double toneFreq[array_MAX_TONES];
	int IFs[array_MAX_BANDS];
	int n, p, i, k, v;
	int tone;
	int toneIndex;
	int band, pol, bandpol, recFreq;
	int freqId;
	int nontiny = 0;
	int A;
	float B, C;
	double mjd;
	char antName[DIFXIO_NAME_LENGTH];
	double cableCal;
	float timeInt, dt;

	/* Note: This is a particular NaN variant the FITS-IDI format/convention 
	 * wants, namely 0xFFFFFFFF */
	union
	{
		int32_t i32;
		float f;
	} nan;
	nan.i32 = -1;
	
	if(dsId != lastDsId)
	{
		for(pol = 0; pol < 2; pol++)
		{
			for(i = 0; i < array_MAX_BANDS; i++)
			{
				tooMany[pol][i] = 0;
				tooFew[pol][i] = 0;
			}
		}
		lastDsId = dsId;
	}

	for(pol = 0; pol < 2; pol++)
	{
		for(toneIndex = 0; toneIndex < array_MAX_TONES; toneIndex++)
		{
			freqs[pol][toneIndex] = 0.0;
			pulseCalRe[pol][toneIndex] = 0.0;
			pulseCalIm[pol][toneIndex] = 0.0;
			pulseCalRate[pol][toneIndex] = 0.0;
		}
		for(i = 0; i < array_MAX_STATES*array_MAX_BANDS; i++)
		{
			/* No state counts are available for difx extracted pcals */
			stateCount[pol][i] = nan.f;
		}
	}

	n = sscanf(line, "%s%lf%f%lf%d%d%d%d%d%n", antName, time, &timeInt, 
		&cableCal, &np, &nb, &nt, &ns, &nRecBand, &p);
	if(n != 9)
	{
		fprintf(stderr, "Error: parseDifxPulseCal: header information not parsable (n=%d)\n", n);
		fprintf(stderr, "Line: %s\n", line);

		return -1;
	}
	line += p;

	*time -= refDay;
	mjd = *time + (int)(D->mjdStart);

	if(mjd < D->mjdStart || mjd > D->mjdStop)
	{
		return -2;
	}

	dd = D->datastream + dsId;

	*scanId = DifxInputGetScanIdByAntennaId(D, mjd, dd->antennaId);
	if(*scanId < 0)
	{	
		return -3;
	}

        if(phasecentre >= D->scan[*scanId].nPhaseCentres)
        {
		return -4;
        }

	*sourceId = D->scan[*scanId].phsCentreSrcs[phasecentre];
	*configId = D->scan[*scanId].configId;
	if(*sourceId < 0 || *configId < 0)	/* not in scan */
	{
		return -5;
	}

	dt = D->config[*configId].tInt/(86400.0*2.001);  
	if(D->scan[*scanId].mjdStart > mjd-dt || D->scan[*scanId].mjdEnd < mjd+dt)
	{
		return -6;
	}
        if(isAntennaFlagged(D->job + jobId, mjd, dd->antennaId))
	{
		return -7;
	}
	/* Read in pulse cal information */
	for(pol = 0; pol < D->nPol; pol++)
	{
		for(band = 0; band < dd->nRecBand; band++)
		{
			v = DifxConfigRecBand2FreqPol(D, *configId, dd->antennaId, band, &recFreq, &bandpol);
			if(v < 0)
			{
				//band unused for this antenna
				continue;
			}
			if(pol != bandpol)
			{
				//band doesn't match
				continue;
			}
			//printf("recFreq=%d pol=%d\n", recFreq, pol);

			freqId = dd->recFreqId[recFreq];
			df = D->freq + freqId;

			
			/* set up pcal information for this recFreq (only up to nRecTones)*/
			nRecTone = DifxDatastreamGetPhasecalTones(toneFreq, dd, df, nt);

			if(nRecTone > 0)
			{
				k = 0; /* tone index within freqs, pulseCalRe and pulseCalIm */
				for(tone = 0; tone < nt; tone++)/*nt is taken from line header and is max number of tones*/
				{
					n = sscanf(line, "%d%d%f%f%n", &difxPol, &A, &B, &C, &p);
					if(n < 4)
					{
						printf("Warning: parseDifxPulseCal: Error scanning line\n");
						
						return -8;
					}
					line += p;

					/* Only write out specified frequencies */
					if(toneFreq[tone] < 0.0)
					{
						continue;
					}

					/*Check that frequency is correct*/
					if(A != (int)(toneFreq[tone]+0.5))
					{
						printf("\nWarning: parseDifxPulseCal: tone frequency in PCAL file %d doesn't match expected %g for antenna %d, mjd %12.6f\n", A, toneFreq[tone], dd->antennaId, mjd);
					}

					if(k >= nTone)
					{
						if(tooMany[pol][recFreq] == 0)
						{
							printf("\nWarning: parseDifxPulseCal: trying to extract too many (%d >= %d) tones in pol %d recFreq %d\n", k, nTone, pol, recFreq);
						}
						tooMany[pol][recFreq]++;
						
						break;
					}

					nIF = DifxInputGetIFsByRecFreq(IFs, D, dsId, *configId, recFreq, pol, array_MAX_BANDS);

					if(nIF > 0)
					{
						for(i = 0; i < nIF; i++)
						{
							if(df->sideband == 'U')
							{
							      toneIndex = IFs[i]*nTone + k;
							}
							else
							{
								toneIndex = (IFs[i]+1)*nTone - k - 1;/*make LSB ascend in frequency*/
							}
							freqs[pol][toneIndex] = toneFreq[tone]*1.0e6;	/* MHz to Hz */
							if(fabs(B) > pcaltiny || fabs(C) > pcaltiny)
							{
								nontiny++;
								pulseCalRe[pol][toneIndex] = B;
								pulseCalIm[pol][toneIndex] = C;
							}
						}
						k++;
					}
				}
				if(k < nTone)
				{
					if(tooFew[pol][recFreq] == 0)
					{
						printf("\nWarning: parseDifxPulseCal: Not enough (%d < %d) extracted tones for pol %d recFreq %d\n", k, nTone, pol, recFreq);
					}
					tooFew[pol][recFreq]++;
					
					continue;
				}
			}
		}
	}
	if(nontiny == 0)
	{
		return -8;
	}

	return 0;
}

int countTones(const DifxDatastream *dd)
{
	int t;
	int n = 0;

	for(t = 0; t < dd->nRecTone; t++)
	{
		if(dd->recToneOut[t])
		{
			n++;
		}
	}

	return n;
}

const DifxInput *DifxInput2FitsPH(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out,
	int phasecentre, double avgSeconds)
{
	char stateFormFloat[8];
	char toneFormDouble[8];
	char toneFormFloat[8];
	
	/*  define the flag FITS table columns */
	struct fitsBinTableColumn columns[] =
	{
		{"TIME", "1D", "time of center of interval", "DAYS"},
		{"TIME_INTERVAL", "1E", "time span of datum", "DAYS"},
		{"SOURCE_ID", "1J", "source id number from source tbl", 0},
		{"ANTENNA_NO", "1J", "antenna id from array geom. tbl", 0},
		{"ARRAY", "1J", "????", 0},
		{"FREQID", "1J", "freq id from frequency tbl", 0},
		{"CABLE_CAL", "1D", "cable length calibration", "SECONDS"},
		{"STATE_1", stateFormFloat, "state counts (4 per baseband)", 0},
		{"PC_FREQ_1", toneFormDouble, "Pcal recorded frequency", "Hz"},
		{"PC_REAL_1", toneFormFloat, "Pcal real", 0},
		{"PC_IMAG_1", toneFormFloat, "Pcal imag", 0},
		{"PC_RATE_1", toneFormFloat, "Pcal rate", 0},
		{"STATE_2", stateFormFloat, "state counts (4 per baseband)", 0},
		{"PC_FREQ_2", toneFormDouble, "Pcal recorded frequency", "Hz"},
		{"PC_REAL_2", toneFormFloat, "Pcal real", 0},
		{"PC_IMAG_2", toneFormFloat, "Pcal imag", 0},
		{"PC_RATE_2", toneFormFloat, "Pcal rate", 0}
	};

	int nColumn;
	int nRowBytes;
	char *fitsbuf, *p_fitsbuf;
	char line[MaxLineLength+1];
	int nBand, nPol;
	int nTone=-2;
	int nDifxTone;
	int nAccum = 0;
	int lastnWindow;
	double time, dumpTime, accumStart=0.0, accumEnd=0.0;
	float timeInt, dumpTimeInt;
	int cableScanId, nextCableScanId, lineCableScanId;
	double cableCal, nextCableCal, cableCalOut, lineCableCal;
	double cableTime, nextCableTime, lineCableTime;
	float cablePeriod, nextCablePeriod, lineCablePeriod;
	double cableSigma, nextCableSigma;
	double freqs[2][array_MAX_TONES];
	float pulseCalRe[2][array_MAX_TONES];
	float pulseCalIm[2][array_MAX_TONES];
	float pulseCalReAcc[2][array_MAX_TONES]; 
	float pulseCalImAcc[2][array_MAX_TONES]; 
	float stateCount[2][array_MAX_STATES*array_MAX_BANDS];
	float pulseCalRate[2][array_MAX_TONES];
	int configId=0, sourceId, currentScanId;
	int scanId;
	int refDay;
	int i, a, dsId, j, k, t, n, v;
	int doDump = 0;
	int nWindow;
	double start, stop;
	double windowDuration=0.0, dumpWindow=0.0;
	FILE *in=0;
	FILE *in2=0;
	char *rv;
	/* The following are 1-based indices for FITS format */
	int32_t antId1, arrayId1, sourceId1, freqId1;

	char antName[DIFXIO_NAME_LENGTH];

	/* Note: This is a particular NaN variant the FITS-IDI format/convention 
	 * wants, namely 0xFFFFFFFF, or 0xFFFFFFFFFFFFFFFF for double */
	union
	{
		int64_t i64;
		double d;
		float f;
	} nan;
	nan.i64 = -1;

	if(D == 0)
	{
		return D;
	}

	printf("\n");
	nBand = D->nIF;
	nPol = D->nPol;
	scanId = -1;

	mjd2dayno((int)(D->mjdStart), &refDay);

	start = D->mjdStart - (int)(D->mjdStart);
	stop  = D->mjdStop  - (int)(D->mjdStart);

	//check for existence of pcal file
	in = fopen("pcal", "r");
	if(in)
	{
		printf("    Station-extracted pcals available\n");
		//check if it will be used and if so, how many tones it contains
		for(i = 0; i < D->nDatastream; i++)
		{
			if(D->datastream[i].phaseCalIntervalMHz < 1)
			{
				nTone = getNTone("pcal", refDay + start, refDay + stop);
				break;
			}
		}
	}
	else
	{
		printf("    Station pcal file not found. No station pcal or cable cal measurements available\n");
	}


	nDifxTone = DifxInputGetMaxTones(D);
	if(nDifxTone == 0)
	{
		printf("    No Tones extracted by DiFX\n");
	}
	else
	{
		printf("    DiFX-extracted tones available:");
	}

	if(nDifxTone > nTone)
	{
		nTone = nDifxTone;
	}

	if(nTone*nBand > array_MAX_TONES)
	{
		printf("Developer Error: nTone(=%d)*nBand(=%d) exceeds array_MAX_TONES(=%d).  No pulse cal data will be enFITSulated.\n",
			nTone, nBand, array_MAX_TONES);

		return D;
	}
	if(nTone < 1)
	{
		return D;
	}

	sprintf(stateFormFloat, "%dE", 4*nBand);
	sprintf(toneFormFloat,  "%dE", nTone*nBand);
	sprintf(toneFormDouble, "%dD", nTone*nBand);
	
	if(nPol == 2)
	{
		nColumn = NELEMENTS(columns);
	}
	else
	{
		nColumn = NELEMENTS(columns) - 5;
	}
	
	nRowBytes = FitsBinTableSize(columns, nColumn);

	/* calloc space for storing table in FITS format */
	fitsbuf = (char *)calloc(nRowBytes, 1);
	if(fitsbuf == 0)
	{
	        fclose(in);
		fprintf(stderr, "Error: DifxInput2FitsPH: Memory allocation failure\n");

		exit(EXIT_FAILURE);
	}

	fitsWriteBinTable(out, nColumn, columns, nRowBytes, "PHASE-CAL");
	arrayWriteKeys (p_fits_keys, out);
	fitsWriteInteger(out, "NO_POL", nPol, "");
	fitsWriteInteger(out, "NO_TONES", nTone, "");
	fitsWriteInteger(out, "TABREV", 2, "");
	fitsWriteEnd(out);

	arrayId1 = 1;

	for(a = 0; a < D->nAntenna; a++)
	{
		for(k = 0; k < 2; k++)
		{
			for(t = 0; t < array_MAX_TONES; t++)
			{
				pulseCalReAcc[k][t] = 0.0;
				pulseCalImAcc[k][t] = 0.0;
			}
		}
		nWindow = 0;
		nAccum = 0;

		currentScanId = -1;
		printf(" %s", D->antenna[a].name);
		fflush(stdout);

		for(j = 0; j < D->nJob; j++)
		{
			dsId = DifxInputGetDatastreamId(D, j, a);
			if(dsId < 0)
			{
				/*antenna not present in this job*/
				continue;/*to next job*/
			}

			if(D->datastream[dsId].phaseCalIntervalMHz > 0 && countTones(&(D->datastream[dsId])) > 0)
			{
				v = getDifxPcalFile(D, a, j, &in2);
				if(v != 0)
				{
					/* Error message given in getDifxPcalFile if needed */
					continue;/*to next job*/
				}
			}
			else
			{
				/*Don't mix DiFX and station pcals*/
				if(nDifxTone)
				{
					if(countTones(&(D->datastream[dsId])) > 0)
					{
						printf("Warning: no pcals for Antenna %s in JobId %d\n", D->antenna[a].name, j);
					}

					continue;/*to next job*/
				}
			}

			/* set defaults */
			cableScanId = -1;
			cableCal = 0.0;
			cableTime = 0.0;
			cablePeriod= -1.0;
			nextCableScanId = -1;
			nextCableCal = 0.0;
			nextCableTime = 0.0;
			nextCablePeriod = -1.0;
			lineCableScanId = -1;
			lastnWindow = nWindow;


			/*rewind(in) below this loop*/
			while(1)
			{
				sourceId = -1;

				if(in && !nDifxTone)/*try reading pcal file*/
				{
					rv = fgets(line, MaxLineLength, in);
					if(rv)
					{
						/* ignore possible comment lines */
						if(line[0] == '#')
						{
							continue;/*to next line in file*/
						}
						else 
						{
							n = sscanf(line, "%s", antName);
							if(n != 1 || strcmp(antName, D->antenna[a].name))
							{
								continue;/*to next line in file*/	
							}
							v = parsePulseCal(line, a, &sourceId, &time, &timeInt, 
								&cableCal, freqs, pulseCalReAcc, pulseCalImAcc,
								stateCount, pulseCalRate, refDay, D, &configId, phasecentre);
							if(v < 0)
							{
								continue;/*to next line in file*/
							}
						}
					}
					doDump = 1;//write out every line for pcal file
				}
				else/*reading difx-extracted pcals*/
				{	
					rv = fgets(line, MaxLineLength, in2);
					if(rv)
					{
						/* ignore possible comment lines */
						if(line[0] == '#')
						{
							continue;/*to next line in file*/
						}
						else 
						{
							v = parseDifxPulseCal(line, dsId, nBand, nTone, &sourceId, &scanId, &time, j,
									      freqs, pulseCalRe, pulseCalIm, stateCount, pulseCalRate,
									      refDay, D, &configId, phasecentre);
							if(v < 0)
							{
								continue;/*to next line in file*/
							}

						}
						if(scanId != currentScanId)
						{
							double s, e;
							int nWindow;
							DifxScan *scan;

							/* Get ready to dump last scan (if it's not the first run through)
							 * Work out time average windows so that there is an integer number within the scan
							 * Get all of the relevant cable cal values for this antenna
							 * n.b. we can safely assume that we are at a valid pcal entry since v > 0 */
							if(currentScanId != -1)
							{
								doDump = 1;
							}

							scan = D->scan + scanId;
							configId = scan->configId;
							s = time;	/* time of first pcal record */
							e = scan->mjdEnd - (int)(D->mjdStart);

							nWindow = (int)((e - s + (0.5*D->config[configId].tInt/86400.))/(avgSeconds/86400.0) + 0.5);

							if(nWindow < 1)
							{
								nWindow = 1;
							}
							windowDuration = (e - s)/nWindow;
							dumpWindow = s + windowDuration;
							currentScanId = scanId;
						}
						else if(time > dumpWindow && dumpWindow > 0.0)
						{
							doDump = 1;

							while(time > dumpWindow)
							{
								dumpWindow += windowDuration;
							}
						}
					}
					else if(j == D->nJob -1)
					{
						/* end of last job reached */
						doDump = 1;
					}
				}
				if(doDump)
				{
					if(nDifxTone)
					{
						dumpTime = (accumStart + accumEnd)*0.5;
						dumpTimeInt = nAccum*D->config[configId].tInt/86400;
						if(in)
						{
							while(nextCableTime < dumpTime || lineCableScanId < 0)
							{

								rv = fgets(line, MaxLineLength, in);
								if(!rv)
								{
									break;/*to out of cablecal search*/
								}
									
								/* ignore possible comment lines */
								if(line[0] == '#')
								{
									continue;/*to next line in file*/
								}
								else 
								{
									n = sscanf(line, "%s", antName);
									if(n != 1 || strcmp(antName, D->antenna[a].name))
									{
										continue;/*to next line in file*/	
									}
									v = parsePulseCalCableCal(line, a, &sourceId, &lineCableScanId, &lineCableTime, &lineCablePeriod, 
										&lineCableCal, refDay, D, &configId, phasecentre);
									if(v < 0)
									{
										continue;/*to next line in file*/
									}
									cableScanId = nextCableScanId;
									cableTime = nextCableTime;
									cablePeriod = nextCablePeriod;
									cableCal = nextCableCal;
									nextCableScanId = lineCableScanId;
									nextCableTime = lineCableTime;
									nextCablePeriod = lineCablePeriod;
									nextCableCal = lineCableCal;
								}
							}
							//fprintf(stderr, ".");
							/*next choose which cable cal value to use (if any)*/
							nextCableSigma = 2*fabs(dumpTime - nextCableTime)/(nextCablePeriod);
							cableSigma = 2*fabs(dumpTime - cableTime)/(cablePeriod);
							if (cableSigma > DefaultDifxCableCalExtrapolate &&
							    nextCableSigma > DefaultDifxCableCalExtrapolate)
							{
								cableCalOut = nan.f;
							}
							else if (cableScanId != scanId && nextCableScanId != scanId)
							{
								cableCalOut = nan.f;
							}
							else if (cableScanId == scanId && nextCableScanId != scanId)
							{
								cableCalOut = cableCal;
							}
							else if (cableScanId != scanId && nextCableScanId == scanId)
							{
								cableCalOut = nextCableCal;
							}
							else if (fabs(cableTime-dumpTime) > 
								 fabs(nextCableTime-dumpTime))
							{
								cableCalOut = nextCableCal;
							}
							else
							{
								cableCalOut = cableCal;
							}
						}
						else
						{
							cableCalOut = 0.0;
						}
					}/*end if nDifxTone*/
					else
					{
						dumpTime = time;
						dumpTimeInt = timeInt;
						cableCalOut = cableCal;
					}

					freqId1 = D->config[configId].fitsFreqId + 1;
					if(sourceId >= 0)
					{
						sourceId1 = D->source[sourceId].fitsSourceIds[configId] + 1;
					}
					else
					{
						sourceId1 = 0;
					}
					antId1 = a + 1;

					p_fitsbuf = fitsbuf;
				
					if(!nDifxTone || nAccum*D->config[configId].tInt > 0.25*avgSeconds || lastnWindow == 1)	/* only write a row if a reasonable amount of data were averaged */
					{
						FITS_WRITE_ITEM(dumpTime, p_fitsbuf);
						FITS_WRITE_ITEM(dumpTimeInt, p_fitsbuf);
						FITS_WRITE_ITEM(sourceId1, p_fitsbuf);
						FITS_WRITE_ITEM(antId1, p_fitsbuf);
						FITS_WRITE_ITEM(arrayId1, p_fitsbuf);
						FITS_WRITE_ITEM(freqId1, p_fitsbuf);
						FITS_WRITE_ITEM(cableCalOut, p_fitsbuf);

						for(k = 0; k < nPol; k++)
						{
							FITS_WRITE_ARRAY(stateCount[k], p_fitsbuf, 4*nBand);
							if(nTone > 0)
							{
								FITS_WRITE_ARRAY(freqs[k], p_fitsbuf, nTone*nBand);
								FITS_WRITE_ARRAY(pulseCalReAcc[k], p_fitsbuf, nTone*nBand);
								FITS_WRITE_ARRAY(pulseCalImAcc[k], p_fitsbuf, nTone*nBand);
								FITS_WRITE_ARRAY(pulseCalRate[k], p_fitsbuf, nTone*nBand);
							}
						}

						testFitsBufBytes(p_fitsbuf - fitsbuf, nRowBytes, "PH");
						
#ifndef WORDS_BIGENDIAN
						FitsBinRowByteSwap(columns, nColumn, fitsbuf);
#endif
						fitsWriteBinRow(out, fitsbuf);
					}

					nAccum = 0;
					doDump = 0;
					for(k = 0; k < nPol; k++)
					{
						for(t = 0; t < nTone*D->datastream[dsId].nRecFreq; t++)
						{
							pulseCalReAcc[k][t] = 0.0;
							pulseCalImAcc[k][t] = 0.0;
						}
					}
				}//end if doDump
				if(!rv)
				{
					/* after dumping, if needed, we go to the next job */
					break;
				}
				for(k = 0; k < nPol; k++)
				{
					for(t = 0; t < nTone*D->datastream[dsId].nRecFreq; t++)
					{
						pulseCalReAcc[k][t] += pulseCalRe[k][t];
						pulseCalImAcc[k][t] += pulseCalIm[k][t];
					}
				}
				if(nAccum == 0)
				{
					accumStart = time;
				}
				accumEnd = time;
				nAccum++;
			}/*end while (line-by-line)*/
			if(!nDifxTone)
			{
				break;/*to next antenna*/
			}
		}/*end job loop*/
		if(in)
		{
			rewind(in);
		}


	}/*end antenna loop*/
	if(in)
	{
		fclose(in);
		in = 0;
	}
	if(in2)
	{
		fclose(in2);
		in2 =0;
	}

	free(fitsbuf);

	if(nDifxTone > 0)
	{	
		printf("\n");
	}

	return D;
}

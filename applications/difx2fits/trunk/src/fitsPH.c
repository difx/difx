/***************************************************************************
 *   Copyright (C) 2008-2011 by Walter Brisken @ John Morgan               *
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

//phase values from mpifxcorr currently have to be inverted to match FITS conversion

#warning FIXME: review once we've got pcal amplitudes sorted
const float pcaltiny = 1e-10;

#warning FIXME: even this could be too small!
const int MaxLineLength=10000;

int pulsecalIsZero(float pulseCal[2][array_MAX_TONES], int nBand, int nTone, int nPol)
{
	int p, b, t;
	
	for(p = 0; p < nPol; p++)
	{
		for(b = 0; b < nBand; b++)
		{
			for(t = 0; t < nBand; t++)
			{
				if(pulseCal[p][b*nTone +t] > pcaltiny || pulseCal[p][b*nTone +t] < -pcaltiny)
				{
					return 0;
				}
			}
		}
	}
	
	return 1;
}

int getDifxPcalFile(const DifxInput *D, int antId, int jobId, FILE **file)
{
	char filename[DIFXIO_FILENAME_LENGTH];
	const char suffix[] = "/PCAL_";
	if(*file)
	{
		fclose(*file);
		*file = 0;
	}

	sprintf(filename, "%s%s%s", D->job[jobId].outputFile,
			 suffix, D->antenna[antId].name);
	//printf("Opening File %s\n", filename);
	*file = fopen(filename, "r");
	if(!*file)
	{
		fprintf(stderr, "Error opening file : %s\n", filename);

		return -1;
	}
	printf("      Opened File %s\n", filename);
	
	return 0;
}
	
/* go through pcal file, determine maximum number of tones.*/
static int getNTone(const char *filename, double t1, double t2)
{
	FILE *in;
	char line[MaxLineLength+1];
	int n, nTone, maxnTone=0;
	double t;
	char *rv;
	char antName1[DIFXIO_NAME_LENGTH];

#warning FIXME: also check that nBand and nPol of each line in the file don't exceed D->nIF and D->nPol
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

/* The following function is for parsing a line of the 
 * station-extracted `pcal' file. */
static int parsePulseCal(const char *line, 
	int antId,
	int *sourceId,
	double *time, float *timeInt, double *cableCal,
	double freqs[2][array_MAX_TONES], 
	float pulseCalRe[2][array_MAX_TONES], 
	float pulseCalIm[2][array_MAX_TONES], 
	float stateCount[2][array_MAX_TONES], 
	int refDay, const DifxInput *D, int *configId, 
	int phasecentre)
{
	int np, nb, nt, ns;
	int nRecBand, recBand;
	int n, p, i, v;
	int freqId, polId, bandId, tone, state;
	int pol, band;
	int scanId;
	double A;
	float B, C;
	double mjd;
	char antName[DIFXIO_NAME_LENGTH];
	
	union
	{
		int32_t i32;
		float f;
	} nan;
	nan.i32 = -1;

	n = sscanf(line, "%s%lf%f%lf%d%d%d%d%d%n", antName, time, timeInt, 
		cableCal, &np, &nb, &nt, &ns, &nRecBand, &p);
	if(n != 9)
	{
		return -1;
	}
	line += p;

	if(*cableCal > 999.89 && *cableCal < 999.91)
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
	
	for(pol = 0; pol < 2; pol++)
	{
		for(i = 0; i < array_MAX_TONES; i++)
		{
			freqs[pol][i] = 0.0;
			pulseCalRe[pol][i] = 0.0;
			pulseCalIm[pol][i] = 0.0;
			stateCount[pol][i] = 0.0;
		}
	}

	*cableCal *= 1e-12;

	/* Read in pulse cal information */
	for(pol = 0; pol < np; pol++)
	{
		for(band = 0; band < nb; band++)
		{
			for(tone = 0; tone < nt; tone++)
			{
				n = sscanf(line, "%d%lf%f%f%n", 
					&recBand, &A, &B, &C, &p);
				if(n < 4)
				{
					return -4;
				}
				line += p;
				if(recBand < 0 || recBand >= nRecBand)
				{
					continue;
				}
				v = DifxConfigRecBand2FreqPol(D, *configId,
					antId, recBand, &freqId, &polId);
				if(v >= 0)
				{
					if(freqId < 0 || polId < 0)
					{
						fprintf(stderr, "Error: derived freqId and polId (%d,%d) are not legit.  From recBand=%d.\n",
							freqId, polId, recBand);
						continue;
					}
					else if(freqId >= D->nFreq)
					{
						fprintf(stderr, "parsePulseCal(1): Developer error: freqId=%d, nFreq=%d\n",
							freqId, D->nFreq);
						continue;
					}
					bandId = D->config[*configId].freqId2IF[freqId];
					if(bandId < 0)
					{
						/* This sub-band is not to go to the FITS file */
						continue;
					}
					freqs[polId][tone + bandId*nt] = A*1.0e6;
					if((B >= 999.89 && B < 999.91) ||
					   (C >= 999.89 && C < 999.91))
					{
					  pulseCalRe[polId][tone + bandId*nt] = nan.f;
					  pulseCalIm[polId][tone + bandId*nt] = nan.f;
					}
					else
					{
					  pulseCalRe[polId][tone + bandId*nt] = B*cos(C*M_PI/180.0);
					  pulseCalIm[polId][tone + bandId*nt] = B*sin(C*M_PI/180.0);
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
			for(band = 0; band < nb; band++)
			{
				n = sscanf(line, "%d%n", &recBand, &p);
				line += p;
				v = DifxConfigRecBand2FreqPol(D, *configId,
					antId, recBand, &freqId, &polId);
				for(state = 0; state < 4; state++)
				{
					if(freqId < 0 || polId < 0)
					{
						fprintf(stderr, "parsePulseCal(2): Developer error: derived "
							"freqId and polId (%d,%d) are not legit.  From recBand=%d.\n",
							freqId, polId, recBand);
						
						continue;
					}
					else if(freqId >= D->nFreq)
					{
						fprintf(stderr, "parsePulseCal(2): Developer error: freqId=%d, nFreq=%d\n",
							freqId, D->nFreq);

						continue;
					}
					bandId = D->config[*configId].freqId2IF[freqId];
					if(bandId < 0)
					{
						/* This sub-band is not to go to the FITS file */
						continue;
					}
					for(state = 0; state < 4; state++)
					{
						if(state < ns)
						{
							n = sscanf(line, "%f%n", &B, &p);
							if(n < 1)
							{
								return -5;
							}
							line += p;
						}
						else
						{
							B = 0.0;
						}
						stateCount[polId][state + bandId*4] = B;
					}
				}
			}
		}
	}

	return 0;
}
/* The following function is for parsing a line of the 
 * station-extracted `pcal' file and extracting only the cable cal
 * value */
static int parsePulseCalCableCal(const char *line, 
	int antId,
	int *sourceId,
	double *time, float *timeInt, double *cableCal,
	int refDay, const DifxInput *D, int *configId, 
	int phasecentre)
{
	int n, p;
	int scanId;
	double mjd;
	char antName[DIFXIO_NAME_LENGTH];

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

	if(*cableCal > 999.89 && *cableCal < 999.91)
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
	
	*cableCal *= 1e-12;

	return 0;
}

/* The following function is for parsing a line of the files containing 
 * The DiFX-extracted pulse cals */
static int parseDifxPulseCal(const char *line, 
	int dsId, int nBand, int nTone,
	int *sourceId, double *time,
	double freqs[2][array_MAX_TONES], 
	float pulseCalRe[2][array_MAX_TONES], 
	float pulseCalIm[2][array_MAX_TONES], 
	float stateCount[2][array_MAX_TONES],
	float pulseCalRate[2][array_MAX_TONES],
	int refDay, const DifxInput *D, int *configId, 
	int phasecentre)
{
	int np, nb, nt, ns;
	int nRecBand, recBand;
	int n, p, i, j, k;
	int tone;
	int pol, band;
	int scanId;
	double A;
	float B, C;
	double mjd;
	char antName[DIFXIO_NAME_LENGTH];
	double cableCal;
	float timeInt;

	union
	{
		int32_t i32;
		float f;
	} nan;
	nan.i32 = -1;

	n = sscanf(line, "%s%lf%f%lf%d%d%d%d%d%n", antName, time, &timeInt, 
		&cableCal, &np, &nb, &nt, &ns, &nRecBand, &p);
	if(n != 9)
	{
		fprintf(stderr, "Error: parseDifxPulseCal: header information not parsable (n=%d)\n", n);

		return -1;
	}
	line += p;

	*time -= refDay;
	mjd = *time + (int)(D->mjdStart);

	if(mjd < D->mjdStart || mjd > D->mjdStop)
	{
		return -1;
	}

	scanId = DifxInputGetScanIdByAntennaId(D, mjd, D->datastream[dsId].antennaId);
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
	
	for(pol = 0; pol < 2; pol++)
	{
		for(i = 0; i < nBand*nTone; i++)
		{
#warning FIXME: should these be set to nan instead?
			freqs[pol][i] = 0.0;
			pulseCalRe[pol][i] = 0.0;
			pulseCalIm[pol][i] = 0.0;
			pulseCalRate[pol][i] = 0.0;
		}
		for(i = 0; i < nBand*4; i++)
		{
			stateCount[pol][i] = 0.0;
		}
	}

	/* Read in pulse cal information */
	for(pol = 0; pol < D->nPol; pol++)
	{
#warning FIXME: double-check there's no possibility of pols getting swapped
		j = 0; /* band index within freqs, pulseCalRe and pulseCalIm */
		for(band = 0; band < D->datastream[dsId].nRecFreq; band++)
		{
			if(j >= nBand)
			{
				printf("Warning: parseDifxPulseCal: trying to read too many bands in pol %d band %d\n", pol, band);
				
				break;
			}
			k = 0; /* tone index within freqs, pulseCalRe and pulseCalIm */
			/* set up pcal information for this band */
			DifxDatastreamCalculatePhasecalTones(&(D->datastream[dsId]), &(D->freq[D->datastream[dsId].recFreqId[band]]));
			for(tone = 0; tone < D->datastream[dsId].nRecTone; tone++)
			{

				n = sscanf(line, "%d%lf%f%f%n", 
					&recBand, &A, &B, &C, &p);
				if(n < 4)
				{
					printf("Warning: parseDifxPulseCal: Error scanning line\n");
					
					return -4;
				}
				line += p;
				/* Only write out specified frequencies */
				if(D->datastream[dsId].recToneOut[tone] == 0)
				{
					continue;
				}

				if(k >= nTone)
				{
					printf("Warning: parseDifxPulseCal: trying to extract too many tones in pol %d band %d\n", pol, band);
					
					break;
				}
				
				freqs[pol][j*nTone + k] = (double) D->datastream[dsId].recToneFreq[tone]*1.0e6;
				if(B < pcaltiny && B > -pcaltiny && C < pcaltiny && C > -pcaltiny)
				{
					pulseCalRe[pol][j*nTone + k] = nan.f;
					pulseCalIm[pol][j*nTone + k] = nan.f;
				}
				else
				{
					pulseCalRe[pol][j*nTone + k] = B;
					pulseCalIm[pol][j*nTone + k] = C;
				}
				k++;
			}
			while(k < nTone)
			{
				pulseCalRe[pol][j*nTone + k] = nan.f;
				pulseCalIm[pol][j*nTone + k] = nan.f;
				k++;
			}
			j++;
		}
		/* n.b. unused bands at the end will be set to zero rather than NaN */
	}

	return 0;
}

const DifxInput *DifxInput2FitsPH(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out,
	int phasecentre)
{
	char stateFormFloat[4];
	char toneFormDouble[4];
	char toneFormFloat[4];
	
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
	double time;
	float timeInt;
	double cableCal, cableCalOut;
	double cableTime;
	float cableTimeInt;
	double freqs[2][array_MAX_TONES];
	float pulseCalRe[2][array_MAX_TONES];
	float pulseCalIm[2][array_MAX_TONES];
	float stateCount[2][array_MAX_TONES];
	float pulseCalRate[2][array_MAX_TONES];
	int configId;
	int antId, sourceId;
	int refDay;
	int i, a, dsId, j, k, n, v;
	double start, stop;
	FILE *in=0;
	FILE *in2=0;
	char *rv;
	/* The following are 1-based indices for FITS format */
	int32_t antId1, arrayId1, sourceId1, freqId1;

	char antName[DIFXIO_NAME_LENGTH];

	/* Note: this particular form of NAN is needed for FITS-IDI compliance */
	union
	{
		int32_t i32;
		float f;
	} nan;
	nan.i32 = -1;

	if(D == 0)
	{
		return D;
	}

	printf("\n");
	nBand = D->nIF;
	nPol = D->nPol;

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
		in = 0;
		printf("    Station pcal file not found. No station pcal or cable cal measurements available\n");
	}


	nDifxTone = DifxInputGetMaxTones(D);
	if(nDifxTone == 0)
	{
		printf("    No Tones extracted by DiFX\n");
	}
	else
	{
		printf("    DiFX-extracted tones available\n");
	}

	if(nDifxTone > nTone)
	{
		nTone = nDifxTone;
	}

	if(nTone*nBand > array_MAX_TONES)
	{
		printf("Developer Error: nTone*nBand exceeds array_MAX_TONES\n");

		return D;
	}
	if(nTone < 1)
	{
		return D;
	}

	if(nTone < 2)
	{
		printf("Warning: nTone <2 \n");
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

		exit(0);
	}

	fitsWriteBinTable(out, nColumn, columns, nRowBytes, "PHASE-CAL");
	arrayWriteKeys (p_fits_keys, out);
	fitsWriteInteger(out, "NO_POL", nPol, "");
	fitsWriteInteger(out, "NO_TONES", nTone, "");
	fitsWriteInteger(out, "TABREV", 2, "");
	fitsWriteEnd(out);

	antId = 0;
	arrayId1 = 1;

	for(a = 0; a < D->nAntenna; a++)
	{
		printf("    Processing %s\n", D->antenna[a].name);

		for(j = 0; j < D->nJob; j++)
		{
			dsId = DifxInputGetDatastreamId(D, j, a);
			if(dsId < 0)
			{
				/*antenna not present in this job*/
				continue;/*to next job*/
			}

			if(D->datastream[dsId].phaseCalIntervalMHz > 0)
			{
				v = getDifxPcalFile(D, a, j, &in2);
				if(v != 0)
				{
					/*Error message given in getDifxPcalFile*/
					continue;/*to next job*/
				}
			}
			else
			{
				/*Don't mix DiFX and station pcals*/
				if(nDifxTone)
				{
					printf("Warning: no pcals for Antenna %s in JobId %d\n", D->antenna[a].name, j);
					continue;/*to next job*/
				}
			}

			/* set defaults */
			cableCal = 0.0;
			cableTime = 0.0;
			cableTimeInt = -1.0;

			/*rewind(in) below this loop*/
			while(1)
			{
				if(in && !nDifxTone)/*try reading pcal file*/
				{
					rv = fgets(line, MaxLineLength, in);
					if(!rv)
					{
						break;/*to next job*/
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
						v = parsePulseCal(line, a, &sourceId, &time, &timeInt, 
							&cableCal, freqs, pulseCalRe, pulseCalIm,
							stateCount, refDay, D, &configId, phasecentre);
						if(v < 0)
						{
							continue;/*to next line in file*/
						}
					}
				}
				else/*reading difx-extracted pcals*/
				{	
					rv = fgets(line, MaxLineLength, in2);
					if(!rv)
					{
						break;/*to next job*/
					}
						
					/* ignore possible comment lines */
					if(line[0] == '#')
					{
						continue;/*to next line in file*/
					}
					else 
					{
						v = parseDifxPulseCal(line, dsId, nBand, nTone, &sourceId, &time,
								      freqs, pulseCalRe, pulseCalIm, stateCount, pulseCalRate,
								      refDay, D, &configId, phasecentre);
						if(v < 0)
						{
							continue;/*to next line in file*/
						}
						if(pulsecalIsZero(pulseCalRe, nBand, nTone, nPol) &&
						   pulsecalIsZero(pulseCalIm, nBand, nTone, nPol))
						{
							continue;/*to next line in file*/
						}
						timeInt = D->config[configId].tInt/86400;
					}

					/*get cable cal from pcal file */
					if(in)
					{
						while(cableTimeInt < 0 || cableTime + cableTimeInt / 2 < time)
						{
							rv = fgets(line, MaxLineLength, in);
							if(!rv)
							{
								break;/*to out of pcal search*/
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
								v = parsePulseCalCableCal(line, a, &sourceId, &cableTime, &cableTimeInt, 
									&cableCalOut, refDay, D, &configId, phasecentre);
								//printf("\n%f %f %e", cableTime, cableTimeInt, cableCalOut);
								if(v < 0)
								{
									continue;/*to next line in file*/
								}
							}
						}
						if(cableTimeInt > 0 && cableTime - cableTimeInt/2 < time && cableTime + cableTimeInt/2 > time)
						{
							cableCal = cableCalOut;
						}
						else
						{
							cableCal = nan.f;
						}
					}
					else
					{
						cableCal = 0.0;
					}

				}

				freqId1 = D->config[configId].fitsFreqId + 1;
				sourceId1 = D->source[sourceId].fitsSourceIds[configId] + 1;
				antId1 = a + 1;

				p_fitsbuf = fitsbuf;
			
				FITS_WRITE_ITEM (time, p_fitsbuf);
				FITS_WRITE_ITEM (timeInt, p_fitsbuf);
				FITS_WRITE_ITEM (sourceId1, p_fitsbuf);
				FITS_WRITE_ITEM (antId1, p_fitsbuf);
				FITS_WRITE_ITEM (arrayId1, p_fitsbuf);
				FITS_WRITE_ITEM (freqId1, p_fitsbuf);
				FITS_WRITE_ITEM (cableCal, p_fitsbuf);

				for(k = 0; k < nPol; k++)
				{
					FITS_WRITE_ARRAY(stateCount[k], p_fitsbuf,
						4*nBand);
					if(nTone > 0)
					{
						FITS_WRITE_ARRAY(freqs[k],
							p_fitsbuf, nTone*nBand);
						FITS_WRITE_ARRAY(pulseCalRe[k], 
							p_fitsbuf, nTone*nBand);
						FITS_WRITE_ARRAY(pulseCalIm[k], 
							p_fitsbuf, nTone*nBand);
						FITS_WRITE_ARRAY(pulseCalRate[k], 
							p_fitsbuf, nTone*nBand);
					}
				}

				testFitsBufBytes(p_fitsbuf - fitsbuf, nRowBytes, "PH");
				
#ifndef WORDS_BIGENDIAN
				FitsBinRowByteSwap(columns, nColumn, fitsbuf);
#endif
				fitsWriteBinRow(out, fitsbuf);
				//printf("Entry Written\n");
			}/*end while*/
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
	}

	free(fitsbuf);

	return D;
}

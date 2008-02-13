#include <math.h>
#include <stdlib.h>
#include <sys/types.h>
#include <strings.h>
#include "config.h"
#include "difx2fits.h"
#include "other.h"


/* go through pcal file, determine maximum number of tones.
 */
static int getNTone(const char *filename, double t1, double t2)
{
	FILE *in;
	char line[1000];
	int n, nTone, maxnTone=0;
	double t;

	in = fopen(filename, "r");
	if(!in)
	{
		return -1;
	}
	
	for(;;)
	{
		fgets(line, 999, in);
		if(feof(in))
		{
			break;
		}
		n = sscanf(line, "%*s%lf%*f%*f%*d%*d%d", &t, &nTone);
		if(n != 2)
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

static int parsePulseCal(const char *line, 
	int *antId, double *time, float *timeInt, double *cableCal,
	double freqs[2][array_MAX_TONES], 
	float pulseCalRe[2][array_MAX_TONES], 
	float pulseCalIm[2][array_MAX_TONES], 
	float stateCount[2][array_MAX_TONES], 
	int refDay, const DifxInput *D, int *configId)
{
	int np, nb, nt, ns;
	int nRecChan, recChan;
	int n, p, i, v;
	int polId, bandId, tone, state;
	int pol, band;
	int sourceId;
	double A;
	const char *L = line;
	float B, C;
	double mjd;
	char antName[20];

	n = sscanf(line, "%s%lf%f%lf%d%d%d%d%d%n", antName, time, timeInt, 
		cableCal, &np, &nb, &nt, &ns, &nRecChan, &p);
	if(n != 9)
	{
		return -1;
	}
	line += p;

	*time -= refDay;
	mjd = *time + (int)(D->mjdStart);

	if(mjd < D->mjdStart || mjd > D->mjdStart + D->duration/86400.0)
	{
		return -1;
	}

	sourceId = DifxInputGetSourceId(D, mjd);
	if(sourceId < 0)	/* not in scan */
	{
		return -2;
	}
	*configId = D->source[sourceId].configId;
	nRecChan = D->config[*configId].nRecChan;
	
	*antId = DifxInputGetAntennaId(D, antName);
	if(*antId < 0)
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
					&recChan, &A, &B, &C, &p);
				if(n < 4)
				{
					return -4;
				}
				line += p;
				if(recChan < 0 || recChan >= nRecChan)
				{
					continue;
				}
				v = DifxConfigRecChan2IFPol(D, *configId,
					*antId, recChan, &bandId, &polId);
				if(v >= 0)
				{
					if(bandId < 0 || polId < 0)
					{
						fprintf(stderr, "Error: derived "
							"bandId and polId (%d,%d) are "
							"not legit.  From "
							"recChan=%d.\n",
							bandId, polId, recChan);
						continue;
					}
					freqs[polId][tone + bandId*nt] = A*1.0e6;
					pulseCalRe[polId][tone + bandId*nt] = 
						B*cos(C*M_PI/180.0);
					pulseCalIm[polId][tone + bandId*nt] = 
						B*sin(C*M_PI/180.0);
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
				n = sscanf(line, "%d%n", &recChan, &p);
				line += p;
				v = DifxConfigRecChan2IFPol(D, *configId,
					*antId, recChan, &bandId, &polId);
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

	return 0;
}

const DifxInput *DifxInput2FitsPH(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out)
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
	char line[1000];
	int nBand, nTone, nPol;
	double time;
	float timeInt;
	double cableCal;
	double freqs[2][array_MAX_TONES];
	float pulseCalRe[2][array_MAX_TONES];
	float pulseCalIm[2][array_MAX_TONES];
	float stateCount[2][array_MAX_TONES];
	float pulseCalRate[2][array_MAX_TONES];
	int configId;
	int antId;
	int refDay;
	int i, v;
	double f;
	FILE *in;
	/* The following are 1-based indices for FITS format */
	int32_t antId1, arrayId1, sourceId1, freqId1;
	
	if(D == 0)
	{
		return D;
	}

	nBand = D->nIF;
	nPol = D->nPol;

	mjd2dayno((int)(D->mjdStart), &refDay);

	/* get the maximum dimensions possibly needed */
	f = D->mjdStart - (int)(D->mjdStart);
	nTone = getNTone("pcal", refDay+f, refDay+f+D->duration/86400.0);

	if(nTone < 0)
	{
		return D;
	}

	in = fopen("pcal", "r");
	if(!in)
	{
		return D;
	}
	
	fgets(line, 999, in);

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
		return 0;
	}


	fitsWriteBinTable(out, nColumn, columns, nRowBytes, "PHASE-CAL");
	arrayWriteKeys (p_fits_keys, out);
	fitsWriteInteger(out, "NO_POL", nPol, "");
	fitsWriteInteger(out, "NO_TONES", nTone, "");
	fitsWriteInteger(out, "TABREV", 2, "");
	fitsWriteEnd(out);

	/* set defaults */
	for(i = 0; i < array_MAX_TONES; i++)
	{
		pulseCalRate[0][i] = 0.0;
		pulseCalRate[1][i] = 0.0;
	}
	antId = 0;
	arrayId1 = 1;

	for(;;)
	{
		fgets(line, 999, in);
		if(feof(in))
		{
			break;
		}
			
		/* ignore possible comment lines */
		if(line[0] == '#')
		{
			continue;
		}
		else 
		{
			v = parsePulseCal(line, &antId, &time, &timeInt, 
				&cableCal, freqs, pulseCalRe, pulseCalIm,
				stateCount, refDay, D, &configId);
			if(v < 0)
			{
				continue;
			}

			freqId1 = D->config[configId].freqId + 1;
			antId1 = antId + 1;

			p_fitsbuf = fitsbuf;
		
			FITS_WRITE_ITEM (time, p_fitsbuf);
			FITS_WRITE_ITEM (timeInt, p_fitsbuf);
			FITS_WRITE_ITEM (sourceId1, p_fitsbuf);
			FITS_WRITE_ITEM (antId1, p_fitsbuf);
			FITS_WRITE_ITEM (arrayId1, p_fitsbuf);
			FITS_WRITE_ITEM (freqId1, p_fitsbuf);
			FITS_WRITE_ITEM (cableCal, p_fitsbuf);

			for(i = 0; i < nPol; i++)
			{
				FITS_WRITE_ARRAY(stateCount[i], p_fitsbuf,
					4*nBand);
				FITS_WRITE_ARRAY(freqs[i], p_fitsbuf,
					nTone*nBand);
				FITS_WRITE_ARRAY(pulseCalRe[i], p_fitsbuf,
					nTone*nBand);
				FITS_WRITE_ARRAY(pulseCalIm[i], p_fitsbuf,
					nTone*nBand);
				FITS_WRITE_ARRAY(pulseCalRate[i], p_fitsbuf,
					nTone*nBand);
			}

			testFitsBufBytes(p_fitsbuf - fitsbuf, nRowBytes, "PH");
			
#ifndef WORDS_BIGENDIAN
			FitsBinRowByteSwap(columns, nColumn, fitsbuf);
#endif
			fitsWriteBinRow(out, fitsbuf);
		}
	}

	/* close the file, free memory, and return */
	fclose(in);
	free(fitsbuf);

	return D;
}

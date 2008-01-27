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
	int n, nt, nTone=0;
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
		n = sscanf(line, "%*s%lf%*f%*f%*d%*d%d", &t, &nt);
		if(n != 2)
		{
			continue;
		}
		if(t >= t1 && t <= t2)
		{
			if(nt > nTone)
			{
				nTone = nt;
			}
		}
		
	}
	fclose(in);

	return nTone;
}

static int parsePulseCal(const char *line, 
	int *antId, double *t, float *dt, double *ccal,
	double freqs[2][64], float pcalR[2][64], float pcalI[2][64], 
	float states[2][64], const DifxInput *D, int configId)
{
	int np, nb, nt, ns;
	int nRecChan, recChan;
	int n, p, i, v;
	int polId, bandId, tone, state;
	int pol, band;
	double A;
	float B, C;
	char antName[20];

	n = sscanf(line, "%s%lf%f%lf%d%d%d%d%d%n", antName, t, dt, ccal, 
		&np, &nb, &nt, &ns, &nRecChan, &p);
	if(n != 9)
	{
		return -1;
	}
	line += p;
	
	*antId = DifxInputGetAntennaId(D, antName);
	if(*antId < 0)
	{
		return -2;
	}

	for(pol = 0; pol < 2; pol++)
	{
		for(i = 0; i < 64; i++)
		{
			freqs[pol][i] = 0.0;
			pcalR[pol][i] = 0.0;
			pcalI[pol][i] = 0.0;
			states[pol][i] = 0.0;
		}
	}

	*ccal *= 1e-12;

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
					return -3;
				}
				line += p;
				if(recChan < 0)
				{
					continue;
				}
				v = DifxConfigRecChan2IFPol(D, configId,
					*antId, recChan, &bandId, &polId);
				if(bandId < 0 || polId < 0)
				{
					fprintf(stderr, "Error: derived "
						"bandId and polId (%d,%d) are "
						"not legit.  From "
						"recChan=%d.\n",
						bandId, polId, recChan);
				}
				if(v < 0)
				{
					continue;
				}
				freqs[polId][tone + bandId*nt] = A*1.0e6;
				pcalR[polId][tone + bandId*nt] = 
					B*cos(C*M_PI/180.0);
				pcalI[polId][tone + bandId*nt] = 
					B*sin(C*M_PI/180.0);
			}
		}
	}
	
	/* Read in state count information */
	for(pol = 0; pol < np; pol++)
	{
		for(band = 0; band < nb; band++)
		{
			n = sscanf(line, "%d%n", &recChan, &p);
			line += p;
			v = DifxConfigRecChan2IFPol(D, configId,
				*antId, recChan, &bandId, &polId);
			for(state = 0; state < 4; state++)
			{
				if(state < ns)
				{
					n = sscanf(line, "%f%n", &B, &p);
					if(n < 1)
					{
						return -4;
					}
					line += p;
				}
				else
				{
					B = 0.0;
				}
				states[polId][state + bandId*4] = B;
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
	int n_row_bytes;
	char *fitsbuf, *p_fitsbuf;
	char line[1000];
	int no_band, no_tone, no_pol;
	double t;
	float dt;
	double ccal;
	double freqs[2][64];
	double mjd, mjdStop;
	float pcalR[2][64], pcalI[2][64];
	float states[2][64];
	float pcalRate[64];
	int antId, arrayId, sourceId, freqId;
	int configId = 0;	/* fix to zero for now */
	int FITSantId = 0;
	int refday;
	int i, v;
	double f;
	FILE *in;
	
	no_band = p_fits_keys->no_band;
	no_pol = D->nPol;

	if(D == 0)
	{
		return D;
	}

	mjd2dayno((int)(D->mjdStart), &refday);
	mjdStop = D->mjdStart + D->duration/86400.0;

	/* get the maximum dimensions possibly needed */
	f = D->mjdStart - (int)(D->mjdStart);
	no_tone = getNTone("pcal", refday+f, refday+f+D->duration/86400.0);

	if(no_tone < 0)
	{
		return D;
	}

	in = fopen("pcal", "r");
	
	fgets(line, 999, in);

	sprintf(stateFormFloat, "%dE", 4*no_band);
	sprintf(toneFormFloat,  "%dE", no_tone*no_band);
	sprintf(toneFormDouble, "%dD", no_tone*no_band);
	
	if(no_pol == 2)
	{
		nColumn = NELEMENTS(columns);
	}
	else
	{
		nColumn = NELEMENTS(columns) - 5;
	}
	
	n_row_bytes = FitsBinTableSize(columns, nColumn);

	/* calloc space for storing table in FITS format */
	if((fitsbuf = (char *)calloc(n_row_bytes, 1)) == 0)
	{
		return 0;
	}


	fitsWriteBinTable(out, nColumn, columns, n_row_bytes, "PHASE-CAL");
	arrayWriteKeys (p_fits_keys, out);
	fitsWriteInteger(out, "NO_POL", no_pol, "");
	fitsWriteInteger(out, "NO_TONES", no_tone, "");
	fitsWriteInteger(out, "TABREV", 2, "");
	fitsWriteEnd(out);

	/* set defaults */
	for(i = 0; i < 64; i++)
	{
		pcalRate[i] = 0;
	}
	arrayId = 1;
	freqId = 1;

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
			v = parsePulseCal(line, 
				&antId, &t, &dt, &ccal, freqs, pcalR, pcalI,
				states, D, configId);
			if(v < 0)
			{
				continue;
			}

			t -= refday;
			mjd = t + (int)(D->mjdStart);
			if(mjd < D->mjdStart || mjd > mjdStop)
			{
				continue;
			}

			if(antId < 0)
			{
				continue;
			}

			sourceId = DifxInputGetSourceId(D, mjd) + 1;

			FITSantId = antId + 1;

			p_fitsbuf = fitsbuf;
		
			/* TIME */
			bcopy((char *)&t, p_fitsbuf, sizeof(t));
			p_fitsbuf += sizeof(t);
		
			/* TIME INTERVAL */
			bcopy((char *)&dt, p_fitsbuf, sizeof(dt));
			p_fitsbuf += sizeof(dt);
		
			/* SOURCE_ID */
			bcopy((char *)&sourceId, p_fitsbuf, sizeof(sourceId));
			p_fitsbuf += sizeof(sourceId);

			/* ANTENNAS */
			bcopy((char *)&FITSantId, p_fitsbuf, sizeof(FITSantId));
			p_fitsbuf += sizeof(FITSantId);

			/* ARRAY */
			bcopy((char *)&arrayId, p_fitsbuf, sizeof(arrayId));
			p_fitsbuf += sizeof(arrayId);

			/* FREQ_ID */
			bcopy((char *)&freqId, p_fitsbuf, sizeof(freqId));
			p_fitsbuf += sizeof(freqId);

			/* CABLE_CAL */
			bcopy((char *)&ccal, p_fitsbuf, sizeof(ccal));
			p_fitsbuf += sizeof(ccal);

			for(i = 0; i < no_pol; i++)
			{
				/* STATE COUNTS */
				bcopy((char *)states[i], p_fitsbuf, 
					4*no_band*sizeof(float));
				p_fitsbuf += 4*no_band*sizeof(float);

				/* PCAL FREQ */
				bcopy((char *)freqs[i], p_fitsbuf,
					no_tone*no_band*sizeof(double));
				p_fitsbuf += no_tone*no_band*sizeof(double);

				/* PCAL REAL */
				bcopy((char *)pcalR[i], p_fitsbuf,
					no_tone*no_band*sizeof(float));
				p_fitsbuf += no_tone*no_band*sizeof(float);

				/* PCAL IMAG */
				bcopy((char *)pcalI[i], p_fitsbuf,
					no_tone*no_band*sizeof(float));
				p_fitsbuf += no_tone*no_band*sizeof(float);

				/* PCAL RATE */
				bcopy((char *)pcalRate, p_fitsbuf,
					no_tone*no_band*sizeof(float));
				p_fitsbuf += no_tone*no_band*sizeof(float);
			}
			
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

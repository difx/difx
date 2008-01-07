#include <math.h>
#include <stdlib.h>
#include <sys/types.h>
#include <strings.h>
#include "config.h"
#include "difx2fits.h"
#include "other.h"

/* go through pcal file, determine maximum shape of pcal info needed for our
 * time range.
 */
int static getShape(const char *filename, double t1, double t2,
		int *no_pol, int *no_band, int *no_tone)
{
	FILE *in;
	char line[1000];
	int n, np, nb, nt;
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
		n = sscanf(line, "%*s%lf%*f%*f%d%d%d", &t, &np, &nb, &nt);
		if(n != 4)
		{
			continue;
		}
		if(t >= t1 && t <= t2)
		{
			if(np > *no_pol)
			{
				*no_pol = np;
			}
			if(nb > *no_band)
			{
				*no_band = nb;
			}
			if(nt > *no_tone)
			{
				*no_tone = nt;
			}
		}
		
	}
	fclose(in);

	return 0;
}

static int parsePulseCal(const char *line, 
	int no_pol, int no_band, int no_tone, 
	char *antName, double *t, float *dt, double *ccal,
	double freqs[][64], float pcalR[][64], float pcalI[][64], 
	float states[][64])
{
	int np, nb, nt, ns;
	int n, p;
	int pol, band, tone, state;
	double A;
	float B, C;

	n = sscanf(line, "%s%lf%f%lf%d%d%d%d%n", antName, t, dt, ccal, 
		&np, &nb, &nt, &ns, &p);
	if(n != 8)
	{
		return -1;
	}

	*ccal *= 1e-12;

	line += p;
	
	if(np > no_pol || nb > no_band || nt > no_tone)
	{
		return -2;
	}

	for(n = 0; n < no_tone*no_band; n++)
	{
		freqs[0][n] = freqs[1][n] = 0.0;
		pcalR[0][n] = pcalR[1][n] = 0.0;
		pcalI[0][n] = pcalI[1][n] = 0.0;
	}

	/* Read in pulse cal information */
	for(pol = 0; pol < np; pol++)
	{
		for(band = 0; band < nb; band++)
		{
			for(tone = 0; tone < nt; tone++)
			{
				n = sscanf(line, "%lf%f%f%n", &A, &B, &C, &p);
				if(n < 3)
				{
					return -1;
				}
				freqs[pol][tone + band*no_tone] = A;
				pcalR[pol][tone + band*no_tone] = B*cos(C*M_PI/180.0);
				pcalI[pol][tone + band*no_tone] = B*sin(C*M_PI/180.0);
				line += p;
			}
		}
	}
	
	/* Read in state count information */
	for(pol = 0; pol < no_pol; pol++)
	{
		for(band = 0; band < no_band; band++)
		{
			for(state = 0; state < 4; state++)
			{
				if(state < ns)
				{
					n = sscanf(line, "%f%n", &B, &p);
					if(n < 1)
					{
						return -1;
					}
				}
				else
				{
					B = 0.0;
				}
				states[pol][state + band*4] = B;
				line += p;
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
	char antName[20];
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
	int refday;
	int i, v;
	double f;
	FILE *in;
	
	no_band = p_fits_keys->no_band;
	no_pol = 1;
	no_tone = 0;

	if(D == 0)
	{
		return D;
	}

	mjd2dayno((int)(D->mjdStart), &refday);
	mjdStop = D->mjdStart + D->duration/86400.0;

	/* get the maximum dimensions possibly needed */
	f = D->mjdStart - (int)(D->mjdStart);
	v = getShape("pcal", refday+f, refday+f+D->duration/86400.0,
		&no_pol, &no_band, &no_tone);

	if(v < 0)
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
	arrayId = 0;
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
			v = parsePulseCal(line, no_pol, no_band, no_tone, 
				antName, &t, &dt, &ccal, freqs, pcalR, pcalI,
				states);

			if(v < 0)
			{
				continue;
			}

			antId = DifxInputGetAntennaId(D, antName) + 1;
			t -= refday;
			mjd = t + (int)(D->mjdStart);

			if(antId <= 0)
			{
				continue;
			}

			if(mjd < D->mjdStart || mjd > mjdStop)
			{
				continue;
			}

			sourceId = DifxInputGetSourceId(D, mjd) + 1;

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

			/* ARRAY */
			bcopy((char *)&arrayId, p_fitsbuf, sizeof(arrayId));
			p_fitsbuf += sizeof(arrayId);

			/* ANTENNAS */
			bcopy((char *)&antId, p_fitsbuf, sizeof(antId));
			p_fitsbuf += sizeof(antId);

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

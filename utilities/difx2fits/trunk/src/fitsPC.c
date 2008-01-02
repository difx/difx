#include <math.h>
#include <stdlib.h>
#include <sys/types.h>
#include <strings.h>
#include "difx2fits.h"
#include "byteorder.h"
#include "other.h"

static int parsePulseCal(const char *line, 
	int no_pol, int no_band, int no_tone, 
	char *antName, double *t, float *dt, double *ccal,
	double freqs[][16], float pcalR[][16], float pcalI[][16])
{
	int n, p;
	int pol, band, tone;
	double A;
	float B, C;

	n = sscanf("%s%lf%f%lf%n", antName, &t, &dt, &ccal, &p);
	if(n != 4)
	{
		return -1;
	}
	line += p;
	
	for(pol = 0; pol < no_pol; pol++)
	{
		for(band = 0; band < no_band; band++)
		{
			for(tone = 0; tone < no_tone; tone++)
			{
				n = sscanf(line, "%lf%f%f%n", &A, &B, &C, &p);
				if(n < 3)
				{
					return -1;
				}
				freqs[pol][tone + band*no_tone] = A;
				pcalR[pol][tone + band*no_tone] = B;
				pcalI[pol][tone + band*no_tone] = C;
				line += p;
			}
		}
	}

	return 0;
}

const DifxInput *DifxInput2FitsPC(const DifxInput *D,
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
		{"SOURCE_ID", "1J", "source id number from source tbl"},
		{"ANTENNA_NO", "1J", "antenna id from array geom. tbl"},
		{"ARRAY", "1J", "????"},
		{"FREQID", "1J", "freq id from frequency tbl"},
		{"CABLE_CAL", "1D", "cable length calibration", "SECONDS"},
		{"STATE_1", stateFormFloat, "state counts (4 per baseband)"},
		{"PC_FREQ_1", toneFormDouble, "Pcal recorded frequency", "Hz"},
		{"PC_REAL_1", toneFormFloat, "Pcal real"},
		{"PC_IMAG_1", toneFormFloat, "Pcal imag"},
		{"PC_RATE_1", toneFormFloat, "Pcal rate"},
		{"STATE_2", stateFormFloat, "state counts (4 per baseband)"},
		{"PC_FREQ_2", toneFormDouble, "Pcal recorded frequency", "Hz"},
		{"PC_REAL_2", toneFormFloat, "Pcal real"},
		{"PC_IMAG_2", toneFormFloat, "Pcal imag"},
		{"PC_RATE_2", toneFormFloat, "Pcal rate"}
	};

	int nColumn;
	int n_row_bytes, irow;
	char *fitsbuf, *p_fitsbuf;
	int swap;
	char line[1000];
	char antName[20];
	int no_band, no_tone, no_pol;
	int nb;
	double t;
	float dt;
	double ccal;
	double freqs[2][16];
	float pcalR[2][16], pcalI[2][16];
	float states[64];
	float pcalRate[16];
	int antId, arrayId, sourceId, freqId;
	int refday;
	int i;
	FILE *in;
	
	no_band = p_fits_keys->no_band;
	
	in = fopen("pcal", "r");
	
	if(!in || D == 0)
	{
		return D;
	}

	fgets(line, 999, in);
	if(line[0] != '#' || feof(in))
	{
		fprintf(stderr, "Error parsing pcal file\n");
		fclose(in);
		return D;
	}
	
	if(sscanf(line, "%*s%d%d%d", &no_tone, &nb, &no_pol) != 3)
	{
		fprintf(stderr, "Error getting geometery of pcal\n");
		fclose(in);
		return D;
	}
	if(no_band != nb)
	{
		fprintf(stderr, "number of IFs disagrees %d != %d\n",
			no_band, nb);
		fclose(in);
		return D;
	}

	sprintf(stateFormFloat, "%dE", 4*no_band);
	sprintf(toneFormFloat,  "%dE", no_tone*no_band);
	sprintf(toneFormDouble, "%dD", no_tone*no_band);
	
	swap = (byteorder() == BO_LITTLE_ENDIAN);

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
	if ((fitsbuf = (char *)calloc (n_row_bytes, 1)) == 0)
	{
		return 0;
	}

	mjd2dayno((int)(D->mjdStart), &refday);

	fitsWriteBinTable(out, nColumn, columns, n_row_bytes, "PHASE-CAL");

	arrayWriteKeys (p_fits_keys, out);
	fitsWriteInteger(out, "NO_POL", no_pol, "");
	fitsWriteInteger(out, "NO_TONES", no_tone, "");
	fitsWriteInteger(out, "TABREV", 2, "");
	fitsWriteEnd(out);

	/* set defaults */
	for(i = 0; i < 64; i++)
	{
		states[i] = 0;
	}
	for(i = 0; i < 16; i++)
	{
		pcalRate[i] = 0;
	}
	arrayId = 0;
	freqId = 0;
	
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
			parsePulseCal(line, no_pol, no_band, no_tone, 
				antName, &t, &dt, &ccal, freqs, pcalR, pcalI);

			ccal *= 1e-12;
			
			antId = DifxInputGetAntennaId(D, antName) + 1;
			
			if(antId <= 0)
			{
				continue;
			}

			sourceId = DifxInputGetSourceId(D,
				t-refday+(int)(D->mjdStart) ) + 1;

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
			bcopy((char *)antId, p_fitsbuf, sizeof(antId));
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
				bcopy((char *)states, p_fitsbuf, 
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
			
			if(swap)
			{
				FitsBinRowByteSwap(columns, nColumn, &fitsbuf);
			}
			fitsWriteBinRow(out, fitsbuf);
		}
	}

	/* close the file, free memory, and return */
	fclose(in);
	free(fitsbuf);

	return D;
}

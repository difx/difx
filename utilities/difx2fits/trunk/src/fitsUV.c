#include <stdlib.h>
#include <sys/types.h>
#include <string.h>
#include <strings.h>
#include <glob.h>
#include "config.h"
#include "fitsUV.h"


static int DifxVisInitData(DifxVis *dv)
{
	int n;
	
	if(!dv)
	{
		return -1;
	}

	if(dv->record)
	{
		free(dv->record);
	}

	dv->nData = dv->nComplex*dv->nFreq*dv->maxPol*dv->maxChan;
	if(dv->nData <= 0)
	{
		return -1;
	}
	n = dv->nFreq*dv->maxPol + dv->nData;
	dv->record = calloc(n*sizeof(float) + sizeof(struct UVrow), 1);
	if(dv->record == 0)
	{
		return -2;
	}
	dv->weight = dv->record->data;
	/* FIXME -- here add a similar thing for gateid */
	dv->data = dv->weight + (dv->nFreq*dv->maxPol);

	return 0;
}

static startGlob(DifxVis *dv)
{
	char globstr[256];
	int verbose = 0;
	int g;

	sprintf(globstr, "%s.difx/DIFX*", dv->D->job[0].fileBase);

	if(dv->jobId > 0)
	{
		globfree(&dv->globbuf);
	}
	glob(globstr, 0, 0, &dv->globbuf);
	dv->nFile = dv->globbuf.gl_pathc;
	if(verbose)
	{
		printf("jobId=%d  %d visibility files to be read:\n", 
			dv->jobId, dv->nFile);
		for(g = 0; g < dv->nFile; g++)
		{
			printf("File %d : %s\n", g+1, dv->globbuf.gl_pathv[g]);
		}
	}
	
	dv->globbuf.gl_offs = 0;
}

DifxVis *newDifxVis(const DifxInput *D, struct fitsPrivate *out)
{
	DifxVis *dv;
	int i, c, v;
	int polMask = 0;

	dv = (DifxVis *)calloc(1, sizeof(DifxVis));

	if(!dv)
	{
		return 0;
	}
	
	dv->jobId = 0;
	dv->curFile = -1;
	startGlob(dv);
	dv->dp = 0;
	dv->spectrum = 0;
	dv->data = 0;
	dv->record = 0;
	dv->nData = 0;
	dv->in = 0;
	dv->out = out;
	dv->D = D;
	dv->configId = -1;
	dv->sourceId = -1;
	dv->baseline = -1;
	dv->maxChan = D->nOutChan;
	dv->maxPol = D->nPolar;
	dv->nFreq = 0;
	dv->nComplex = 2;	/* for now don't write weights */
	for(c = 0; c < D->nConfig; c++)
	{
		if(D->nIF > dv->nFreq)
		{
			dv->nFreq = D->nIF;
		}
		for(i = 0; i < D->nIF; i++)
		{
			if(D->config[c].IF[i].pol[0] == 'R' ||
			   D->config[c].IF[i].pol[1] == 'R')
			{
				polMask |= 0x01;
			}
			if(D->config[c].IF[i].pol[0] == 'L' ||
			   D->config[c].IF[i].pol[1] == 'L')
			{
				polMask |= 0x02;
			}
			if(D->config[c].IF[i].pol[0] == 'X' ||
			   D->config[c].IF[i].pol[1] == 'X')
			{
				polMask |= 0x10;
			}
			if(D->config[c].IF[i].pol[0] == 'Y' ||
			   D->config[c].IF[i].pol[1] == 'Y')
			{
				polMask |= 0x20;
			}
		}
	}

	/* check for polarization confusion */
	if( ((polMask & 0x0F) > 0 && (polMask & 0xF0) > 0) || polMask == 0 )
	{
		fprintf(stderr, "Error: bad polarization combinations : %x\n",
			polMask);
		deleteDifxVis(dv);
		return 0;
	}

	if(polMask & 0x01)
	{
		dv->polStart = -1;
	}
	else if(polMask & 0x02)
	{
		dv->polStart = -2;
	}
	else if(polMask & 0x10)
	{
		dv->polStart = -5;
	}
	else /* must be YY only! who would do that? */
	{
		dv->polStart = -6;
	}

	dv->spectrum = (float *)calloc(dv->maxChan*dv->nComplex*
		dv->D->specAvg, sizeof(complex float));

	dv->dp = newDifxParameters();

	v = DifxVisInitData(dv);
	if(v < 0)
	{
		fprintf(stderr, "Error %d allocating %d + %d bytes\n",
			v, (int)(sizeof(struct UVrow)),
			dv->nComplex*dv->nFreq*dv->maxPol*dv->maxChan);
		deleteDifxVis(dv);
		return 0;
	}

	if(DifxVisNextFile(dv) < 0)
	{
		deleteDifxVis(dv);
		return 0;
	}

	return dv;
}

void deleteDifxVis(DifxVis *dv)
{
	if(!dv)
	{
		return;
	}

	globfree(&dv->globbuf);
	if(dv->in)
	{
		fclose(dv->in);
	}
	if(dv->spectrum)
	{
		free(dv->spectrum);
	}
	if(dv->dp)
	{
		deleteDifxParameters(dv->dp);
	}
	if(dv->record)
	{
		free(dv->record);
	}
	
	free(dv);
}

static int getPolProdId(const DifxVis *dv, const char *polPair)
{
	int p;
	char polSeq[8][4] = {"RR", "LL", "RL", "LR", "XX", "YY", "XY", "YX"};

	for(p = 0; p < 8; p++)
	{
		if(strncmp(polPair, polSeq[p], 2) == 0)
		{
			p = (p+1) + dv->polStart;
			
			if(p < 0 || p >= dv->maxPol)
			{
				return -1;
			}
			else
			{
				return p;
			}
		}
	}
	
	return -2;
}

int DifxVisNextFile(DifxVis *dv)
{
	if(dv->in)
	{
		fclose(dv->in);
		dv->in = 0;
	}
	dv->curFile++;
	if(dv->curFile >= dv->nFile)
	{
		dv->jobId++;
		if(dv->jobId >= dv->D->nJob)
		{
			return -1;
		}
		startGlob(dv);
		dv->curFile = 0;
	}
	printf("    JobId %d/%d File %d/%d : %s\n", 
		dv->jobId, dv->D->nJob,
		dv->curFile+1, dv->nFile,
		dv->globbuf.gl_pathv[dv->curFile]);
	dv->in = fopen(dv->globbuf.gl_pathv[dv->curFile], "r");
	if(!dv->in)
	{
		fprintf(stderr, "Error opening file : %s\n", 
			dv->globbuf.gl_pathv[dv->curFile]);
		return -1;
	}

	return 0;
}
	
int DifxVisNewUVData(DifxVis *dv)
{
	const char difxKeys[][MAX_DIFX_KEY_LEN] = 
	{
		"BASELINE NUM",
		"MJD",
		"SECONDS",
		"CONFIG INDEX",
		"SOURCE INDEX",
		"FREQ INDEX",
		"POLARISATION PAIR",
		"WEIGHTS WRITTEN",
		"U (METRES)",
		"V (METRES)",
		"W (METRES)"
	};
	const int N_DIFX_ROWS = sizeof(difxKeys)/sizeof(difxKeys[0]);
	int rows[N_DIFX_ROWS];
	int i, i1, v, N;
	int a1, a2;
	int bl, c;
	double mjd;
	int changed = 0;
	int nFloat;
	char line[100];
	int freqNum;
	int configId;

	resetDifxParameters(dv->dp);

	for(i = 0; i < 13; i++)
	{
		fgets(line, 99, dv->in);
		if(feof(dv->in))
		{
			/* EOF should not happen in middle of text */
			if(i != 0)
			{
				return -1;
			}
			v = DifxVisNextFile(dv);
			if(v < 0)
			{
				return -2;
			}
			fgets(line, 99, dv->in);
		}
		DifxParametersaddrow(dv->dp, line);
	}

	/* parse the text header */
	N = DifxParametersbatchfind(dv->dp, 0, difxKeys, N_DIFX_ROWS, rows);
	if(N < N_DIFX_ROWS)
	{
		return -3;
	}

	bl           = atoi(DifxParametersvalue(dv->dp, rows[0]));
	mjd          = atoi(DifxParametersvalue(dv->dp, rows[1])) +
	               atof(DifxParametersvalue(dv->dp, rows[2]))/86400.0;
	c            = atoi(DifxParametersvalue(dv->dp, rows[3]));
	freqNum      = atoi(DifxParametersvalue(dv->dp, rows[5]));


	dv->sourceId = DifxInputGetSourceIdByJobId(dv->D, mjd, dv->jobId);
	if(dv->sourceId < 0)
	{
		return -4;
	}

	configId = dv->D->source[dv->sourceId].configId;
	dv->freqId = dv->D->config[configId].freqId;

	/* FIXME -- is the below baseline to antenna map completely general? */
	a1 = bl/256 - 1;
	a2 = bl%256 - 1;

	dv->bandId = dv->D->config[configId].baselineFreq2IF[a1][a2][freqNum];
	dv->polId  = getPolProdId(dv, DifxParametersvalue(dv->dp, rows[6]));
	
	/* if weights are written the data volume is 3/2 as large */
	nFloat       = atoi(DifxParametersvalue(dv->dp, rows[7])) > 0 ? 3 : 2;
	if(bl != dv->baseline || fabs(mjd -  dv->mjd) > 1.0/86400000.0)
	{
		changed = 1;
		dv->baseline = bl;

		/* swap phase/uvw for FITS-IDI conformance */
		dv->U = -atof(DifxParametersvalue(dv->dp, rows[8]));
		dv->V = -atof(DifxParametersvalue(dv->dp, rows[9]));
		dv->W = -atof(DifxParametersvalue(dv->dp, rows[10]));
		dv->mjd = mjd;
	}
	if(c != dv->configId)
	{
		if(!changed)	/* cannot change config within integration */
		{
			return -5;
		}
		dv->configId = c;
		dv->nChan = dv->D->nOutChan;
		dv->tInt = dv->D->config[c].tInt;
	}

	if(dv->bandId <  0 || dv->bandId >= dv->nFreq  ||
	   dv->polId  <  0 || dv->polId  >= dv->maxPol ||
	   dv->nChan  <= 0 || dv->nChan  >  dv->maxChan)
	{
		fprintf(stderr, "Parameter problem: %d %d  %d %d  %d %d\n",
			dv->bandId, dv->nFreq,
			dv->polId, dv->maxPol,
			dv->nChan, dv->maxChan);
		return -6;
	}

	/* don't read weighted data into unweighted slot */
	if(nFloat > dv->nComplex)
	{
		printf("nFloat > dv->nComplex\n");
		return -7;
	}
	
	/* read in the binary formatted visibility spectrum */
	v = fread(dv->spectrum, sizeof(float), dv->nChan*nFloat*dv->D->specAvg, dv->in);
	if(v < dv->nChan)
	{
		return -8;
	}

	/* reorder data and set weights if weights not provided */
	if(nFloat < dv->nComplex)
	{
		for(i = 3*dv->nChan*dv->D->specAvg-3; i > 0; i-=3)
		{
			i1 = i*2/3;
			dv->spectrum[i+2] = 1.0;                 /* weight */
			dv->spectrum[i+1] = dv->spectrum[i1+1];  /* imag */
			dv->spectrum[i]   = dv->spectrum[i1];    /* real */
		}
		/* The first one needs no reordering, but needs weight */
		dv->spectrum[2] = 1.0;
	}

	return changed;
}

int DifxVisCollectRandomParams(const DifxVis *dv)
{
	const double C=2.99792458e8;	/* speed of light in m/s */
	
	dv->record->U = dv->U/C;
	dv->record->V = dv->V/C;
	dv->record->W = dv->W/C;

	/* FIXME -- are two these correct ? */
	dv->record->jd = 2400000.5 + (int)dv->mjd;
	dv->record->iat = dv->mjd - (int)dv->mjd;

	/* note antennaIds, sourceId, freqId are 1-based in FITS */
	dv->record->baseline = dv->baseline;
	dv->record->filter = 0;
	dv->record->sourceId1 = dv->sourceId + 1;
	dv->record->freqId1   = dv->freqId + 1;
	dv->record->intTime   = dv->tInt;

	return 0;
}

int RecordIsInvalid(const DifxVis *dv)
{
	int i, n;
	int invalid=1;
	const float *d;

	d = dv->data;
	n = dv->nData;

	for(i = 0; i < n; i++)
	{
		if(!finite(d[i]))
		{
			printf("Warning -- record with !finite value\n");
			return 1;
		}
		/* don't look at weight in deciding whether data is valid */
		if((d[i] != 0.0) && (i % dv->nComplex != 2))
		{
			invalid = 0;
		}
	}
	
	return invalid;
}

int RecordIsFlagged(const DifxVis *dv)
{
	double mjd;
	int a1, a2;
	int i;

	if(dv->D->nFlag <= 0)
	{
		return 0;
	}

	a1  = dv->baseline/256 - 1;
	a2  = dv->baseline%256 - 1;
	mjd = dv->mjd;

	for(i = 0; i < dv->D->nFlag; i++)
	{
		if(dv->D->flag[i].mjd1 <= mjd &&
		   dv->D->flag[i].mjd2 >= mjd)
		{
			if(dv->D->flag[i].antennaId == a1 ||
			   dv->D->flag[i].antennaId == a2)
			{
				return 1;
			}
		}
	}

	return 0;
}

int DifxVisConvert(DifxVis *dv, struct fits_keywords *p_fits_keys, double s)
{
	int first = 1;
	int v;
	int index;
	int changed;
	int i, k;
	int specAvg;
	float visScale;
	double scale;
	char dateStr[12];
	char fluxFormFloat[8];
	char gateFormInt[8];
	char weightFormFloat[8];
	int isLSB;	/* boolean -- true if lower side band */
	int nRowBytes;
	int nColumn;
	int nWeight;
	int nInvalid = 0;
	int nFlagged = 0;
	int nWritten = 0;

	/* define the columns in the UV data FITS Table */
	struct fitsBinTableColumn columns[] =
	{
		{"UU--SIN", "1E", "u", "SECONDS"},
		{"VV--SIN", "1E", "v", "SECONDS"},
		{"WW--SIN", "1E", "w", "SECONDS"},
		{"DATE", "1D", "Julian day at 0 hr current day", "DAYS"},
		{"TIME", "1D", "IAT time", "DAYS"},
		{"BASELINE", "1J", "baseline: ant1*256 + ant2", 0},
		{"FILTER", "1J", "filter id number", 0},
		{"SOURCE", "1J", "source id number from source tbl", 0},
		{"FREQID", "1J", "freq id number from frequency tbl", 0},
		{"INTTIM", "1E", "time span of datum", "SECONDS"},
		{"WEIGHT", weightFormFloat, "weights proportional to time", 0},
		{"GATEID", gateFormInt, "gate id from gate model table", 0},
		{"FLUX", fluxFormFloat, "data matrix", "UNCALIB"}
	};

	visScale = 1.0;

	/* FIXME -- nees love here! */
	if(s > 0.0)
	{
		scale = s;
	}
	else
	{
		scale = 1.0/(dv->D->chanBW*6.25e6*
			dv->D->config[0].tInt*dv->D->specAvg);
	}

	nWeight = dv->nFreq*dv->maxPol;

	/* set the number of weight and flux values*/
	sprintf(weightFormFloat, "%dE", nWeight);
	sprintf(gateFormInt, "%dJ", 0);
	sprintf(fluxFormFloat, "%dE", dv->nData);

	nColumn = NELEMENTS(columns);
	nRowBytes = FitsBinTableSize(columns, nColumn);

	fitsWriteBinTable(dv->out, nColumn, columns, nRowBytes, "UV_DATA");
	fitsWriteInteger(dv->out, "NMATRIX", 1, "");

	/* get the job ref_date from the fits_keyword struct, convert it into
	   a FITS string and save it in the FITS header */
	mjd2fits((int)dv->D->mjdStart, dateStr);
	fitsWriteString(dv->out, "DATE-OBS", dateStr, "");

	fitsWriteString(dv->out, "TELESCOP", "VLBA", "");
	fitsWriteString(dv->out, "OBSERVER", "PLUTO", "");
	
	arrayWriteKeys(p_fits_keys, dv->out);
	
	fitsWriteInteger(dv->out, "TABREV", 2, "ARRAY changed to FILTER");
	fitsWriteFloat(dv->out, "VIS_SCAL", visScale, "");

	fitsWriteString(dv->out, "SORT", "T*", "");

	/* define the data matrix columns */
	fitsWriteInteger(dv->out, "MAXIS", 6, "");
	fitsWriteInteger(dv->out, "MAXIS1", dv->nComplex, "");

	fitsWriteString(dv->out, "CTYPE1", "COMPLEX", "");
	fitsWriteFloat(dv->out, "CDELT1", 1.0, "");
	fitsWriteFloat(dv->out, "CRPIX1", 1.0, "");
	fitsWriteFloat(dv->out, "CRVAL1", 1.0, "");
	fitsWriteInteger(dv->out, "MAXIS2", dv->maxPol, "");
	fitsWriteString(dv->out, "CTYPE2", "STOKES", "");
	fitsWriteFloat(dv->out, "CDELT2", -1.0, "");
	fitsWriteFloat(dv->out, "CRPIX2", 1.0, "");
	fitsWriteFloat(dv->out, "CRVAL2", (float)dv->polStart, "");
	fitsWriteInteger(dv->out, "MAXIS3", dv->D->nOutChan, "");
	fitsWriteString(dv->out, "CTYPE3", "FREQ", "");
	fitsWriteFloat(dv->out, "CDELT3", 
		dv->D->chanBW*1.0e6/dv->D->nOutChan, "");
	fitsWriteFloat(dv->out, "CRPIX3", p_fits_keys->ref_pixel, "");
	fitsWriteFloat(dv->out, "CRVAL3", dv->D->refFreq*1000000.0, "");
	fitsWriteInteger(dv->out, "MAXIS4", dv->nFreq, "");
	fitsWriteString(dv->out, "CTYPE4", "BAND", "");
	fitsWriteFloat(dv->out, "CDELT4", 1.0, "");
	fitsWriteFloat(dv->out, "CRPIX4", 1.0, "");
	fitsWriteFloat(dv->out, "CRVAL4", 1.0, "");
	fitsWriteInteger(dv->out, "MAXIS5", 1, "");
	fitsWriteString(dv->out, "CTYPE5", "RA", "");
	fitsWriteFloat(dv->out, "CDELT5", 0.0, "");
	fitsWriteFloat(dv->out, "CRPIX5", 1.0, "");
	fitsWriteFloat(dv->out, "CRVAL5", 0.0, "");
	fitsWriteInteger(dv->out, "MAXIS6", 1, "");
	fitsWriteString(dv->out, "CTYPE6", "DEC", "");
	fitsWriteFloat(dv->out, "CDELT6", 0.0, "");
	fitsWriteFloat(dv->out, "CRPIX6", 1.0, "");
	fitsWriteFloat(dv->out, "CRVAL6", 0.0, "");
	fitsWriteLogical(dv->out, "TMATX11", 1, "");
	
	fitsWriteEnd(dv->out);

	/* now loop over visibility records and write out rows */
	
	specAvg = dv->D->specAvg;

	for(;;)
	{
		changed = DifxVisNewUVData(dv);
		if(changed < 0)	/* done! */
		{
			break;
		}
		if(changed)
		{
			if(first)
			{
				first = 0;
			}
			else
			{
				if(RecordIsInvalid(dv))
				{
					nInvalid++;
				}
				else if(RecordIsFlagged(dv))
				{
					nFlagged++;
				}
				else
				{
					for(i=0; i < dv->nFreq*dv->maxPol; i++)
					{
						dv->weight[i] = 1.0;
					}
#ifndef WORDS_BIGENDIAN
					FitsBinRowByteSwap(columns, nColumn, 
						dv->record);
#endif
					fitsWriteBinRow(dv->out, 
						(char *)dv->record);
					nWritten++;
				}
			}
			v = DifxVisCollectRandomParams(dv);
			if(v < 0)
			{
				fprintf(stderr,
				  "Error in DifxVisCollectRandomParams : %d\n", 
				  v);
				return -3;
			}

			/* blank array */
			memset(dv->data, 0, dv->nData*sizeof(float));
		}
		isLSB = dv->D->config[dv->configId].IF[dv->bandId].sideband == 'L';
		for(i = 0; i < dv->nChan*dv->D->specAvg; i++)
		{
			if(isLSB)
			{
				int j;
				j = dv->nChan*dv->D->specAvg - 1 - i;
				index = ((dv->bandId*dv->nChan + j/specAvg)*
					dv->maxPol+dv->polId)*dv->nComplex;
			}
			else
			{
				index = ((dv->bandId*dv->nChan + i/specAvg)*
					dv->maxPol+dv->polId)*dv->nComplex;
			}
			for(k = 0; k < dv->nComplex; k++)
			{
				/* swap phase/uvw for FITS-IDI conformance */
				if(k % 3 == 1 && !isLSB)
				{
					dv->data[index+k] -= scale*
						dv->spectrum[dv->nComplex*i+k];
				}
				else
				{
					dv->data[index+k] += scale*
						dv->spectrum[dv->nComplex*i+k];
				}
			}
		}
	}

	/* write that last bit of data */
	if(RecordIsInvalid(dv))
	{
		nInvalid++;
	}
	else if(RecordIsFlagged(dv))
	{
		nFlagged++;
	}
	else
	{
		for(i = 0; i < dv->nFreq*dv->maxPol; i++)
		{
			dv->weight[i] = 1.0;
		}
#ifndef WORDS_BIGENDIAN
		FitsBinRowByteSwap(columns, nColumn, dv->record);
#endif
		fitsWriteBinRow(dv->out, (char *)dv->record);
		nWritten++;
	}

	printf("      %d flagged records dropped\n", nFlagged);
	printf("      %d invalid records dropped\n", nInvalid);
	printf("      %d records written\n", nWritten);

	return 0;
}

const DifxInput *DifxInput2FitsUV(const DifxInput *D,
	struct fits_keywords *p_fits_keys,
	struct fitsPrivate *out, double scale)
{
	DifxVis *dv;
	
	if(D == 0)
	{
		return 0;
	}

	dv = newDifxVis(D, out);
	if(!dv)
	{
		return 0;
	}

	DifxVisConvert(dv, p_fits_keys, scale);

	deleteDifxVis(dv);

	return D;
}


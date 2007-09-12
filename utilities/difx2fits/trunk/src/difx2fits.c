#include <stdio.h>
#include <string.h>
#include "difx2fits.h"

const char program[] = "difx2fits";
const char author[]  = "Walter Brisken";
const char version[] = "0.1";
const char verdata[] = "2007/05/06";

int usage(const char *pgm)
{
	fprintf(stderr, "Usage : %s <basefilename> <outfile>\n", pgm);

	return 0;
}

int populateFitsKeywords(const DifxInput *D, struct fits_keywords *keys)
{
	strcpy(keys->obscode, D->obsCode);
	keys->no_stkd = (1+D->config[0].doPolar)*D->config[0].IF[0].nPol;
	if(D->config[0].IF[0].pol[0] == 'L')
	{
		keys->stk_1 = -2;
	}
	else
	{
		keys->stk_1 = -1;
	}
	keys->no_band = D->config[0].nIF;
	keys->no_chan = D->nOutChan;
	keys->ref_freq = D->refFreq*1.0e6;
	keys->ref_date = D->mjdStart;
	keys->chan_bw = 1.0e6*D->config[0].IF[0].bw/D->nOutChan;
	keys->ref_pixel = 0.5 + 1.0/(2.0*D->specAvg);
	keys->no_pol = D->config[0].IF[0].nPol;
	
	return 0;
}

const DifxInput *DifxInput2FitsTables(const DifxInput *D, 
	const char *filebase, struct fitsPrivate *out)
{
	struct fits_keywords keys;

	populateFitsKeywords(D, &keys);
	D = DifxInput2FitsHeader(D, &keys, out);
	D = DifxInput2FitsAG(D, &keys, out);
	D = DifxInput2FitsSO(D, &keys, out);
	D = DifxInput2FitsAN(D, &keys, out);
	D = DifxInput2FitsFQ(D, &keys, out);
	D = DifxInput2FitsML(D, &keys, out);
	D = DifxInput2FitsCT(D, &keys, out);
	D = DifxInput2FitsMC(D, &keys, out);
	D = DifxInput2FitsUV(D, &keys, filebase, out);
	
	
	return D;
}

int main(int argc, char **argv)
{
	DifxInput *D;
	struct fitsPrivate outfile;

	if(argc < 3)
	{
		return usage(argv[0]);
	}

	D = loadDifxInput(argv[1]);
	if(!D)
	{
		fprintf(stderr, "Cannot proceed.  Aborting\n");
		return 0;
	}

	if(strcmp(D->taperFunction, "UNIFORM") != 0)
	{
		fprintf(stderr, "Taper func %s not supported.  "
			"Using UNIFORM.\n", D->taperFunction);
		strcpy(D->taperFunction, "UNIFORM");
	}

//	printDifxInput(D);

	if(fitsWriteOpen(&outfile, argv[2]) < 0)
	{
		deleteDifxInput(D);
		fprintf(stderr, "Cannot open output file\n");
		return 0;
	}

	if(DifxInput2FitsTables(D, argv[1], &outfile) == D)
	{
		fprintf(stderr, "Conversion successful\n");
	}

	fitsWriteClose(&outfile);
	
	deleteDifxInput(D);
	
	return 0;
}

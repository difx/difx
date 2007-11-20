#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "difx2fits.h"
#include "../config.h"

const char program[] = PACKAGE_NAME;
const char author[]  = PACKAGE_BUGREPORT;
const char version[] = VERSION;

int usage(const char *pgm)
{
	fprintf(stderr, "\n%s ver. %s   %s\n\n",
		program, version, author);
	fprintf(stderr, "A program to convert DiFX format data to "
		"FITS-IDI\n\n");
	fprintf(stderr, "Usage : %s [options] <basefilename> <outfile>\n\n", 
		pgm);
	fprintf(stderr, "It is assumed that at least 3 input files exist:\n");
	fprintf(stderr, "  <basefilename>.input    DiFX input file\n");
	fprintf(stderr, "  <basefilename>.uvw      DiFX UVW file\n");
	fprintf(stderr, "  <basefilename>.delay    DiFX delay model\n\n");
	fprintf(stderr, "Three other files are optionally read:\n");
	fprintf(stderr, "  <basefilename>.calc     Base file for calcif \n");
	fprintf(stderr, "  <basefilename>.rates    Extra calcif output\n");
	fprintf(stderr, "  <basefilename>.flag     Antenna-based flagging\n\n");
	fprintf(stderr, "The output file <outfile> will be written in "
		"FITS-IDI format nearly\n");
	fprintf(stderr, "identical to that made at the VLBA HW correlator.  "
		"The first two optional files\n");
	fprintf(stderr, "are required for full model accountability.\n\n");
	fprintf(stderr, "options can include:\n");
	fprintf(stderr, "  --no-model\n");
	fprintf(stderr, "  -n               Don't write model (ML) table\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --scale <scale>\n");
	fprintf(stderr, "  -s      <scale>  Scale visibility data "
			"by <scale>\n");
	fprintf(stderr, "\n");

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
	const char *filebase, struct fitsPrivate *out, int write_model,
	double scale)
{
	struct fits_keywords keys;
	long long last_bytes = 0;

	populateFitsKeywords(D, &keys);
	
	printf("\nWriting FITS tables:\n");

	printf("  Header                    ");
	fflush(stdout);
	D = DifxInput2FitsHeader(D, &keys, out);
	printf("%d bytes\n", out->bytes_written - last_bytes);
	last_bytes = out->bytes_written;

	printf("  AG -- array geometry      ");
	fflush(stdout);
	D = DifxInput2FitsAG(D, &keys, out);
	printf("%d bytes\n", out->bytes_written - last_bytes);
	last_bytes = out->bytes_written;

	printf("  SO -- source              ");
	fflush(stdout);
	D = DifxInput2FitsSO(D, &keys, out);
	printf("%d bytes\n", out->bytes_written - last_bytes);
	last_bytes = out->bytes_written;

	printf("  AN -- antenna             ");
	fflush(stdout);
	D = DifxInput2FitsAN(D, &keys, out);
	printf("%d bytes\n", out->bytes_written - last_bytes);
	last_bytes = out->bytes_written;

	printf("  FQ -- frequency           ");
	fflush(stdout);
	D = DifxInput2FitsFQ(D, &keys, out);
	printf("%d bytes\n", out->bytes_written - last_bytes);
	last_bytes = out->bytes_written;

	if(write_model)
	{
		printf("  ML -- model               ");
		fflush(stdout);
		D = DifxInput2FitsML(D, &keys, out);
		printf("%d bytes\n", out->bytes_written - last_bytes);
		last_bytes = out->bytes_written;
	}

	printf("  CT -- correlator (eop)    ");
	fflush(stdout);
	D = DifxInput2FitsCT(D, &keys, out);
	printf("%d bytes\n", out->bytes_written - last_bytes);
	last_bytes = out->bytes_written;

	printf("  MC -- model components    ");
	fflush(stdout);
	D = DifxInput2FitsMC(D, &keys, out);
	printf("%d bytes\n", out->bytes_written - last_bytes);
	last_bytes = out->bytes_written;

	printf("  UV -- visibility          \n");
	fflush(stdout);
	D = DifxInput2FitsUV(D, &keys, filebase, out, scale);
	printf("                            ");
	printf("%d bytes\n", out->bytes_written - last_bytes);
	last_bytes = out->bytes_written;

	printf("                            -----\n");
	printf("  Total                     %d bytes\n", last_bytes);
	
	return D;
}

int main(int argc, char **argv)
{
	DifxInput *D;
	struct fitsPrivate outfile;
	const char *basefile=0, *fitsfile=0;
	int writemodel = 1;
	int i;
	double scale = 0.0;

	for(i = 1; i < argc; i++)
	{
		if(argv[i][0] == '-')
		{
			if(strcmp(argv[i], "--no-model") == 0 ||
			   strcmp(argv[i], "-n") == 0)
			{
				writemodel = 0;
			}
			else if(i+1 < argc) /* one parameter arguments */
			{
				if(strcmp(argv[i], "--scale") == 0 ||
				   strcmp(argv[i], "-s") == 0)
				{
					i++;
					scale = atof(argv[i]);
					printf("Scaling data by %f\n", scale);
				}
			}
		}
		else if(basefile == 0)
		{
			basefile = argv[i];
		}
		else if(fitsfile == 0)
		{
			fitsfile = argv[i];
		}
		else
		{
			return usage(argv[0]);
		}
	}


	if(basefile == 0 || fitsfile == 0)
	{
		return usage(argv[0]);
	}

	D = loadDifxInput(basefile);
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

	if(fitsWriteOpen(&outfile, fitsfile) < 0)
	{
		deleteDifxInput(D);
		fprintf(stderr, "Cannot open output file\n");
		return 0;
	}

	if(DifxInput2FitsTables(D, basefile, &outfile, writemodel, scale) == D)
	{
		printf("\nConversion successful\n");
	}

	fitsWriteClose(&outfile);
	
	deleteDifxInput(D);
	
	return 0;
}

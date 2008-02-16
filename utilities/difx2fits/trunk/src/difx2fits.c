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
	fprintf(stderr, "It assumed that SWIN format visibility file(s) to be converted live\n");
	fprintf(stderr, "in directory <basefilename>.difx/\n");
	fprintf(stderr, "It is also assumed that at least 3 additional files exist:\n");
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
	fprintf(stderr, "  --verbose\n");
	fprintf(stderr, "  -v               Be verbose\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --scale <scale>\n");
	fprintf(stderr, "  -s      <scale>  Scale visibility data "
			"by <scale>\n");
	fprintf(stderr, "\n");

	return 0;
}

int populateFitsKeywords(const DifxInput *D, struct fits_keywords *keys)
{
	strcpy(keys->obscode, D->job->obsCode);
	keys->no_stkd = D->nPolar;
	switch(D->polPair[0])
	{
		case 'R':
			keys->stk_1 = -1;
			break;
		case 'L':
			keys->stk_1 = -2;
			break;
		case 'X':
			keys->stk_1 = -5;
			break;
		case 'Y':
			keys->stk_1 = -6;
			break;
		default:
			fprintf(stderr, "Error -- unknown polarization (%c)\n", 
				D->polPair[0]);
			exit(0);
	}
	keys->no_band = D->nIF;
	keys->no_chan = D->nOutChan;
	keys->ref_freq = D->refFreq*1.0e6;
	keys->ref_date = D->mjdStart;
	keys->chan_bw = 1.0e6*D->chanBW/D->nOutChan;
	keys->ref_pixel = 0.5 + 1.0/(2.0*D->specAvg);
	if(D->nPolar > 1)
	{
		keys->no_pol = 2;
	}
	else
	{
		keys->no_pol = 1;
	}
	
	return 0;
}

const DifxInput *DifxInput2FitsTables(const DifxInput *D, 
	struct fitsPrivate *out, int write_model, double scale)
{
	struct fits_keywords keys;
	long long last_bytes = 0;

	populateFitsKeywords(D, &keys);
	
	printf("\nWriting FITS tables:\n");

	printf("  Header                    ");
	fflush(stdout);
	D = DifxInput2FitsHeader(D, out);
	printf("%lld bytes\n", out->bytes_written - last_bytes);
	last_bytes = out->bytes_written;

	printf("  AG -- array geometry      ");
	fflush(stdout);
	D = DifxInput2FitsAG(D, &keys, out);
	printf("%lld bytes\n", out->bytes_written - last_bytes);
	last_bytes = out->bytes_written;

	printf("  SU -- source              ");
	fflush(stdout);
	D = DifxInput2FitsSU(D, &keys, out);
	printf("%lld bytes\n", out->bytes_written - last_bytes);
	last_bytes = out->bytes_written;

	printf("  AN -- antenna             ");
	fflush(stdout);
	D = DifxInput2FitsAN(D, &keys, out);
	printf("%lld bytes\n", out->bytes_written - last_bytes);
	last_bytes = out->bytes_written;

	printf("  FR -- frequency           ");
	fflush(stdout);
	D = DifxInput2FitsFR(D, &keys, out);
	printf("%lld bytes\n", out->bytes_written - last_bytes);
	last_bytes = out->bytes_written;

	if(write_model)
	{
		printf("  ML -- model               ");
		fflush(stdout);
		D = DifxInput2FitsML(D, &keys, out);
		printf("%lld bytes\n", out->bytes_written - last_bytes);
		last_bytes = out->bytes_written;
	}

	printf("  CT -- correlator (eop)    ");
	fflush(stdout);
	D = DifxInput2FitsCT(D, &keys, out);
	printf("%lld bytes\n", out->bytes_written - last_bytes);
	last_bytes = out->bytes_written;

	printf("  MC -- model components    ");
	fflush(stdout);
	D = DifxInput2FitsMC(D, &keys, out);
	printf("%lld bytes\n", out->bytes_written - last_bytes);
	last_bytes = out->bytes_written;

	printf("  SO -- spacecraft orbit    ");
	fflush(stdout);
	D = DifxInput2FitsSO(D, &keys, out);
	printf("%lld bytes\n", out->bytes_written - last_bytes);
	last_bytes = out->bytes_written;

	printf("  UV -- visibility          \n");
	fflush(stdout);
	D = DifxInput2FitsUV(D, &keys, out, scale);
	printf("                            ");
	printf("%lld bytes\n", out->bytes_written - last_bytes);
	last_bytes = out->bytes_written;

	printf("  FL -- flag                ");
	fflush(stdout);
	D = DifxInput2FitsFL(D, &keys, out);
	printf("%lld bytes\n", out->bytes_written - last_bytes);
	last_bytes = out->bytes_written;

	printf("  TS -- system temperature  ");
	fflush(stdout);
	D = DifxInput2FitsTS(D, &keys, out);
	printf("%lld bytes\n", out->bytes_written - last_bytes);
	last_bytes = out->bytes_written;

	printf("  PH -- phase cal           ");
	fflush(stdout);
	D = DifxInput2FitsPH(D, &keys, out);
	printf("%lld bytes\n", out->bytes_written - last_bytes);
	last_bytes = out->bytes_written;

	printf("  WR -- weather             ");
	fflush(stdout);
	D = DifxInput2FitsWR(D, &keys, out);
	printf("%lld bytes\n", out->bytes_written - last_bytes);
	last_bytes = out->bytes_written;

	printf("  GN -- gain curve          ");
	fflush(stdout);
	D = DifxInput2FitsGN(D, &keys, out);
	printf("%lld bytes\n", out->bytes_written - last_bytes);
	last_bytes = out->bytes_written;

	printf("                            -----\n");
	printf("  Total                     %lld bytes\n", last_bytes);

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
	int verbose = 0;

	for(i = 1; i < argc; i++)
	{
		if(argv[i][0] == '-')
		{
			if(strcmp(argv[i], "--no-model") == 0 ||
			   strcmp(argv[i], "-n") == 0)
			{
				writemodel = 0;
			}
			if(strcmp(argv[i], "--verbose") == 0 ||
			   strcmp(argv[i], "-v") == 0)
			{
				verbose = 1;
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
		fprintf(stderr, "loadDifxInput failed on <%s>.  Aborting\n",
			basefile);
		return 0;
	}
	D = updateDifxInput(D);
	if(!D)
	{
		fprintf(stderr, "updateDifxInput failed.  Aborting\n");
		return 0;
	}

	if(verbose)
	{
		printDifxInput(D);
	}

	if(D->nIF <= 0 || D->nPolar <= 0)
	{
		fprintf(stderr, "Data geometry changes during obs, cannot "
			"make into FITS.\n");
		deleteDifxInput(D);
		return 0;
	}

	if(strcmp(D->job->taperFunction, "UNIFORM") != 0)
	{
		fprintf(stderr, "Taper func %s not supported.  "
			"Using UNIFORM.\n", D->job->taperFunction);
		strcpy(D->job->taperFunction, "UNIFORM");
	}

	if(fitsWriteOpen(&outfile, fitsfile) < 0)
	{
		deleteDifxInput(D);
		fprintf(stderr, "Cannot open output file\n");
		return 0;
	}

	if(DifxInput2FitsTables(D, &outfile, writemodel, scale) == D)
	{
		printf("\nConversion successful\n");
	}

	fitsWriteClose(&outfile);
	
	deleteDifxInput(D);
	
	return 0;
}

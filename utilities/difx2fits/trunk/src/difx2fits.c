#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "difx2fits.h"
#include "../config.h"

const char program[] = PACKAGE_NAME;
const char author[]  = PACKAGE_BUGREPORT;
const char version[] = VERSION;

static int usage(const char *pgm)
{
	fprintf(stderr, "\n%s ver. %s   %s\n\n",
		program, version, author);
	fprintf(stderr, "A program to convert DiFX format data to "
		"FITS-IDI\n\n");
	fprintf(stderr, "Usage : %s [options] <baseFilename1> "
		"[<baseFilename2> ... ] <outfile>\n\n", pgm);
	fprintf(stderr, "It assumed that SWIN format visibility file(s) "
		"to be converted live\n");
	fprintf(stderr, "in directory <baseFilename>.difx/\n");
	fprintf(stderr, "It is also assumed that at least 3 additional "
		"files exist:\n");
	fprintf(stderr, "  <baseFilename>.input    DiFX input file\n");
	fprintf(stderr, "  <baseFilename>.uvw      DiFX UVW file\n");
	fprintf(stderr, "  <baseFilename>.delay    DiFX delay model\n\n");
	fprintf(stderr, "Three other files are optionally read:\n");
	fprintf(stderr, "  <baseFilename>.calc     Base file for calcif \n");
	fprintf(stderr, "  <baseFilename>.rates    Extra calcif output\n");
	fprintf(stderr, "  <baseFilename>.flag     Antenna-based flagging\n\n");
	fprintf(stderr, "VLBA calibration transfer will produce 4 files:\n");
	fprintf(stderr, "  flag, tsys, pcal, weather\n");
	fprintf(stderr, "If these are present in the current directory, they "
		"will be used to\n");
	fprintf(stderr, "form the FL, TS, PH and WR tables\n\n");
	fprintf(stderr, "If env variable GAIN_CURVE_PATH is set, gain curves "
		"will be looked for\n");
	fprintf(stderr, "and turned into a GN table\n\n");
		
	fprintf(stderr, "The output file <outfile> will be written in "
		"FITS-IDI format nearly\n");
	fprintf(stderr, "identical to that made at the VLBA HW correlator.  "
		"The first two optional\n");
	fprintf(stderr, "files are required for full model accountability.\n");
	fprintf(stderr, "\noptions can include:\n");
	fprintf(stderr, "  --average <nchan>\n");
	fprintf(stderr, "  -a        <nchan>   Average <nchan> channels\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --beginchan <chan>\n");
	fprintf(stderr, "  -b          <chan>  Skip <chan> correlated channels\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --no-model\n");
	fprintf(stderr, "  -n                  Don't write model (ML) table\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --outchans <nchan>\n");
	fprintf(stderr, "  -o         <nchan>  Write <nchan> channels\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --scale <scale>\n");
	fprintf(stderr, "  -s      <scale>     Scale visibility data "
		"by <scale>\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --verbose\n");
	fprintf(stderr, "  -v                  Be verbose.  -v -v for more!\n");
	fprintf(stderr, "\n");

	return 0;
}

static int populateFitsKeywords(const DifxInput *D, struct fits_keywords *keys)
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
	keys->chan_bw = 1.0e6*D->chanBW*D->specAvg/D->nInChan;
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

static const DifxInput *DifxInput2FitsTables(const DifxInput *D, 
	struct fitsPrivate *out, int write_model, double scale, int verbose)
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

	printf("  GD -- pulsar gate duty    ");
	fflush(stdout);
	D = DifxInput2FitsGD(D, &keys, out);
	printf("%lld bytes\n", out->bytes_written - last_bytes);
	last_bytes = out->bytes_written;

	printf("  GM -- pulsar gate model   ");
	fflush(stdout);
	D = DifxInput2FitsGM(D, &keys, out);
	printf("%lld bytes\n", out->bytes_written - last_bytes);
	last_bytes = out->bytes_written;

	printf("  UV -- visibility          \n");
	fflush(stdout);
	D = DifxInput2FitsUV(D, &keys, out, scale, verbose);
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
	DifxInput *D, *D1, *D2;
	struct fitsPrivate outfile;
	const char *baseFile[1024], *fitsFile=0;
	int writemodel = 1;
	int i;
	int pretend = 0;
	double scale = 0.0;
	int verbose = 0;
	int nBaseFile = 0;
	/* some overrides */
	int specAvg = 0;
	float nOutChan = 0.0;
	float startChan = 0.0;

	for(i = 1; i < argc; i++)
	{
		if(argv[i][0] == '-')
		{
			if(strcmp(argv[i], "--no-model") == 0 ||
			   strcmp(argv[i], "-n") == 0)
			{
				writemodel = 0;
			}
			if(strcmp(argv[i], "--quiet") == 0 ||
			   strcmp(argv[i], "-q") == 0)
			{
				verbose++;
			}
			if(strcmp(argv[i], "--verbose") == 0 ||
			   strcmp(argv[i], "-v") == 0)
			{
				verbose++;
			}
			if(strcmp(argv[i], "--pretend") == 0 ||
			   strcmp(argv[i], "-p") == 0)
			{
				pretend = 1;
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
				if(strcmp(argv[i], "--average") == 0 ||
				   strcmp(argv[i], "-a") == 0)
				{
					i++;
					specAvg = atoi(argv[i]);
				}
				if(strcmp(argv[i], "--outchans") == 0 ||
				   strcmp(argv[i], "-o") == 0)
				{
					i++;
					nOutChan = atof(argv[i]);
				}
				if(strcmp(argv[i], "--beginchan") == 0 ||
				   strcmp(argv[i], "-b") == 0)
				{
					i++;
					startChan = atof(argv[i]);
				}
			}
		}
		else
		{
			if(fitsFile)
			{
				baseFile[nBaseFile] = fitsFile;
				nBaseFile++;
			}
			fitsFile = argv[i];
		}
	}

	if(nBaseFile == 0 || fitsFile == 0)
	{
		return usage(argv[0]);
	}

	D = 0;

	for(i = 0; i < nBaseFile; i++)
	{
		if(verbose > 1)
		{
			printf("Loading %s\n", baseFile[i]);
		}
		D2 = loadDifxInput(baseFile[i]);
		if(!D2)
		{
			fprintf(stderr, "loadDifxInput failed on <%s>.\n",
				baseFile[i]);
			return 0;
		}
		if(specAvg)
		{
			D2->specAvg = specAvg;
		}
		if(nOutChan >= 1)
		{
			D2->nOutChan = nOutChan;
		}
		else if(nOutChan > 0.0) /* interpret in fractional sense */
		{
			D2->nOutChan = D2->config[0].nChan*nOutChan/D2->specAvg;
		}
		if(startChan >= 1)
		{
			D2->startChan = startChan;
		}
		else if(startChan > 0.0)
		{
			D2->startChan = (D2->config[0].nChan*startChan) + 0.5;
		}
		if(D)
		{
			D1 = D;
			if(verbose > 1)
			{
				printf("Merging %s\n", baseFile[i]);
			}
			if(areDifxInputsMergable(D1, D2))
			{
				D = mergeDifxInputs(D1, D2, verbose);
			}
			else
			{
				D = 0;
				fprintf(stderr, "%s is incompatible with "
					"other inputs\n", baseFile[i]);
			}
			deleteDifxInput(D1);
			deleteDifxInput(D2);
			if(!D)
			{
				fprintf(stderr, "merging failed on <%s>.\n",
					baseFile[i]);
				return 0;
			}
		}
		else
		{
			D = D2;
		}
	}

	if(verbose > 2)
	{
		printDifxInput(D);
	}

	D = updateDifxInput(D);

	if(!D)
	{
		fprintf(stderr, "updateDifxInput failed.  Aborting\n");
		return 0;
	}

	if(verbose > 1)
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

	if(!pretend)
	{
		if(fitsWriteOpen(&outfile, fitsFile) < 0)
		{
			deleteDifxInput(D);
			fprintf(stderr, "Cannot open output file\n");
			return 0;
		}

		if(DifxInput2FitsTables(D, &outfile, writemodel, 
			scale, verbose) == D)
		{
			printf("\nConversion successful\n\n");
		}
		
		fitsWriteClose(&outfile);
	}

	deleteDifxInput(D);

	if(nBaseFile > 1)
	{
		printf("*** Warning -- combining multiple files with difx2fits "
			"is still a beta\nfeature.  Please check your results "
			"carefully and report problems to\n%s.  Please always "
			"include the output\nof difx2fits -v -v with any "
			"error report.  Have a groovy day.\n\n", author);
	}
	
	return 0;
}

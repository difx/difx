/***************************************************************************
 *   Copyright (C) 2008-2018 by Walter Brisken & Helge Rottmann            *
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
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <glob.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/resource.h>
#include "difx2fits.h"
#include "util.h"
#include "../config.h"

const char program[] = PACKAGE_NAME;
const char author[]  = PACKAGE_BUGREPORT;
const char version[] = VERSION;

const double DefaultSniffInterval = 30.0;	/* sec */
const double DefaultJobMatrixInterval = 20.0;	/* sec */
const double DefaultDifxTsysInterval = 30.0;	/* sec */
const double DefaultDifxPCalInterval = 30.0;	/* sec */

/* FIXME: someday add option to specify EOP merge mode from command line */

static void usage(const char *pgm)
{
	fprintf(stderr, "\n%s ver. %s   %s\n\n", program, version, author);
	fprintf(stderr, "A program to convert DiFX format data to FITS-IDI\n\n");
	fprintf(stderr, "Usage : %s [options] <baseFilename1> [<baseFilename2> ... ] [<outfile>]\n\n", pgm);
	fprintf(stderr, "It assumed that SWIN format visibility file(s) to be converted live\n");
	fprintf(stderr, "in directory <baseFilename>.difx/\n");
	fprintf(stderr, "It is also assumed that at least 3 additional files exist:\n");
	fprintf(stderr, "  <baseFilename>.input    DiFX input file\n");
	fprintf(stderr, "  <baseFilename>.calc     Base file for calcif \n");
	fprintf(stderr, "  <baseFilename>.im       Polynomial UVW and model\n");
	fprintf(stderr, "One other files is optionally read:\n");
	fprintf(stderr, "  <baseFilename>.flag     Antenna-based flagging\n\n");
	fprintf(stderr, "VLBA calibration transfer will produce 4 files:\n");
	fprintf(stderr, "  flag, tsys, pcal, weather\n");
	fprintf(stderr, "If these are present in the current directory, they will be used to\n");
	fprintf(stderr, "form the FL, TS, PH and WR tables\n\n");
	fprintf(stderr, "If env variable GAIN_CURVE_PATH is set, gain curves will be looked for\n");
	fprintf(stderr, "and turned into a GN table\n\n");
		
	fprintf(stderr, "The output file <outfile> will be written in FITS-IDI format nearly\n");
	fprintf(stderr, "identical to that made at the VLBA HW correlator.  The first two optional\n");
	fprintf(stderr, "files are required for full model accountability.\n");
	fprintf(stderr, "\noptions can include:\n");
	fprintf(stderr, "  --help\n");
	fprintf(stderr, "  -h                  Print this help message\n"); 
	fprintf(stderr, "\n");
	fprintf(stderr, "  --bin        <bin>\n");
	fprintf(stderr, "  -B           <bin>  Select on this pulsar bin number\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --difx\n");
	fprintf(stderr, "  -d                  Run on all .difx files in directory\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --no-model\n");
	fprintf(stderr, "  -n                  Don't write model (ML) table\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --dont-combine\n");
	fprintf(stderr, "  -1                  Don't combine jobs\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --scale <scale>\n");
	fprintf(stderr, "  -s      <scale>     Scale visibility data " "by <scale>\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --deltat <deltat>\n");
	fprintf(stderr, "  -t       <deltat>   Set interval (sec) in printing job matrix (default %3.1f)\n", DefaultJobMatrixInterval);
	fprintf(stderr, "\n");
	fprintf(stderr, "  --difx-tsys-interval\n");
	fprintf(stderr, "  -i       <interval> Set the Difx-derived tsys interval (sec) (default %3.1f)\n", DefaultDifxTsysInterval);
	fprintf(stderr, "\n");
	fprintf(stderr, "  --difx-pcal-interval\n");
	fprintf(stderr, "           <interval> Set the Difx-derived pcal interval (sec) (default %3.1f)\n", DefaultDifxPCalInterval);
	fprintf(stderr, "\n");
	fprintf(stderr, "  --phaseCentre <p>\n");
	fprintf(stderr, "  --phasecenter <p>   Create a fits file for all the " "<p>th phase centres (default 0)\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --keep-order\n");
	fprintf(stderr, "  -k                  Keep antenna order\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --ac-always\n");
	fprintf(stderr, "  -a                  Write standard autocorrelations into every output file\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --profilemode       Don't discard autocorrelations for pulsar bins other than bin 0\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --skip-extra-autocorrs\n");
	fprintf(stderr, "                      Ignore e.g. LL autocorrs in a job with only RR cross-corrs\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --history <file>\n");
	fprintf(stderr, "  -H <file>           Read file <file> and populate FITS History\n");
	fprintf(stderr, "\n");
#ifdef HAVE_FFTW
	fprintf(stderr, "  --sniff-all\n");
	fprintf(stderr, "  -S                  Sniff all bins and centers\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --dont-sniff\n");
	fprintf(stderr, "  -x                  Don't produce sniffer output\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --sniff-time <t>\n");
	fprintf(stderr, "  -T           <t>    Sniff output on a <t> second timescale (default %3.1f)\n", DefaultSniffInterval);
	fprintf(stderr, "\n");
#endif
	fprintf(stderr, "  --union\n");
	fprintf(stderr, "  -u                  Form union of frequency setups\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --max-jobs <max>\n");
	fprintf(stderr, "  -m <max>            Set maximum number of jobs to merge into one FITS file to <max>\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --eop-merge-mode    Set the mode for merging differerent EOPs. Legal modes are strict (default), drop, relaxed.\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --verbose\n");
	fprintf(stderr, "  -v                  Be verbose.  -v -v for more!\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --override-version  Ignore difx versions\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --zero\n");
	fprintf(stderr, "  -0                  Don't put visibility data in FITS file\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --primary-band <pb> Add PRIBAND keyword with value <pb> to FITS file\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "PLEASE file all bug reports at http://svn.atnf.csiro.au/trac/difx .\n");
	fprintf(stderr, "Include at a minimum the output of difx2fits with extra verbosity\n");
	fprintf(stderr, "(that is with -v -v).  The .input, .im & .calc files may help too.\n");
	fprintf(stderr, "\n");
}

struct CommandLineOptions *newCommandLineOptions()
{
	struct CommandLineOptions *opts;

	opts = (struct CommandLineOptions *)calloc(1, sizeof(struct CommandLineOptions));
	
	opts->writemodel = 1;
	opts->sniffTime = DefaultSniffInterval;
	opts->jobMatrixDeltaT = DefaultJobMatrixInterval;
	opts->DifxTsysAvgSeconds = DefaultDifxTsysInterval;
	opts->DifxPcalAvgSeconds = DefaultDifxPCalInterval;

	return opts;
}

void deleteCommandLineOptions(struct CommandLineOptions *opts)
{
	if(opts)
	{
		if(opts->nBaseFile > 0)
		{
			int i;

			for(i = 0; i < opts->nBaseFile; ++i)
			{
				if(opts->baseFile[i])
				{
					free(opts->baseFile[i]);
					opts->baseFile[i] = 0;
				}
			}
		}
		if(opts->fitsFile)
		{
			free(opts->fitsFile);
			opts->fitsFile = 0;
		}
		if(opts->primaryBand)
		{
			free(opts->primaryBand);
			opts->primaryBand = 0;
		}
		if(opts->historyFile)
		{
			free(opts->historyFile);
			opts->historyFile = 0;
		}
		free(opts);
	}
}

/* return 0 on success */
int exceedOpenFileLimit(int numFiles)
{
	struct rlimit limit;
	
	/* Get max number of open files that the OS allows */
	if(getrlimit(RLIMIT_NOFILE, &limit) != 0) 
	{
		printf("Cannot determine user file open limit (errno=%d)\n", errno);

		return 1;
	}
	
	/* Check if the number of DIFX files (plus a buffer of 20) exceed OS limit */
	if(numFiles + 20 >= limit.rlim_cur)
	{
		return 1;
	}

	return 0;
}

struct CommandLineOptions *parseCommandLine(int argc, char **argv)
{
	struct CommandLineOptions *opts;
	int i, l;
	glob_t globBuffer;

	opts = newCommandLineOptions();

	for(i = 1; i < argc; ++i)
	{
		if(argv[i][0] == '-')
		{
			if(strcmp(argv[i], "--no-model") == 0 ||
			   strcmp(argv[i], "-n") == 0)
			{
				opts->writemodel = 0;
			}
			else if(strcmp(argv[i], "--quiet") == 0 ||
			        strcmp(argv[i], "-q") == 0)
			{
				--opts->verbose;
			}
			else if(strcmp(argv[i], "--difx") == 0 ||
			        strcmp(argv[i], "-d") == 0)
			{
				++opts->doalldifx;
			}
			else if(strcmp(argv[i], "--verbose") == 0 ||
			        strcmp(argv[i], "-v") == 0)
			{
				++opts->verbose;
			}
			else if(strcmp(argv[i], "--union") == 0 ||
			        strcmp(argv[i], "-u") == 0)
			{
				opts->mergeOptions.freqMergeMode = FreqMergeModeUnion;
				fprintf(stderr, "\nWarning: using mode that merges all frequency setups that are encountered into one master frequency setup.  This is experimental at this time and in most cases is not what you want!  GMVA and RadioAstron correlation are known cases where this should be a useful capability.\n\n");
			}
			else if(strcmp(argv[i], "--zero") == 0 ||
			        strcmp(argv[i], "-0") == 0)
			{
				opts->dontIncludeVisibilities = 1;
			}
#ifdef HAVE_FFTW
			else if(strcmp(argv[i], "--dont-sniff") == 0 ||
			        strcmp(argv[i], "-x") == 0)
			{
				opts->sniffTime = -1.0;
			}
			else if(strcmp(argv[i], "--sniff-all") == 0 ||
			        strcmp(argv[i], "-S") == 0)
			{
				opts->sniffAllBins = 1;
				opts->sniffAllPhaseCentres = 1;
			}
#endif
			else if(strcmp(argv[i], "--dont-combine") == 0 ||
			        strcmp(argv[i], "-1") == 0)
			{
				opts->dontCombine = 1;
			}
			else if(strcmp(argv[i], "--pretend") == 0 ||
			        strcmp(argv[i], "-p") == 0)
			{
				opts->pretend = 1;
			}
			else if(strcmp(argv[i], "--help") == 0 ||
			        strcmp(argv[i], "-h") == 0)
			{
				usage(program);
				deleteCommandLineOptions(opts);
				return 0;
			}
			else if(strcmp(argv[i], "--keep-order") == 0 ||
			        strcmp(argv[i], "-k") == 0)
			{
				opts->keepOrder = 1;
			}
			else if(strcmp(argv[i], "--ac-always") == 0 ||
			        strcmp(argv[i], "-a") == 0)
			{
				opts->alwaysWriteAutocorr = 1;
			}
			else if(strcmp(argv[i], "--profilemode") == 0)
			{
				opts->profileMode = 1;
			}
			else if(strcmp(argv[i], "--override-version") == 0)
			{
				opts->overrideVersion = 1;
			}
			else if(strcmp(argv[i], "--skip-extra-autocorrs") == 0)
			{
				opts->skipExtraAutocorrs = 1;
			}
			else if(i+1 < argc) /* one parameter arguments */
			{
				if(strcmp(argv[i], "--scale") == 0 ||
				   strcmp(argv[i], "-s") == 0)
				{
					++i;
					opts->scale = atof(argv[i]);
					printf("Scaling data by %f\n", opts->scale);
				}
				else if(strcmp(argv[i], "--deltat") == 0 ||
				        strcmp(argv[i], "-t") == 0)
				{
					++i;
					opts->jobMatrixDeltaT = atof(argv[i]);
				}
				else if(strcmp(argv[i], "--max-jobs") == 0 ||
				        strcmp(argv[i], "-m") == 0)
				{
					++i;
					opts->maxJobsToMerge = atoi(argv[i]);
				}
				else if(strcmp(argv[i], "--difx-tsys-interval") == 0 ||
				        strcmp(argv[i], "-i") == 0)
				{
					++i;
					opts->DifxTsysAvgSeconds = atof(argv[i]);
				}
				else if(strcmp(argv[i], "--eop-merge-mode") == 0)
				{
					++i;
					if (strcmp(argv[i], "strict") == 0)
					{
						opts->mergeOptions.eopMergeMode = EOPMergeModeStrict;
					}
					else if (strcmp(argv[i], "drop") == 0)
					{
						opts->mergeOptions.eopMergeMode = EOPMergeModeLoose;
						fprintf(stderr, "\nWarning: using mode that drops all EOPs, allowing merging of files including incompatible EOP values.\n\n");
					}
					else if (strcmp(argv[i], "relaxed") == 0)
					{
						opts->mergeOptions.eopMergeMode = EOPMergeModeRelaxed;
					}
					else
					{
						fprintf(stderr, "Illegal EOP merge mode: %s\n", argv[i]);
						fprintf(stderr, "Legal values are: drop relaxed strict\n");
							
						deleteCommandLineOptions(opts);
						return 0;
					}
					
				}
				else if(strcmp(argv[i], "--difx-pcal-interval") == 0)
				{
					++i;
					opts->DifxPcalAvgSeconds = atof(argv[i]);
				}
				else if(strcmp(argv[i], "--sniff-time") == 0 ||
				        strcmp(argv[i], "-T") == 0)
				{
					++i;
					opts->sniffTime = atof(argv[i]);
				}
				else if(strcmp(argv[i], "--bin") == 0 ||
				        strcmp(argv[i], "-B") == 0)
				{
					++i;
					opts->pulsarBin = atoi(argv[i]);
				}
				else if(strcasecmp(argv[i], "--phaseCentre") == 0 ||
				        strcasecmp(argv[i], "--phasecenter") == 0)
				{
					++i;
					opts->phaseCentre = atoi(argv[i]);
				}
				else if(strcmp(argv[i], "--primary-band") == 0)
				{
					++i;

					opts->primaryBand = strdup(argv[i]);
				}
				else if(strcmp(argv[i], "--history") == 0 ||
				        strcmp(argv[i], "-H") == 0)
				{
					++i;
					opts->historyFile = strdup(argv[i]);
				}
				else
				{
					printf("Unknown param %s\n", argv[i]);
					deleteCommandLineOptions(opts);

					return 0;
				}
			}
			else
			{
				printf("Unknown param %s\n", argv[i]);
				deleteCommandLineOptions(opts);

				return 0;
			}
		}
		else
		{
	
			if(exceedOpenFileLimit(opts->nBaseFile))
			{
				printf("Error: The number of input files exceeds the OS limit of allowed open files!\n");
				printf("Run ulimit -n to increase that number.\n");
				printf("Note: This might require increasing the hard limit in /etc/security/limits.conf\n");
				deleteCommandLineOptions(opts);

				return 0;
			}
	
			if(opts->nBaseFile >= MAX_INPUT_FILES)
			{
				printf("Error: too many input files!\n");
				printf("Max = %d\n", MAX_INPUT_FILES);
				deleteCommandLineOptions(opts);

				return 0;
			}
			l = strlen(argv[i]);
			if(l > 5 && strcasecmp(argv[i]+l-5, ".FITS") == 0)
			{
				opts->fitsFile = strdup(argv[i]);
			}
			else
			{
				opts->baseFile[opts->nBaseFile] = strdup(argv[i]);
				++opts->nBaseFile;
			}
		}
	}

	if(opts->profileMode > 0 && opts->alwaysWriteAutocorr > 0)
	{
		printf("Error: Setting profilemode at the same time as ac-always is not allowed\n");
		printf("If this is a pulsar profile mode correlation, then you don't want ac-always\n");
		printf("If this is not a pulsar profile mode correlation, don't use --profilemode\n");

		deleteCommandLineOptions(opts);

		return 0;
	}

	if((opts->nBaseFile >  0 && opts->doalldifx >  0) ||
	   (opts->nBaseFile == 0 && opts->doalldifx == 0))
	{
		deleteCommandLineOptions(opts);

		return 0;
	}

	if(opts->doalldifx)
	{
		glob2(__FUNCTION__, "*.difx", 0, 0, &globBuffer);
		if(exceedOpenFileLimit(globBuffer.gl_pathc))
		{
			printf("Error: The number of input files exceeds the OS limit of allowed open files!\n");
			printf("Run ulimit -n to increase that number.\n");
			printf("Note: This might require increasing the hard limit in /etc/security/limits.conf\n");
			deleteCommandLineOptions(opts);

			return 0;
		}

		if(globBuffer.gl_pathc > MAX_INPUT_FILES)
		{
			printf("Error: too many input files!\n");
			printf("Max = %d\n", MAX_INPUT_FILES);
			deleteCommandLineOptions(opts);
			
			return 0;
		}
		opts->nBaseFile = globBuffer.gl_pathc;
		for(i = 0; i < opts->nBaseFile; ++i)
		{
			opts->baseFile[i] = strdup(globBuffer.gl_pathv[i]);
		}
		globfree(&globBuffer);
	}

	if(opts->nBaseFile > 0 && opts->dontCombine && opts->fitsFile)
	{
		printf("Error: Cannot supply output filename for multiple output files.\n");
		deleteCommandLineOptions(opts);

		return 0;
	}

	/* if input file ends in .difx, trim it */
	for(i = 0; i < opts->nBaseFile; ++i)
	{
		l = strlen(opts->baseFile[i]);
		if(l < 6)
		{
			continue;
		}
		if(strcmp(opts->baseFile[i]+l-5, ".difx") == 0)
		{
			opts->baseFile[i][l-5] = 0;
		}
	}

	opts->mergeOptions.verbose = opts->verbose;

	return opts;
}

static int populateFitsKeywords(const DifxInput *D, struct fits_keywords *keys)
{
	int i, j;
	int fqindex=-1;

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
		fprintf(stderr, "Error: unknown polarization (%c)\n", D->polPair[0]);

		exit(EXIT_FAILURE);
	}
	keys->no_band = D->nIF;
	if(D->nIF > array_MAX_BANDS)
	{
		fprintf(stderr, "Error: too many (%d) IFs in this data.  This program is compiled for only %d.\n", D->nIF, array_MAX_BANDS);

		exit(EXIT_FAILURE);
	}
	keys->no_chan = -1;
	keys->chan_bw = -1;
	keys->ref_pixel = -1;
	for(i = 0; i < D->nBaseline; ++i)
	{
		for(j = 0; j < D->baseline[i].nFreq; ++j)
		{
			double d_tmp;
			
			fqindex = DifxInputGetFreqIdByBaselineFreq(D, i, j);
			if(fqindex < 0)
			{
				fprintf(stderr, "Error: populateFitsKeywords: fqindex=%d for baseline=%d freq=%d\n", fqindex, i, j);

				exit(EXIT_FAILURE);
			}

			if(keys->no_chan == -1)
			{
				keys->no_chan = D->freq[fqindex].nChan/D->freq[fqindex].specAvg;
			}
			else if(D->freq[fqindex].nChan/D->freq[fqindex].specAvg != keys->no_chan)
			{
				fprintf(stderr, "Error: populateFitsKeywords: not all used frequencies have the same number of output channels: %d/%d != %d\n", D->freq[fqindex].nChan, D->freq[fqindex].specAvg, keys->no_chan);

				exit(EXIT_FAILURE);
			}

			/* extra precision to deal with non ^2 rounding errors */
			d_tmp = 1.0e6*D->freq[fqindex].bw*D->freq[fqindex].specAvg/D->freq[fqindex].nChan;
			if(keys->chan_bw == -1)
			{
				keys->chan_bw = (float)d_tmp;
			}
			else if(keys->chan_bw != (float)d_tmp)
			{
				fprintf(stderr, "Error: populateFitsKeywords: not all used frequencies have the same final channel bandwidth\n");

				exit(EXIT_FAILURE);
			}

			/* extra precision to deal with non ^2 rounding errors */
			d_tmp = 0.5 + 1.0/(2.0*D->freq[fqindex].specAvg*D->specAvg);
			if(keys->ref_pixel == -1)
			{
				keys->ref_pixel = (float)d_tmp;
			}
			else if(keys->ref_pixel != (float)d_tmp)
			{
				fprintf(stderr, "Error: populateFitsKeywords: not all used frequencies have the same reference pixel\n");

				exit(EXIT_FAILURE);
			}
		}
	}
	if(keys->no_chan == -1 || fqindex == -1 || keys->chan_bw == -1 || keys->ref_pixel == -1)
	{
		fprintf(stderr, "Error: populateFitsKeywords: Didn't find any used frequencies\n");

		exit(EXIT_FAILURE);
	}
	keys->ref_freq = D->refFreq*1.0e6;
	keys->ref_date = D->mjdStart;

#ifdef DEBUG
	printf("Channel bandwidth is %f, ref pixel is %f\n", keys->chan_bw, keys->ref_pixel);
#endif

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

static const DifxInput *DifxInput2FitsTables(const DifxInput *D, struct fitsPrivate *out, const struct CommandLineOptions *opts)
{
	struct fits_keywords keys;
	long long last_bytes = 0;

	populateFitsKeywords(D, &keys);

	if(opts->verbose > 0)
	{
		printf("\n");
		printFitsKeywords(&keys);
	}
	
	printf("\nWriting FITS tables:\n");

	printf("  Header                    ");
	fflush(stdout);
	D = DifxInput2FitsHeader(D, out, opts);
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
	D = DifxInput2FitsFR(D, &keys, out, opts);
	printf("%lld bytes\n", out->bytes_written - last_bytes);
	last_bytes = out->bytes_written;

	printf("  ML -- model               ");
	fflush(stdout);
	D = DifxInput2FitsML(D, &keys, out, opts->phaseCentre);
	printf("%lld bytes\n", out->bytes_written - last_bytes);
	last_bytes = out->bytes_written;

	printf("  CT -- correlator (eop)    ");
	fflush(stdout);
	D = DifxInput2FitsCT(D, &keys, out);
	printf("%lld bytes\n", out->bytes_written - last_bytes);
	last_bytes = out->bytes_written;

	printf("  MC -- model components    ");
	fflush(stdout);
	D = DifxInput2FitsMC(D, &keys, out, opts->phaseCentre);
	printf("%lld bytes\n", out->bytes_written - last_bytes);
	last_bytes = out->bytes_written;

	printf("  SO -- spacecraft orbit    ");
	fflush(stdout);
	D = DifxInput2FitsSO(D, &keys, out);
	if(out->bytes_written == last_bytes)
	{
		printf("No table written\n");
	}
	else
	{
		printf("%lld bytes\n", out->bytes_written - last_bytes);
		last_bytes = out->bytes_written;
	}

	printf("  GD -- pulsar gate duty    ");
	fflush(stdout);
	D = DifxInput2FitsGD(D, &keys, out);
	if(out->bytes_written == last_bytes)
	{
		printf("No table written\n");
	}
	else
	{
		printf("%lld bytes\n", out->bytes_written - last_bytes);
		last_bytes = out->bytes_written;
	}

	printf("  GM -- pulsar gate model   ");
	fflush(stdout);
	D = DifxInput2FitsGM(D, &keys, out, opts);
	if(out->bytes_written == last_bytes)
	{
		printf("No table written\n");
	}
	else
	{
		printf("%lld bytes\n", out->bytes_written - last_bytes);
		last_bytes = out->bytes_written;
	}

	printf("  UV -- visibility          \n");
	fflush(stdout);
	D = DifxInput2FitsUV(D, &keys, out, opts);
	printf("%lld bytes\n", out->bytes_written - last_bytes);
	last_bytes = out->bytes_written;

	printf("  FL -- flag                ");
	fflush(stdout);
	D = DifxInput2FitsFL(D, &keys, out);
	if(out->bytes_written == last_bytes)
	{
		printf("No table written\n");
	}
	else
	{
		printf("%lld bytes\n", out->bytes_written - last_bytes);
		last_bytes = out->bytes_written;
	}

	printf("  TS -- system temperature  ");
	fflush(stdout);
	D = DifxInput2FitsTS(D, &keys, out, opts->phaseCentre, opts->DifxTsysAvgSeconds);
	if(out->bytes_written == last_bytes)
	{
		printf("No table written\n");
	}
	else
	{
		printf("%lld bytes\n", out->bytes_written - last_bytes);
		last_bytes = out->bytes_written;
	}

	printf("  PH -- phase cal           ");
	fflush(stdout);
	D = DifxInput2FitsPH(D, &keys, out, opts->phaseCentre, opts->DifxPcalAvgSeconds, opts->verbose);
	printf("                            ");
	if(out->bytes_written == last_bytes)
	{
		printf("No table written\n");
	}
	else
	{
		printf("%lld bytes\n", out->bytes_written - last_bytes);
		last_bytes = out->bytes_written;
	}

	printf("  WR -- weather             ");
	fflush(stdout);
	D = DifxInput2FitsWR(D, &keys, out);
	if(out->bytes_written == last_bytes)
	{
		printf("No table written\n");
	}
	else
	{
		printf("%lld bytes\n", out->bytes_written - last_bytes);
		last_bytes = out->bytes_written;
	}

	printf("  GN -- gain curve          ");
	fflush(stdout);
	D = DifxInput2FitsGN(D, &keys, out);
	if(out->bytes_written == last_bytes)
	{
		printf("No table written\n");
	}
	else
	{
		printf("%lld bytes\n", out->bytes_written - last_bytes);
		last_bytes = out->bytes_written;
	}

	printf("                            -----\n");
	printf("  Total                     %lld bytes\n", last_bytes);

	return D;
}

static void deleteDifxInputSet(DifxInput **Dset, int n)
{
	if(Dset)
	{
		int i;

		for(i = 0; i < n; ++i)
		{
			if(Dset[i])
			{
				deleteDifxInput(Dset[i]);
				Dset[i] = 0;
			}
		}
		free(Dset);
	}
}

static DifxInput **loadDifxInputSet(const struct CommandLineOptions *opts)
{
	DifxInput **Dset;
	int i;

	Dset = (DifxInput **)calloc(opts->nBaseFile, sizeof(DifxInput *));
	if(!Dset)
	{
		fprintf(stderr, "Error: cannot allocate %d bytes for %d DifxInput structures\n", (int)(opts->nBaseFile*sizeof(DifxInput *)), opts->nBaseFile);

		return 0;
	}

	for(i = 0; i < opts->nBaseFile; ++i)
	{
		if(opts->verbose > 1)
		{
			printf("Loading %s\n", opts->baseFile[i]);
		}

		Dset[i] = loadDifxInput(opts->baseFile[i]);
		if(!Dset[i])
		{
			fprintf(stderr, "loadDifxInput failed on <%s>.\n", opts->baseFile[i]);

			deleteDifxInputSet(Dset, opts->nBaseFile);

			return 0;
		}

		Dset[i] = updateDifxInput(Dset[i], &opts->mergeOptions);

		if(opts->specAvg)
		{
			Dset[i]->specAvg = opts->specAvg;
		}
		if(opts->nOutChan >= 1)
		{
			Dset[i]->nOutChan = opts->nOutChan;
		}
		else if(opts->nOutChan > 0.0) /* interpret in fractional sense */
		{
			Dset[i]->nOutChan = Dset[i]->freq[0].nChan*opts->nOutChan/Dset[i]->freq[0].specAvg;
		}
		if(opts->startChan >= 1)
		{
			Dset[i]->startChan = opts->startChan;
		}
		else if(opts->startChan > 0.0)
		{
			Dset[i]->startChan = (Dset[i]->freq[0].nChan*opts->startChan) + 0.5;
		}
	}

	return Dset;
}

/* FIXME: use opts->eopMergeMode to drive merging of files */
static int convertFits(const struct CommandLineOptions *opts, DifxInput **Dset, int passNum, int *nWithoutPhaseCentre)
{
	DifxInput *D;
	struct fitsPrivate outfile;
	char outFitsName[DIFXIO_FILENAME_LENGTH];
	int i, c;
	int nConverted = 0;
	const char *difxVersion;
	const char *difxLabel;

	difxVersion = getenv("DIFX_VERSION");
	if(!difxVersion)
	{
		printf("Warning: env. var. DIFX_VERSION is not set.\n");
	}
	difxLabel = getenv("DIFX_LABEL");
	if(!difxLabel && opts->verbose > 0)
	{
		printf("Note: env. var. DIFX_LABEL is not set.\n");
	}

	D = 0;

	for(i = 0; i < opts->nBaseFile; ++i)
	{
		DifxInput *D2;

		D2 = Dset[i];
		if(Dset[i] == 0)
		{
			/* must have already been merged */
			continue;
		}

		if(DifxInputGetMaxPhaseCentres(D2) <= opts->phaseCentre)
		{
			if(opts->verbose > 0)
			{
				printf("Skipping %s because it doesn't contain phase centre %d\n", opts->baseFile[i], opts->phaseCentre);
			}

			/* signal not to try this one again */
			deleteDifxInput(D2);
			D2 = Dset[i] = 0;

			++(*nWithoutPhaseCentre);

			continue;
		}

		if(D)
		{
			DifxInput *D1;

			D1 = D;

			if(!areDifxInputsCompatible(D1, D2, &opts->mergeOptions))
			{
				continue;
			}
			else if(opts->verbose > 1)
			{
				printf("Merging %s\n", opts->baseFile[i]);
			}

			D = mergeDifxInputs(D1, D2, &opts->mergeOptions);

			if(D->nEOP < D1->nEOP || D->nEOP < D2->nEOP)
			{
				fprintf(stderr, "  Warning: merging of EOP table from file %s failed.  EOP data will be lost for entire merged job set.\n", opts->baseFile[i]);
			}

			deleteDifxInput(D1);
			deleteDifxInput(D2);
			D1 = D2 = 0;

			if(!D)
			{
				fprintf(stderr, "Merging failed on <%s>.\n", opts->baseFile[i]);

				return 0;
			}
		}
		else
		{
			D = D2;
		}

		/* Dset[i] was absorbed and will be freed automatically at end of conversion */
		/* Setting to zero prevents future reuse of this job */
		Dset[i] = 0;

		++nConverted;
		if(opts->dontCombine)
		{
			break;
		}
		if(opts->maxJobsToMerge > 0 && nConverted >= opts->maxJobsToMerge)
		{
			break;
		}
	}

	if(!D)
	{
		return 0;
	}

	if(opts->verbose > 2)
	{
		printDifxInput(D);
	}

	D = updateDifxInput(D, &opts->mergeOptions);

	if(!D)
	{
		fprintf(stderr, "updateDifxInput failed.  Aborting\n");

		return 0;
	}

	for(c = 0; c < D->nConfig; ++c)
	{
		if((D->config[c].polMask & DIFXIO_POL_RL) && (D->config[c].polMask & DIFXIO_POL_XY))
		{
			fprintf(stderr, "Error: both linear and circular polarizations are present.  This combination is unsupported by FITS.\n");

			return 0;
		}

		if(D->config[c].polMask & DIFXIO_POL_ERROR)
		{
			fprintf(stderr, "Error: an unknown polarization (not R, L, X or Y) is present.  FITS cannot allow other types.\n");

			return 0;
		}
	}

	if(difxVersion && D->job->difxVersion[0])
	{
		if(strncmp(difxVersion, D->job->difxVersion, DIFXIO_VERSION_LENGTH))
		{
			fprintf(stderr, "Attempting to run difx2fits from version %s on a job make for version %s\n", difxVersion, D->job->difxVersion);
			if(opts->overrideVersion)
			{
				fprintf(stderr, "Continuing because of --override-version but not setting a version\n");
				D->job->difxVersion[0] = 0;
			}
			else
			{
				fprintf(stderr, "Not converting.\n");
				deleteDifxInput(D);

				return 0;
			}
		}
	}
	else if(!D->job->difxVersion[0])
	{
		fprintf(stderr, "Warning: working on unversioned job\n");
	}

	/* Check user-defined DIFX LABEL and complain if discrepancies are found */
	if(difxLabel && D->job->difxLabel[0])
	{
		if(strncmp(difxLabel, D->job->difxLabel, DIFXIO_VERSION_LENGTH))
		{
			fprintf(stderr, "Attempting to run difx2fits from label %s on a job make for label %s\n", difxLabel, D->job->difxLabel);
			if(opts->overrideVersion)
			{
				fprintf(stderr, "Continuing because of --override-version but not setting a version\n");
				D->job->difxLabel[0] = 0;
			}
			else
			{
				fprintf(stderr, "Not converting.\n");
				deleteDifxInput(D);

				return 0;
			}
		}
	}

	if(opts->verbose > 1)
	{
		printDifxInput(D);
	}

	if(D->nIF <= 0 || D->nPolar <= 0)
	{
		fprintf(stderr, "Data geometry changes during obs, cannot make into FITS.\n");
		deleteDifxInput(D);

		return 0;
	}

	if(opts->fitsFile)
	{
		if(passNum == 0)
		{
			strcpy(outFitsName, opts->fitsFile);
		}
		else
		{
#warning "FIXME: multi-setup input would overwrite previously generated FITS, make tidier fix than a rename"
			sprintf(outFitsName, "%s_setup%d", opts->fitsFile, passNum+1);
			fprintf(stderr, "Warning: input file list contains difx output with different setup(s). Writing setup %d to '%s'\n", passNum+1, outFitsName);
		}
	}
	else
	{
		sprintf(outFitsName, "%s%s.%d.bin%04d.source%04d.FITS",
			D->job[0].obsCode,
			D->job[0].obsSession,
			passNum,
			opts->pulsarBin,
			opts->phaseCentre);
	}

	if(!opts->pretend)
	{
		if(!opts->keepOrder)
		{
			DifxInputSortAntennas(D, opts->verbose);
		}

		if(opts->verbose > 2)
		{
			printDifxInput(D);
		}

		if(fitsWriteOpen(&outfile, outFitsName) < 0)
		{
			deleteDifxInput(D);
			fprintf(stderr, "Cannot open output file\n");

			return 0;
		}

		if(DifxInput2FitsTables(D, &outfile, opts) == D)
		{
			printf("\nConversion successful\n\n");
		}
		
		fitsWriteClose(&outfile);
	}

	deleteDifxInput(D);

	return nConverted;
}

int main(int argc, char **argv)
{
	struct CommandLineOptions *opts;
	int nConverted = 0;
	int nWithoutPhaseCentre = 0;
	int nFits = 0;
	DifxInput **Dset;
	unsigned int n;

	if(argc < 2)
	{
		usage(argv[0]);

		return EXIT_FAILURE;
	}

	if(getenv("DIFX_GROUP_ID"))
	{
		umask(2);
	}

	opts = parseCommandLine(argc, argv);
	if(opts == 0)
	{
		return EXIT_FAILURE;
	}

	resetDifxInputCompatibilityStatistics();

	Dset = loadDifxInputSet(opts);

	if(Dset == 0)
	{
		deleteCommandLineOptions(opts);

		return EXIT_FAILURE;
	}

	for(;;)
	{
		int n;

		n = convertFits(opts, Dset, nFits, &nWithoutPhaseCentre);
		if(n <= 0)
		{
			break;
		}
		nConverted += n;
		++nFits;
	}

	deleteDifxInputSet(Dset, opts->nBaseFile);

	printf("%d of %d jobs converted to %d FITS files\n", nConverted, opts->nBaseFile, nFits);
	if(nWithoutPhaseCentre > 0)
	{
		printf("%d of %d jobs lacked requested phase centre (%d)\n", nWithoutPhaseCentre, opts->nBaseFile, opts->phaseCentre);
	}

	printf("\n");
	n = printDifxInputCompatibilityStatistics(opts->verbose);
	if(n > 1 || opts->verbose > 0)
	{
		printf("\n");
	}

	if(nConverted + nWithoutPhaseCentre != opts->nBaseFile)
	{
		printf("\n*** Warning: not all input files converted!\n");
	}

	printf("\n");
	
	deleteCommandLineOptions(opts);

	return EXIT_SUCCESS;
}

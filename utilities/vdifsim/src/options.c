#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include <mpi.h>
#include "config.h"
#include "options.h"

const char program[] = PACKAGE_NAME;
const char author[]  = PACKAGE_BUGREPORT;
const char version[] = VERSION;

void usage(const CommandLineOptions *opts)
{
	fprintf(stderr, "\n%s ver. %s   %s\n\n", program, version, author);
	fprintf(stderr, "Usage : %s [options] <inputFile> [ <inputFile> ... ]\n\n", opts->program);
	fprintf(stderr, "options can include:\n");
	fprintf(stderr, "  --help\n");
	fprintf(stderr, "  -h                  Print this help message\n\n");
	fprintf(stderr, "  --verbose\n");
	fprintf(stderr, "  -v                  Be verbose.  -v -v for more!\n\n");
	fprintf(stderr, "  --force\n");
	fprintf(stderr, "  -f                  Continue, even if output files exist\n\n");
	fprintf(stderr, "  --debug\n");
	fprintf(stderr, "  -d                  Write some debug information to files\n\n");
	fprintf(stderr, "  --test\n");
	fprintf(stderr, "  -t                  Enter test mode\n\n");
	fprintf(stderr, "  --message\n");
	fprintf(stderr, "  -m                  Broadcast status using difxmessage\n\n");
	fprintf(stderr, "  --config <file>\n");
	fprintf(stderr, "  -c <file>           Read <file> as a configuration file\n\n");
	fprintf(stderr, "  --seed <seed>\n");
	fprintf(stderr, "  -s <seed>           Set random number seed; default is random\n\n");
};

void resetCommandLineOptions(CommandLineOptions *opts)
{
	memset(opts, 0, sizeof(CommandLineOptions));
	opts->fluxDensity = DEFAULT_FLUX_DENSITY;
	opts->SEFD = DEFAULT_SEFD;
	opts->filterTransition = DEFAULT_FILTER_TRANSITION;
	opts->nProcess = 1;
}

CommandLineOptions *newCommandLineOptions(int argc, char **argv)
{
	CommandLineOptions *opts;
	int a;
	int i;

	opts = (CommandLineOptions *)malloc(sizeof(CommandLineOptions));
	if(!opts)
	{
		fprintf(stderr, "Error: newCommandLineOptions: cannot allocate %d bytes.\n", (int)sizeof(CommandLineOptions));

		exit(EXIT_FAILURE);
	}
	resetCommandLineOptions(opts);

	MPI_Comm_size(MPI_COMM_WORLD, &opts->nProcess);
	if(opts->nProcess < 1)
	{
		opts->nProcess = 1;
	}

	opts->version = version;
	opts->program = argv[0];
	for(i = 0; argv[0][i]; ++i)
	{
		if(argv[0][i] == '/')
		{
			opts->program = argv[0] + i + 1;
		}
	}

	opts->inputFile = (const char **)malloc((argc-1)*sizeof(const char *));
	if(!opts->inputFile)
	{
		fprintf(stderr, "Error: cannot allocate space for %d input filename pointers.\n", argc-1);

		exit(EXIT_FAILURE);
	}

	for(a = 1; a < argc; ++a)
	{
		if(argv[a][0] == '-')
		{
			if(strcmp(argv[a], "--verbose") == 0 || strcmp(argv[a], "-v") == 0)
			{
				++opts->verbose;
			}
			else if(strcmp(argv[a], "--quiet") == 0 || strcmp(argv[a], "-q") == 0)
			{
				--opts->verbose;
			}
			else if(strcmp(argv[a], "--help") == 0 || strcmp(argv[a], "-h") == 0)
			{
				++opts->usage;
			}
			else if(strcmp(argv[a], "--force") == 0 || strcmp(argv[a], "-f") == 0)
			{
				++opts->force;
			}
			else if(strcmp(argv[a], "--debug") == 0 || strcmp(argv[a], "-d") == 0)
			{
				++opts->debug;
			}
			else if(strcmp(argv[a], "--test") == 0 || strcmp(argv[a], "-t") == 0)
			{
				opts->testMode = 1;
			}
			else if(strcmp(argv[a], "--message") == 0 || strcmp(argv[a], "-m") == 0)
			{
				opts->useDifxMessage = 1;
			}
			else if(a < argc - 1)
			{
				if(strcmp(argv[a], "--config") == 0 || strcmp(argv[a], "-c") == 0)
				{
					++a;
					opts->configFile = argv[a];
				}
				else if(strcmp(argv[a], "--seed") == 0 || strcmp(argv[a], "-s") == 0)
				{
					++a;
					opts->randSeed = atoi(argv[a]);
					if(opts->randSeed <= 0)
					{
						fprintf(stderr, "Error: random number seed must be a positive integer.  %s was provided.\n", argv[a]);
						deleteCommandLineOptions(opts);

						return 0;
					}
				}
				else
				{
					fprintf(stderr, "Unknown parameter %s\n", argv[a]);
					deleteCommandLineOptions(opts);

					return 0;
				}
			}
			else
			{
				fprintf(stderr, "Unknown parameter %s\n", argv[a]);
				deleteCommandLineOptions(opts);

				return 0;
			}
		}
		else
		{
			opts->inputFile[opts->nInputFile++] = argv[a];
		}
	}

	if(opts->testMode > 0)
	{
		/* This set is for test mode */

		printf("RUNNING IN TEST MODE.\n");
	}

	return opts;
}

void deleteCommandLineOptions(CommandLineOptions *opts)
{
	if(opts)
	{
		if(opts->inputFile)
		{
			free(opts->inputFile);
		}
		free(opts);
	}
	else
	{
		fprintf(stderr, "Warning: deleteCommandLineOptions: null pointer\n");
	}
}

void printCommandLineOptions(const CommandLineOptions *opts)
{
	int i;

	printf("Command Line Options\n\n");
	printf("  verbose         = %d\n", opts->verbose);
	printf("  usage           = %d\n", opts->usage);
	printf("  program         = %s\n", opts->program);
	printf("  test mode       = %d\n", opts->testMode);
	printf("  nInputFile      = %d\n", opts->nInputFile);
	for(i = 0; i < opts->nInputFile; ++i)
	{
		printf("    %s\n", opts->inputFile[i]);
	}
}

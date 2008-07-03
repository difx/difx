#include <stdio.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <fcntl.h>
#include <rpc/rpc.h>
#include <glob.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include "MATHCNST.H"
#include "CALCServer.h"
#include "difxio.h"
#include "config.h"

#define MAX_FILES	2048

const char program[] = "calcif2";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.1";
const char verdate[] = "20080703";

typedef struct
{
	int verbose;
	int force;
	int nFile;
	char *files[MAX_FILES];
} CommandLineOptions;

int usage()
{
	printf("%s ver. %s  %s  %s\n", program, version, author, verdate);
	return 0;
}

void deleteCommandLineOptions(CommandLineOptions *opts)
{
	int i;

	if(!opts)
	{
		return;
	}

	for(i = 0; i < opts->nFile; i++)
	{
		free(opts->files[i]);
	}

	free(opts);
}

CommandLineOptions *newCommandLineOptions(int argc, char **argv)
{
	CommandLineOptions *opts;
	int i;

	opts = (CommandLineOptions *)calloc(1, sizeof(CommandLineOptions));

	for(i = 1; i < argc; i++)
	{
		if(strcmp(argv[i], "-v") == 0 ||
		   strcmp(argv[i], "--verbose") == 0)
		{
			opts->verbose++;
		}
		else if(strcmp(argv[i], "-f") == 0 ||
		        strcmp(argv[i], "--force") == 0)
		{
			opts->force++;
		}
		else if(strcmp(argv[i], "-h") == 0 ||
			strcmp(argv[i], "--help") == 0)
		{
			usage();
			deleteCommandLineOptions(opts);
			return 0;
		}
		else if(argv[i][0] == '-')
		{
			printf("Illegal option : %s\n", argv[i]);
			deleteCommandLineOptions(opts);
			return 0;
		}
		else
		{
			opts->files[opts->nFile] = strdup(argv[i]);
			opts->nFile++;
			if(opts->nFile >= MAX_FILES)
			{
				printf("Too many files (%d max)\n", MAX_FILES);
				deleteCommandLineOptions(opts);
				return 0;
			}
		}
	}

	return opts;
}

int runfile(const char *prefix, const CommandLineOptions *opts)
{
	DifxInput *D;

	D = loadDifxCalc(prefix);
	D = updateDifxInput(D);
	if(opts->verbose > 1)
	{
		printDifxInput(D);
	}
	deleteDifxInput(D);

	return 0;
}

int run(const CommandLineOptions *opts)
{
	int i, l;

	if(opts == 0)
	{
		return 0;
	}

	for(i = 0; i < opts->nFile; i++)
	{
		l = strlen(opts->files[i]);
		if(l > 6)
		{
			if(strcmp(opts->files[i]+l-6, ".input") == 0)
			{
				opts->files[i][l-6] = 0;
			}
			else if(strcmp(opts->files[i]+l-5, ".calc") == 0)
			{
				opts->files[i][l-5] = 0;
			}
		}
		if(opts->verbose > 0)
		{
			printf("Processing file %d/%d = %s\n",
				i+1, opts->nFile, opts->files[i]);
		}
		runfile(opts->files[i], opts);
	}
}

int main(int argc, char **argv)
{
	CommandLineOptions *opts;

	opts = newCommandLineOptions(argc, argv);

	run(opts);

	deleteCommandLineOptions(opts);

	return 0;
}

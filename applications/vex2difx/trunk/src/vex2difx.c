#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "../config.h"

const char program[] = PACKAGE_NAME;
const char author[]  = PACKAGE_BUGREPORT;
const char version[] = VERSION;


static int usage(const char *pgm)
{
	fprintf(stderr, "\n%s ver. %s   %s\n\n",
		program, version, author);

	return 0;
}

struct CommandLineOptions
{
	int verbose;
};

struct CommandLineOptions *newCommandLineOptions()
{
	struct CommandLineOptions *opts;

	opts = (struct CommandLineOptions *)calloc(1, 
		sizeof(struct CommandLineOptions));
	
	return opts;
}

void deleteCommandLineOptions(struct CommandLineOptions *opts)
{
	if(opts)
	{
		free(opts);
	}
}

struct CommandLineOptions *parseCommandLine(int argc, char **argv)
{
	struct CommandLineOptions *opts;
	int i, l;

	opts = newCommandLineOptions();

	for(i = 1; i < argc; i++)
	{
		if(argv[i][0] == '-')
		{
			if(strcmp(argv[i], "--verbose") == 0 ||
			        strcmp(argv[i], "-v") == 0)
			{
				opts->verbose++;
			}
			else if(i+1 < argc) /* one parameter arguments */
			{
				if(strcmp(argv[i], "--FIXME") == 0 ||
				   strcmp(argv[i], "-F") == 0)
				{
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
		}
	}

	return opts;
}

int main(int argc, char **argv)
{
	struct CommandLineOptions *opts;
	int nConverted = 0;
	int n, nFits = 0;

	if(argc < 2)
	{
		return usage(argv[0]);
	}

	if(getenv("DIFX_GROUP_ID"))
	{
		umask(2);
	}

	opts = parseCommandLine(argc, argv);
	if(opts == 0)
	{
		return 0;
	}
	
	deleteCommandLineOptions(opts);

	return 0;
}

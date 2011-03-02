#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <ctype.h>
#include "transient_wrapper_data.h"

const char program[] = "transient_wrapper";
const char author[] = "Walter Brisken";
const char version[] = "0.1";
const char verdate[] = "2011 Mar 1";

const char defaultOutputPath[] = "/home/boom/TESTDATA/CAPTURES";
const int minFreeMB = 1000000;	/* don't copy unless there are this many MB free in the above path */

static int usage(const char *pgm)
{
	printf("\n%s ver. %s  %s  %s\n\n", program, version, author, verdate);

	return 0;
}

static int execute(int argc, char **argv, TransientWrapperData *T)
{
	const unsigned int MaxCommandLength = 1023;
	char command[MaxCommandLength + 1];
	int a, rv;
	int start = 1;
	time_t t1, t2;

	for(a = 1; a < argc; a++)
	{
		if(strcmp(argv[a], "--") == 0)
		{
			start = a + 1;
		}
	}

	command[0] = 0;
	for(a = start; a < argc; a++)
	{
		if(command[0] != 0)
		{
			strncat(command, " ", MaxCommandLength);
		}
		strncat(command, argv[a], MaxCommandLength);
	}

	if(strlen(command) >= MaxCommandLength)
	{
		difxMessageSendDifxAlert("Command line is too long", DIFX_ALERT_LEVEL_ERROR);
		
		return -1;
	}

	if(T->verbose)
	{
		printf("Executing: %s\n", command);
	}

	t1 = time(0);
	rv = system(command);
	t2 = time(0);

	return t2 - t1;
}

static void updateenvironment(const char *inputFile)
{
	const int MaxLineLength=1024;
	char line[MaxLineLength], key[MaxLineLength], value[MaxLineLength];
	char envFile[DIFXIO_FILENAME_LENGTH];
	int v;
	char *rv;
	FILE *in;

	v = snprintf(envFile, DIFXIO_FILENAME_LENGTH, "%s.env", inputFile);
	if(v >= DIFXIO_FILENAME_LENGTH)
	{
		fprintf(stderr, "Developer error: not enough space for filename (%d >= %d)\n", v, DIFXIO_FILENAME_LENGTH);
		
		return;
	}

	in = fopen(envFile, "r");
	if(in)
	{
		while(!feof(in))
		{
			rv = fgets(line, MaxLineLength, in);
			if(!rv)
			{
				break;
			}
			if(line[0] == '#')
			{
				continue;
			}
			if(sscanf(line, "%s%s", key, value) != 2)
			{
				continue;
			}
			setenv(key, value, 1);
		}
		fclose(in);
	}
}

static int parsecommandline(int argc, char **argv, TransientWrapperData *T)
{
	int a;
	int stop = 0;

	for(a = 1; a < argc; a++)
	{
		if(strcmp(argv[a], "--") == 0)
		{
			stop = a;
		}
	}

	if(stop)
	{
		for(a = 1; a < stop; a++)
		{
			if(strcmp(argv[a], "-v") == 0 ||
			   strcmp(argv[a], "--verbose") == 0)
			{
				T->verbose++;
			}
			else if(strcmp(argv[a], "-q") == 0 ||
			   strcmp(argv[a], "--quite") == 0)
			{
				T->verbose--;
			}
			else if(strcmp(argv[a], "-h") == 0 ||
			   strcmp(argv[a], "--help") == 0)
			{
				usage(argv[0]);

				exit(0);
			}
			else if(strcmp(argv[a], "-n") == 0 ||
			   strcmp(argv[a], "--nocopy") == 0)
			{
				T->doCopy = -1;
			}
		}
	}

	else if(argc == 2 && 
	  (strcmp(argv[1], "-h") == 0 ||
           strcmp(argv[1], "--help") == 0))
	{
		usage(argv[0]);

		exit(0);
	}

	if(stop + 3 > argc)
	{
		return -1;
	}

	return stop;
}

char *stripInputFile(const char *inputFile)
{
	char *filePrefix;
	int l;

	filePrefix = strdup(inputFile);
	l = strlen(filePrefix);
	if(l < 7)
	{
		printf("Input file expected");
		free(filePrefix);

		return 0;
	}
	if(strcmp(filePrefix+l-6, ".input") != 0)
	{
		printf("Input file expected");
		free(filePrefix);

		return 0;
	}

	filePrefix[l-6] = 0;

	return filePrefix;
}

static int getFreeMB(const char *path)
{
	const int MaxCommandLength=256;
	const int MaxLineLength=256;
	char cmd[MaxCommandLength];
	char line[MaxLineLength];
	FILE *pin;
	int v, size = 0;
	char *s;

	v = snprintf(cmd, MaxCommandLength, "df -m %s\n", path);

	pin = popen(cmd, "r");
	if(!pin)
	{
		fprintf(stderr, "Error: pipe for <%s> failed\n", cmd);
	}
	else
	{
		while(!feof(pin))
		{
			s = fgets(line, MaxLineLength-1, pin);
			if(!s)
			{
				break;
			}
			if(isalpha(line[0]))
			{
				v = sscanf(line, "%*s %*d %*d %d", &size);
			}
			else
			{
				v = sscanf(line, "%*d %*d %d", &size);
			}
			if(v == 1)
			{
				break;
			}
		}

		fclose(pin);
	}

	return size;
}

TransientWrapperData *initialize(int argc, char **argv)
{
	TransientWrapperData *T;
	const char *inputFile, *pgm;
	char message[DIFX_MESSAGE_LENGTH];
	int df, i, index;

	T = newTransientWrapperData();

	T->outputPath = defaultOutputPath;

	index = parsecommandline(argc, argv, T);

	if(T->rank > 0)
	{
		/* demote verbosity of all but the main thread by one.  Use an extra -v if needed */
		T->verbose--;
	}

	pgm = argv[index+1];
	inputFile = argv[index+2];

	updateenvironment(inputFile);

	if(index < 0)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "%s", "Malformed command line");
		difxMessageSendDifxAlert("Malformed command line", DIFX_ALERT_LEVEL_ERROR);
		printf("Error: %s\n", message);

		deleteTransientWrapperData(T);

		return 0;
	}

	printf("Verbose = %d  pgm = %s  inputFile = %s\n", T->verbose, program, inputFile);

	T->filePrefix = stripInputFile(inputFile);
	if(!T->filePrefix)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Malformed .input file name: %s", inputFile);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		printf("Error: %s\n", message);

		deleteTransientWrapperData(T);

		return 0;
	}

	T->D = loadDifxInput(T->filePrefix);
	if(!T->D)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Problem opening DiFX job %s", T->filePrefix);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		printf("Error: %s\n", message);

		deleteTransientWrapperData(T);

		return 0;
	}

	T->identifier = T->filePrefix;
	for(i = 0; T->filePrefix[i]; i++)
	{
		if(T->filePrefix[i] == '/')
		{
			T->identifier = T->filePrefix + i + 1;
		}
	}

	if(T->rank > 0 && T->rank <= T->D->nDatastream)
	{
		if( (T->D->datastream[T->rank-1].dataSource == DataSourceModule ||
		     T->D->datastream[T->rank-1].dataSource == DataSourceFile) &&
		   T->doCopy == 0)
		{
			T->doCopy = 1;
		}
	}

	if(T->doCopy > 0)
	{
		/* Make sure there is > 1TB free on disks */
		df = getFreeMB(defaultOutputPath);
		if(T->verbose)
		{
			printf("Free space on %s = %d MB\n", defaultOutputPath, df);
		}
		if(df < minFreeMB)
		{
			T->doCopy = 0;
		}
	}

	return T;
}

int main(int argc, char **argv)
{
	TransientWrapperData *T;

	difxMessageInit(-1, program);
	
	T = initialize(argc, argv);
	if(T == 0)
	{
		return 0;
	}

	if(T->verbose > 0)
	{
		printTransientWrapperData(T);
	}

	if(T->verbose > 1)
	{
		printDifxInput(T->D);
	}

	if(T->doCopy > 0)
	{
		startMulticastMonitor(T);
	}

	T->executeTime = execute(argc, argv, T);

	if(T->doCopy > 0)
	{
		stopMulticastMonitor(T);

		if(T->difxState == DIFX_STATE_DONE && T->executeTime > 3 && T->nEvent > 0)
		{
			copyBasebandData(T);
		}
	}

	if(T->verbose > 0)
	{
		printTransientWrapperData(T);
	}

	deleteTransientWrapperData(T);

	return 0;
}

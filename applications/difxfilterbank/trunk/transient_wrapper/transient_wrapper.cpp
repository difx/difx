#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "transient_wrapper_data.h"

const char program[] = "transient_wrapper";
const char author[] = "Walter Brisken";
const char version[] = "0.1";
const char verdate[] = "2010 Nov. 11";

int usage(const char *pgm)
{
	printf("\n%s ver. %s  %s  %s\n\n", program, version, author, verdate);

	return 0;
}

int execute(int argc, char **argv, TransientWrapperData *T)
{
	const unsigned int MaxCommandLength = 1023;
	char command[MaxCommandLength+1];
	int a, rv;
	int start = 1;

	for(a = 1; a < argc; a++)
	{
		if(strcmp(argv[a], "--") == 0)
		{
			start = a+1;
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

	rv = system(command);

	return rv;
}

int parsecommandline(int argc, char **argv, TransientWrapperData *T)
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

TransientWrapperData *initialize(int argc, char **argv)
{
	TransientWrapperData *T;
	const char *inputFile, *pgm;
	const char *rankStr;
	char message[DIFX_MESSAGE_LENGTH];
	int i, index;

	T = newTransientWrapperData();

	index = parsecommandline(argc, argv, T);

	pgm = argv[index+1];
	inputFile = argv[index+2];

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

	rankStr = getenv("OMPI_COMM_WORLD_RANK");
	if(rankStr)
	{
		T->rank = atoi(rankStr);
		if(T->rank > 0 && T->rank <= T->D->nDatastream)
		{
			if(T->D->datastream[T->rank-1].dataSource == DataSourceModule)
			{
				T->doCopy = 1;
			}
		}
	}
	else
	{
		T->rank = -1;
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

	printTransientWrapperData(T);

	if(T->verbose > 1)
	{
		printDifxInput(T->D);
	}

	if(T->doCopy)
	{
		startMulticastMonitor(T);
	}

	execute(argc, argv, T);

	if(T->doCopy)
	{
		stopMulticastMonitor(T);

		if(T->difxState == DIFX_STATE_DONE)
		{
			/* Here is where we do the copying! */
		}
	}

	printTransientWrapperData(T);

	deleteTransientWrapperData(T);

	return 0;
}

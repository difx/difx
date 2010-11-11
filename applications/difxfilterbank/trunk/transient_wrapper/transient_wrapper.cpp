#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <difxmessage.h>
#include <difxio.h>

const char program[] = "transient_wrapper";
const char author[] = "Walter Brisken";
const char version[] = "0.1";
const char verdate[] = "2010 Nov. 11";

int verbose = 0;

int usage(const char *pgm)
{
	printf("%s ver. %s  %s  %s\n\n", program, version, verdate, author);

	return 0;
}

int execute(int argc, char **argv)
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

	if(verbose)
	{
		printf("Executing: %s\n", command);
	}

	rv = system(command);

	return rv;
}

int parsecommandline(int argc, char **argv)
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
				verbose++;
			}
			else if(strcmp(argv[a], "-q") == 0 ||
			   strcmp(argv[a], "--quite") == 0)
			{
				verbose--;
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

	printf("before = %s\n", inputFile);
	printf("after  = %s\n", filePrefix);

	return filePrefix;
}

int main(int argc, char **argv)
{
	const char *inputFile, *pgm;
	char *filePrefix;
	char message[DIFX_MESSAGE_LENGTH];
	int index;
	DifxInput *D;
	
	index = parsecommandline(argc, argv);

	pgm = argv[index+1];
	inputFile = argv[index+2];

	difxMessageInit(-1, program);

	if(index < 0)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "%s", "Malformed command line");
		difxMessageSendDifxAlert("Malformed command line", DIFX_ALERT_LEVEL_ERROR);
		printf("Error: %s\n", message);

		return 0;
	}

	printf("Verbose = %d  pgm = %s  inputFile = %s\n", verbose, program, inputFile);

	filePrefix = stripInputFile(inputFile);
	if(!filePrefix)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Malformed .input file name: %s", inputFile);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		printf("Error: %s\n", message);

		return 0;
	}

	D = loadDifxInput(filePrefix);
	if(!D)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Problem opening DiFX job %s", filePrefix);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		printf("Error: %s\n", message);

		return 0;
	}

	if(verbose > 1)
	{
		printDifxInput(D);
	}

	execute(argc, argv);

	if(D)
	{
		deleteDifxInput(D);
	}

	if(filePrefix)
	{
		free(filePrefix);
	}

	return 0;
}

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <ctype.h>
#include "transient_wrapper_data.h"

const char program[] = "transient_wrapper";
const char author[] = "Walter Brisken";
const char version[] = "0.2";
const char verdate[] = "2011 Jul 27";

const char defaultConfigFile[] = "/home/boom/difx/vfastr.conf";

static int usage(const char *pgm)
{
	printf("\n%s ver. %s  %s  %s\n\n", program, version, author, verdate);
	printf("Usage: %s [<options> --] <program> <inputFile>\n\n", pgm);
	printf("Note that if options are provided, two hyphens (--) must be used to separate the command line\n\n");
	printf("Env. var VFASTR_CONFIG_FILE can point to a configuration file\n");
	printf("If this variable is not set, the default (%s) will be used\n\n", defaultConfigFile);

	return 0;
}

void logExecute(const char *str)
{
	char timeStr[100];
	time_t rawtime;
	struct tm timeinfo;
	int l;

	time(&rawtime);
	localtime_r(&rawtime, &timeinfo);

	asctime_r(&timeinfo, timeStr);
	l = strlen(timeStr);
	if(l > 0)
	{
		timeStr[l-1] = 0;
	}
	printf("[%s] Executing: %s\n", timeStr, str);
	fflush(stdout);
}

static int execute(int argc, char **argv, TransientWrapperData *T)
{
	const unsigned int MaxCommandLength = 1023;
	char command[MaxCommandLength + 1];
	int a, rv;
	int start = 1;
	time_t t1, t2;

	for(a = 1; a < argc; ++a)
	{
		if(strcmp(argv[a], "--") == 0)
		{
			start = a + 1;
		}
	}

	command[0] = 0;
	for(a = start; a < argc; ++a)
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
		logExecute(command);
	}

	t1 = time(0);
	rv = system(command);
	if(rv < 0)
	{
		fprintf(stderr, "Error executing %s\n", command);
	}
	t2 = time(0);

	return t2 - t1;
}

static void updateenvironment(const char *inputFile)
{
	const int MaxLineLength = 1024;
	char envFile[DIFXIO_FILENAME_LENGTH];
	int v;
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
			char *rv;
			char line[MaxLineLength], key[MaxLineLength], value[MaxLineLength];
			
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

static int parseCommandLine(int argc, char **argv, TransientWrapperData *T)
{
	int a;
	int stop = 0;

	for(a = 1; a < argc; ++a)
	{
		if(strcmp(argv[a], "--") == 0)
		{
			stop = a;
		}
	}

	if(stop)
	{
		for(a = 1; a < stop; ++a)
		{
			if(strcmp(argv[a], "-v") == 0 ||
			   strcmp(argv[a], "--verbose") == 0)
			{
				++T->verbose;
			}
			else if(strcmp(argv[a], "-q") == 0 ||
			   strcmp(argv[a], "--quiet") == 0)
			{
				--T->verbose;
			}
			else if(strcmp(argv[a], "-h") == 0 ||
			   strcmp(argv[a], "--help") == 0)
			{
				usage(argv[0]);

				exit(EXIT_FAILURE);
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

		exit(EXIT_FAILURE);
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
		printf("Input file expected (%s not legit)\n", inputFile);
		free(filePrefix);

		return 0;
	}
	if(strcmp(filePrefix+l-6, ".input") != 0)
	{
		printf("Input file expected (%s not legit)\n", inputFile);
		free(filePrefix);

		return 0;
	}

	filePrefix[l-6] = 0;

	return filePrefix;
}

static int getFreeMB(const char *path)
{
	const int MaxCommandLength = 256;
	const int MaxLineLength = 256;
	char cmd[MaxCommandLength];
	FILE *pin;
	int v;
	int size = 0;

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
			char line[MaxLineLength];
			char *s;

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

		pclose(pin);
	}

	return size;
}

TransientWrapperData *initialize(int argc, char **argv, const TransientWrapperConf *conf)
{
	TransientWrapperData *T;
	const char *inputFile, *pgm;
	int df, i, index;

	T = newTransientWrapperData(conf);

	index = parseCommandLine(argc, argv, T);

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
		char message[DIFX_MESSAGE_LENGTH];
		
		snprintf(message, DIFX_MESSAGE_LENGTH, "%s", "Malformed command line (run with -h for usage)");
		difxMessageSendDifxAlert("Malformed command line", DIFX_ALERT_LEVEL_ERROR);
		printf("Error: %s\n", message);

		deleteTransientWrapperData(T);

		return 0;
	}

	printf("Verbose = %d  pgm = %s  inputFile = %s\n", T->verbose, pgm, inputFile);

	T->filePrefix = stripInputFile(inputFile);
	if(!T->filePrefix)
	{
		char message[DIFX_MESSAGE_LENGTH];
		
		snprintf(message, DIFX_MESSAGE_LENGTH, "Malformed .input file name: %s", inputFile);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		printf("Error: %s\n", message);

		deleteTransientWrapperData(T);

		return 0;
	}

	T->D = loadDifxInput(T->filePrefix);
	if(!T->D)
	{
		char message[DIFX_MESSAGE_LENGTH];
		
		snprintf(message, DIFX_MESSAGE_LENGTH, "Problem opening DiFX job %s", T->filePrefix);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		printf("Error: %s\n", message);

		deleteTransientWrapperData(T);

		return 0;
	}

	T->identifier = T->filePrefix;
	for(i = 0; T->filePrefix[i]; ++i)
	{
		if(T->filePrefix[i] == '/')
		{
			T->identifier = T->filePrefix + i + 1;
		}
	}

	if(T->rank > 0 && T->rank <= T->D->nDatastream && T->conf->enable && T->conf->maxCopyOverhead > 0.0)
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
		/* Make sure there is enough free on disks */
		df = getFreeMB(T->conf->path);
		if(T->verbose)
		{
			printf("Free space on %s = %d MB\n", T->conf->path, df);
		}
		if(df < T->conf->minFreeDiskMB)
		{
			T->doCopy = 0;
		}
	}

	return T;
}

int main(int argc, char **argv)
{
	TransientWrapperConf *conf;
	TransientWrapperData *T;
	const char *configFile;

	printf("%s version %s date %s starting\n", program, version, verdate);

	difxMessageInit(-1, program);
	
	configFile = getenv("VFASTR_CONFIG_FILE");
	if(!configFile)
	{
		configFile = defaultConfigFile;
	}

	conf = newTransientWrapperConf();
	loadTransientWrapperConf(conf, configFile);

	T = initialize(argc, argv, conf);
	if(T == 0)
	{
		return 0;
	}

	if(T->verbose > 0)
	{
		printTransientWrapperConf(T->conf);

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
	deleteTransientWrapperConf(conf);

	return EXIT_SUCCESS;
}

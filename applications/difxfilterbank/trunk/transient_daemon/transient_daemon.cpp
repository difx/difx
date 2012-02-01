#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <difxmessage.h>
#include <difxio/parsedifx.h>
#include <sys/inotify.h>
#include <sys/select.h>
#include <sys/statfs.h>
#include "recorr.h"

const char program[] = "transient_daemon";
const char author[]  = "Walter Brisken";
const char version[] = "0.3";
const char verdate[] = "2011 Jul 27";

const char defaultConfigFile[] = "/home/boom/difx/vfastr.conf";	/* overridden by env var VFASTR_CONFIG_FILE */

const char dispatcherProgram[] = "transient_dispatcher";
/* Command line params are: configfile jobid dmfile */

enum archive_mode {NONE, ALL, RAW};
const char archiveModeName[][8] = {"none", "all", "raw"};

/* default parameters to use if no config file is found */
const int defaultVfastrEnable = 1;
const int defaultMinDiskSpaceGB = 100;
const double defaultDetectionThreshold = 5.0;
const double defaultRecorrThreshold = 5.0;
const char defaultOutputPath[] = "/home/boom/data/products";
const int defaultDifxStaChannels = 32;
const int defaultOnlineTrainingEnable = 1;
const int defaultArchiveDedispersed = 0;
const int defaultArchivePulses = 0;
const int defaultArchiveMerged = 0;
const int defaultArchiveScores = 0;
const enum archive_mode defaultArchiveFilterbank = NONE;
const int defaultConcurrentPipeline = 0;
const int defaultStubPipeline = 0;
const char defaultRecorrQueueFile[] = "";
const char defaultDmgenProgram[] = "makedmlist";
const double defaultMinDM = 0.0;
const double defaultMaxDM = 5000.0;
const int defaultNegDM = 0;
const int defaultNDM = 200;
const double defaultTDM = 2;	/* ms */
const double defaultMaxDispersionDelay = 2;	/* seconds */


int die = 0;

const int CommandLength = 1024;

const char testMessage[] = 
"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><difxMessage><header><from>gui</from><to>boom</to><mpiProcessId>-1</mpiProcessId><identifier>boom_x</identifier><type>DifxStart</type></header><body><difxStart><input>/home/swc/difx/projects/td019/td019_15.input</input><manager node=\"boom\"/><datastream nodes=\"boom1 boom2\"/><process threads=\"7\" nodes=\"boom3 boom4 boom4\"/><force>1</force><difxVersion>DIFX-trunk</difxVersion></difxStart></body></difxMessage>";

class TransientDaemonState
{
public:
	int verbose;
	int selfTest;
	int startEnable;
	int nLaunch;
	char lastCommand[CommandLength];
	char hostname[DIFX_MESSAGE_PARAM_LENGTH];
	RecorrQueue *recorrQueue;

	TransientDaemonState();
	~TransientDaemonState();
	void print() const;
};

typedef struct
{
	int vfastrEnable;
    int min_disk_space_GB;
	double detectionThreshold;
	double recorrThreshold;
	char outputPath[DIFX_MESSAGE_FILENAME_LENGTH];
	int difxStaChannels;
	int onlineTrainingEnable;
	int archiveDedispersed;
	int archivePulses;
	int archiveMerged;
	int archiveScores;
	enum archive_mode archiveFilterbank;
	int concurrentPipeline;
	int stubPipeline;
	char recorrQueueFile[DIFX_MESSAGE_FILENAME_LENGTH];

	char dmgenProgram[DIFX_MESSAGE_FILENAME_LENGTH];
	double minDM;
	double maxDM;
	int negDM;
	int nDM;
	double tDM;
	double maxDispersionDelay;
} TransientDaemonConf;

TransientDaemonState::TransientDaemonState()
{
	verbose = 0;
	selfTest = 0;
	startEnable = 1;
	nLaunch = 0;
	lastCommand[0] = 0;
	gethostname(hostname, DIFX_MESSAGE_PARAM_LENGTH);
	recorrQueue = 0;
}

TransientDaemonState::~TransientDaemonState()
{
	if(recorrQueue)
	{
		delete recorrQueue;

		recorrQueue = 0;
	}
}

void TransientDaemonState::print() const
{
	printf("TransientDaemonState [%s]:\n", hostname);
	printf("  verbose=%d\n", verbose);
	printf("  selfTest=%d\n", selfTest);
	printf("  startEnable=%d\n", startEnable);
	printf("  nLaunch=%d\n", nLaunch);
	printf("  Last Command=%s\n", lastCommand);
}

int usage(const char *cmd)
{
	printf("\n");
	printf("%s ver. %s   %s  %s\n\n", program, version, author, verdate);
	printf("Usage:  %s  [options]\n\n", cmd);
	printf("Options can include:\n\n");
	printf("  --verbose\n");
	printf("  -v           Be more verbose in execution\n\n");
	printf("  --quiet\n");
	printf("  -q           Be quieter\n\n");
	printf("  --test\n");
	printf("  -t           Perform self test and quit (watch for weirdness)\n\n");
	printf("  --help\n");
	printf("  -h           Print this useful help information.\n\n");
	printf("  --disable\n");
	printf("  -d           Don't actually spawn any processes.\n\n");
	printf("To quite: Ctrl-c or send a sigint\n\n");
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


/* check available disk space on the output file system and return 1 if there is
    enough space available or 0 otherwise */
int diskSpaceIsAvailable(const TransientDaemonConf *conf) {
    struct statfs fs;
    int res;

    res = statfs(conf->outputPath,&fs);
    if (res==0) {
        long long bytes_available,min_required;
        min_required = 1000000000LL*conf->min_disk_space_GB;
        bytes_available = (long long)fs.f_bsize*(long long)fs.f_bavail;
        printf("Available disk space: %lld GB. Min required: %d GB\n",
                bytes_available/1000000000,conf->min_disk_space_GB);
        if (bytes_available > min_required) return 1;
    }
    return 0;
}


int setTransientDispatcherOptions(char *options, int maxLength, const TransientDaemonConf *conf)
{
	int v;
	const char *fbArchiveMode = "";

	switch(conf->archiveFilterbank)
	{
	case NONE:
		fbArchiveMode = ""; 
		break;
	case ALL:
		fbArchiveMode = " -a"; 
		break;
	case RAW:
		fbArchiveMode = " -r"; 
		break;
	}

	v = snprintf(options, maxLength, "-o %s -c %d -T %f%s%s%s%s%s%s%s %s",
		conf->outputPath,
		conf->difxStaChannels,
		conf->detectionThreshold,
		conf->onlineTrainingEnable ? ""    : " -F",
		conf->archiveDedispersed   ? " -d" : "",
		conf->archivePulses        ? " -p" : "",
		conf->archiveMerged        ? " -m" : "",
		conf->archiveScores        ? " -s" : "",
		conf->concurrentPipeline   ? " -C" : "",
		conf->stubPipeline         ? " -S" : "",
		fbArchiveMode);

	if(v >= maxLength)
	{
		fprintf(stderr, "setTransientDispatcherOptions: too little space allocated for options: %d < %d\n", maxLength, v);

		return -1;
	}

	return 0;
}

TransientDaemonConf *newTransientDaemonConf()
{
	TransientDaemonConf *conf;

	conf = (TransientDaemonConf *)calloc(1, sizeof(TransientDaemonConf));

	if(!conf)
	{
		fprintf(stderr, "Error: cannot allocate a Transient Daemon Configuration Object\n");

		exit(EXIT_FAILURE);
	}
	conf->vfastrEnable = defaultVfastrEnable;
    conf->min_disk_space_GB = defaultMinDiskSpaceGB;
	conf->detectionThreshold = defaultDetectionThreshold;
	snprintf(conf->outputPath, DIFX_MESSAGE_FILENAME_LENGTH, "%s", defaultOutputPath);
	conf->difxStaChannels = defaultDifxStaChannels;
	conf->onlineTrainingEnable = defaultOnlineTrainingEnable;
	conf->archiveDedispersed = defaultArchiveDedispersed;
	conf->archivePulses = defaultArchivePulses;
	conf->archiveMerged = defaultArchiveMerged;
	conf->archiveScores = defaultArchiveScores;
	conf->archiveFilterbank = defaultArchiveFilterbank;
	conf->concurrentPipeline = defaultConcurrentPipeline;
	conf->stubPipeline = defaultStubPipeline;
	snprintf(conf->recorrQueueFile, DIFX_MESSAGE_FILENAME_LENGTH, "%s", defaultRecorrQueueFile);

	snprintf(conf->dmgenProgram, DIFX_MESSAGE_FILENAME_LENGTH, "%s", defaultDmgenProgram);
	conf->minDM = defaultMinDM;
	conf->maxDM = defaultMaxDM;
	conf->negDM = defaultNegDM;
	conf->nDM = defaultNDM;
	conf->tDM = defaultTDM;
	conf->maxDispersionDelay = defaultMaxDispersionDelay;

	return conf;
}

void deleteTransientDaemonConf(TransientDaemonConf *conf)
{
	free(conf);
}

int loadTransientDaemonConf(TransientDaemonConf *conf, const char *filename)
{
	FILE *in;
	int n;
	char line[DIFX_MESSAGE_COMMENT_LENGTH];
	char A[DIFX_MESSAGE_COMMENT_LENGTH];
	char B[DIFX_MESSAGE_COMMENT_LENGTH];
	char C[DIFX_MESSAGE_COMMENT_LENGTH];

	in = fopen(filename, "r");
	if(!in)
	{
		return -1;
	}

	for(int l = 1;; l++)
	{
		fgets(line, DIFX_MESSAGE_COMMENT_LENGTH-1, in);
		if(feof(in))
		{
			break;
		}
		for(int i = 0; line[i]; i++)
		{
			if(line[i] == '#')	/* break at a comment charcter */
			{
				line[i] = 0;
				break;
			}
			if(line[i] == '=')	/* turn equal signs into spaces */
			{
				line[i] = ' ';
			}
		}
		n = sscanf(line, "%s %s %s", A, B, C);
		if(n <= 0)
		{
			continue;
		}
		if(n != 2)
		{
			fprintf(stderr, "Config file %s : parse error line %d n=%d\n", filename, l, n);
		}

		/* transient_wrapper specific code here */
		if(strcmp(A, "vfastr_enable") == 0)
		{
			conf->vfastrEnable = atoi(B);
		}
		else if(strcmp(A, "min_disk_space_GB") == 0)
		{
			conf->min_disk_space_GB = atoi(B);
		}
		else if(strcmp(A, "detection_threshold") == 0)
		{
			conf->detectionThreshold = atof(B);
		}
		else if(strcmp(A, "recorr_threshold") == 0)
		{
			conf->recorrThreshold = atof(B);
		}
		else if(strcmp(A, "output_path") == 0)
		{
			snprintf(conf->outputPath, DIFX_MESSAGE_FILENAME_LENGTH, "%s", B);
		}
		else if(strcmp(A, "difx_sta_channels") == 0)
		{
			conf->difxStaChannels = atoi(B);
		}
		else if(strcmp(A, "online_training_enable") == 0)
		{
			conf->onlineTrainingEnable = atoi(B);
		}
		else if(strcmp(A, "archive_dedispersed") == 0)
		{
			conf->archiveDedispersed = atoi(B);
		}
		else if(strcmp(A, "archive_pulses") == 0)
		{
			conf->archivePulses = atoi(B);
		}
		else if(strcmp(A, "archive_merged") == 0)
		{
			conf->archiveMerged = atoi(B);
		}
		else if(strcmp(A, "archive_detectorscores") == 0)
		{
			conf->archiveScores = atoi(B);
		}
		else if(strcmp(A, "archive_filterbank") == 0)
		{
			if(strcasecmp(B, "none") == 0)
			{
				conf->archiveFilterbank = NONE;
			}
			else if(strcasecmp(B, "all") == 0)
			{
				conf->archiveFilterbank = ALL;
			}
			else if(strcasecmp(B, "raw") == 0)
			{
				conf->archiveFilterbank = RAW;
			}
		}
		else if(strcmp(A, "concurrent_pipeline") == 0)
		{
			conf->concurrentPipeline = atoi(B);
		}
		else if(strcmp(A, "stub_pipeline") == 0)
		{
			conf->stubPipeline = atoi(B);
		}
		else if(strcmp(A, "recorr_queue") == 0)
		{
			snprintf(conf->recorrQueueFile, DIFX_MESSAGE_FILENAME_LENGTH, "%s", B);
		}
		else if(strcmp(A, "dm_generator_program") == 0)
		{
			snprintf(conf->dmgenProgram, DIFX_MESSAGE_FILENAME_LENGTH, "%s", B);
		}
		else if(strcmp(A, "min_search_dm") == 0)
		{
			conf->minDM = atof(B);
		}
		else if(strcmp(A, "max_search_dm") == 0)
		{
			conf->maxDM = atof(B);
		}
		else if(strcmp(A, "negative_dm_sparsity") == 0)
		{
			conf->negDM = atoi(B);
		}
		else if(strcmp(A, "max_dm_values") == 0)
		{
			conf->nDM = atoi(B);
		}
		else if(strcmp(A, "dm_delta_t") == 0)
		{
			conf->tDM = atof(B);
		}
		else if(strcmp(A, "max_dispersion_delay") == 0)
		{
			conf->maxDispersionDelay = atof(B);
		}
		/* else ignore the parameter */
	}

	fclose(in);

	return 0;
}

void printTransientDaemonConf(const TransientDaemonConf *conf)
{
	printf("TransientDaemonConf [%p]\n", conf);
	printf("  vfastrEnable = %d\n", conf->vfastrEnable);
	printf("  minDiskSpaceGB = %d\n", conf->min_disk_space_GB);
	printf("  detectionThreshold = %f\n", conf->detectionThreshold);
	printf("  outputPath = %s\n", conf->outputPath);
	printf("  difxStaChannels = %d\n", conf->difxStaChannels);
	printf("  onlineTrainingEnable = %d\n", conf->onlineTrainingEnable);
	printf("  archiveDedispersed = %d\n", conf->archiveDedispersed);
	printf("  archivePulses = %d\n", conf->archivePulses);
	printf("  archiveMerged = %d\n", conf->archiveMerged);
	printf("  archiveScores = %d\n", conf->archiveScores);
	printf("  archiveFilterbank = %d -> %s\n", conf->archiveFilterbank, archiveModeName[conf->archiveFilterbank]);
	printf("  stubPipeline = %d\n", conf->stubPipeline);
	printf("  recorrQueueFile = %s\n", conf->recorrQueueFile);
	printf("  recorrThreshold = %f\n", conf->recorrThreshold);
	printf("  dmgenProgram = %s\n", conf->dmgenProgram);
	printf("  minDM = %f\n", conf->minDM);
	printf("  maxDM = %f\n", conf->maxDM);
	printf("  negative DM sparsity factor = %d\n", conf->negDM);
	printf("  max number of DM trials (including negative) = %d\n", conf->nDM);
	printf("  time interval to consider in DM setting = %f ms\n", conf->tDM);
	printf("  max dispersion delay = %f sec\n", conf->maxDispersionDelay);

	fflush(stdout);
}

void siginthand(int j)
{
	die = 1;
}

static void generateIdentifier(const char *inputfile, int myID, char *identifier)
{
	int l, s=0;

	for(int i = 0; inputfile[i]; i++)
	{
		if(inputfile[i] == '/')
		{
			s = i+1;
		}
	}

	if(inputfile[s] == 0)
	{
		s = 0;
	}

	strncpy(identifier, inputfile+s, DIFX_MESSAGE_IDENTIFIER_LENGTH-1);
	identifier[DIFX_MESSAGE_IDENTIFIER_LENGTH-1] = 0;
	l = strlen(identifier);

	// strip off ".input"
	for(int i = l-1; i > 0; i--)
	{
		if(identifier[i] == '.')
		{
			identifier[i] = 0;

			break;
		}
	}
}

/* This function needs to look through a .input file and extract some information.  Care
 * needs to be made to allow access to both DiFX 1.5 and 2.0 files.
 */
static int getDMGenCommand(const char *inputFile, char *command, const TransientDaemonConf *conf)
{
	int r, v, nFreq, maxfreq_ind;
	DifxParameters *dp;
	double freq, minFreq = 1.0e9;	/* MHz */
	double maxFreq = 1.0;           /* MHz */
	double bw = 16.0;               /* MHz, guess a default here */

	dp = newDifxParametersfromfile(inputFile);
	if(!dp)
	{
		fprintf(stderr, "Cannot open %s for read.\n", inputFile);
		
		return -1;
	}

	r = DifxParametersfind(dp, 0, "FREQ ENTRIES");
	if(r < 0)
	{
		fprintf(stderr, "Cannot find FREQ ENTRIES in %s.\n", inputFile);
		
		return -2;
	}
	nFreq = atoi(DifxParametersvalue(dp, r));
	maxfreq_ind = nFreq-1;
	for(int i = 0; i < nFreq; i++)
	{
		r = DifxParametersfind1(dp, r, "FREQ (MHZ) %d", i);
		if(r < 0)
		{
			fprintf(stderr, "Cannot find FREQ (MHZ) %d in %s.\n", i, inputFile);
			
			return -3;
		}
		freq = atof(DifxParametersvalue(dp, r));
		if(freq < minFreq)
		{
			minFreq = freq;
		}
		if(freq > maxFreq)
		{
			maxFreq = freq;
            maxfreq_ind = i;    /* track the index of the highest freq, so that we can add its BW below */
		}
	}
	/* need to add the bandwdith of the highest freq to overall freq span calculation*/
	/* try to retrieve the BW from the config file, but guess a default if not */
	r = DifxParametersfind1(dp, 0, "BW (MHZ) %d", maxfreq_ind);
	if (r < 0)
	{
		fprintf(stderr, "Warning: failed to find BW of freq index %d. Assuming %g MHz\n", nFreq-1, bw);
	}
	else
	{
		bw = atof(DifxParametersvalue(dp, r));
	}
	maxFreq += bw;

	v = snprintf(command, CommandLength, "`%s -f %f -l %f -n %d -L %f -H %f -T %f -M %d -D %f`", 
		conf->dmgenProgram, 
		conf->minDM,
		conf->maxDM,
		conf->negDM,
		minFreq*0.001, 	/* MHz to GHz */
		maxFreq*0.001,	/* MHz to GHz */
		conf->tDM,
		conf->nDM,
		conf->maxDispersionDelay);
	if(v >= CommandLength)
	{
		fprintf(stderr, "Developer error: CommandLength=%d is too small.  Needs to be > %d.\n", CommandLength, v);
		
		return -3;
	}

	return 0;
}

int runCommand(const char *command, int verbose)
{
	int pid;

	if(verbose > 0)
	{
		logExecute(command);
	}
	pid = fork();
	if(pid)
	{
		return pid;
	}
	else
	{
		int v = system(command);
		
		exit(EXIT_SUCCESS);
	}
}

static int isTrue(const char *str)
{
	if(strcmp(str, "True") == 0 || 
	   strcmp(str, "true") == 0 || 
	   strcmp(str, "1") == 0)
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

static int handleMessage(const char *message, TransientDaemonState *state, const TransientDaemonConf *conf)
{
	DifxMessageGeneric G;
	char command[CommandLength];
	char options[CommandLength];
	int v, pid;
	char identifier[DIFX_MESSAGE_IDENTIFIER_LENGTH];
	char alertMessage[DIFX_MESSAGE_LENGTH];
	char dmGenCmd[CommandLength];

	difxMessageParse(&G, message);

	switch(G.type)
	{
	case DIFX_MESSAGE_STATUS:
		if(state->verbose > 1)
		{
			printf("%s: STATUS %s\n", G.identifier, DifxStateStrings[G.body.status.state]);
		}
		break;
	case DIFX_MESSAGE_START:
		if(conf->vfastrEnable)
		{
            if (!diskSpaceIsAvailable(conf)) {
                printf("Received start message, but not enough space is left on disk. Ignoring\n");
                return -3;
            }
			state->lastCommand[0] = 0;
			if(state->verbose > 1)
			{
				printf("%s: START %s %s\n", G.identifier, G.body.start.inputFilename, G.body.start.difxVersion);
			}

			generateIdentifier(G.body.start.inputFilename, 0, identifier);
			v = getDMGenCommand(G.body.start.inputFilename, dmGenCmd, conf);
			if(v != 0)
			{
				fprintf(stderr, "Error %d generating the DM command.\n", v);

				return -1;
			}

			v = setTransientDispatcherOptions(options, CommandLength, conf);
			if(v != 0)
			{
				fprintf(stderr, "Error %d generating dispatcher options.\n", v);

				return -1;
			}

			v = snprintf(command, CommandLength, "%s.%s %s %s %s %s\n", dispatcherProgram, G.body.start.difxVersion, options, G.body.start.inputFilename, identifier, dmGenCmd);
			if(v >= CommandLength)
			{
				fprintf(stderr, "Error: CommandLength=%d is too small (needs to be > %d).\n", CommandLength, v);

				return -2;
			}

			strcpy(state->lastCommand, command);

			if(state->startEnable)
			{
				pid = runCommand(command, state->verbose);
				state->nLaunch++;
				if(state->verbose > 1)
				{
					logExecute(command);
					printf("  pid = %d\n", pid);
					fflush(stdout);
				}
			}
			else
			{
				if(state->verbose > 0)
				{
					printf("Start message received, but starting of processes is not enabled.  Doing nothing.\n");
				}
			}
		}
		else
		{
			if(state->verbose > 1)
			{
				printf("%s: Not starting: %s %s\n", G.identifier, G.body.start.inputFilename, G.body.start.difxVersion);
			}
		}
		break;
	case DIFX_MESSAGE_PARAMETER:
		if(G.nTo == 1 && strcmp(G.to[0], state->hostname) == 0)
		{
			if(strcmp(G.body.param.paramName, "verbose") == 0)
			{
				state->verbose = atoi(G.body.param.paramValue);
			}
			else if(strcmp(G.body.param.paramName, "enable") == 0)
			{
				state->startEnable = isTrue(G.body.param.paramValue);
			}
			else if(strcmp(G.body.param.paramName, "restart") == 0)
			{
				runCommand(state->lastCommand, state->verbose);
				state->nLaunch++;
			}
			else if(strcmp(G.body.param.paramName, "queuerecorr") == 0)
			{
				if(state->verbose > 0)
				{
					printf("Received recorrelation request: %s\n", G.body.param.paramValue);
					fflush(stdout);
				}
				state->recorrQueue->add(G.body.param.paramValue, conf->recorrThreshold);
			}
			else if(strcmp(G.body.param.paramName, "test") == 0)
			{
				state->selfTest = atoi(G.body.param.paramValue);
			}
			else if(strcmp(G.body.param.paramName, "kill") == 0)
			{
				v = snprintf(command, CommandLength, "killall -INT %s", dispatcherProgram);
				if(v >= CommandLength)
				{
					printf("Error: CommandLength=%d is too short (needs to be > %d)\n", 
						CommandLength, v);
					fflush(stdout);
				}
				else
				{
					if(state->verbose > 0)
					{
						logExecute(command);
					}
					v = system(command);
				}
			}
			else if(strcmp(G.body.param.paramName, "getstate") == 0)
			{
				v = snprintf(alertMessage, DIFX_MESSAGE_LENGTH, "%s state: verbose=%d selfTest=%d enable=%d nLaunch=%d", program, state->verbose, state->selfTest, state->startEnable, state->nLaunch);
				if(v >= DIFX_MESSAGE_LENGTH)
				{
					fprintf(stderr, "Error: alertMessage truncated due to length\n");
				}
				difxMessageSendDifxAlert(alertMessage, DIFX_ALERT_LEVEL_INFO);
			}
			else if(strcmp(G.body.param.paramName, "stop_transient_daemon") == 0)
			{
				die = 1;
			}
			else
			{
				printf("Unknown Parameter %s received\n", G.body.param.paramName);
				fflush(stdout);
			}

			printf("DiFX Parameter Received: %s = %s\n", G.body.param.paramName, G.body.param.paramValue);
			fflush(stdout);
			state->print();
		}
		/* else ignore */
		break;
	case DIFX_MESSAGE_STOP:
		break;
	default:
		/* do nothing */
		break;
	}

	return 0;
}

int transientdaemon(TransientDaemonState *state)
{
	TransientDaemonConf *conf;
	const int MaxLineLength = 512;
	char message[DIFX_MESSAGE_LENGTH];
	char from[DIFX_MESSAGE_PARAM_LENGTH];
	char str[MaxLineLength+1];
	typedef void (*sighandler_t)(int);
	int l, v, sock, inotify_fd, wd;
	struct timeval tv;
	fd_set readset;
	int maxfd;
	const char *vfastrConfigFile;

	signal(SIGINT, siginthand);

	difxMessageInit(-1, program);

	if(state->verbose > 0)
	{
		difxMessagePrint();
	}

	vfastrConfigFile = getenv("VFASTR_CONFIG_FILE");
	if(!vfastrConfigFile)
	{
		vfastrConfigFile = defaultConfigFile;
	}
	conf = newTransientDaemonConf();
	loadTransientDaemonConf(conf, vfastrConfigFile);
	if(state->verbose > 0)
	{
		printTransientDaemonConf(conf);
	}

	state->recorrQueue = new RecorrQueue(conf->recorrQueueFile);
	state->recorrQueue->load();

	snprintf(message, DIFX_MESSAGE_LENGTH, "%s starting.", program);
	difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_VERBOSE);

	sock = difxMessageReceiveOpen();

	inotify_fd = inotify_init();
	wd = inotify_add_watch(inotify_fd, vfastrConfigFile, IN_CLOSE_WRITE | IN_DELETE_SELF);
	
	maxfd = (sock > inotify_fd ? sock : inotify_fd) + 1;
	
	if(state->verbose > 0)
	{
		printf("inotify watch descriptor for %s is %d\n", vfastrConfigFile, wd);
	}

	fflush(stdout);

	/* Program event loop */
	for(;;)
	{
		FD_ZERO(&readset);
		FD_SET(sock, &readset);
		FD_SET(inotify_fd, &readset);
		tv.tv_sec = 1;
		tv.tv_usec = 0;
		
		if(state->selfTest > 0)
		{
			state->selfTest--;
			v = handleMessage(testMessage, state, conf);
			if(v)
			{
				fprintf(stderr, "Error=%d handling command '%s'.\n", v, testMessage);
			}
		}

		v = select(maxfd, &readset, 0, 0, &tv);
		if(v < 0)
		{
			if(!die)
			{
				fprintf(stderr, "Error: select failed\n");
			}

			break;
		}
		if(die)
		{
			if(state->verbose > 0)
			{
				fprintf(stderr, "%s caught sigint.  Will die now.\n", program);
			}

			FD_ZERO(&readset);

			break;
		}
		if(v == 0)
		{
			usleep(100000);

			continue;
		}
			
		if(FD_ISSET(sock, &readset))
		{
			from[0] = 0;
			l = difxMessageReceive(sock, message, DIFX_MESSAGE_LENGTH-1, from);
			if(l > 0)
			{
				v = handleMessage(message, state, conf);
				if(v)
				{
					fprintf(stderr, "Error=%d handling command '%s'.\n", v, message);
				}
			}
		}
		if(FD_ISSET(inotify_fd, &readset))
		{
			const struct inotify_event *ie;
			l = read(inotify_fd, str, MaxLineLength);
			if(l > 0)
			{
				str[l] = 0;
				ie = (const struct inotify_event *)str;
				if(state->verbose > 1)
				{
					printf("inotify_event: %d %u\n", ie->wd, ie->mask);
				}
				loadTransientDaemonConf(conf, vfastrConfigFile);
				if(state->verbose > 0)
				{
					printTransientDaemonConf(conf);
				}
			}
		}
	}

	inotify_rm_watch(inotify_fd, wd);

	snprintf(message, DIFX_MESSAGE_LENGTH, "%s stopping", program);
	difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_VERBOSE);

	return 0;
}

int main(int argc, char **argv)
{
	int a;
	TransientDaemonState *state;

	/* Don't let children become zombies */
	signal(SIGCHLD, SIG_IGN);

	state = new TransientDaemonState;

	for(a = 1; a < argc; a++)
	{
		if(argv[a][0] == '-')
		{
			if(strcmp(argv[a], "--verbose") == 0 ||
				strcmp(argv[a], "-v") == 0)
			{
				state->verbose++;
			}
			else if(strcmp(argv[a], "--quiet") == 0 ||
				strcmp(argv[a], "-q") == 0)
			{
				state->verbose--;
			}
			else if(strcmp(argv[a], "--help") == 0 ||
				strcmp(argv[a], "-h") == 0)
			{
				return usage(argv[0]);
			}
			else if(strcmp(argv[a], "--test") == 0 ||
				strcmp(argv[a], "-t") == 0)
			{
				state->selfTest = 1;
			}
			else if(strcmp(argv[a], "--disable") == 0 ||
				strcmp(argv[a], "-d") == 0)
			{
				state->startEnable = 0;
			}
			else
			{
				fprintf(stderr, "I'm not sure what to do with command line argument '%s'.\n",
					argv[a]);

				return 0;
			}
		}
	}

	printf("Starting %s version %s verdate %s\n\n", program, version, verdate);

	state->print();

	transientdaemon(state);

	delete state;

	return EXIT_SUCCESS;
}

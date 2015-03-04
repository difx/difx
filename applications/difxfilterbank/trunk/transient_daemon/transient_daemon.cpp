#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <difxmessage.h>
#include <difxio/parsedifx.h>
#include <sys/inotify.h>
#include <sys/select.h>
#include <sys/statfs.h>
#include <fcntl.h>
#include <arpa/inet.h>
#include "recorr.h"

const char program[] = "transient_daemon";
const char author[]  = "Walter Brisken";
const char version[] = "0.4";
const char verdate[] = "2012 Feb 02";

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
const int defaultTCPPort = 31000;


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
	time_t startTime;
	char lastCommand[CommandLength];
	char hostname[DIFX_MESSAGE_PARAM_LENGTH];
	RecorrQueue *recorrQueue;

	TransientDaemonState();
	~TransientDaemonState();
	void fprint(FILE *out) const;
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
	int TCPPort;
    char *approvedJobs;
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
	time(&startTime);
}

TransientDaemonState::~TransientDaemonState()
{
	if(recorrQueue)
	{
		delete recorrQueue;

		recorrQueue = 0;
	}
}

void TransientDaemonState::fprint(FILE *out) const
{
	char timeStr[100];
	struct tm timeinfo;
	int l;

	localtime_r(&startTime, &timeinfo);
	asctime_r(&timeinfo, timeStr);
	l = strlen(timeStr);
	if(l > 0)
	{
		timeStr[l-1] = 0;
	}

	fprintf(out, "TransientDaemonState [%s]:\n", hostname);
	fprintf(out, "  startTime=%d # %s\n", static_cast<int>(startTime), timeStr);
	fprintf(out, "  verbose=%d\n", verbose);
	fprintf(out, "  selfTest=%d\n", selfTest);
	fprintf(out, "  startEnable=%d\n", startEnable);
	fprintf(out, "  nLaunch=%d\n", nLaunch);
	fprintf(out, "  last command=%s\n", lastCommand);

	fflush(out);
}

void TransientDaemonState::print() const
{
	fprint(stdout);
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

static int setNonBlocking(int sock)
{
	int opts;

	opts = fcntl(sock, F_GETFL);
	if(opts < 0)
	{
		return -1;
	}

	opts = (opts | O_NONBLOCK);
	if(fcntl(sock, F_SETFL, opts) < 0)
	{
		return -1;
	}

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
int diskSpaceIsAvailable(const TransientDaemonConf *conf)
{
	struct statfs fs;
	int res;

	res = statfs(conf->outputPath,&fs);
	if(res==0)
	{
		long long bytes_available,min_required;

		min_required = 1000000000LL*conf->min_disk_space_GB;
		bytes_available = static_cast<long long>(fs.f_bsize)*static_cast<long long>(fs.f_bavail);
		printf("Available disk space: %lld GB. Min required: %d GB\n",
		bytes_available/1000000000,conf->min_disk_space_GB);
		if(bytes_available > min_required)
		{
			return 1;
		}
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
	conf->TCPPort = defaultTCPPort;
	conf->maxDispersionDelay = defaultMaxDispersionDelay;
    conf->approvedJobs = NULL;

	return conf;
}

void deleteTransientDaemonConf(TransientDaemonConf *conf)
{
    if (conf->approvedJobs != NULL) free(conf->approvedJobs);
	free(conf);
}

// A is key, B is value
int setTransientDaemonConf(TransientDaemonConf *conf, const char *A, const char *B)
{
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
	else if(strcmp(A, "TCP_port") == 0)
	{
		conf->TCPPort = atoi(B);
	}
	else if(strcmp(A, "approved_jobs") == 0)
	{
        if(conf->approvedJobs != NULL) free(conf->approvedJobs);
		conf->approvedJobs = strdup(B);
	}
	else
	{
		return -1;
	}

	return 0;
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

	for(int l = 1;; ++l)
	{
		fgets(line, DIFX_MESSAGE_COMMENT_LENGTH-1, in);
		if(feof(in))
		{
			break;
		}
		for(int i = 0; line[i]; ++i)
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
		setTransientDaemonConf(conf, A, B);
	}

	fclose(in);

	return 0;
}

void fprintTransientDaemonConf(FILE *out, const TransientDaemonConf *conf)
{
	fprintf(out, "TransientDaemonConf [%p]\n", conf);
	fprintf(out, "  vfastr_enable = %d\n", conf->vfastrEnable);
	fprintf(out, "  min_disk_space_GB = %d\n", conf->min_disk_space_GB);
	fprintf(out, "  detection_threshold = %f\n", conf->detectionThreshold);
	fprintf(out, "  output_path = %s\n", conf->outputPath);
	fprintf(out, "  difx_sta_channels = %d\n", conf->difxStaChannels);
	fprintf(out, "  online_training_enable = %d\n", conf->onlineTrainingEnable);
	fprintf(out, "  archive_dedispersed = %d\n", conf->archiveDedispersed);
	fprintf(out, "  archive_pulses = %d\n", conf->archivePulses);
	fprintf(out, "  archive_merged = %d\n", conf->archiveMerged);
	fprintf(out, "  archive_scores = %d\n", conf->archiveScores);
	fprintf(out, "  archive_filterbank = %d # %s\n", conf->archiveFilterbank, archiveModeName[conf->archiveFilterbank]);
	fprintf(out, "  stub_pipeline = %d\n", conf->stubPipeline);
	fprintf(out, "  recorr_queue_file = %s\n", conf->recorrQueueFile);
	fprintf(out, "  recorr_threshold = %f\n", conf->recorrThreshold);
	fprintf(out, "  dmgen_program = %s\n", conf->dmgenProgram);
	fprintf(out, "  min_search_dm = %f\n", conf->minDM);
	fprintf(out, "  max_search_dm = %f\n", conf->maxDM);
	fprintf(out, "  negative_dm_sparsity = %d # sparsity factor\n", conf->negDM);
	fprintf(out, "  max_dm_values = %d # including negative\n", conf->nDM);
	fprintf(out, "  dm_delta_t = %f # ms\n", conf->tDM);
	fprintf(out, "  max_dispersion_delay = %f # sec\n", conf->maxDispersionDelay);
	fprintf(out, "  TCP_port = %d\n", conf->TCPPort);
    fprintf(out, "  Approved jobs: %s\n",conf->approvedJobs);

	fflush(out);
}

void printTransientDaemonConf(const TransientDaemonConf *conf)
{
	fprintTransientDaemonConf(stdout, conf);
}

void siginthand(int j)
{
	die = 1;
}

static void generateIdentifier(const char *inputfile, int myID, char *identifier)
{
	int l, s=0;

	for(int i = 0; inputfile[i]; ++i)
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
	for(int i = l-1; i > 0; --i)
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
	for(int i = 0; i < nFreq; ++i)
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
		system(command);
		
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

/* look for approved job codes */
static int isApprovedJob(const char  *approvedJobs, const char *id) {
    char *job=NULL,*jobs=NULL,*p,*id_upper;
    int result = 0;

    if (approvedJobs==NULL || approvedJobs[0] == '\0') return 1;    // empty approved list means approve everything

    /* make an uppercase job ID */
    id_upper = strdup(id);
    p=id_upper;
    while (*p != '\0') {
        *p = toupper(*p);
        p++;
    }
    /* copy approved job IDs and make upper case */
    jobs=strdup(approvedJobs);
    p=jobs;
    while (*p != '\0') {
        *p = toupper(*p);
        p++;
    }
    job=jobs;
    while ((p=strpbrk(job,",")) !=NULL) {
        /* terminate job string */
        *p='\0';

        /* check for match */
        if(strncmp(job,id_upper,strlen(id_upper))==0) result=1;

        /* move to next */
        job = p+1;
    }

    /* check remaining job name */
    if(strncmp(job,id_upper,strlen(id_upper))==0) result=1;

    printf("Job ID %s is %s found in approved jobs: <%s>\n",id,(result ?  "": "NOT"),approvedJobs);

    if (jobs!=NULL) free(jobs);
    if (id_upper!=NULL) free(id_upper);

    return result;
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
            time_t thetime;
            thetime = time(NULL);
            printf("Got START message for job ID %s at %s",G.identifier,ctime(&thetime));

            /* check for approved job codes */
            if(!isApprovedJob(conf->approvedJobs,G.identifier)) {
                printf("Job ID %s is not approved. Not starting dispatcher.\n",G.identifier);
                fflush(stdout);
                break;
            }

 			if(!diskSpaceIsAvailable(conf))
			{
 				printf("Received start message, but not enough space is left on disk. Ignoring\n");
                fflush(stdout);
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
				++state->nLaunch;
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
				++state->nLaunch;
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

int handleTCP(TransientDaemonState *state, TransientDaemonConf *conf, int sock, FILE *sockfd)
{
	int n, v;
	char message[DIFX_MESSAGE_LENGTH];
	char A[100], B[100], C[100];

	fflush(stdout);

	v = recv(sock, message, DIFX_MESSAGE_LENGTH-1, 0);
	if(v <= 0)
	{
		return -1;
	}
	message[v] = 0;

	n = sscanf(message, "%s %s %s", A, B, C);

	if(strcmp(A, "get") == 0 && n == 2)
	{
		if(strcmp(B, "state") == 0)
		{
			state->fprint(sockfd);
		}
		else if(strcmp(B, "conf") == 0)
		{
			fprintTransientDaemonConf(sockfd, conf);
		}
		else if(strcmp(B, "program") == 0)
		{
			fprintf(sockfd, "%s ver. %s   %s\n", program, version, verdate);
			fflush(sockfd);
		}
	}
	else if(strcmp(A, "set") == 0 && n == 3)
	{
		if(strcmp(B, "verbose") == 0)
		{
			state->verbose = atoi(C);
			fprintf(sockfd, "verbose = %d\n", state->verbose);
		}
		else if(strcmp(B, "enable") == 0)
		{
			state->startEnable = atoi(C);
			fprintf(sockfd, "startEnable = %d\n", state->startEnable);
		}
		else
		{
			v = setTransientDaemonConf(conf, B, C);
			if(v == 0)
			{
				fprintf(sockfd, "%s = %s\n", B, C);
			}
			else
			{
				fprintf(sockfd, "Error: unknown parameter %s\n", B);
			}
		}
		fflush(sockfd);
	}
	else if(strcmp(A, "exit") == 0)
	{
		fprintf(sockfd, "Bye.\n");
		fflush(sockfd);

		return 1;
	}
	else
	{
		fprintf(sockfd, "Unrecognized command.\n");
		fflush(sockfd);
	}

	return 0;
}

/* FIXME: IPv6 non-compliant code lurks here */
int transientdaemon(TransientDaemonState *state)
{
	TransientDaemonConf *conf;
	const int MaxLineLength = 512;
	const int MaxConnections = 8;
	char message[DIFX_MESSAGE_LENGTH];
	char from[DIFX_MESSAGE_PARAM_LENGTH];
	char str[MaxLineLength+1];
	typedef void (*sighandler_t)(int);
	int l, v, sock, inotify_fd, wd;
	struct timeval tv;
	fd_set readset;
	int maxfd;
	int acceptSock;
	int clientSocks[MaxConnections];
	FILE *clientFDs[MaxConnections];
	const int reuse_addr = 1;
	struct sockaddr_in server_address;
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

	for(int c = 0; c < MaxConnections; ++c)
	{
		clientSocks[c] = -1;
	}
	acceptSock = socket(AF_INET, SOCK_STREAM, 0);
	if(acceptSock < 0)
	{
		printf("acceptSock creation failed.\n");
	}
	else
	{
		setsockopt(acceptSock, SOL_SOCKET, SO_REUSEADDR, &reuse_addr, sizeof(reuse_addr));

		if(setNonBlocking(acceptSock) < 0)
		{
			printf("Cannot non-block acceptSock\n");
			close(acceptSock);
			acceptSock = -1;
		}
		else
		{
			memset((char *)(&server_address), 0, sizeof(server_address));
			server_address.sin_family = AF_INET;
			server_address.sin_addr.s_addr = htonl(INADDR_ANY);
			server_address.sin_port = htons(conf->TCPPort);
			if(bind(acceptSock, (struct sockaddr *)(&server_address), sizeof(server_address)) < 0 )
			{
				printf("Cannot bind acceptSock to port %d\n", conf->TCPPort);
				printf("Possibly another %s is running\n", program);
				close(acceptSock);
				acceptSock = -1;
			}
			else
			{
				listen(acceptSock, MaxConnections);
			}
		}
	}

	inotify_fd = inotify_init();
	wd = inotify_add_watch(inotify_fd, vfastrConfigFile, IN_CLOSE_WRITE | IN_DELETE_SELF);
	
	maxfd = sock > inotify_fd ? sock : inotify_fd;
	if(acceptSock > maxfd)
	{
		maxfd = acceptSock;
	}

	if(state->verbose > 0)
	{
		printf("inotify watch descriptor for %s is %d\n", vfastrConfigFile, wd);
	}

	fflush(stdout);

	/* Program event loop */
	for(;;)
	{
		int fdCeiling = maxfd;

		FD_ZERO(&readset);
		FD_SET(sock, &readset);
		FD_SET(inotify_fd, &readset);
		if(acceptSock >= 0)
		{
			FD_SET(acceptSock, &readset);
		}
		for(int c = 0; c < MaxConnections; ++c)
		{
			if(clientSocks[c] > 0)
			{
				FD_SET(clientSocks[c], &readset);
				if(clientSocks[c] > fdCeiling)
				{
					fdCeiling = clientSocks[c];
				}
			}
		}
		tv.tv_sec = 1;
		tv.tv_usec = 0;
		++fdCeiling;

		if(state->selfTest > 0)
		{
			--state->selfTest;
			v = handleMessage(testMessage, state, conf);
			if(v)
			{
				fprintf(stderr, "Error=%d handling command '%s'.\n", v, testMessage);
			}
		}

		v = select(fdCeiling, &readset, 0, 0, &tv);
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
		if(FD_ISSET(acceptSock, &readset))
		{
			int newSock = accept(acceptSock, 0, 0);

			if(newSock < 0)
			{
				printf("Error accepting acceptSock\n");
				sleep(1);
			}
			else
			{
				for(int c = 0; c < MaxConnections; ++c)
				{
					if(clientSocks[c] == -1)
					{
						clientSocks[c] = newSock;
						clientFDs[c] = fdopen(clientSocks[c], "w");
						newSock = -1;
						printf("New connection %d to slot %d of %d\n", clientSocks[c], c, MaxConnections);
						break;
					}

				}
				if(newSock != -1)
				{
					printf("No room for new TCP connection\n");
					close(newSock);
				}
			}	
		}
		for(int c = 0; c < MaxConnections; ++c)
		{
			if(clientSocks[c] > 0 && FD_ISSET(clientSocks[c], &readset))
			{
				int v;

				v = handleTCP(state, conf, clientSocks[c], clientFDs[c]);
				if(v < 0)  // Connection closed?
				{
					close(clientSocks[c]);
					fclose(clientFDs[c]);
					clientSocks[c] = -1;
					printf("Connection on slot %d closed\n", c);
				}
				if(v == 1)	// close requested remotely
				{
					close(clientSocks[c]);
					fclose(clientFDs[c]);
					clientSocks[c] = -1;
					printf("Connection on slot %d exited\n", c);
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

	for(a = 1; a < argc; ++a)
	{
		if(argv[a][0] == '-')
		{
			if(strcmp(argv[a], "--verbose") == 0 ||
				strcmp(argv[a], "-v") == 0)
			{
				++state->verbose;
			}
			else if(strcmp(argv[a], "--quiet") == 0 ||
				strcmp(argv[a], "-q") == 0)
			{
				--state->verbose;
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
